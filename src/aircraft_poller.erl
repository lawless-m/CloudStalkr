-module(aircraft_poller).
-behaviour(gen_server).

%% API
-export([start_link/0, poll_now/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ADSB_BASE_URL, "https://adsbexchange.com/api/aircraft/json").

-record(state, {
    timer_ref = undefined,
    last_poll = undefined,
    api_errors = 0,
    backoff_time = 1000  % Start with 1 second backoff
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

poll_now() ->
    gen_server:cast(?SERVER, poll_now).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Start inets for HTTP client
    inets:start(),
    ssl:start(),

    %% Schedule first poll
    State = schedule_next_poll(#state{}),

    io:format("Aircraft poller started~n"),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(poll_now, State) ->
    NewState = do_poll(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(poll, State) ->
    NewState = do_poll(State),
    NewState2 = schedule_next_poll(NewState),
    {noreply, NewState2};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.timer_ref of
        undefined -> ok;
        TimerRef -> erlang:cancel_timer(TimerRef)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

schedule_next_poll(State) ->
    %% Cancel existing timer if any
    case State#state.timer_ref of
        undefined -> ok;
        OldRef -> erlang:cancel_timer(OldRef)
    end,

    %% Get polling interval from config
    Interval = case config_server:get_config(polling_interval_seconds) of
        undefined -> 30;
        Value -> Value
    end,

    %% Use backoff time if there were errors
    ActualInterval = if
        State#state.api_errors > 0 ->
            min(State#state.backoff_time, Interval * 1000 * 10);  % Max 10x interval
        true ->
            Interval * 1000
    end,

    TimerRef = erlang:send_after(ActualInterval, self(), poll),
    State#state{timer_ref = TimerRef}.

do_poll(State) ->
    %% Check if we're within active hours
    case config_server:is_within_active_hours() of
        false ->
            %% Outside active hours, skip polling
            State;
        true ->
            %% Get active regions
            Regions = config_server:get_active_regions(),

            %% Poll each region
            Results = lists:map(fun(Region) ->
                poll_region(Region)
            end, Regions),

            %% Count errors
            Errors = length([R || R <- Results, element(1, R) =:= error]),

            %% Update state based on results
            NewState = if
                Errors > 0 ->
                    NewBackoff = min(State#state.backoff_time * 2, 60000),  % Max 60s
                    io:format("Polling errors: ~p, backing off to ~pms~n",
                             [Errors, NewBackoff]),
                    State#state{
                        api_errors = State#state.api_errors + 1,
                        backoff_time = NewBackoff,
                        last_poll = erlang:timestamp()
                    };
                true ->
                    State#state{
                        api_errors = 0,
                        backoff_time = 1000,
                        last_poll = erlang:timestamp()
                    }
            end,

            NewState
    end.

poll_region(Region) ->
    #{id := RegionId, lat := Lat, lon := Lon, radius_miles := RadiusMiles} = Region,

    case query_adsb_exchange(Lat, Lon, RadiusMiles) of
        {ok, Aircraft} ->
            %% Process each aircraft
            lists:foreach(fun(A) ->
                store_sighting(A, RegionId),
                %% Send to alert processor
                alert_processor:process_aircraft(A, RegionId)
            end, Aircraft),
            {ok, length(Aircraft)};

        {error, Reason} ->
            io:format("Error polling region ~p: ~p~n", [RegionId, Reason]),
            {error, Reason}
    end.

query_adsb_exchange(Lat, Lon, RadiusMiles) ->
    %% Convert miles to nautical miles for API
    RadiusNm = geo_utils:miles_to_nautical_miles(RadiusMiles),

    %% Build URL
    Url = lists:flatten(
        io_lib:format("~s/lat/~.6f/lon/~.6f/dist/~.1f/",
                     [?ADSB_BASE_URL, Lat, Lon, RadiusNm])),

    %% Make HTTP request
    case httpc:request(get, {Url, []}, [{timeout, 10000}], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            %% Parse JSON
            case jsx:decode(list_to_binary(Body), [return_maps]) of
                #{<<"ac">> := AircraftList} ->
                    ParsedAircraft = lists:filtermap(fun parse_aircraft/1, AircraftList),
                    {ok, ParsedAircraft};
                _ ->
                    {ok, []}
            end;

        {ok, {{_, StatusCode, _}, _Headers, _Body}} ->
            {error, {http_error, StatusCode}};

        {error, Reason} ->
            {error, Reason}
    end.

parse_aircraft(AircraftJson) when is_map(AircraftJson) ->
    try
        %% Extract required fields
        Icao24 = maps:get(<<"hex">>, AircraftJson, <<>>),
        Lat = maps:get(<<"lat">>, AircraftJson, undefined),
        Lon = maps:get(<<"lon">>, AircraftJson, undefined),

        %% Skip if no position
        case {Lat, Lon} of
            {undefined, _} -> false;
            {_, undefined} -> false;
            _ ->
                Aircraft = #{
                    icao24 => Icao24,
                    lat => Lat,
                    lon => Lon,
                    altitude_ft => maps:get(<<"alt_baro">>, AircraftJson, 0),
                    heading => maps:get(<<"track">>, AircraftJson, 0),
                    callsign => string:trim(
                        binary_to_list(maps:get(<<"flight">>, AircraftJson, <<>>))),
                    category => maps:get(<<"category">>, AircraftJson, <<>>),
                    type_code => maps:get(<<"t">>, AircraftJson, <<>>),
                    speed_knots => maps:get(<<"gs">>, AircraftJson, 0)
                },
                {true, Aircraft}
        end
    catch
        _:_ ->
            false
    end;
parse_aircraft(_) ->
    false.

store_sighting(Aircraft, RegionId) ->
    #{icao24 := Icao24, lat := Lat, lon := Lon,
      altitude_ft := Altitude, heading := Heading,
      callsign := Callsign} = Aircraft,

    %% Create unique key for this sighting
    Key = {Icao24, RegionId, erlang:system_time(millisecond)},

    Record = {recent_sightings, Key, Icao24, RegionId, erlang:timestamp(),
              false, Lat, Lon, Altitude, Heading, Callsign},

    mnesia:dirty_write(Record),

    %% Clean up old sightings (older than 24 hours)
    cleanup_old_sightings().

cleanup_old_sightings() ->
    %% Delete sightings older than 24 hours
    CutoffTime = erlang:system_time(millisecond) - (24 * 60 * 60 * 1000),

    MatchSpec = [{
        {recent_sightings, {'$1', '_', '$2'}, '_', '_', '_', '_', '_', '_', '_', '_', '_'},
        [{'<', '$2', CutoffTime}],
        ['$1']
    }],

    OldKeys = mnesia:dirty_select(recent_sightings, MatchSpec),
    lists:foreach(fun(Key) ->
        mnesia:dirty_delete(recent_sightings, Key)
    end, OldKeys).
