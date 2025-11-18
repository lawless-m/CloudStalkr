-module(alert_processor).
-behaviour(gen_server).

%% API
-export([start_link/0, process_aircraft/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    alert_queue = queue:new(),
    processing = false
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

process_aircraft(Aircraft, RegionId) ->
    gen_server:cast(?SERVER, {process_aircraft, Aircraft, RegionId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    io:format("Alert processor started~n"),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({process_aircraft, Aircraft, RegionId}, State) ->
    NewState = handle_aircraft(Aircraft, RegionId, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_aircraft(Aircraft, RegionId, State) ->
    %% Get region details
    case mnesia:dirty_read(regions, RegionId) of
        [] ->
            State;
        [{regions, _Id, Name, RegionLat, RegionLon, _Radius, _Active}] ->
            Region = #{
                id => RegionId,
                name => Name,
                lat => RegionLat,
                lon => RegionLon
            },

            %% Get alert rules for this region
            AlertRules = config_server:get_alert_rules(RegionId),

            %% Check each rule
            lists:foreach(fun(Rule) ->
                check_alert_rule(Aircraft, Region, Rule)
            end, AlertRules),

            State
    end.

check_alert_rule(Aircraft, Region, Rule) ->
    #{lat := AircraftLat, lon := AircraftLon,
      altitude_ft := Altitude, icao24 := Icao24,
      heading := Heading, speed_knots := Speed} = Aircraft,

    #{id := RegionId, lat := RegionLat, lon := RegionLon} = Region,

    %% Calculate distance from region center
    Distance = geo_utils:haversine_distance(RegionLat, RegionLon,
                                            AircraftLat, AircraftLon),

    %% Get rule parameters
    MaxDistance = maps:get(max_distance_miles, Rule),
    MinAlt = maps:get(min_altitude_ft, Rule),
    MaxAlt = maps:get(max_altitude_ft, Rule),
    AircraftTypes = maps:get(aircraft_types, Rule),
    Priority = maps:get(priority, Rule),

    %% Check if aircraft matches rule conditions
    MatchesDistance = Distance =< MaxDistance,
    MatchesAltitude = (Altitude >= MinAlt) andalso (Altitude =< MaxAlt),

    %% Check aircraft type
    MatchesType = case AircraftTypes of
        any -> aircraft_types:is_military(
                 maps:get(callsign, Aircraft, ""),
                 maps:get(category, Aircraft, ""));
        Types when is_list(Types) ->
            TypeCode = maps:get(type_code, Aircraft, ""),
            Category = maps:get(category, Aircraft, ""),
            lists:any(fun(T) ->
                T =:= TypeCode orelse T =:= Category
            end, Types)
    end,

    %% Check if we should alert
    ShouldAlert = MatchesDistance andalso MatchesAltitude andalso MatchesType,

    case ShouldAlert of
        true ->
            %% Check for duplicate alerts
            case has_recent_alert(Icao24, RegionId) of
                false ->
                    %% Send alert!
                    send_alert(Aircraft, Region, Distance, Priority, Heading, Speed);
                true ->
                    %% Already alerted recently, skip
                    ok
            end;
        false ->
            ok
    end.

has_recent_alert(Icao24, RegionId) ->
    %% Get duplicate window from config
    WindowMinutes = case config_server:get_config(duplicate_alert_window_minutes) of
        undefined -> 15;
        Value -> Value
    end,

    %% Calculate cutoff time
    CutoffTime = erlang:system_time(millisecond) - (WindowMinutes * 60 * 1000),

    %% Check for recent sightings that were notified
    MatchSpec = [{
        {recent_sightings, {'$1', '$2', '$3'}, '$1', '$2', '_', '$4', '_', '_', '_', '_', '_'},
        [{'=:=', '$1', Icao24},
         {'=:=', '$2', RegionId},
         {'>', '$3', CutoffTime},
         {'=:=', '$4', true}],
        ['$_']
    }],

    case mnesia:dirty_select(recent_sightings, MatchSpec) of
        [] -> false;
        _ -> true
    end.

send_alert(Aircraft, Region, Distance, Priority, Heading, Speed) ->
    #{icao24 := Icao24, region_id := RegionId} = maps:merge(Aircraft,
        #{region_id => maps:get(id, Region)}),

    %% Format alert message
    {Title, Message} = format_alert_message(Aircraft, Region, Distance, Heading, Speed),

    %% Get tags for notification
    Tags = aircraft_types:get_tags(Aircraft),

    %% Send notification
    notification_sender:send_notification(Title, Message, Priority, Tags),

    %% Mark this sighting as notified
    mark_as_notified(Icao24, RegionId),

    io:format("ALERT: ~s - ~s~n", [Title, Message]).

format_alert_message(Aircraft, Region, Distance, Heading, _Speed) ->
    #{callsign := Callsign, altitude_ft := Altitude,
      lat := AircraftLat, lon := AircraftLon,
      type_code := TypeCode} = Aircraft,

    #{name := RegionName, lat := RegionLat, lon := RegionLon} = Region,

    %% Get friendly aircraft name
    AircraftName = aircraft_types:get_aircraft_type_name(TypeCode),

    %% Calculate direction from region to aircraft
    Bearing = geo_utils:bearing(RegionLat, RegionLon, AircraftLat, AircraftLon),
    Direction = geo_utils:format_direction(Bearing),

    %% Determine if approaching
    IsApproaching = geo_utils:is_approaching(AircraftLat, AircraftLon, Heading,
                                             RegionLat, RegionLon, 100),

    ApproachingText = case IsApproaching of
        true -> " (approaching)";
        false -> " (departing)"
    end,

    %% Format callsign
    CallsignText = case Callsign of
        "" -> "";
        _ -> lists:flatten(io_lib:format(" '~s'", [Callsign]))
    end,

    %% Build title
    Title = case AircraftName of
        "Aircraft" -> lists:flatten(io_lib:format("~s Overhead!", [TypeCode]));
        Name -> lists:flatten(io_lib:format("~s Overhead!", [Name]))
    end,

    %% Build message
    Message = lists:flatten(
        io_lib:format("~s~s at ~wft, ~.1f miles ~s, heading ~w°~s - GO NOW!",
                     [AircraftName, CallsignText, Altitude,
                      Distance, Direction, round(Heading), ApproachingText])),

    {Title, Message}.

mark_as_notified(Icao24, RegionId) ->
    %% Find recent sightings for this aircraft in this region
    MatchSpec = [{
        {recent_sightings, '$1', '$2', '$3', '$4', '_', '$5', '$6', '$7', '$8', '$9'},
        [{'=:=', '$2', Icao24},
         {'=:=', '$3', RegionId}],
        ['$_']
    }],

    Sightings = mnesia:dirty_select(recent_sightings, MatchSpec),

    %% Update each to mark as notified
    lists:foreach(fun({recent_sightings, Key, Icao, RId, Timestamp,
                       _Notified, Lat, Lon, Alt, Head, Call}) ->
        Updated = {recent_sightings, Key, Icao, RId, Timestamp,
                   true, Lat, Lon, Alt, Head, Call},
        mnesia:dirty_write(Updated)
    end, Sightings).
