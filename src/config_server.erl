-module(config_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_config/1, set_config/2, get_active_regions/0,
         is_within_active_hours/0, reload_config/0, get_alert_rules/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CONFIG_FILE, "config/aircraft_alert.config").

-record(state, {
    config = #{},
    regions = [],
    alert_rules = []
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_config(Key) ->
    gen_server:call(?SERVER, {get_config, Key}).

set_config(Key, Value) ->
    gen_server:call(?SERVER, {set_config, Key, Value}).

get_active_regions() ->
    gen_server:call(?SERVER, get_active_regions).

get_alert_rules(RegionId) ->
    gen_server:call(?SERVER, {get_alert_rules, RegionId}).

is_within_active_hours() ->
    gen_server:call(?SERVER, is_within_active_hours).

reload_config() ->
    gen_server:call(?SERVER, reload_config).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Load configuration from file
    {ok, State} = load_config_from_file(),

    %% Load or initialize config in Mnesia
    sync_config_to_mnesia(State),

    io:format("Configuration server started~n"),
    {ok, State}.

handle_call({get_config, Key}, _From, State) ->
    Value = maps:get(Key, State#state.config, undefined),
    {reply, Value, State};

handle_call({set_config, Key, Value}, _From, State) ->
    %% Update in memory
    NewConfig = maps:put(Key, Value, State#state.config),
    NewState = State#state{config = NewConfig},

    %% Update in Mnesia
    mnesia:dirty_write({config, Key, Value}),

    {reply, ok, NewState};

handle_call(get_active_regions, _From, State) ->
    ActiveRegions = lists:filter(fun(#{active := Active}) -> Active end,
                                 State#state.regions),
    {reply, ActiveRegions, State};

handle_call({get_alert_rules, RegionId}, _From, State) ->
    Rules = lists:filter(fun(#{region_id := RId}) -> RId =:= RegionId end,
                         State#state.alert_rules),
    {reply, Rules, State};

handle_call(is_within_active_hours, _From, State) ->
    Result = check_active_hours(State#state.config),
    {reply, Result, State};

handle_call(reload_config, _From, _State) ->
    {ok, NewState} = load_config_from_file(),
    sync_config_to_mnesia(NewState),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

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

load_config_from_file() ->
    case file:consult(?CONFIG_FILE) of
        {ok, [ConfigList]} ->
            %% Extract aircraft_alert application config
            AircraftConfig = proplists:get_value(aircraft_alert, ConfigList, []),

            %% Build config map
            Config = #{
                polling_interval_seconds => proplists:get_value(polling_interval_seconds, AircraftConfig, 30),
                active_hours => proplists:get_value(active_hours, AircraftConfig, {{7, 0}, {22, 0}}),
                active_days => proplists:get_value(active_days, AircraftConfig, all),
                ntfy_topic => proplists:get_value(ntfy_topic, AircraftConfig, "aircraft_alerts"),
                ntfy_server => proplists:get_value(ntfy_server, AircraftConfig, "https://ntfy.sh"),
                duplicate_alert_window_minutes => proplists:get_value(duplicate_alert_window_minutes, AircraftConfig, 15),
                weather_check_enabled => proplists:get_value(weather_check_enabled, AircraftConfig, false),
                min_visibility_km => proplists:get_value(min_visibility_km, AircraftConfig, 5.0),
                max_cloud_cover_percent => proplists:get_value(max_cloud_cover_percent, AircraftConfig, 90)
            },

            Regions = proplists:get_value(regions, AircraftConfig, []),
            AlertRules = proplists:get_value(alert_rules, AircraftConfig, []),

            State = #state{
                config = Config,
                regions = Regions,
                alert_rules = AlertRules
            },

            {ok, State};

        {error, enoent} ->
            %% No config file, use defaults
            io:format("Warning: Config file not found, using defaults~n"),
            {ok, #state{
                config = default_config(),
                regions = [],
                alert_rules = []
            }};

        {error, Reason} ->
            io:format("Error loading config file: ~p~n", [Reason]),
            {ok, #state{
                config = default_config(),
                regions = [],
                alert_rules = []
            }}
    end.

default_config() ->
    #{
        polling_interval_seconds => 30,
        active_hours => {{7, 0}, {22, 0}},
        active_days => all,
        ntfy_topic => "aircraft_alerts",
        ntfy_server => "https://ntfy.sh",
        duplicate_alert_window_minutes => 15,
        weather_check_enabled => false,
        min_visibility_km => 5.0,
        max_cloud_cover_percent => 90
    }.

sync_config_to_mnesia(State) ->
    %% Store config values in Mnesia
    maps:foreach(fun(Key, Value) ->
        mnesia:dirty_write({config, Key, Value})
    end, State#state.config),

    %% Store regions
    lists:foreach(fun(Region) ->
        Id = maps:get(id, Region),
        mnesia:dirty_write({regions,
                           Id,
                           maps:get(name, Region),
                           maps:get(lat, Region),
                           maps:get(lon, Region),
                           maps:get(radius_miles, Region),
                           maps:get(active, Region)})
    end, State#state.regions),

    %% Store alert rules
    lists:foreach(fun(Rule) ->
        Id = erlang:unique_integer([positive]),
        mnesia:dirty_write({alert_rules,
                           Id,
                           maps:get(region_id, Rule),
                           maps:get(aircraft_types, Rule),
                           maps:get(min_altitude_ft, Rule),
                           maps:get(max_altitude_ft, Rule),
                           maps:get(max_distance_miles, Rule),
                           maps:get(priority, Rule)})
    end, State#state.alert_rules).

check_active_hours(Config) ->
    case maps:get(active_hours, Config, undefined) of
        undefined -> true;
        {{StartH, StartM}, {EndH, EndM}} ->
            {_Date, {Hour, Min, _Sec}} = calendar:local_time(),
            StartMinutes = StartH * 60 + StartM,
            EndMinutes = EndH * 60 + EndM,
            CurrentMinutes = Hour * 60 + Min,

            %% Check if within active hours
            InHours = if
                StartMinutes =< EndMinutes ->
                    CurrentMinutes >= StartMinutes andalso CurrentMinutes =< EndMinutes;
                true ->
                    %% Wraps around midnight
                    CurrentMinutes >= StartMinutes orelse CurrentMinutes =< EndMinutes
            end,

            %% Check active days
            ActiveDays = maps:get(active_days, Config, all),
            InDays = case ActiveDays of
                all -> true;
                Days when is_list(Days) ->
                    DayOfWeek = calendar:day_of_the_week(element(1, calendar:local_time())),
                    DayName = day_number_to_name(DayOfWeek),
                    lists:member(DayName, Days)
            end,

            InHours andalso InDays
    end.

day_number_to_name(1) -> monday;
day_number_to_name(2) -> tuesday;
day_number_to_name(3) -> wednesday;
day_number_to_name(4) -> thursday;
day_number_to_name(5) -> friday;
day_number_to_name(6) -> saturday;
day_number_to_name(7) -> sunday.
