-module(aircraft_alert_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([install_mnesia/0]).

start(_StartType, _StartArgs) ->
    %% Ensure Mnesia is started
    ensure_mnesia_started(),

    %% Initialize tables if they don't exist
    init_mnesia_tables(),

    %% Start the supervision tree
    case aircraft_alert_sup:start_link() of
        {ok, Pid} ->
            io:format("Aircraft Alert System started successfully~n"),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%% Internal functions

ensure_mnesia_started() ->
    case mnesia:system_info(is_running) of
        yes -> ok;
        no ->
            mnesia:start(),
            ok;
        starting ->
            timer:sleep(100),
            ensure_mnesia_started();
        stopping ->
            timer:sleep(100),
            ensure_mnesia_started()
    end.

init_mnesia_tables() ->
    %% Create tables if they don't exist
    Tables = [regions, alert_rules, recent_sightings, config],
    ExistingTables = mnesia:system_info(tables),

    lists:foreach(fun(Table) ->
        case lists:member(Table, ExistingTables) of
            false -> create_table(Table);
            true -> ok
        end
    end, Tables),

    %% Wait for tables to be ready
    mnesia:wait_for_tables(Tables, 5000).

create_table(regions) ->
    mnesia:create_table(regions,
        [{attributes, [id, name, lat, lon, radius_miles, active]},
         {disc_copies, [node()]},
         {type, set}]);

create_table(alert_rules) ->
    mnesia:create_table(alert_rules,
        [{attributes, [id, region_id, aircraft_types, min_altitude_ft,
                      max_altitude_ft, max_distance_miles, priority]},
         {disc_copies, [node()]},
         {type, bag}]);

create_table(recent_sightings) ->
    mnesia:create_table(recent_sightings,
        [{attributes, [key, icao24, region_id, timestamp, notified,
                      lat, lon, altitude_ft, heading, callsign]},
         {disc_copies, [node()]},
         {type, set},
         {index, [icao24, timestamp]}]);

create_table(config) ->
    mnesia:create_table(config,
        [{attributes, [key, value]},
         {disc_copies, [node()]},
         {type, set}]).

%% Install Mnesia schema (run once before first use)
install_mnesia() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    init_mnesia_tables(),
    io:format("Mnesia schema installed successfully~n"),
    ok.
