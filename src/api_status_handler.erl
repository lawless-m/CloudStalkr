-module(api_status_handler).
-export([init/2]).

init(Req0, State) ->
    Status = get_system_status(),
    Req = json_response(200, Status, Req0),
    {ok, Req, State}.

get_system_status() ->
    %% Get process status
    Processes = #{
        config_server => is_alive(config_server),
        aircraft_poller => is_alive(aircraft_poller),
        alert_processor => is_alive(alert_processor),
        notification_sender => is_alive(notification_sender),
        web_server => is_alive(web_server)
    },

    %% Get database stats
    DbStats = #{
        regions => count_records(regions),
        alert_rules => count_records(alert_rules),
        recent_sightings => count_records(recent_sightings)
    },

    #{
        status => <<"running">>,
        processes => Processes,
        database => DbStats,
        is_active => config_server:is_within_active_hours(),
        uptime_seconds => get_uptime()
    }.

is_alive(Name) ->
    case whereis(Name) of
        undefined -> false;
        Pid -> is_process_alive(Pid)
    end.

count_records(Table) ->
    try
        mnesia:table_info(Table, size)
    catch
        _:_ -> 0
    end.

get_uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    UpTime div 1000.

json_response(Status, Data, Req) ->
    Body = jsx:encode(Data),
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req).
