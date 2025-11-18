-module(api_config_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Req = handle_request(Method, Req0),
    {ok, Req, State}.

handle_request(<<"GET">>, Req) ->
    Config = get_config(),
    json_response(200, Config, Req);

handle_request(<<"PUT">>, Req) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    ConfigMap = jsx:decode(Body, [return_maps]),

    update_config(ConfigMap),
    json_response(200, #{success => true}, Req1);

handle_request(_, Req) ->
    json_response(405, #{error => <<"Method not allowed">>}, Req).

get_config() ->
    #{
        polling_interval_seconds => config_server:get_config(polling_interval_seconds),
        active_hours => format_active_hours(config_server:get_config(active_hours)),
        ntfy_topic => list_to_binary(config_server:get_config(ntfy_topic)),
        ntfy_server => list_to_binary(config_server:get_config(ntfy_server)),
        duplicate_alert_window_minutes => config_server:get_config(duplicate_alert_window_minutes),
        is_within_active_hours => config_server:is_within_active_hours()
    }.

format_active_hours({{SH, SM}, {EH, EM}}) ->
    #{
        start => #{hour => SH, minute => SM},
        end_ => #{hour => EH, minute => EM}
    };
format_active_hours(_) ->
    null.

update_config(ConfigMap) ->
    maps:foreach(fun(Key, Value) ->
        AtomKey = binary_to_atom(Key),
        ActualValue = case AtomKey of
            ntfy_topic -> binary_to_list(Value);
            ntfy_server -> binary_to_list(Value);
            _ -> Value
        end,
        config_server:set_config(AtomKey, ActualValue)
    end, ConfigMap).

json_response(Status, Data, Req) ->
    Body = jsx:encode(Data),
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req).
