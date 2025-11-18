-module(api_rules_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Req = handle_request(Method, Req0),
    {ok, Req, State}.

handle_request(<<"GET">>, Req) ->
    Rules = get_all_rules(),
    json_response(200, Rules, Req);

handle_request(<<"POST">>, Req) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Rule = jsx:decode(Body, [return_maps]),

    case create_rule(Rule) of
        ok ->
            json_response(201, #{success => true}, Req1);
        {error, Reason} ->
            json_response(400, #{error => Reason}, Req1)
    end;

handle_request(<<"DELETE">>, Req) ->
    Id = binary_to_integer(cowboy_req:binding(id, Req)),
    delete_rule(Id),
    json_response(200, #{success => true}, Req);

handle_request(_, Req) ->
    json_response(405, #{error => <<"Method not allowed">>}, Req).

get_all_rules() ->
    Records = mnesia:dirty_match_object({alert_rules, '_', '_', '_', '_', '_', '_', '_'}),
    [rule_to_map(R) || R <- Records].

create_rule(Rule) ->
    Id = erlang:unique_integer([positive]),
    RegionId = binary_to_atom(maps:get(<<"region_id">>, Rule)),

    AircraftTypes = case maps:get(<<"aircraft_types">>, Rule) of
        <<"any">> -> any;
        Types when is_list(Types) -> [binary_to_list(T) || T <- Types]
    end,

    MinAlt = maps:get(<<"min_altitude_ft">>, Rule),
    MaxAlt = maps:get(<<"max_altitude_ft">>, Rule),
    MaxDist = maps:get(<<"max_distance_miles">>, Rule),
    Priority = maps:get(<<"priority">>, Rule),

    Record = {alert_rules, Id, RegionId, AircraftTypes, MinAlt, MaxAlt, MaxDist, Priority},
    mnesia:dirty_write(Record),
    ok.

delete_rule(Id) ->
    mnesia:dirty_delete(alert_rules, Id),
    ok.

rule_to_map({alert_rules, Id, RegionId, AircraftTypes, MinAlt, MaxAlt, MaxDist, Priority}) ->
    Types = case AircraftTypes of
        any -> <<"any">>;
        List -> [list_to_binary(T) || T <- List]
    end,

    #{
        id => Id,
        region_id => RegionId,
        aircraft_types => Types,
        min_altitude_ft => MinAlt,
        max_altitude_ft => MaxAlt,
        max_distance_miles => MaxDist,
        priority => Priority
    }.

json_response(Status, Data, Req) ->
    Body = jsx:encode(Data),
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req).
