-module(api_sightings_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Req = handle_request(Method, Req0),
    {ok, Req, State}.

handle_request(<<"GET">>, Req) ->
    %% Get query parameters for filtering
    QsVals = cowboy_req:parse_qs(Req),
    Limit = case proplists:get_value(<<"limit">>, QsVals) of
        undefined -> 100;
        LimitBin -> binary_to_integer(LimitBin)
    end,

    Sightings = get_recent_sightings(Limit),
    json_response(200, Sightings, Req);

handle_request(_, Req) ->
    json_response(405, #{error => <<"Method not allowed">>}, Req).

get_recent_sightings(Limit) ->
    Records = mnesia:dirty_match_object({recent_sightings, '_', '_', '_', '_', '_', '_', '_', '_', '_', '_'}),

    %% Sort by timestamp (newest first) and limit
    Sorted = lists:sort(fun({_, _, _, _, _, T1, _, _, _, _, _}, {_, _, _, _, _, T2, _, _, _, _, _}) ->
        T1 >= T2
    end, Records),

    Limited = lists:sublist(Sorted, Limit),
    [sighting_to_map(S) || S <- Limited].

sighting_to_map({recent_sightings, _Key, Icao24, RegionId, Timestamp, Notified, Lat, Lon, Alt, Heading, Callsign}) ->
    #{
        icao24 => Icao24,
        region_id => RegionId,
        timestamp => timestamp_to_unix(Timestamp),
        notified => Notified,
        lat => Lat,
        lon => Lon,
        altitude_ft => Alt,
        heading => Heading,
        callsign => list_to_binary(Callsign)
    }.

timestamp_to_unix({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.

json_response(Status, Data, Req) ->
    Body = jsx:encode(Data),
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req).
