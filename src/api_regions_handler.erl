-module(api_regions_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Req = handle_request(Method, Req0),
    {ok, Req, State}.

%% GET /api/regions - List all regions
handle_request(<<"GET">>, Req) ->
    case cowboy_req:binding(id, Req) of
        undefined ->
            %% List all regions
            Regions = get_all_regions(),
            json_response(200, Regions, Req);
        Id ->
            %% Get specific region
            case get_region(binary_to_atom(Id)) of
                {ok, Region} ->
                    json_response(200, Region, Req);
                {error, not_found} ->
                    json_response(404, #{error => <<"Region not found">>}, Req)
            end
    end;

%% POST /api/regions - Create new region
handle_request(<<"POST">>, Req) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Region = jsx:decode(Body, [return_maps]),

    case create_region(Region) of
        ok ->
            json_response(201, #{success => true}, Req1);
        {error, Reason} ->
            json_response(400, #{error => Reason}, Req1)
    end;

%% PUT /api/regions/:id - Update region
handle_request(<<"PUT">>, Req) ->
    Id = cowboy_req:binding(id, Req),
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Region = jsx:decode(Body, [return_maps]),

    case update_region(binary_to_atom(Id), Region) of
        ok ->
            json_response(200, #{success => true}, Req1);
        {error, Reason} ->
            json_response(400, #{error => Reason}, Req1)
    end;

%% DELETE /api/regions/:id - Delete region
handle_request(<<"DELETE">>, Req) ->
    Id = cowboy_req:binding(id, Req),

    case delete_region(binary_to_atom(Id)) of
        ok ->
            json_response(200, #{success => true}, Req);
        {error, Reason} ->
            json_response(400, #{error => Reason}, Req)
    end;

handle_request(_, Req) ->
    json_response(405, #{error => <<"Method not allowed">>}, Req).

%% Internal functions

get_all_regions() ->
    Records = mnesia:dirty_match_object({regions, '_', '_', '_', '_', '_', '_'}),
    [region_to_map(R) || R <- Records].

get_region(Id) ->
    case mnesia:dirty_read(regions, Id) of
        [] -> {error, not_found};
        [{regions, Id, Name, Lat, Lon, Radius, Active}] ->
            {ok, #{
                id => Id,
                name => list_to_binary(Name),
                lat => Lat,
                lon => Lon,
                radius_miles => Radius,
                active => Active
            }}
    end.

create_region(Region) ->
    Id = binary_to_atom(maps:get(<<"id">>, Region)),
    Name = binary_to_list(maps:get(<<"name">>, Region)),
    Lat = maps:get(<<"lat">>, Region),
    Lon = maps:get(<<"lon">>, Region),
    Radius = maps:get(<<"radius_miles">>, Region),
    Active = maps:get(<<"active">>, Region, true),

    Record = {regions, Id, Name, Lat, Lon, Radius, Active},
    mnesia:dirty_write(Record),
    ok.

update_region(Id, Region) ->
    case mnesia:dirty_read(regions, Id) of
        [] -> {error, not_found};
        _ ->
            Name = binary_to_list(maps:get(<<"name">>, Region)),
            Lat = maps:get(<<"lat">>, Region),
            Lon = maps:get(<<"lon">>, Region),
            Radius = maps:get(<<"radius_miles">>, Region),
            Active = maps:get(<<"active">>, Region),

            Record = {regions, Id, Name, Lat, Lon, Radius, Active},
            mnesia:dirty_write(Record),
            ok
    end.

delete_region(Id) ->
    mnesia:dirty_delete(regions, Id),
    ok.

region_to_map({regions, Id, Name, Lat, Lon, Radius, Active}) ->
    #{
        id => Id,
        name => list_to_binary(Name),
        lat => Lat,
        lon => Lon,
        radius_miles => Radius,
        active => Active
    }.

json_response(Status, Data, Req) ->
    Body = jsx:encode(Data),
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req).
