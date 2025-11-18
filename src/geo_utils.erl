-module(geo_utils).

-export([haversine_distance/4, bearing/4, format_direction/1,
         miles_to_nautical_miles/1, nautical_miles_to_miles/1,
         is_approaching/6]).

-define(EARTH_RADIUS_MILES, 3958.8).
-define(EARTH_RADIUS_KM, 6371.0).

%%%===================================================================
%%% API
%%%===================================================================

%% Calculate distance between two points using Haversine formula
%% Returns distance in miles
haversine_distance(Lat1, Lon1, Lat2, Lon2) ->
    %% Convert to radians
    Lat1Rad = deg_to_rad(Lat1),
    Lat2Rad = deg_to_rad(Lat2),
    Lon1Rad = deg_to_rad(Lon1),
    Lon2Rad = deg_to_rad(Lon2),

    %% Haversine formula
    DLat = Lat2Rad - Lat1Rad,
    DLon = Lon2Rad - Lon1Rad,

    A = math:pow(math:sin(DLat / 2), 2) +
        math:cos(Lat1Rad) * math:cos(Lat2Rad) *
        math:pow(math:sin(DLon / 2), 2),

    C = 2 * math:atan2(math:sqrt(A), math:sqrt(1 - A)),

    Distance = ?EARTH_RADIUS_MILES * C,
    Distance.

%% Calculate bearing from point 1 to point 2
%% Returns bearing in degrees (0-359)
bearing(Lat1, Lon1, Lat2, Lon2) ->
    Lat1Rad = deg_to_rad(Lat1),
    Lat2Rad = deg_to_rad(Lat2),
    Lon1Rad = deg_to_rad(Lon1),
    Lon2Rad = deg_to_rad(Lon2),

    DLon = Lon2Rad - Lon1Rad,

    Y = math:sin(DLon) * math:cos(Lat2Rad),
    X = math:cos(Lat1Rad) * math:sin(Lat2Rad) -
        math:sin(Lat1Rad) * math:cos(Lat2Rad) * math:cos(DLon),

    BearingRad = math:atan2(Y, X),
    BearingDeg = rad_to_deg(BearingRad),

    %% Normalize to 0-359
    normalize_bearing(BearingDeg).

%% Format bearing as compass direction (N, NE, E, etc.)
format_direction(Bearing) when Bearing >= 0, Bearing =< 360 ->
    Directions = ["N", "NE", "E", "SE", "S", "SW", "W", "NW"],
    Index = round(Bearing / 45) rem 8,
    lists:nth(Index + 1, Directions).

%% Convert miles to nautical miles
miles_to_nautical_miles(Miles) ->
    Miles * 0.868976.

%% Convert nautical miles to miles
nautical_miles_to_miles(NauticalMiles) ->
    NauticalMiles * 1.15078.

%% Determine if aircraft is approaching the target location
%% Compares current distance with a hypothetical previous position
%% based on the aircraft's heading
is_approaching(CurrentLat, CurrentLon, AircraftHeading,
               TargetLat, TargetLon, SpeedKnots) ->
    %% Calculate current distance
    CurrentDistance = haversine_distance(CurrentLat, CurrentLon,
                                         TargetLat, TargetLon),

    %% Calculate bearing from aircraft to target
    BearingToTarget = bearing(CurrentLat, CurrentLon, TargetLat, TargetLon),

    %% Calculate the difference between aircraft heading and bearing to target
    %% If the difference is small (< 90 degrees), aircraft is approaching
    HeadingDiff = abs(normalize_bearing(AircraftHeading - BearingToTarget)),

    %% Approaching if heading towards target (within 90 degrees)
    if
        HeadingDiff =< 90.0 -> true;
        HeadingDiff >= 270.0 -> true;  %% Wraps around
        true -> false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

deg_to_rad(Degrees) ->
    Degrees * math:pi() / 180.0.

rad_to_deg(Radians) ->
    Radians * 180.0 / math:pi().

normalize_bearing(Bearing) when Bearing < 0 ->
    normalize_bearing(Bearing + 360.0);
normalize_bearing(Bearing) when Bearing >= 360 ->
    normalize_bearing(Bearing - 360.0);
normalize_bearing(Bearing) ->
    Bearing.
