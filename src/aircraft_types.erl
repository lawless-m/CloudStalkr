-module(aircraft_types).

-export([is_military/2, is_interesting/1, get_aircraft_type_name/1,
         classify_aircraft/1, get_tags/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% Determine if aircraft is military based on callsign and category
is_military(Callsign, Category) when is_binary(Callsign) ->
    is_military(binary_to_list(Callsign), Category);
is_military(Callsign, Category) when is_list(Callsign) ->
    %% Check callsign patterns
    CallsignMilitary = is_military_callsign(string:to_upper(Callsign)),

    %% Check category (A1 = military ops, A2 = military, A3 = mil rotorcraft)
    CategoryMilitary = case Category of
        <<"A1">> -> true;
        <<"A2">> -> true;
        <<"A3">> -> true;
        "A1" -> true;
        "A2" -> true;
        "A3" -> true;
        _ -> false
    end,

    CallsignMilitary orelse CategoryMilitary;
is_military(_, _) ->
    false.

%% Determine if aircraft is interesting (military or special operations)
is_interesting(Aircraft) when is_map(Aircraft) ->
    Callsign = maps:get(callsign, Aircraft, ""),
    Category = maps:get(category, Aircraft, ""),
    TypeCode = maps:get(type_code, Aircraft, ""),

    IsMilitary = is_military(Callsign, Category),
    IsHelicopter = is_helicopter(TypeCode),
    IsLowAltitude = case maps:get(altitude_ft, Aircraft, 999999) of
        Alt when is_integer(Alt) -> Alt < 5000;
        _ -> false
    end,

    %% Interesting if military, or helicopter at low altitude
    IsMilitary orelse (IsHelicopter andalso IsLowAltitude).

%% Get friendly name for aircraft type code
get_aircraft_type_name(<<"H47">>) -> "Chinook";
get_aircraft_type_name(<<"CH47">>) -> "Chinook";
get_aircraft_type_name(<<"H60">>) -> "Black Hawk";
get_aircraft_type_name(<<"UH60">>) -> "Black Hawk";
get_aircraft_type_name(<<"AH64">>) -> "Apache";
get_aircraft_type_name(<<"H64">>) -> "Apache";
get_aircraft_type_name(<<"C130">>) -> "Hercules";
get_aircraft_type_name(<<"A400">>) -> "Atlas";
get_aircraft_type_name(<<"C17">>) -> "Globemaster";
get_aircraft_type_name(<<"KC135">>) -> "Stratotanker";
get_aircraft_type_name(<<"E3">>) -> "AWACS";
get_aircraft_type_name(<<"F15">>) -> "Eagle";
get_aircraft_type_name(<<"F16">>) -> "Fighting Falcon";
get_aircraft_type_name(<<"F22">>) -> "Raptor";
get_aircraft_type_name(<<"F35">>) -> "Lightning II";
get_aircraft_type_name(<<"HELO">>) -> "Helicopter";
get_aircraft_type_name("H47") -> "Chinook";
get_aircraft_type_name("CH47") -> "Chinook";
get_aircraft_type_name("H60") -> "Black Hawk";
get_aircraft_type_name("UH60") -> "Black Hawk";
get_aircraft_type_name("AH64") -> "Apache";
get_aircraft_type_name("H64") -> "Apache";
get_aircraft_type_name("C130") -> "Hercules";
get_aircraft_type_name("A400") -> "Atlas";
get_aircraft_type_name("C17") -> "Globemaster";
get_aircraft_type_name("KC135") -> "Stratotanker";
get_aircraft_type_name("E3") -> "AWACS";
get_aircraft_type_name("F15") -> "Eagle";
get_aircraft_type_name("F16") -> "Fighting Falcon";
get_aircraft_type_name("F22") -> "Raptor";
get_aircraft_type_name("F35") -> "Lightning II";
get_aircraft_type_name("HELO") -> "Helicopter";
get_aircraft_type_name(_) -> "Aircraft".

%% Classify aircraft as military, civil, or unknown
classify_aircraft(Aircraft) when is_map(Aircraft) ->
    Callsign = maps:get(callsign, Aircraft, ""),
    Category = maps:get(category, Aircraft, ""),

    case is_military(Callsign, Category) of
        true -> military;
        false ->
            case Category of
                <<>> -> unknown;
                "" -> unknown;
                _ -> civil
            end
    end.

%% Get notification tags for aircraft type
get_tags(Aircraft) when is_map(Aircraft) ->
    BaseTags = ["airplane"],

    MilitaryTag = case classify_aircraft(Aircraft) of
        military -> ["military"];
        _ -> []
    end,

    TypeCode = maps:get(type_code, Aircraft, ""),
    TypeTag = case is_helicopter(TypeCode) of
        true -> ["helicopter"];
        false -> []
    end,

    lists:flatten([BaseTags, MilitaryTag, TypeTag]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_military_callsign(Callsign) ->
    %% RAF callsigns
    has_prefix(Callsign, "RRR") orelse       % RAF Rotary
    has_prefix(Callsign, "RAFAIR") orelse    % RAF Air
    has_prefix(Callsign, "ASCOT") orelse     % RAF Transport
    has_prefix(Callsign, "TARTN") orelse     % RAF Scotland
    has_prefix(Callsign, "VORTX") orelse     % RAF Display
    has_prefix(Callsign, "COBRA") orelse     % RAF Fast Jet
    has_prefix(Callsign, "EVAC") orelse      % RAF Medical

    %% US Military callsigns
    has_prefix(Callsign, "REACH") orelse     % US Air Mobility Command
    has_prefix(Callsign, "RCH") orelse       % US AMC short
    has_prefix(Callsign, "TEAM") orelse      % US Navy
    has_prefix(Callsign, "NAVY") orelse      % US Navy
    has_prefix(Callsign, "ARMY") orelse      % US Army
    has_prefix(Callsign, "PEDRO") orelse     % USAF Rescue
    has_prefix(Callsign, "SPAR") orelse      % USAF Special Air Mission
    has_prefix(Callsign, "DUKE") orelse      % USAF C-32

    %% Other military
    has_prefix(Callsign, "GAF") orelse       % German Air Force
    has_prefix(Callsign, "FAF") orelse       % French Air Force
    has_prefix(Callsign, "IAM") orelse       % Italian Air Force

    %% Helicopter specific
    has_prefix(Callsign, "HELO") orelse
    has_prefix(Callsign, "CHOPPER") orelse

    %% Check for military hex codes (this is a simplification)
    false.

is_helicopter(<<"H47">>) -> true;
is_helicopter(<<"CH47">>) -> true;
is_helicopter(<<"H60">>) -> true;
is_helicopter(<<"UH60">>) -> true;
is_helicopter(<<"AH64">>) -> true;
is_helicopter(<<"H64">>) -> true;
is_helicopter(<<"HELO">>) -> true;
is_helicopter("H47") -> true;
is_helicopter("CH47") -> true;
is_helicopter("H60") -> true;
is_helicopter("UH60") -> true;
is_helicopter("AH64") -> true;
is_helicopter("H64") -> true;
is_helicopter("HELO") -> true;
is_helicopter(TypeCode) when is_binary(TypeCode) ->
    is_helicopter(binary_to_list(TypeCode));
is_helicopter(TypeCode) when is_list(TypeCode) ->
    string:str(string:to_upper(TypeCode), "HELO") > 0 orelse
    string:str(string:to_upper(TypeCode), "HELI") > 0;
is_helicopter(_) -> false.

has_prefix(String, Prefix) when is_list(String), is_list(Prefix) ->
    case string:prefix(String, Prefix) of
        nomatch -> false;
        _ -> true
    end;
has_prefix(String, Prefix) when is_binary(String) ->
    has_prefix(binary_to_list(String), Prefix);
has_prefix(_, _) -> false.
