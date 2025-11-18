# Real-Time Military Aircraft Alert System - Implementation Plan

## Project Overview
Erlang/OTP application that monitors ADS-B Exchange for military and unusual aircraft, sends real-time alerts via ntfy.sh when interesting aircraft are nearby, with configurable regions of interest and alert conditions.

## Technology Stack
- **Language**: Erlang/OTP
- **Database**: Mnesia (already available)
- **HTTP Client**: httpc (built-in) or gun/hackney
- **JSON**: jsx or jiffy
- **API**: ADS-B Exchange v2
- **Notifications**: ntfy.sh
- **Weather** (enhancement): OpenWeatherMap or similar

---

## Application Structure

### OTP Supervision Tree
```
aircraft_alert_sup (one_for_one)
├── config_server (GenServer) - manages configuration
├── aircraft_poller (GenServer) - polls ADS-B Exchange API
├── alert_processor (GenServer) - evaluates alert conditions
├── notification_sender (GenServer) - sends ntfy.sh notifications
└── weather_poller (GenServer) - optional weather data (enhancement)
```

---

## Mnesia Schema Design

### Table: `regions`
```erlang
{region,
  id,              % atom: home | holiday_spain | etc
  name,            % string: "Grimsby Home"
  lat,             % float: 53.567
  lon,             % float: -0.081
  radius_miles,    % float: 5.0
  active           % boolean: true/false
}
```

### Table: `alert_rules`
```erlang
{alert_rule,
  region_id,          % atom: which region this applies to
  aircraft_types,     % list: ["H47", "HELO", "MIL"] or 'any'
  min_altitude_ft,    % integer: 0
  max_altitude_ft,    % integer: 5000
  max_distance_miles, % float: 2.0 (for immediate alerts)
  priority            % integer: 1-5 (ntfy priority)
}
```

### Table: `recent_sightings`
```erlang
{sighting,
  icao24,          % string: aircraft unique identifier
  region_id,       % atom
  timestamp,       % erlang:timestamp()
  notified,        % boolean: have we alerted about this?
  lat,             % float
  lon,             % float
  altitude_ft,     % integer
  heading,         % integer: 0-359
  callsign         % string
}
```

### Table: `config`
```erlang
{config,
  key,             % atom: polling_interval | active_hours | etc
  value            % term
}
```

**Config Keys:**
- `polling_interval_seconds` - integer (default: 30)
- `active_hours` - tuple: `{{StartHour, StartMin}, {EndHour, EndMin}}` e.g., `{{7,0}, {22,0}}`
- `active_days` - list: [monday, tuesday, etc] or 'all'
- `ntfy_topic` - string: "grimsby_aircraft_alerts"
- `ntfy_server` - string: "https://ntfy.sh" (customizable for self-hosted)
- `duplicate_alert_window_minutes` - integer: 15 (don't re-alert same aircraft)
- `weather_check_enabled` - boolean
- `min_visibility_km` - float: 5.0 (don't alert if visibility too poor)
- `max_cloud_cover_percent` - integer: 90

---

## Core Modules

### 1. `aircraft_alert_app.erl`
- Standard OTP application behaviour
- Starts supervision tree
- Initializes Mnesia tables if they don't exist

### 2. `aircraft_alert_sup.erl`
- Root supervisor (one_for_one strategy)
- Starts all child GenServers

### 3. `config_server.erl` (GenServer)
**Responsibilities:**
- Load configuration from file on startup (`config.config` or similar)
- Provide `get_config/1`, `set_config/2` API
- Validate configuration values
- Persist changes to Mnesia `config` table
- Notify other processes when config changes (pub/sub pattern)

**API:**
```erlang
config_server:get_config(Key) -> Value
config_server:set_config(Key, Value) -> ok
config_server:get_active_regions() -> [Region]
config_server:is_within_active_hours() -> boolean()
```

### 4. `aircraft_poller.erl` (GenServer)
**Responsibilities:**
- Periodic polling of ADS-B Exchange API based on `polling_interval_seconds`
- Query each active region
- Parse JSON responses
- Store raw aircraft data in Mnesia `recent_sightings`
- Send aircraft data to `alert_processor`

**State:**
```erlang
#{
  timer_ref => reference(),
  last_poll => erlang:timestamp(),
  api_errors => integer()  % track consecutive errors
}
```

**API Integration:**
- Endpoint: `https://adsbexchange.com/api/aircraft/json/lat/{lat}/lon/{lon}/dist/{dist}/`
- Distance parameter in nautical miles (convert from config miles)
- Handle rate limits gracefully (exponential backoff on errors)
- Parse aircraft fields: icao24, lat, lon, alt_baro, track, callsign, category

**Functions:**
```erlang
poll_regions() -> ok
query_adsb_exchange(Lat, Lon, RadiusMiles) -> {ok, AircraftList} | {error, Reason}
parse_aircraft_json(Json) -> [Aircraft]
store_sighting(Aircraft, RegionId) -> ok
```

### 5. `alert_processor.erl` (GenServer)
**Responsibilities:**
- Receive aircraft data from poller
- Match against alert rules for each region
- Calculate distance from region center
- Calculate heading/trajectory (is it approaching or departing?)
- Determine if alert should be sent
- Check duplicate alert window (don't spam same aircraft)
- Send alerts to `notification_sender`

**State:**
```erlang
#{
  alert_queue => queue:queue(),  % pending alerts
  processing => boolean()
}
```

**Functions:**
```erlang
process_aircraft(Aircraft, RegionId) -> ok
calculate_distance(Lat1, Lon1, Lat2, Lon2) -> Miles  % Haversine formula
calculate_bearing(Lat1, Lon1, Lat2, Lon2) -> Degrees
is_approaching(Aircraft, Region) -> boolean()
should_alert(Aircraft, Region, Rules) -> {true, Priority} | false
has_recent_alert(Icao24, RegionId, WindowMinutes) -> boolean()
format_alert_message(Aircraft, Region, Distance) -> {Title, Message}
```

**Alert Message Format:**
```
Title: "Chinook Overhead!"
Message: "H47 'RRR123' at 800ft, 1.2 miles NE, heading 240° (approaching) - GO NOW!"
```

### 6. `notification_sender.erl` (GenServer)
**Responsibilities:**
- Send notifications via ntfy.sh
- Queue notifications if needed (rate limiting)
- Retry failed notifications
- Log notification history

**State:**
```erlang
#{
  ntfy_topic => string(),
  ntfy_server => string(),
  notification_queue => queue:queue(),
  last_sent => erlang:timestamp()
}
```

**Functions:**
```erlang
send_notification(Title, Message, Priority, Tags) -> ok | {error, Reason}
post_to_ntfy(Topic, Title, Message, Priority, Tags) -> ok | {error, Reason}
```

**ntfy.sh Request Format:**
```erlang
POST https://ntfy.sh/grimsby_aircraft_alerts
Content-Type: application/json

{
  "topic": "grimsby_aircraft_alerts",
  "title": "Chinook Overhead!",
  "message": "...",
  "priority": 5,
  "tags": ["airplane", "military", "helicopter"]
}
```

### 7. `weather_poller.erl` (GenServer) - Enhancement
**Responsibilities:**
- Periodically check weather for active regions (every 15-30 minutes)
- Cache weather data in state
- Provide weather data to alert_processor

**State:**
```erlang
#{
  weather_cache => #{RegionId => WeatherData},
  last_update => erlang:timestamp(),
  timer_ref => reference()
}
```

**Functions:**
```erlang
get_weather(Lat, Lon) -> {ok, WeatherData} | {error, Reason}
is_good_visibility(RegionId) -> boolean()
parse_weather_json(Json) -> WeatherData
```

**Weather API Options:**
- OpenWeatherMap (free tier: 1000 calls/day)
- WeatherAPI.com (free tier: 1M calls/month)
- Met Office (UK specific, good for Grimsby)

**Data needed:**
- Cloud cover percentage
- Visibility (metres or km)
- Precipitation

### 8. `geo_utils.erl`
**Utility functions for geographical calculations:**

```erlang
haversine_distance(Lat1, Lon1, Lat2, Lon2) -> Miles
bearing(Lat1, Lon1, Lat2, Lon2) -> Degrees  % 0-359
is_approaching(CurrentLat, CurrentLon, PrevLat, PrevLon, TargetLat, TargetLon) -> boolean()
format_direction(Degrees) -> string()  % "N", "NE", "E", etc.
miles_to_nautical_miles(Miles) -> NauticalMiles
```

### 9. `aircraft_types.erl`
**Aircraft type classification and filtering:**

```erlang
is_military(Callsign, Category) -> boolean()
is_interesting(Aircraft) -> boolean()
get_aircraft_type_name(TypeCode) -> string()  % "H47" -> "Chinook"
classify_aircraft(Aircraft) -> military | civil | unknown
```

**Military indicators:**
- Callsigns: RRR*, RAFAIR*, EVAC*, ASCOT*, etc.
- Categories: A1 (military helicopter), A2 (military jet)
- Known registration patterns

---

## Configuration File Format

### `config/aircraft_alert.config`
```erlang
[
  {aircraft_alert, [
    {polling_interval_seconds, 30},
    {active_hours, {{7, 0}, {22, 0}}},
    {active_days, all},  % or [monday, tuesday, wednesday, ...]
    {ntfy_topic, "grimsby_aircraft_alerts"},
    {ntfy_server, "https://ntfy.sh"},
    {duplicate_alert_window_minutes, 15},
    {weather_check_enabled, false},  % true for enhancement
    {min_visibility_km, 5.0},
    {max_cloud_cover_percent, 90},
    
    {regions, [
      #{
        id => home,
        name => "Grimsby Home",
        lat => 53.567,
        lon => -0.081,
        radius_miles => 5.0,
        active => true
      },
      #{
        id => holiday_spain,
        name => "Malaga Holiday",
        lat => 36.721,
        lon => -4.421,
        radius_miles => 3.0,
        active => false
      }
    ]},
    
    {alert_rules, [
      #{
        region_id => home,
        aircraft_types => ["H47", "A1"],  % Chinooks and military helicopters
        min_altitude_ft => 0,
        max_altitude_ft => 5000,
        max_distance_miles => 2.0,
        priority => 5
      },
      #{
        region_id => home,
        aircraft_types => any,  % Any military
        min_altitude_ft => 0,
        max_altitude_ft => 3000,
        max_distance_miles => 1.0,
        priority => 4
      }
    ]}
  ]}
].
```

---

## Implementation Phases

### Phase 1: Core Infrastructure
1. Set up OTP application structure
2. Create supervision tree
3. Initialize Mnesia schema and tables
4. Implement `config_server` with file loading
5. Basic logging setup

### Phase 2: API Integration
1. Implement `aircraft_poller` GenServer
2. ADS-B Exchange API integration
3. JSON parsing (jsx/jiffy)
4. Error handling and retry logic
5. Store raw data in Mnesia

### Phase 3: Alert Logic
1. Implement `alert_processor` GenServer
2. Geo calculation utilities (haversine, bearing)
3. Aircraft type classification
4. Alert rule matching
5. Duplicate detection logic
6. Approaching/departing detection

### Phase 4: Notifications
1. Implement `notification_sender` GenServer
2. ntfy.sh integration
3. Message formatting
4. Priority handling
5. Notification history/logging

### Phase 5: Time & Schedule Management
1. Active hours checking in `config_server`
2. Active days checking
3. Automatic pause/resume based on schedule
4. Region activation/deactivation

### Phase 6: Enhancement - Weather Integration
1. Implement `weather_poller` GenServer
2. Weather API integration (OpenWeatherMap/Met Office)
3. Visibility and cloud cover checking
4. Alert suppression based on weather conditions

### Phase 7: Testing & Refinement
1. Unit tests for geo calculations
2. Mock API responses for testing
3. End-to-end testing with test ntfy topic
4. Performance tuning (polling frequency vs API limits)
5. Error recovery testing

---

## Testing Strategy

### Unit Tests
- `geo_utils` - Haversine distance calculation
- `aircraft_types` - Classification logic
- `alert_processor` - Rule matching
- Configuration validation

### Integration Tests
- API polling with mock responses
- End-to-end alert flow
- Mnesia operations
- ntfy.sh notification delivery

### Manual Testing Checklist
1. Subscribe to ntfy topic on phone
2. Configure test region with small radius
3. Monitor API responses
4. Verify notifications arrive with correct priority
5. Test duplicate suppression
6. Test active hours enforcement
7. Test multiple regions
8. Test weather integration (if enabled)

---

## Error Handling

### API Failures
- Exponential backoff on consecutive failures
- Log errors but don't crash GenServers
- Notify user if API is down for extended period (separate low-priority alert)

### Network Issues
- Timeout configuration for HTTP requests (5-10 seconds)
- Retry logic with backoff
- Graceful degradation

### Data Quality
- Handle missing fields in API responses
- Validate lat/lon ranges
- Handle malformed JSON

---

## Performance Considerations

### API Rate Limits
- ADS-B Exchange: Be respectful, ~1 request per 5-10 seconds per region
- Weather API: Cache for 15-30 minutes
- ntfy.sh: No strict limits, but batch if needed

### Mnesia Optimization
- Index on `icao24` and `timestamp` for `recent_sightings`
- Periodic cleanup of old sightings (keep last 24 hours)
- Use `dirty_read` for performance where consistency isn't critical

### Memory Management
- Limit `recent_sightings` table size (rolling window)
- Clear old notification history periodically

---

## Deployment & Operations

### Start/Stop
```bash
# Start application
./rebar3 shell

# Start in detached mode
erl -detached -sname aircraft_alert -s aircraft_alert_app
```

### Configuration Updates
- Modify `config/aircraft_alert.config`
- Reload config: `config_server:reload_config()`
- Or restart application

### Monitoring
- Log to file: `/var/log/aircraft_alert/`
- Track API error rates
- Monitor notification delivery
- Periodic health checks

### Useful Commands (via shell)
```erlang
% Check active regions
config_server:get_active_regions().

% Manually trigger poll
aircraft_poller:poll_now().

% Test notification
notification_sender:send_test_notification().

% View recent sightings
mnesia:dirty_match_object({sighting, '_', home, '_', '_', '_', '_', '_', '_'}).

% Clear old sightings
aircraft_alert_db:cleanup_old_sightings(24).  % hours
```

---

## Future Enhancements

1. **Web Dashboard**
   - Real-time map showing tracked aircraft
   - Historical sighting data visualization
   - Configuration UI

2. **Machine Learning**
   - Pattern detection (regular flights/routes)
   - Predict interesting activity times
   - Anomaly detection

3. **Photo Integration**
   - Link to camera trigger
   - Store photos with sighting data

4. **Community Features**
   - Share sightings with other users
   - Collaborative tracking

5. **Advanced Alerts**
   - Formation flights
   - Unusual altitude/speed combinations
   - Specific tail numbers/registrations

---

## Dependencies

### rebar.config
```erlang
{deps, [
    {jsx, "3.1.0"},  % JSON parsing
    {gun, "2.0.1"}   % Modern HTTP client (alternative to httpc)
]}.

{relx, [
    {release, {aircraft_alert, "1.0.0"},
     [aircraft_alert, sasl]},
    {dev_mode, true},
    {include_erts, false},
    {extended_start_script, true}
]}.
```

---

## File Structure
```
aircraft_alert/
├── rebar.config
├── config/
│   └── aircraft_alert.config
├── src/
│   ├── aircraft_alert.app.src
│   ├── aircraft_alert_app.erl
│   ├── aircraft_alert_sup.erl
│   ├── config_server.erl
│   ├── aircraft_poller.erl
│   ├── alert_processor.erl
│   ├── notification_sender.erl
│   ├── weather_poller.erl
│   ├── geo_utils.erl
│   └── aircraft_types.erl
├── test/
│   ├── geo_utils_tests.erl
│   ├── alert_processor_tests.erl
│   └── aircraft_types_tests.erl
└── README.md
```

---

## Summary

This plan provides a complete roadmap for building a robust, production-ready aircraft alert system in Erlang/OTP. The modular design allows for incremental development and testing of each component, with clear separation of concerns and well-defined APIs between modules.

Key features:
- ✅ Real-time ADS-B Exchange monitoring
- ✅ Configurable regions and alert rules
- ✅ ntfy.sh notifications with priority levels
- ✅ Active hours and day scheduling
- ✅ Duplicate alert suppression
- ✅ Approaching/departing detection
- ✅ Weather integration (optional enhancement)
- ✅ Robust error handling and recovery
- ✅ Mnesia for persistent storage
- ✅ OTP supervision for reliability

The system is designed to be reliable, efficient, and extensible for future enhancements!
