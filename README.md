# CloudSkanr - Real-Time Military Aircraft Alert System

CloudSkanr is an Erlang/OTP application that monitors ADS-B Exchange for military and unusual aircraft, sending real-time alerts via ntfy.sh when interesting aircraft are detected nearby.

## Features

- **Real-time monitoring** of ADS-B Exchange data
- **Configurable regions** - monitor multiple locations (home, holiday, etc.)
- **Smart alert rules** - filter by aircraft type, altitude, distance
- **Duplicate suppression** - avoid spam from the same aircraft
- **Active hours** - only alert during specified times and days
- **Approaching/departing detection** - know if aircraft is coming or going
- **Priority levels** - different notification urgency based on rules
- **ntfy.sh integration** - get push notifications on your phone

## Requirements

- Erlang/OTP 24 or later
- rebar3
- Internet connection (for ADS-B Exchange and ntfy.sh)

## Installation

1. Clone the repository:
```bash
git clone <repository-url>
cd CloudStalkr
```

2. Install dependencies:
```bash
rebar3 get-deps
```

3. Compile the project:
```bash
rebar3 compile
```

4. Install Mnesia schema (first time only):
```bash
rebar3 shell
> aircraft_alert_app:install_mnesia().
> q().
```

## Configuration

Edit `config/aircraft_alert.config` to customize your settings:

### Key Configuration Options

- **polling_interval_seconds**: How often to check for aircraft (default: 30)
- **active_hours**: Time window for alerts, e.g., `{{7, 0}, {22, 0}}`
- **active_days**: `all` or list of days `[monday, tuesday, ...]`
- **ntfy_topic**: Your ntfy.sh topic name
- **duplicate_alert_window_minutes**: Suppress duplicate alerts (default: 15)

### Regions

Define areas to monitor:

```erlang
{regions, [
  #{
    id => home,
    name => "Grimsby Home",
    lat => 53.567,
    lon => -0.081,
    radius_miles => 5.0,
    active => true
  }
]}
```

### Alert Rules

Define what triggers notifications:

```erlang
{alert_rules, [
  #{
    region_id => home,
    aircraft_types => ["H47", "CH47"],  % Chinooks
    min_altitude_ft => 0,
    max_altitude_ft => 5000,
    max_distance_miles => 2.0,
    priority => 5  % Highest priority (1-5)
  }
]}
```

**Aircraft types:**
- Use specific codes: `"H47"`, `"CH47"`, `"A1"`, etc.
- Use `any` to match any military aircraft
- `"A1"` = Military operations
- `"A2"` = Military aircraft
- `"A3"` = Military rotorcraft

## Setting up ntfy.sh

1. Download the ntfy app on your phone:
   - [Android](https://play.google.com/store/apps/details?id=io.heckel.ntfy)
   - [iOS](https://apps.apple.com/us/app/ntfy/id1625396347)

2. Subscribe to your topic (e.g., "grimsby_aircraft_alerts")

3. Update the `ntfy_topic` in your config file

## Running the Application

### Development mode (interactive shell):
```bash
rebar3 shell
```

### Production mode (detached):
```bash
rebar3 release
_build/default/rel/aircraft_alert/bin/aircraft_alert start
```

## Usage

### Interactive Commands

When running in the Erlang shell:

```erlang
% Check active regions
config_server:get_active_regions().

% Manually trigger a poll
aircraft_poller:poll_now().

% Send a test notification
notification_sender:send_test_notification().

% View recent sightings for a region
mnesia:dirty_match_object({recent_sightings, '_', '_', home, '_', '_', '_', '_', '_', '_', '_'}).

% Reload configuration
config_server:reload_config().
```

## How It Works

1. **aircraft_poller** periodically queries ADS-B Exchange for each active region
2. Aircraft data is stored in Mnesia database
3. **alert_processor** evaluates each aircraft against your alert rules
4. If an aircraft matches rules and hasn't been alerted recently, it proceeds
5. **notification_sender** sends a push notification via ntfy.sh
6. You get an alert on your phone with details like:
   - `"Chinook Overhead!"`
   - `"Chinook 'RRR123' at 800ft, 1.2 miles NE, heading 240° (approaching) - GO NOW!"`

## Project Structure

```
CloudStalkr/
├── rebar.config                    # Rebar3 configuration
├── config/
│   └── aircraft_alert.config      # Application configuration
├── src/
│   ├── aircraft_alert.app.src    # OTP application resource
│   ├── aircraft_alert_app.erl    # Application behaviour
│   ├── aircraft_alert_sup.erl    # Supervision tree
│   ├── config_server.erl         # Configuration management
│   ├── aircraft_poller.erl       # ADS-B Exchange API polling
│   ├── alert_processor.erl       # Alert rule evaluation
│   ├── notification_sender.erl   # ntfy.sh integration
│   ├── geo_utils.erl             # Geographic calculations
│   └── aircraft_types.erl        # Aircraft classification
└── README.md
```

## Database Schema

CloudSkanr uses Mnesia with four tables:

- **regions**: Your monitored locations
- **alert_rules**: Alert triggering conditions
- **recent_sightings**: Aircraft sightings (last 24 hours)
- **config**: Runtime configuration

## Troubleshooting

### No alerts received?

1. Check if within active hours:
   ```erlang
   config_server:is_within_active_hours().
   ```

2. Test notifications:
   ```erlang
   notification_sender:send_test_notification().
   ```

3. Check for aircraft in range:
   ```erlang
   aircraft_poller:poll_now().
   ```

### API errors?

- Check internet connection
- Verify ADS-B Exchange is accessible
- The poller will automatically back off and retry

### Mnesia issues?

```bash
# Remove and recreate database
rm -rf Mnesia.nonode@nohost/
rebar3 shell
> aircraft_alert_app:install_mnesia().
```

## Known Aircraft Callsigns

Military callsigns automatically detected:
- **RAF**: RRR*, RAFAIR*, ASCOT*, TARTN*, COBRA*, EVAC*
- **USAF**: REACH*, SPAR*, PEDRO*
- **US Navy**: TEAM*, NAVY*
- **US Army**: ARMY*

## Future Enhancements

- Weather integration (skip alerts in poor visibility)
- Web dashboard with live map
- Photo integration (camera trigger)
- Machine learning for pattern detection
- Community features (share sightings)

## Contributing

Contributions welcome! Please open issues or pull requests.

## License

Apache 2.0 - See LICENSE file for details

## Credits

- ADS-B Exchange for aircraft data
- ntfy.sh for push notifications
- Built with Erlang/OTP

---

**Happy plane spotting!** ✈️
