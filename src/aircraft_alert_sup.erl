-module(aircraft_alert_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 60},

    %% Child specifications
    ConfigServer = #{
        id => config_server,
        start => {config_server, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [config_server]
    },

    AircraftPoller = #{
        id => aircraft_poller,
        start => {aircraft_poller, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [aircraft_poller]
    },

    AlertProcessor = #{
        id => alert_processor,
        start => {alert_processor, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [alert_processor]
    },

    NotificationSender = #{
        id => notification_sender,
        start => {notification_sender, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [notification_sender]
    },

    WebServer = #{
        id => web_server,
        start => {web_server, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [web_server]
    },

    Children = [ConfigServer, AircraftPoller, AlertProcessor, NotificationSender, WebServer],

    {ok, {SupFlags, Children}}.
