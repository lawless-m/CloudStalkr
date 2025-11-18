-module(web_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PORT, 8080).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            %% Static files
            {"/", cowboy_static, {priv_file, aircraft_alert, "static/index.html"}},
            {"/static/[...]", cowboy_static, {priv_dir, aircraft_alert, "static"}},

            %% API endpoints
            {"/api/regions", api_regions_handler, []},
            {"/api/regions/:id", api_regions_handler, []},
            {"/api/rules", api_rules_handler, []},
            {"/api/rules/:id", api_rules_handler, []},
            {"/api/sightings", api_sightings_handler, []},
            {"/api/config", api_config_handler, []},
            {"/api/status", api_status_handler, []}
        ]}
    ]),

    %% Start Cowboy HTTP listener
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, ?PORT}],
        #{env => #{dispatch => Dispatch}}
    ),

    io:format("Web server started on http://localhost:~p~n", [?PORT]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(http_listener),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
