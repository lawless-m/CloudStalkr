-module(notification_sender).
-behaviour(gen_server).

%% API
-export([start_link/0, send_notification/4, send_test_notification/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    ntfy_topic = "aircraft_alerts",
    ntfy_server = "https://ntfy.sh",
    notification_queue = queue:new(),
    last_sent = undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send_notification(Title, Message, Priority, Tags) ->
    gen_server:cast(?SERVER, {send_notification, Title, Message, Priority, Tags}).

send_test_notification() ->
    gen_server:cast(?SERVER, send_test_notification).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Get configuration
    Topic = case config_server:get_config(ntfy_topic) of
        undefined -> "aircraft_alerts";
        T -> T
    end,

    Server = case config_server:get_config(ntfy_server) of
        undefined -> "https://ntfy.sh";
        S -> S
    end,

    State = #state{
        ntfy_topic = Topic,
        ntfy_server = Server
    },

    io:format("Notification sender started (topic: ~s)~n", [Topic]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({send_notification, Title, Message, Priority, Tags}, State) ->
    %% Send immediately (could add queuing logic here if needed)
    NewState = do_send_notification(Title, Message, Priority, Tags, State),
    {noreply, NewState};

handle_cast(send_test_notification, State) ->
    NewState = do_send_notification(
        "Test Alert",
        "This is a test notification from CloudSkanr",
        3,
        ["airplane", "test"],
        State
    ),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_send_notification(Title, Message, Priority, Tags, State) ->
    case post_to_ntfy(State#state.ntfy_topic, State#state.ntfy_server,
                      Title, Message, Priority, Tags) of
        ok ->
            State#state{last_sent = erlang:timestamp()};
        {error, Reason} ->
            io:format("Failed to send notification: ~p~n", [Reason]),
            State
    end.

post_to_ntfy(Topic, Server, Title, Message, Priority, Tags) ->
    %% Build URL
    Url = lists:flatten(io_lib:format("~s/~s", [Server, Topic])),

    %% Build JSON body
    Body = jsx:encode(#{
        <<"topic">> => list_to_binary(Topic),
        <<"title">> => list_to_binary(Title),
        <<"message">> => list_to_binary(Message),
        <<"priority">> => Priority,
        <<"tags">> => [list_to_binary(Tag) || Tag <- Tags]
    }),

    %% Make HTTP POST request
    Headers = [
        {"Content-Type", "application/json"}
    ],

    case httpc:request(post, {Url, Headers, "application/json", Body},
                      [{timeout, 10000}], []) of
        {ok, {{_, 200, _}, _RespHeaders, _RespBody}} ->
            ok;
        {ok, {{_, StatusCode, _}, _RespHeaders, RespBody}} ->
            io:format("ntfy.sh returned status ~p: ~s~n", [StatusCode, RespBody]),
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.
