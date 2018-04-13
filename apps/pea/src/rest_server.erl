-module(rest_server).

-behaviour(gen_server).

-include_lib("inets/include/httpd.hrl").

%% API
-export([start_link/0,
         add/3,
         search/3,
         update/3,
         delete/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%====================================================================
%% API functions
%%====================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
-spec init(list()) -> {ok, #state{}}.
init([]) ->
    inets:stop(),
    application:ensure_started(inets),
    inets:start(httpd, [
        {modules, [mod_esi]},
        {port, 8080},
        {server_name, "localhost"},
        {document_root, "."},
        {server_root, "."},
        {erl_script_alias, {"/", [rest_server]}}]),
    {ok, #state{}}.

%% http://localhost:8080/rest_server:FUN?key=value
add(SessionID, _Env, Input) ->
    [Key, Value] = string:tokens(Input, "="),
    response_ok(SessionID),
    gen_server:call(?MODULE, {add, Key, Value}).

search(SessionID, _Env, Input) -> 
    response_ok(SessionID),   
    gen_server:call(?MODULE, {search, Input}).

update(SessionID, _Env, Input) ->
    [Key, Value] = string:tokens(Input, "="),
    response_ok(SessionID),
    gen_server:call(?MODULE, {add, Key, Value}).

delete(SessionID, _Env, Input) ->
    response_ok(SessionID),
    gen_server:call(?MODULE, {delete, Input}).



-spec handle_call(term(), term(), #state{}) -> {reply, term(), #state{}}.
handle_call({add, Key, Value}, _From, State) ->
    spawn(kv_server, add, [Key, Value]),
    {reply, ok, State};
handle_call({search, Key}, _From, State) ->
    spawn(kv_server, search, [Key]),
    {reply, ok, State};
handle_call({update, Key, Value}, _From, State) ->
    spawn(kv_server, update, [Key, Value]),
    {reply, ok, State};
handle_call({delete, Key}, _From, State) ->
    spawn(kv_server, delete, [Key]),
    {reply, ok, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Message, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Message, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
response_ok(SessionID) ->
    Header = "Content-Type: text/plain\r\n\r\n",
    Content = "ok",
    mod_esi:deliver(SessionID, Header ++ Content).