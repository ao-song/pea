-module(kv_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add_node/0,
         remove_node/0,
         add/2,
         search/1,
         update/2,
         delete/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {node, nodes, configs}).

-record(kv, {key, value}).

-define(CONFIG_FILE, "config").
-define(NODE_NAME, "kv").
-define(TABLE_INIT_TIMEOUT, 5000).
-define(DB_RETRY, 3).

%%====================================================================
%% API functions
%%====================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_node() -> ok.
remove_node() -> ok.

add(Key, Value) ->
    gen_server:call(?MODULE, {add, Key, Value}).

search(Key) ->
    gen_server:call(?MODULE, {search, Key}).

update(Key, Value) ->
    add(Key, Value).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
-spec init(list()) -> {ok, #state{}}.
init([]) ->
    Configs = get_configs(),
    {ok, Node} = init_node(Configs),
    {ok, Nodes} = init_nodes(Configs),

    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),

    mnesia:create_table(kv,
                        [{attributes, record_info(fields, kv)},
                         {disc_copies, Nodes},
                         {type, set}]),
    mnesia:wait_for_tables([kv], ?TABLE_INIT_TIMEOUT),

    {ok, #state{node = Node,
                nodes = Nodes,
                configs = Configs}}.

-spec handle_call(term(), term(), #state{}) -> {reply, term(), #state{}}.
handle_call({add, Key, Value}, _From, State) ->
    F = fun() ->
        mnesia:write(#kv{key=Key, value=Value})
    end,
    mnesia:activity({sync_transaction, ?DB_RETRY}, F),

    {reply, ok, State};
handle_call({search, Key}, _From, State) ->
    F = fun() ->
        ets:lookup(kv, Key)
    end,
    Res = mnesia:activity(ets, F),

    {reply, Res, State};
handle_call({delete, Key}, _From, State) ->
    F = fun() ->
        mnesia:delete({kv, Key})
    end,
    mnesia:activity({sync_transaction, ?DB_RETRY}, F),

    {reply, ok, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Message, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Message, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{nodes = Nodes}) ->
    net_kernel:stop(),    
    rpc:multicall(Nodes, application, stop, [mnesia]),
    mnesia:delete_schema(Nodes),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
get_configs() ->
    case file:consult(?CONFIG_FILE) of
        {ok, Configs} -> 
            Configs;
        {error, _Reason} ->
            []
    end.

init_node({node, Node}) ->
    case net_kernel:start([Node, longnames]) of
        {ok, _Pid} ->
            {ok, Node};
        {error, {already_started, _Pid}} ->
            {ok, node()};
        {error, {already_exists, _Pid}} ->
            {ok, node()};
        {error, Reason} ->
            {error, Reason}
    end;
init_node(Configs) ->
    case [N || {node, N} <- Configs] of
        [] ->
            Node = ?NODE_NAME ++ "@" ++ inet:ntoa(get_first_local_ip_v4()),
            init_node({node, erlang:list_to_atom(Node)});
        [Node|_T] ->
            init_node({node, Node})
    end.

get_first_local_ip_v4() ->
    {ok, AddrList} = inet:getifaddrs(),

    hd([Addr || {_InterfaceName, Opts} <- AddrList, 
    	        {flags, Flags} <- Opts,
    	        {addr, Addr} <- Opts,
    	        not lists:member(loopback, Flags),
    	        size(Addr) == 4,
    	        Addr =/= {127, 0, 0, 1}]).

init_nodes(Configs) ->
    case lists:keyfind(nodes, 1, Configs) of
        {nodes, Nodes} ->
            [net_kernel:connect_node(Node) || Node <- Nodes],
            {ok, Nodes};
        false ->
            {ok, []}
    end.