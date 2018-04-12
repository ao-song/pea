-module(kv_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

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

%%====================================================================
%% API functions
%%====================================================================
-spec start_link(term()) -> {ok, pid()}.
start_link(_Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
-spec init(list()) -> {ok, #state{}}.
init([]) ->
    Configs = get_configs(),
    {ok, Node} = init_node(Configs),
    {ok, Nodes} = init_nodes(Configs),

    {ok, #state{node = Node,
                nodes = Nodes,
                configs = Configs}}.

-spec handle_call(term(), term(), #state{}) -> {reply, term(), #state{}}.
handle_call(_Message, _From, State) ->
    Response = ok,
    {reply, Response, State}.

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
        {error, Reason} ->
            {error, Reason}
    end;
init_node(Configs) ->
    case [N | {node, N} <- Configs] of
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
    end,