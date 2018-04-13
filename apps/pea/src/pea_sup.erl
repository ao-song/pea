%%%-------------------------------------------------------------------
%% @doc pea top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pea_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).

-define(CHILD(I, Type, Arg), 
        {I, {I, start_link, Arg}, temporary, infinity, Type, [I]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all,  ?MAX_RESTART, ?MAX_TIME}, 
           [?CHILD(kv_server, worker, []),
            ?CHILD(rest_server, worker, [])]} }.

%%====================================================================
%% Internal functions
%%====================================================================
