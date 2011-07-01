%%%-------------------------------------------------------------------
%%% File    : weazard_sup.erl
%%% Author  : Boris Okner <boris.okner@gmail.com>
%%% Description :
%%%
%%% Created : 21 Aug 2009 by Boris Okner <boris.okner@gmail.com>
%%%-------------------------------------------------------------------
-module(weazard_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Args) ->
		crypto:start(),
		exmpp:start(),
		supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init([ComponentName, Host, Port, Password]) ->
		AChild = {weazard,{gen_component,start_component,
											 [ComponentName, Host, Port, Password, weazard:new(ComponentName), []]},
							permanent, 2000, worker,[gen_component, weazard]},
		{ok,{{one_for_all, 1,1}, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
