%%% File    : node_utils.erl
%%% Author  : Boris Okner <boris.okner@gmail.com>
%%% Description :
%%% Created : 22 Aug 2009 by Boris Okner <boris.okner@gmail.com>
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(node_utils).
-export([start_app/4, stop_node/2, create_nonslave_node/1]).

%% Start application App on Node@Host, and keep it alive (monitor and restart if node down)
%% CodePath should contain code path sufficient to find application App on Node@Host.
start_app(App, Host, Node, CodePath) ->
		NN = case slave:start(Host, Node, "-setcookie " ++ atom_to_list(erlang:get_cookie()) ++ " -pa " ++ atom_to_list(CodePath)) of
				{ok, N} ->
						N;
				{error, {already_running, N}} ->
						N;
				{error, Reason} ->
						throw({error, Reason})
		end,
		%% At this point we are connected to the node
		%% Start monitor
		Pid = spawn(fun() ->  keep_alive(App, Host, Node, CodePath, NN, true) end),
		%% Start your app
		rpc:call(NN, application, start, [App]),
		Pid.

keep_alive(App, Host, Node, CodePath, NN, Monitor) ->
		case Monitor of
				true ->
						monitor_node(NN, true);
				false ->
						void
		end,
		receive
				{nodedown, _N} ->
						start_app(App, Host, Node, CodePath);
				stop_monitor ->
						monitor_node(NN, false);
				_Other ->
						keep_alive(App, Host, Node, CodePath, NN, false) %% loop on receiving without creating a new monitor
		end.

%% Node should be in form node@host
stop_node(Handler, Node) ->
		%% Stop monitoring
		Handler ! stop_monitor,
		%% Stop node
		slave:stop(Node).


create_nonslave_node(Node) ->
		os:cmd("erl -sname " ++ atom_to_list(Node) ++ " -setcookie "
					 ++ atom_to_list(erlang:get_cookie())  ++ " -r net_adm ping " ++
 atom_to_list(node()) ++ " &").
