%%% File    : xep0114.erl
%%% Author  : Boris Okner <boris.okner@gmail.com>
%%% Description: Connects Jabber server and service component using XEP-0114.
%%% Created :  June 27, 2009 by Boris Okner <boris.okner@gmail.com>
%%%-------------------------------------------------------------------
%%% This code is based on xmpp_component.erl from Epeios 1.0.0, created by Mickael Remond <mremond@process-one.net>
%%%
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

-module(xep0114).

-behaviour(gen_fsm).

-export([start_component/6, stop_component/1,
	 send_to_server/2]).

-export([test/0]).

%% FSM states:
-export([wait_open_stream/2,
         wait_handshake/2,
   	 connected/2]).

-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         code_change/4,
         handle_info/3,
         terminate/3]).

-export([receiver/3]).


-record(state, {name, secret, host, port, socket, module, module_state, parser}).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).


-endif.

%% Start component
start_component(ComponentName, Server, Port, Secret, Module, Args) ->
    gen_fsm:start_link({local, list_to_atom(ComponentName)},
		       ?MODULE,
		       [ComponentName, Server, Port, Secret, Module, Args],
		       [?FSMOPTS]).

stop_component(ComponentName) ->
		% We need to make sure we send "unavailable" presence before connection is shut down.
		gen_fsm:sync_send_all_state_event(list_to_atom(ComponentName), unavailable),
		gen_fsm:send_all_state_event(list_to_atom(ComponentName), stop).

%% Send the message via the server we are connected to
send_to_server(ComponentName, Event) ->
		io:format("Sending via server: ~p~n", [Event]),
    gen_fsm:send_event(list_to_atom(ComponentName), Event).

init([ComponentName, Server, Port, Secret, Module, Args]) ->
    Socket = connect(ComponentName, Server, Port),
	Parser = exmpp_xml:start_parser(),
    Pid = start_xml_parser(Socket, Parser),
    gen_tcp:controlling_process(Socket, Pid),
    ok = stream_open(Socket, ComponentName, Server),
		{ok, ModuleState} = Module:init(ComponentName, Args),
    {ok, wait_open_stream, #state{name=ComponentName,
				  secret=Secret,
				  host=Server,
				  port=Port,
				  socket=Socket,
				  module=Module,
					module_state = 	ModuleState,
				parser = Parser}}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName,
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also
%% called if a timeout occurs.
%%--------------------------------------------------------------------
wait_open_stream({xmlstreamstart, stream, Attrs} = Event, State) ->
    io:format("EV: ~p~n", [Event]),
    {value, StreamID} = xml:get_attr("id", Attrs),
    handshake(State#state.socket, StreamID, State#state.secret),
    {next_state, wait_handshake, State};
wait_open_stream(Event, State) ->
    io:format("EV: ~p~n", [Event]),
    {next_state, wait_open_stream, State}.

wait_handshake({xmlstreamelement,{xmlel, 'jabber:component:accept', [], handshake, [],[]}}, #state{name = ComponentName} = State) ->
		%{ok, ModuleState} = Module:start(ComponentName),
    io:format("Component : ~p started.~n", [ComponentName]),
{next_state, connected, State};
wait_handshake(Event, State) ->
    io:format("EV: ~p~n",[Event]),
    {next_state, wait_handshake, State}.

connected({packet, XMLString}, State) ->
    gen_tcp:send(State#state.socket, XMLString),
    {next_state, connected, State};
connected({xmlstreamelement, Packet}, State) ->
    Module = State#state.module,
		ModuleState = gen_component:receive_packet(Packet, Module, State#state.module_state),
    {next_state, connected, State#state{module_state = ModuleState}};
connected({feed, Feed}, State) ->
    Module = State#state.module,
		{ok, ModuleState} = Module:handle_feed(Feed, State#state.module_state),
    {next_state, connected, State#state{module_state = ModuleState}};

connected(Event, State) ->
    io:format("connected: ~p~n",[Event]),
    {next_state, connected, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, StateName, State) -> {next_state, NextStateName,
%%						  NextState} |
%%                                          {next_state, NextStateName,
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(stop, _StateName, State) ->
	{stop, normal, State};

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_sync_event(Event, From, StateName,
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState,
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState,
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(get_component_name, _From, StateName, State) ->
	{reply, State#state.name, StateName, State};

handle_sync_event(unavailable, _From, StateName, #state{module = Module, module_state = ModuleState} = State) ->
		Module:terminate(ModuleState),
	{reply, ok, StateName, State};

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState,
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket = Socket, parser = Parser, module = Module}) ->

	%exmpp_xml:stop_parser(Parser),
	gen_tcp:close(Socket),

    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Connect to the Jabber / XMPP server using the component protocol.
connect(_ComponentName, Server, Port) ->
    Socket = case gen_tcp:connect(Server, Port, [binary,{active, false},
						 {packet, 0}], 10000) of
		 {ok, Sock} ->
		     Sock;
		 {error, econnrefused = Reason} ->
		     io:format("Error: server ~s:~p refused the connection.~n~n",
			       [Server, Port]),
		     exit(Reason);
		 {error, Reason} ->
		     io:format("Connection error: [~p]~n~n", [Reason]),
		     exit(Reason)
	     end,
    Socket.

stream_open(Socket, ComponentName, _Server) ->
    Packet =["<stream:stream xmlns='jabber:component:accept'"
	     " xmlns:stream='http://etherx.jabber.org/streams'"
	     " to='", ComponentName, "'>"],
    gen_tcp:send(Socket, Packet).

handshake(Socket, StreamID, Secret) ->
	io:format("Handshake: ~p, ~p~n", [StreamID, Secret]),
    Handshake = sha:sha(StreamID ++ Secret),
    Packet =["<handshake>", Handshake, "</handshake>"],
    gen_tcp:send(Socket, Packet).


%% Parsing and reception process
start_xml_parser(Socket, Parser) ->
    %ok = erl_ddll:load_driver(epeios_config:lib_path(), expat_erl),
    XMLStreamState = xml_stream:new(self(), Parser),
    spawn_link(?MODULE, receiver, [Socket, XMLStreamState, Parser]).


receiver(Socket, XMLStreamState, Parser) ->
    process_flag(trap_exit, true),
    receiver_loop(Socket, XMLStreamState, Parser).
receiver_loop(Socket, XMLStreamState, Parser) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            NewXMLStreamState = xml_stream:parse(XMLStreamState, Data),
            receiver_loop(Socket, NewXMLStreamState, Parser);
        {error, Reason} ->
            io:format("Receiver loop: error: ~p~n",[Reason]),
            catch(xml_stream:close(XMLStreamState)),
            exit({error, Reason});
        %% Trap exit signal:
        Other ->
            io:format("Receiver loop: received: ~p~n",[Other]),
			catch(xml_stream:close(XMLStreamState)),
            exit({error, Other})
    end.


%%
test() ->
	application:start(exmpp),
	application:start(crypto),
	xep0114:start_component("test1.zephyr.local", "zephyr.local", 7047, "secret", test_component:new("test1.zephyr.local")),
	xep0114:start_component("test2.zephyr.local", "zephyr.local", 9049, "secret", test_component:new("test2.zephyr.local")),
	ok.
