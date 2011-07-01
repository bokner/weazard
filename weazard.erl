%%% File    : test_component.erl
%%% Author  : Boris Okner <b.okner@rogers.com>
%%% Description : Test component implementing gen_component
%%% Created : 27 Jun 2009 by Boris Okner <b.okner@rogers.com>
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


-module(weazard, [Name]).



-behavior(gen_component).

% gen_component functions
-export([init/2, on_connect/1, terminate/1, send_packet/1,
				 handle_presence/4, handle_iq/4, handle_message/4,
				 disco_info/2, disco_items/2, handle_feed/2]).

%%% Tests
-export([weather_stations/1, weather_points/0, create_jid_table/1]).

-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").
-include_lib("exmpp/include/internal/exmpp_xmpp.hrl").
-include_lib("stdlib/include/qlc.hrl" ).

%% State has
%% - registrations: dict of services registered with users;
%% - weather_points: structure describing service nodes;
%% - jid_table - to associate escaped and lowercased jids with the original ones, i.e. "usa.california.los\20angeles" will be associated with "USA.California.Los Angeles".
%% We will use lowercased jids in and out in order to unify access to our service. jid_table is the way to associate jids with services
-record(state, {registrations = dict:new(), weather_points = [], stations = [], jid_table = dict:new()}).

init(_Name, []) ->


		WeatherPoints = weather_points(),
		WeatherStations = weather_stations(WeatherPoints),
%%lists:foreach(fun(S) -> send_available('xep-0106':escape_jid(S) ++ "@" ++ Name, undefined) end, WeatherStations),
		subscribe_for_weather_alerts(WeatherStations),

		{ok, #state{weather_points = WeatherPoints, stations = WeatherStations, jid_table = create_jid_table(WeatherPoints)}}.

on_connect(State) ->
		% Check if registration node is up
		% Send presence for all service nodes
		%lists:foreach(fun(S) -> send_available('xep-0106':escape_jid(S) ++ "@" ++ Name, undefined) end, State#state.stations),
		%send_available(Name, Name),
		{ok, State}.

terminate(State) ->
		% Send "unavailable" for all service nodes
		%lists:foreach(fun(S) -> send_unavailable('xep-0106':escape_jid(S) ++ "@" ++ Name, undefined) end, State#state.stations),
		%send_unavailable(Name, Name),
		ok.

%% Weather points database
weather_points() ->
		[{"Canada",
			[
			 {"Ontario", ["Toronto", "Ottawa", "Niagara Falls"]},
			 {"Quebec", ["Montreal", "Quebec"]},
			 {"Alberta", ["Calgary", "Edmonton"]},
			 {"British Columbia", ["Vancouver", "Victoria"]}
			]},
		 {"USA",
			[
			 {"Colorado", ["Boulder", "Denver"]},
			 {"California", ["San Francisco", "Los Angeles", "San Diego"]},
			 {"Texas", ["Dallas", "Houston", "Austin"]}

			]}
		].

weather_stations(WeatherPoints) ->
		weather_stations(WeatherPoints, []).

weather_stations([], Stations) ->
		io:format("Stations:~p~n", [Stations]),
		Stations;
weather_stations([ {NodeName, Children} | T], Stations) ->
		lists:append([Stations, lists:map(fun(C) -> NodeName ++ "." ++ C end, weather_stations(Children)),
									weather_stations(T)]);
weather_stations([Station | T], Stations)  ->
		weather_stations(T, [Station | Stations]).


create_jid_table(WeatherPoints) ->
		create_jid_table(WeatherPoints, dict:new()).

create_jid_table([], JidTable) ->
		JidTable;
create_jid_table([ {Location, Children} | T], JidTable) ->
		F = fun(K, V1, V2) -> V1 end,
		dict:merge(F,
							 dict:merge(F, dict:store('xep-0106':escape_jid(Location), Location, JidTable),
													dict:fold(fun(K, V, Acc) ->
																						NewLocation = Location ++ "." ++ V,
																						dict:store('xep-0106':escape_jid(NewLocation), NewLocation, Acc)
																		end, dict:new(), create_jid_table(Children))),

							 create_jid_table(T));
create_jid_table([Location | T], JidTable)  ->
		create_jid_table(T, dict:store('xep-0106':escape_jid(Location), Location, JidTable)).

get_weather_nodes([], Nodes) ->
		lists:map(fun(N) when is_tuple(N) -> {NodeName, _} = N, NodeName; (N) -> N end, Nodes);
get_weather_nodes([N | R] = Path, Nodes) when is_list(Path) ->
		case lists:keysearch(N, 1, Nodes) of
				{value, {N, Children}} ->
						get_weather_nodes(R, Children);
				false ->
						[]
		end.

get_weather_nodes_by_jid(Jid, Nodes) ->
		get_weather_nodes(jid_to_path(Jid), Nodes).

jid_to_path(Jid) ->
		string:tokens(Jid, ".").

find_node(Jid, JidTable) ->
		case dict:find(Jid, JidTable) of
				{ok, Node} ->
						Node;
				error ->
						Jid
		end.


% Node registrations
online_registrations(Registrations) ->
		dict:fold(fun(K, Status, Acc) -> case Status of
																				 available ->
																						 [K | Acc];
																				 _ ->
																						 Acc
																		 end
							end, [], Registrations).

online_registrations(Node, Registrations) ->
		dict:fold(fun({N, _} = K, Status, Acc) -> case (Status ==
																										available andalso Node == N ) of
																									true ->
																											[K | Acc];
																									false ->
																											Acc
																							end
							end, [], Registrations).

%% create_tables() ->
%% 	mnesia:create_table(registration, [{ram_copies, [node()]}, {attributes,record_info(fields, registration)},
%% 						  {index, [user]}]).


register_user(Node, JID, #state{registrations = Registrations} = State) ->
		Bare =  hd(exmpp_jid:binary_split(JID, $/)),
		R = dict:store({Node, Bare}, available, Registrations),
		{ok, State#state{registrations = R}}.

% Generic operation on registration record.
% Returns either not_registered, ok or {ok, Result}
operate_registration(Node, JID, Operation, Registrations) ->
		Bare = hd(exmpp_jid:binary_split(JID, $/)),
		case dict:find({Node, Bare}, Registrations) of
				error ->
						not_registered;
				{ok, Value} ->
						Operation({Node, Bare, Value})

		end.

unregister_user(Node, JID,  #state{registrations = R} = State) ->
		Result = operate_registration(Node, JID, fun({N, Bare, _Status}) ->
																										 {ok, State#state{registrations = dict:erase({N, Bare}, R)}}
																						 end, R),
		case Result of
				not_registered ->
						ok;
				_ ->
						Result
		end.

% Update status if record exists; otherwise return not_registered
set_status(Node, JID, Status, #state{registrations = R} = State) ->
		operate_registration(Node, JID, fun({N, Bare, _Status}) ->
																						{ok, State#state{registrations = dict:store({N, Bare}, Status, R)}}
																		end, R).

get_status(Node, JID, #state{registrations = R}) ->
		operate_registration(Node, JID, fun({_N, _Bare, Status}) ->
																						{ok, Status}
																		end, R).


% We are using gen_component helper by default; we could have
% implemented filtering etc. if needed.
send_packet(XmlString) ->
		io:format("Outgoing: ~p~n", [XmlString]),
		gen_component:send_packet(Name, XmlString).


% Stanzas
send_stanza(Stanza, From, To) ->
		Xmlel = exmpp_stanza:set_sender(Stanza, From),
		Xml = exmpp_xml:document_to_list(
						exmpp_stanza:set_recipient(Xmlel, To)),
		send_packet(Xml).

% "Avalable" packet
send_available(From, To) ->
		send_stanza(exmpp_presence:available(), From, To).

% "Unavalable" packet
send_unavailable(From, To) ->
		send_stanza(exmpp_presence:unavailable(), From, To).

% Presence probe
send_probe(From, To) ->
		send_stanza(exmpp_presence:probe(), From, To).

% Subscription request
send_subscribe(From, To) ->
		send_stanza(exmpp_presence:subscribe(), From, To).

%Subscription confirmation
send_subscribed(From, To) ->
		send_stanza(exmpp_presence:subscribed(), From, To).

%Unsubscription confirmation
send_unsubscribed(From, To) ->
		send_stanza(exmpp_presence:unsubscribed(), From, To).

%Unsubscription confirmation
send_unsubscribe(From, To) ->
		send_stanza(exmpp_presence:unsubscribed(), From, To).


disco_info(#jid{prep_node = undefined}, _State) ->

		#xmlel{name = 'query',
					 ns = ?NS_DISCO_INFO,
					 children =
					 [
						#xmlel{name = identity,
									 attrs = [
														#xmlattr{name = name, value = <<"Weather alert service">>},
														#xmlattr{name = category, value = <<"headline">>},
														#xmlattr{name = type, value = <<"weather">>}

													 ]
									}
					 ]};

disco_info(#jid{orig_jid = Jid}, #state{jid_table = JidTable}) ->
		Node = find_node(hd(string:tokens(binary_to_list(Jid), "@")), JidTable),
		#xmlel{name = 'query',
					 ns = ?NS_DISCO_INFO,
					 children =
					 [
						#xmlel{name = identity,
									 attrs = [
														#xmlattr{name = name, value = list_to_binary("Weather alerts for " ++ Node)},
														#xmlattr{name = category, value = <<"hierarchy">>},
														#xmlattr{name = type, value = <<"leaf">>}

													 ]
									},
						#xmlel{name = feature,
									 attrs = [
														#xmlattr{name = var, value = <<?NS_INBAND_REGISTER_s>>}
													 ]
									}
					 ]}.



disco_items(#jid{prep_node = undefined}, #state{weather_points = WeatherPoints}) ->

		#xmlel{name = 'query',
					 ns = ?NS_DISCO_ITEMS,
					 children = disco_items_xml(nil, get_weather_nodes([], WeatherPoints))
					};

disco_items(#jid{orig_jid = Jid}, #state{weather_points = WeatherPoints, jid_table = JidTable}) ->
		BareJid = hd(string:tokens(binary_to_list(Jid), "@")),
		Node = find_node(BareJid, JidTable),
		#xmlel{name = 'query',
					 ns = ?NS_DISCO_ITEMS,
					 children = disco_items_xml(BareJid, get_weather_nodes_by_jid(Node, WeatherPoints))
					}.

disco_items_xml(Prefix, Nodes) ->
		PrefixFunc = case Prefix of
										 nil -> fun(N) -> lists:concat([N, "@", Name]) end;
										 P -> fun(N) -> lists:concat([P, ".", N, "@", Name]) end
								 end,
		[

		 #xmlel{name = item,

						attrs = [
										 #xmlattr{name = jid, value = list_to_binary(PrefixFunc('xep-0106':escape_jid(Node)))},
										 #xmlattr{name = name, value = list_to_binary(Node)}

										]

					 } || Node <- Nodes

									 ].

% Disco#info
handle_iq(#iq{kind = request, type = get,  ns = ?NS_DISCO_INFO, iq_ns = ?NS_COMPONENT_ACCEPT} = IQ, From, To, State) ->
		io:format("disco#info request~n"),
		Result = exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, disco_info(exmpp_jid:parse(To), State))
							),
		send_stanza(Result, To, From),
		ok;

% Disco#items
handle_iq(#iq{kind = request, type = get,  ns = ?NS_DISCO_ITEMS, iq_ns = ?NS_COMPONENT_ACCEPT} = IQ, From, To, State) ->
		io:format("disco#items request~n"),
		Result = exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, disco_items(exmpp_jid:parse(To), State))
							),
		send_stanza(Result, To, From),
		ok;

% Registration
% Here we simply acknowledge registration;
% we could have sent a registration form (see, for example, "XMPP definitive guide" book for examples of advanced registration)
handle_iq(#iq{kind = request, type = Type, ns = ?NS_INBAND_REGISTER, iq_ns = ?NS_COMPONENT_ACCEPT} = IQ, From, To, State) ->
		io:format("~p: registration request received from ~p to ~p:~n", [Name, From, To]),
		Result = exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ)
							),
		send_stanza(Result, To, From),
		% send availability and subscription request in case type = set
		case Type of
				set ->
						send_available(To, From),
						send_probe(To, From),
						send_subscribe(To, From),
						send_subscribed(To, From),
						register_user(To, From, State);
				_ -> ok
		end;

% Unknown IQ type
handle_iq(#iq{kind = Kind, type = Type, ns = NS, iq_ns = IQ_NS}, From, To, _State) ->
		io:format("~p: unknown IQ received from ~p to ~p:~n", [Name, From, To]),
		io:format("kind = ~p, type = ~p, ns = ~p, iq_ns = ~p~n", [Kind, Type, NS, IQ_NS]),
		ok.

% Presence handling
handle_presence(Packet, From, To, State) ->
		handle_presence2(exmpp_presence:get_type(Packet),
										 exmpp_presence:get_show(Packet),
										 exmpp_presence:get_status(Packet),
										 exmpp_presence:get_priority(Packet),
										 From,
										 To, State).

handle_presence2(Type, _Show, _Status, _Priority, From, To, State) when Type == probe; Type == available ->
%%case set_status(To, From, available, State) of
%%		{ok, NewState} ->
		send_available(To, From),
		if To == Name ->
						ok; %% This is an initial presence probe or directed presence; no need to register user
			 true ->
						register_user(To, From, State)
		end;
%%	Other ->
%%			io:format("set_status returned ~p~n", [Other]),
%%			ok
%%end;

handle_presence2(unavailable, _Show, _Status, _Priority, From, To, State) ->
		case set_status(To, From, unavailable, State) of
				{ok, NewState} ->
						{ok, NewState};
				Other ->
						io:format("set_status returned ~p~n", [Other]),
						ok
		end;

handle_presence2(subscribe, _Show, _Status, _Priority, From, To, State) ->
		case get_status(To, From, State) of
				{ok, _} ->
						send_subscribed(To, From),
						send_available(To, From),
						ok;
				not_registered ->
						send_subscribed(To, From),
						send_subscribe(To, From),
						send_available(To, From),
				    send_probe(To, From),
						register_user(To, From, State);
				Other ->
						throw({get_status_error, Other})
		end;

handle_presence2(unsubscribe, _Show, _Status, _Priority, From, To, State) ->
		send_unsubscribed(To, From),
		send_unsubscribe(To, From),
		unregister_user(To, From, State);

handle_presence2(_,_,_,_,_,_, _State) ->
		ok.

handle_message(_Packet, _From, _To, _State) ->
		ok.

% Handle feed from underlying service
handle_feed(Feed, State) ->
		{send_alert(Feed, State), State}.




%% Subscribe to weather alerts (WA) on behalf of component;
%% Provide a callback for the WA service.

subscribe_for_weather_alerts(Stations) ->
		timer:apply_interval(15000, weather_alerts, generateWeatherAlerts,[Stations, fun(Alert) -> gen_component:feed_component(Name, Alert) end]).


send_alert({Node, Subject, Description, Temperature, Image} = Alert, State) ->
		io:format("Alert:~p~n", [Alert]),
		NodeJID = exmpp_jid:to_binary('xep-0106':escape_jid(Node), Name),
		Registrations = online_registrations(NodeJID, State#state.registrations),


		lists:foreach(fun({_Node, R}) ->

													HTML = exmpp_xml:append_children(
																	 #xmlel{name ='html', ns = ?NS_XHTML_IM},
																	 [
																		exmpp_xml:append_children(#xmlel{name = 'body', ns = ?NS_XHTML},
																															[

																															 exmpp_xml:append_children(#xmlel{name = 'p'},
																																												 [
																																													#xmlel{name = 'img', attrs = [#xmlattr{name = 'src', value = Image}]}
																																												 ])
																															])

																	 ]
																	),
													send_stanza(exmpp_xml:append_children(
																				exmpp_message:headline(Subject, Description ++ "::" ++ Temperature), [HTML]),
																			NodeJID, R)
									end,
									Registrations),
		ok.
