-module('xep-0106').

-export([escape_jid/1]).

%% Escapes jid according to XEP-0106
escape_jid(Jid) ->
		escape_jid(Jid, []).

escape_jid([], Escaped) ->
		lists:reverse(Escaped);
escape_jid([32 | T], Acc) ->
		escape_jid(T, "02\\" ++ Acc);
escape_jid([34 | T], Acc) ->
		escape_jid(T, "22\\" ++ Acc);

escape_jid([38 | T], Acc) ->
		escape_jid(T, "62\\" ++ Acc);
escape_jid([39 | T], Acc) ->
		escape_jid(T, "72\\" ++ Acc);

escape_jid([47 | T], Acc) ->
		escape_jid(T, "f2\\" ++ Acc);
escape_jid([58 | T], Acc) ->
		escape_jid(T, "a3\\" ++ Acc);

escape_jid([60 | T], Acc) ->
		escape_jid(T, "c3\\" ++ Acc);
escape_jid([62 | T], Acc) ->
		escape_jid(T, "e3\\" ++ Acc);

escape_jid([64 | T], Acc) ->
		escape_jid(T, "04\\" ++ Acc);
escape_jid([92 | T], Acc) ->
		escape_jid(T, "c5\\" ++ Acc);

escape_jid([H | T], Acc) ->
		escape_jid(T, [string:to_lower(H) |Acc]).
