-module(test).

-export([test_gen_component/0]).

test_gen_component() ->
		application:start(exmpp),
	application:start(crypto),
	gen_component:start_component("test1.zephyr.local", "zephyr.local", 7047, "secret", test_component:new("test1.zephyr.local"), []),
	%gen_component:start_component("test2.zephyr.local", "zephyr.local", 9049, "secret", test_component:new("test2.zephyr.local")),
		ok.
