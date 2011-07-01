{application, weazard,
[{description,"Weazard XMPP component"},
{vsn,"0.1"},
{modules,[weazard, gen_component, 'xep-0106', weather_alerts,
									 xml, xml_stream, sha
]},
{registered,[]},
{applications,[kernel, stdlib]},
{mod,{weazard_app,["test1.zephyr.local", "zephyr.local", 7047, "secret"]}}]}.
