//TODO - handle service-unavailable when component is not running
//TODO - unavailable presence from service nodes
//TODO - find icons for buttons (i.e. connected, disconnected, running)
//TODO - handle component failure - improper-addressing etc
//TODO - unavailable from connection.jid to itself causes unavailable for each registered node
//TODO - restoring after service failure - requires subscription to root Jid (and changes to server side)


var BOSH_SERVICE = '/http-bind';
var connection = null;
var weazard_bot = '_weazard_bot_@zephyr.local';
var weazard_bot_passw = 'weazard';
var weazard_admin = 'admin@zephyr.local';
var VHOST = "zephyr.local";
var WEATHER_SERVICE = "test1.zephyr.local";
var DISCONNECTION_TIMEOUT = 10000;

var service_nodes = new Array();

var registration_flag = false; // Indicates if registration taook place upon signing in  
var login_flag = false;  // Indicates if we are logged in

var handler_counter = 0;
// Counter of disco item handlers
function log(msg)
 {
    $('<div></div>').append(document.createTextNode(msg)).prependTo('#log');
    //alert(msg);
}


function stat(msg)
 {
    $('#status').text(msg);
}
function rawInput(data)
 {
    log('RECV: ' + data);
}

function rawOutput(data)
 {
    log('SENT: ' + data);
}

function jid(node, host) {
    return node + "@" + host;
}

function bare(jid) {
    return jid.split('@')[0].replace(/[\.\\]/g, "_");
}


function createConnection() {
	connection = new Strophe.Connection(BOSH_SERVICE);
	connection.rawInput = rawInput;
	connection.rawOutput = rawOutput;
}
// Logging in
function login() {
    log("Logging in...");
	login_flag = false;
	createConnection();
    connection.connect(jid($('#name').val(), VHOST),
    $('#password').val(),
    onConnect);
}  

function reconnect() {
	 login_flag = false;
	 setTimeout(function() {login()}, DISCONNECTION_TIMEOUT);	
	
}

var div_colors = ['rgb(51, 51, 153)', 'rgb(51, 51, 102)'];
var color_idx = 1;


function onConnect(status)
 {
    log("Connection status:" + status);
    if (status == Strophe.Status.CONNECTING) {
        log('Strophe is connecting.');
    } else if (status == Strophe.Status.CONNFAIL) {
        error('The connection to Jabber cannot be established, will try again in ' + DISCONNECTION_TIMEOUT/1000 + " sec...");  
		reconnect();
    } else if (status == Strophe.Status.AUTHFAIL) {
        error('Invalid username and/or password');
    } else if (status == Strophe.Status.DISCONNECTING) {
        log('Strophe is disconnecting.');
    } else if (status == Strophe.Status.DISCONNECTED) {
  		show_connected(false);
		if (login_flag) { // session disconnected, trying to restore;
			error('The connection to Jabber has been lost, will reconnect in ' + DISCONNECTION_TIMEOUT/1000 + ' sec...');
	       
			 reconnect();
		}

        //$('#connect').get(0).value = 'connect';
    } else if (status == Strophe.Status.CONNECTED) {
        login_flag = true; 
        info('Connection to Jabber server is up.');
		show_connected(true);
 
        $("#login").dialog("close");


            connect_to_weather_service();

    } 

    //Stop spinner
    $.spinnerStop();
    $("#spinner").hide();


}                                

function connect_to_weather_service() {
    // discover weather service nodes
    var id = connection.getUniqueId("weather");

    var discoInfoIq = $iq({
        'from': connection.jid,
        'to': WEATHER_SERVICE,
        'id': id,
        'type': 'get'
    }
    )
    .c('query', {
        'xmlns': Strophe.NS.DISCO_INFO
    });	
    // Add error handler
 	connection.addHandler(onWeatherServiceError, null, null, 'error', null, WEATHER_SERVICE);

    connection.addHandler(onDiscoInfo, Strophe.NS.DISCO_INFO, 'iq', 'result', null);
                                
	show_service_status("#weather_service", "service_connecting", ": connecting...");
    connection.send(discoInfoIq.tree());	
	
}
                                           
function show_service_status(service, status, message) {
	$(service).removeAttr("class").addClass(status).find(".service_msg").text(message);
}

function show_connected(flag) {
	if (flag) { 
			show_service_status("#jabber_service", "service_available", "is up");
	} else {
		show_service_status("#jabber_service", "service_unavailable", "is down");
		// Weather server is not available if HTTP is down
		show_service_status("#weather_service", "service_unavailable", "not accessible");
		
	}
}

function registerAccount() {
    log("registration");
	createConnection();
    // We connect with preregistered account
    connection.connect(weazard_bot + "/" + $('#name').val(),
    weazard_bot_passw,
    onShadowConnect);
    // Callback will do registration
    return true;
}

// We should be connected, otherwise there is a severe problem with a bot account/server
function onShadowConnect(status) {
    //stat("Status:" + status);
    if (status == Strophe.Status.DISCONNECTED) {
        if (registration_flag) {
            //disconnection was requested in order to reconnect with newly created account
            registration_flag = false;
            login();
        } 
    } else if (status == Strophe.Status.CONNFAIL) {
		error('System error on registration attempt.');


    } else if (status == Strophe.Status.CONNECTED) {
        log("Shadow connect");
        // We have logged in using bot account, now trying to register...
        connection.addHandler(onRegisterCred, 'jabber:iq:register', 'iq', null, null, null);
        var iq = $iq(
        {
            "id": connection.getUniqueId("weather"),
            "type": "get"
        })
        .c("query", {
            "xmlns": "jabber:iq:register"
        });

        connection.send(iq.tree());
    }
    //return true;
}

function onRegisterCred(e) {
    log("onRegisterCred");
    var reg_id = connection.getUniqueId("weather");
    var iq = $iq(
    {
        "id": reg_id,
        "type": "set"
    })
    .c("query", {
        "xmlns": "jabber:iq:register"
    })
    .c('username').t($('#name').val())
    .up()
    .c('password').t($('#password').val());
    connection.addHandler(onAck, null, 'iq', null, reg_id, null);
    connection.send(iq.tree());
    return false;
}

function onAck(msg) {
    log("Ack:");
    var error_code = $(msg).parent().find('error').attr('code');

    if (error_code == "409") {
        error("Account " + $(msg).find('username').text() + " has been taken");
    } else if (error_code != undefined) {
        error("Account " + $(msg).find('username').text() + " registration has failed, error code =" + error_code);
    } else {
        //Successful registration
        info("Registration successfull, logging in...");
        // Reconnect with newly registered account
        registration_flag = true;
        connection.disconnect();
        // The callback will reconnect...

    }


}

function onWeatherServiceError(msg) {
    var error_code = $(msg).parent().find('error').attr('code');
	show_service_status("#weather_service", "service_unavailable", ": Error code " + error_code);
	log('Reconnecting to weather service in ' + DISCONNECTION_TIMEOUT/1000 + ' sec...'); 
	error('Weather Service is unavailable, will try to reconnect in ' + DISCONNECTION_TIMEOUT/1000 + " sec...");
	setTimeout(function() {connect_to_weather_service();}, DISCONNECTION_TIMEOUT)
	return false;
}

function onDiscoInfo(e) {
    var elem = $(e);
    // make this Element a JQuery Element
    var service_node = elem.parent().find('iq').attr('from');
    var id = connection.getUniqueId("weather");
    var discoItemsIq = $iq({
        'from': connection.jid,
        'to': service_node,
        'id': id,
        'type': 'get'
    }
    )
    .c('query', {
        'xmlns': Strophe.NS.DISCO_ITEMS
    });


    if (service_node == WEATHER_SERVICE) {
        // We only create "items" handler once while we are at the top level
        //connection.addHandler(function(e) {return onDiscoItems(e, $('#service_nodes'))}, Strophe.NS.DISCO_ITEMS, 'iq', 'result', id);
        addDiscoItemsHandler($('#service_nodes'), service_node)
    }
    //connection.send(discoItemsIq.tree());
    throttle_send(discoItemsIq.tree(), 100);

    return false;
}

function addDiscoItemsHandler(context, from) {
    handler_counter = handler_counter + 1;
    //We count handlers in order to know when discovery process ends
    log(from + ": Handler+:" + handler_counter);
    connection.addHandler(function(e) {
        return onDiscoItems(e, context, from)
    },
    Strophe.NS.DISCO_ITEMS, 'iq', 'result', null, from);
}

var token = true; //Sync token;

function onDiscoItems(e, context, source) {
    handler_counter = handler_counter - 1;
    //log(source + ": Handler -:" + handler_counter);
    var elem = $(e);
	var jid = elem.attr('from');
    var items = elem.find('query item')
    if (items.length == 0) { //Leaf node 
		var anchor = context.find('span a');
	       context.bind('click.file',
        function(event)
        {   
			//var jid = $(this).children("span").attr('jid');
            if ($(this).children("span").is('.file'))
            if ($(this).find("span a").is('.offline')) {
                subscribeToAlerts(jid);
            } else {
                unsubscribeFromAlerts(jid);
            }
        });
		anchor.addClass('offline');
		// Register presence and message handlers
	        connection.addHandler(function(e)
	        {
	            return onServiceMessage(e, jid)
	        },
	        null, 'message', null, null, jid);  //message handler

	        connection.addHandler(function(e)
	        {
				return onServicePresence(e, jid)
	        },
	        null, 'presence', null, null, jid); //presence handler

	} else {
		items.map(function() {
	
        var service_node = $(this).attr('jid');
		// Check if node is already there 	 
		if ($("#" + bare(service_node)).is("*")) {return;}
		// This is new node; add to the tree.
        var node_name = $(this).attr('name');
        var id = connection.getUniqueId("weather");
        var discoItemsIq = $iq({
            'from': connection.jid,
            'to': service_node,
            'id': id,
            'type': 'get'
        }
        )
        .c('query', {
            'xmlns': Strophe.NS.DISCO_ITEMS
        });
        // Add new node to the treeview
        var button = $('<span id="' + bare(service_node) + '" class="file"></span>').append($('<a href="#" >' + node_name + '</a>'))

		.attr('jid', service_node)
		.attr('nodename', node_name);

        new_dom_node = $('<li></li>').append(button);

        // Add to folder
        var new_context;
        if (context.is('.filetree')) {
            // Adding to root
            new_context = new_dom_node.appendTo(context);
        } else

        if (context.children('span').is('.folder')) {
            new_context = new_dom_node.appendTo(context.find('ul'))
        } else {
            // Turn a leaf into the folder
            context.children('span').addClass("folder").removeClass("file");

            // Replace button to text
            context.children('span').text(context.find('span a').text());

            new_context = new_dom_node.appendTo($('<ul></ul>').appendTo(context));

        }
        //new_context.siblings().children('span').removeClass('last');

        //connection.addHandler(function(e) {return onDiscoItems(e, new_context)}, Strophe.NS.DISCO_ITEMS, 'iq', 'result', id);
        //connection.send(discoItemsIq.tree());
        addDiscoItemsHandler(new_context, service_node);
        //connection.send(discoItemsIq.tree());
        throttle_send(discoItemsIq.tree(), 20);
        //var jid = $(this).attr("jid");
        //add_node(jid);
        return $(this).attr("name");
    }).get(); 
	}

    //Check if we are finished with discovery process
    if (handler_counter == 0) {
        log("Service discovery completed");
        // Build and show service nodes
        buildTree();          
		show_service_status("#weather_service", "service_available", "is up");
		info("Weather service is up.");
    }
    // Return token so deferred sends could continue
    token = true;

    return false;
}






function throttle_send(stanza, interval) {
    if (!token) {
        setTimeout(function() {
            throttle_send(stanza, interval)
        },
        interval);
        // Defer sending
    } else {
        token = false;
        // grab token
        connection.send(stanza);
    }
}


function buildTree() {
    //Create treeview (data should be in)
    $('#service_nodes').treeview();
    //Roster push handler; stanzas will be sent by server, so no individual handlers required
    connection.addHandler(onRosterPush, Strophe.NS.ROSTER, 'iq', 'set', null, null);


	// on connection.jid unavailable
	connection.addHandler(function(e) {
		log("Presence");
		if ($(e).attr('type') == 'unavailable' && $(e).attr('to') == connection.jid) {
		   //error("We are offline.. Will try to go back online in 5 sec...");

    	setTimeout(function() {connection.send($pres().tree())}, 5000);

		}
 		return true;
	}, null, 'presence', null, null, null)
    //broadcast our presence
    connection.send($pres().tree());

}

function onRosterPush(e) {
    log("Roster push");
    var elem = $(e);
    elem.find('query item').each(function(i) {
        var jid = $(this).attr('jid').toLowerCase();

        var subscription = $(this).attr('subscription');

        if (subscription == 'to') {
            // We have subscribed to service, now let service subscribe to us
            log("Subscription:" + subscription);
            confirmSubscription(jid);
        } else if (subscription == 'both') {
            // We have completed a subscription handshake
            serviceActive(jid);
        } else if (subscription == 'remove') {
            serviceOffline(jid);
        } else if (subscription == 'from') {
			// We have unsubscribed from service, have service unsubscribe from us
			confirmUnsubscription(jid);
	        serviceOffline(jid);
		} else if (subscription == 'none') {
	        serviceOffline(jid);
		}
    })
    return true;

}

function serviceActive(jid) {
	var anchor = $('#' + bare(jid) + " a");
	if (!anchor.is(".active")) {
	// Show service node as subscribed
    	anchor.addClass("active").removeClass("offline").removeClass("connecting");
    	section_message(jid, "Waiting for incoming data...");
	}
}

function serviceOffline(jid) {
    $('#' + bare(jid) + " a").addClass("offline").removeClass("active").removeClass("connecting");
    removeSection(jid);
}

function serviceRequested(jid, msg) {
    $('#' + bare(jid) + " a").removeClass("offline").removeClass("active").addClass("connecting");
    //activateSection(jid, msg);
}


function onServicePresence(e, jid) {
    var type = $(e).attr('type');
    if (type == 'subscribed') {
        //The service has confirmed your subscription
        info("Successfully subscribed to:" + jid);
    } else if (type == 'subscribe') {
        //The service sends a subscription request
        confirmSubscription(jid);

    } else if (type == 'unsubscribed') {
        //
        info("You have been unsubscribed from " + jid);
        serviceOffline(jid);
    } else if (type == 'unavailable') {
        // Service has gone offline
        log("Service " + jid + " has gone offline");
        serviceOffline(jid);
    } else if (type == 'unsubscribe') {
        // Service requires unsubscription
		confirmUnsubscription(jid);
        serviceOffline(jid);
    } else if (type == undefined) {
        // Service has sent presence
        log("Service " + jid + " is online");
        serviceActive(jid);
    }
    return true;
}

function onServiceMessage(msg, jid) {
	log("Alert from " + jid);

    $(msg).parent().find("message[from]:has(body, html)")
    .each(function() {
        var body = $(this).find("body:first").text();
		var subject = $(this).find("subject:first").text();
		var s = body.split('::');
		var description = s[0];
		var temperature = s[1] + " &deg;C";
        //var html_obj = $(this).find("html:first");
        var from = $(this).attr("from");
        var image_url = $(this).find("html img").attr("src");
        html_text =
		 "<div style='position:absolute; left:0;'><img style='width:80px; height:80px;' src='" + image_url + "' ></div>" +
				"<div style = 'display:block; position:absolute; top:0;  margin-left:130px;  '><p style='font-size:22px; color:rgb(252,105,252);'>" + subject + ", " + getNodeName(jid) + "</p></div>" +
		"<div style = 'display:block; position:absolute; margin-top:45px;  margin-left:130px; border-width: 3px; '><span style='font-size:18px; color:white;'>" + description + ", </span><span style='font-size:18px; color:yellow;'>" + temperature + "</span></div>" //+
	    //"<div style = 'display:block; position:absolute; right:20px; margin-top:45px;  border-width: 3px; '><span style='font-size:20px; color:yellow;'>" + temperature + "</span></div>";

        if (color_idx == 1) {
            div_color = div_colors[0];
        }
        else {
            div_color = div_colors[1];
        };
        color_idx = -color_idx;
        //if (html_text != "undefined") {
        $("#section_" + bare(jid)).children("p").html("<div>" + html_text + "</div>")
        //Strophe.serialize(html_text[0])
        .css({
            'background-color': div_color,
            'font-weight': '',
            'color': 'rgb(0,40,244)',
            'border-width': '1px',
            'border-color': 'white',
            'border-style': 'solid',
            'width': '100%',
			'maxwidth': '300px',
            'height': '85px'
        }).corner("bite");
		// Adjust header
        $("#header_" + bare(jid)).find('img').attr('src', image_url);
        // Show section
		showSection(jid);
		// Animate service node
		//$('#' + bare(jid) + " a").animate({ color: "#68BFEF", fontSize:"200%" }, 2000).animate({ color: "black", fontSize: "100%" }, 1000);


    });
    return true;
}

function confirmSubscription(jid) {
    // Confirm subscription
    log("Sending confirmation for " + jid);
    connection.send($pres({
        'from': connection.jid,
        'to': jid,
        'type': 'subscribed'
    }
    ).tree())
}

function confirmUnsubscription(jid) {
    // Confirm subscription
    log("Sending confirmation for " + jid);
    connection.send($pres({
        'from': connection.jid,
        'to': jid,
        'type': 'unsubscribed'
    }
    ).tree())
}

function subscribeToAlerts(jid) {
	// Add section
	//addSection(jid, node_name, "Loading...");
    //$("#alerts").accordion("activate", $("#alerts").find("h3").length - 1);
    // Show service node as connecting
    serviceRequested(jid, "Waiting for subscription confirmation...");
    // Request subscription
    connection.send($pres({
        'from': connection.jid,
        'id': connection.getUniqueId(),
        'to': jid,
        'type': 'subscribe'
    }
    ).tree());

}

function addSection(jid, node_name, text) {
    // Add section to accordion
    //$("#alerts").accordion("disable");
    var section = $('<h3><a class="alert_header" id="header_' + bare(jid)+  '" href="#"><div style="display:block; position:absolute; "><p style="vertical-align: middle">' + node_name + '</p></div><div style="display:block; position:absolute; right:10px; "><img style="vertical-align: middle; border: medium none;" src="/images/weather/na.png"></div></a></h3><div id="section_' + bare(jid) + '"><p>'+ text + '</p></div>').appendTo("#alerts");

	$("#alerts").accordion("destroy");
    $("#alerts").accordion({ 'clearStyle': true });
}

function removeSection(jid) {
	$('#header_' + bare(jid)).parent('h3').remove();
	$('#section_' + bare(jid)).remove();
}

function section_message(jid, text) {
	var section = $("#section_" + bare(jid));

	//var section_index =  $("#alerts div").index(section);
    if (!section.is("*")) { //No section for this jid, create one
	   addSection(jid, getNodeName(jid), text);
	} else {
		section.children("p").text(text);
	}
	//$("#header_" + bare(jid)).trigger("click");
	//$("#alerts").accordion("disable");
    //$("#alerts").accordion("activate", section_index);
    //$("#alerts").accordion("enable");
}

function showSection(jid) {
	$("#header_" + bare(jid)).trigger("click");
}

function getNodeName(jid) {
	return $("#service_nodes").find("span[jid='"+ jid + "']").attr('nodename');
}

function unsubscribeFromAlerts(jid) {
	serviceRequested(jid, "Unsubscribing...");
    connection.send($pres({
        'from': connection.jid,
        'id': connection.getUniqueId(),
        'to': jid,
        'type': 'unsubscribe'
    }
    ).tree());
    connection.send($pres({
        'from': connection.jid,
        'id': connection.getUniqueId(),
        'to': jid,
        'type': 'unsubscribed'
    }
    ).tree());

}


function startSpinner() {
	//$('#spinner').vCenter();
	// Start spinning
	$("#spinner").spinner({
		height: 48,
				width: 48,
				speed: 50,
				image: 'images/spotlight_spinner_big.png'
				});
}



function show_help() {
	alert("Show help");
} 

function ask_admin() {
		$('#subject').val('');
		$('#message_body').val('');
	    $("#admin_message").dialog({
	        buttons: { 
				"Send" : function() {
						//
						$("#admin_message").dialog("destroy");
						send_message(weazard_admin, $('#subject').val(), $('#message_body').val());
					},
	            "Cancel": function() {
						$("#admin_message").dialog("destroy"); 
	            }

	        },
	        width: 400,
	        title: "Please submit your message to Weazard admin",
	        modal: true,
			position: 'center',
			autoOpen: true
	    }).show();

		

}
  

function send_message(to, subject, body) {
	var msg = $msg({to: to, from: connection.jid, id: connection.getUniqueId(), type: 'chat'}).c("body").t(body).up().c("subject").t(subject);
	connection.send(msg.tree()); 
	info("Your message has been sent.")
} 


function info(msg) { 
  	//$.message.defaults.template = '<div class="jquery-message"><div class="round"></div><p></p><div class="round"></div></div>';
	$(".jquery-message").css("color", "green")
	$().message(msg);   
	
} 

function error(msg) { 
  	//$.message.defaults.template = '<div class="jquery-error-message"><div class="round"></div><p></p><div class="round"></div></div>';
	$(".jquery-message").css("color", "red");	
	$().message(msg);	
}
// JQuery's document.ready
$(document).ready(function() {
	// Do disclaimer message
	info("Disclaimer:\nThis site is intended to be used for testing and education purposes only.\nThe data displayed on this site doesn't represent any real data.\We expressly disclaim any liability, loss or damage resulting in using this site."); 
    // Do layout
    $('body').layout({
        applyDefaultStyles: true,
		east__paneSelector:		".outer-east",
		west__paneSelector: 		".outer-west",
		center__paneSelector: 	".outer-center",
		west__size: 350,
		east__size: 350
    });

	var outer_east = $(".outer-east").layout({
        applyDefaultStyles: true, north__resizable:false, north__slidable:false, north__size: 300}); 
		//outer_east.attr("")
	var outer_west = $(".outer-west").layout({applyDefaultStyles: true, north__resizable:false, closable: false, north__slidable:false, north__size: 45});
	outer_west.resetOverflow("north");

	var outer_center = $(".outer-center").layout({applyDefaultStyles: true,
        north__resizable:false, closable: false, north__slidable:false, north__size: 45});
		outer_center.resetOverflow("north"); 
	
	// Prepare support section	
	$('#help').click(function(e) {show_help()});
	$('#ask_admin').click(function(e) {ask_admin()})	
    // Accordion                     
    $('#alerts').accordion();
		//Prepare for login/registration
		log('Start');



    // Ask for credentials
    $("#login").dialog({
        buttons: { 
			"Register new account" : function() {
					startSpinner();
					registerAccount();
				},
            "Connect": function() {
							startSpinner();
							// Connect with the provided password
							login();
            }
 
        },
        width: 400,
        title: "Please enter your Weazard credentials.",
		position: 'center',
        modal: true

    });
    // Preload images
	$(window).bind('load', function(){
	   var preload = [
	     '/images/cross.png',
	     '/images/tick.png',
	     '/images/hourglass.png'
	   ];          
	   $(document.createElement('img')).bind('load', function(){
	    if(preload[0]) this.src = preload.shift();
	   }).trigger('load');          

	});

});



$(window).unload(function()
 {
    var unavailable = $pres({
        type: 'unavailable'
    });
    connection.send(unavailable.tree());

});



