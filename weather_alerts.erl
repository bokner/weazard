-module(weather_alerts).

-export([generateWeatherAlerts/2]).

%% Testing
-export([choose_condition/0, weather_conditions/1]).
% Randow alert generator
-define(IMAGE_ROOT, "images/weather/").

generateWeatherAlerts(Cities, Callback) ->
		Subjects = ["Forecast for tomorrow", "Current conditions", "Weekend forecast", "Forecast for tomorrow", "Current conditions", "Weekend forecast"],
		Subject = lists:nth(random:uniform(length(Subjects)), Subjects),

		{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
		random:uniform(1000),

		% Temperature = %lists:nth(random:uniform(length(Bodies)), Bodies),
		%		num_to_str(random:uniform(90) - 55),
		Node = lists:nth(random:uniform(length(Cities)), Cities),
		{Image, Description, Temperature} = choose_condition(),
		Callback({Node, Subject, Description, num_to_str(Temperature), Image}).

choose_condition() ->
		{_Year, Month, _Day} = erlang:date(),
		{Low, High, Conditions} = conditions_by_month(Month),
		Index = lists:nth(random:uniform(length(Conditions)), Conditions),
		{get_image_url(Index), weather_conditions(Index), random:uniform(High - Low) + Low}.

conditions_by_month(Month) ->
		Season = if
								 Month < 3 orelse Month == 12 ->
										 winter;
								 Month > 2 andalso Month < 6 ->
										 spring;
								 Month > 5 andalso Month < 9 ->
										 summer;
								 Month > 8 ->
										 fall
						 end,
		% { Low, High, Conditions}
		case Season of
				winter ->
						{-20, 10, [5, 7, 13, 14,15,16, 20, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 41, 42, 43, 46]};
				spring ->
						{0, 20, [0, 1, 2, 3, 4, 6, 9, 11, 12, 17, 23, 24, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 37, 38, 39, 40, 44, 45, 47]};
				summer ->
						{20, 35, [0, 1, 2, 4, 6, 9, 11, 12, 17, 19, 20, 21, 22, 23, 24, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 44, 45, 47]};
				fall ->
						{10, 20, [1, 2, 5, 7, 8, 9, 10, 11, 12, 13, 14, 18, 20, 22, 23, 24, 26, 27, 28, 29, 30, 31, 32, 33, 34, 36, 39, 40, 45, 47]}
		end.

get_image_url(Number) ->
		list_to_binary(
			?IMAGE_ROOT ++
			%"http://i.cdn.turner.com/cnn/.element/img/2.0/global/nav/header%/header_cnn_com_logo.gif" %++
			num_to_str(Number) ++ ".png"
		 ).

num_to_str(Number) ->
		lists:flatten(io_lib:format("~p", [Number])).

weather_conditions(Index) ->
		case Index of
				0-> "Thunderstorm";
				1-> "Rain and windy";
				2-> "Light rain and windy";
				3-> "Thunderstorms in the vicinity";
				4-> "Heavy thunderstorm";
				5-> "Sleet";
				6-> "Hail";
				7-> "Freezing sleet";
				8-> "Freezing drizzle";
				9-> "Drizzle";
				10-> "Freezing rain";
				11-> "Light rain";
				12-> "Rain";
				13-> "Snow flurries";
				14-> "Light snow";
				15-> "Snow blowing";
				16-> "Snow";
				17-> "Light rain with thunder";
				18-> "Melting hail";
				19-> "Dust";
				20-> "Fog";
				21-> "Haze";
				22-> "Smoke";
				23-> "Windy";
				24-> "Variable wind";
				25-> "Frigid";
				26-> "Cloudy";
				27-> "Mostly cloudy (night)";
				28-> "Mostly cloudy (day)";
				29-> "Partly cloudy (night)";
				30-> "Partly cloudy (day)";
				31-> "Clear";
				32-> "Sunny";
				33-> "Fair (night)";
				34-> "Fair (day)";
				35-> "Rain with thunder";
				36-> "Hot";
				37-> "Scattered thunderstorms (day)";
				38-> "Thunder";
				39-> "Scattered showers (day)";
				40-> "Heavy rain";
				41-> "Scattered snow showers (day)";
				42-> "Heavy snow";
				43-> "Snow and windy";
				44-> "Showers in the vicinity";
				45-> "Scattered showers (night)";
				46-> "Scattered snow showers (night)";
				47-> "Scattered thunderstorms (night)"
		end.
