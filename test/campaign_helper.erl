-module(campaign_helper).

-compile(export_all).


%% BID REQUESTS
campaign_1() ->
	<<"{
			\"id\": \"53e292e0\",
			\"name\": \"Balistreri, Kuphal and Homenick\",
			\"description\": \"deserunt eaque id voluptate nostrum\",
			\"start_time\": \"2017-01-25 14:48:26\",
			\"end_time\": \"2017-02-08 14:48:26\",
			\"bid\": 2.0,
			\"bidmax\": 3.1,
			\"adomain\": [\"will.com\"],
			\"ctrurl\": \"http:\/\/adserver.tapklik.com\/www\/delivery\/ck.php?n=a9d047cc&amp;cb=1485355706\",
			\"status\": \"running\",
			\"approved\": 0,
			\"test\": false,
			\"weight\": 1,
			\"node\": \"\",
			\"exchange\": [1],
			\"device\": {
				\"data\": {
					\"type\": 4,
					\"make\": \"Apple\",
					\"model\": \"iPhone\",
					\"os\": \"iOS\",
					\"ua\": \"Mozilla\/5.0\"
				}
			},
			\"user\": [2, 5, 10],
			\"pacing\": {
				\"enable\": false,
				\"settings\": []
			},
			\"custom_filters\": {
				\"data\": []
			},
			\"bid_logic\": {
				\"data\": []
			},
			\"hourofweek\": {
				\"data\": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168]
			},
			\"budget\": {
				\"data\": {
					\"total\": 1907,
					\"daily\": {
						\"total\": 62,
						\"spend\": 15
					}
				}
			},
			\"cat\": {
				\"data\": [\"IAB24\", \"IAB20\", \"IAB1\", \"IAB18\", \"IAB6\", \"IAB7\", \"IAB24\", \"IAB21\", \"IAB19\", \"IAB13\", \"IAB23\", \"IAB21\", \"IAB18\"]
			},
			\"creatives\": {
				\"data\": [{
					\"expdir\": 0,
					\"crid\": \"801c9561-66dd-4a3d-b97d-c8a7e04c0ac6\",
					\"adm\": \"<iframe id='a3b53e1d' name='a3b53e1d' src='http:\/\/adserver.tapklik.com\/www\/delivery\/afr.php?zoneid=1&amp;cb=12323' frameborder='0' scrolling='no' width='300' height='250'><a href='http:\/\/adserver.tapklik.com\/www\/delivery\/ck.php?n=a9d047cc&amp;cb=123432' target='_blank'><img src='http:\/\/adserver.tapklik.com\/www\/delivery\/avw.php?zoneid=1&amp;cb=12343&amp;n=a9d047cc' border='0' alt='' \/><\/a><\/iframe>\",
					\"ctrurl\": \"http:\/\/adserver.tapklik.com\/www\/delivery\/ck.php?n=a9d047cc&amp;cb=1485355702\",
					\"iurl\": \"http:\/\/lorempixel.com\/640\/480\/?93292\",
					\"type\": 1069,
					\"btype\": [],
					\"attr\": [1, 3],
					\"pos\": 0
				}, {
					\"expdir\": 0,
					\"crid\": \"28b920b3-e365-4d19-a188-b5d82060b23b\",
					\"adm\": \"<iframe id='a3b53e1d' name='a3b53e1d' src='http:\/\/adserver.tapklik.com\/www\/delivery\/afr.php?zoneid=1&amp;cb=12323' frameborder='0' scrolling='no' width='300' height='250'><a href='http:\/\/adserver.tapklik.com\/www\/delivery\/ck.php?n=a9d047cc&amp;cb=123432' target='_blank'><img src='http:\/\/adserver.tapklik.com\/www\/delivery\/avw.php?zoneid=1&amp;cb=12343&amp;n=a9d047cc' border='0' alt='' \/><\/a><\/iframe>\",
					\"ctrurl\": \"http:\/\/adserver.tapklik.com\/www\/delivery\/ck.php?n=a9d047cc&amp;cb=1485355703\",
					\"iurl\": \"http:\/\/lorempixel.com\/640\/480\/?88741\",
					\"type\": 1059,
					\"btype\": [],
					\"attr\": [1, 3],
					\"pos\": 0
				}]
			},
			\"geo\": {
				\"data\": [{
					\"country\": \"USA\",
					\"city\": \"\",
					\"region\": \"LA\",
					\"region_name\": \"California\"
				}, {
					\"country\": \"ARE\",
					\"city\": \"Dubai\",
					\"region\": \"DU\",
					\"region_name\": \"Dubai\"
				}]
			}
		}">>.



%% DATA
regions() ->
	[
		{<<"ARE-AZ">>, <<"Abu Dhabi">>},
		{<<"ARE-AJ">>, <<"Ajman">>},
		{<<"ARE-FU">>, <<"Fujairah">>},
		{<<"ARE-SH">>, <<"Sharjah">>},
		{<<"ARE-DU">>, <<"Dubai">>},
		{<<"ARE-RK">>, <<"Ras al-Khaimah">>},
		{<<"ARE-UQ">>, <<"Umm al-Quwain">>},
		{<<"SAU-11">>, <<"Al Bahah">>},
		{<<"SAU-08">>, <<"Al Hudud Al Shamaliyah">>},
		{<<"SAU-12">>, <<"Al Jawf">>},
		{<<"SAU-03">>, <<"Al Madinah al Munawwarah">>},
		{<<"SAU-05">>, <<"Al Qasim">>},
		{<<"SAU-01">>, <<"Riyadh">>},
		{<<"SAU-04">>, <<"Al Sharqiya">>},
		{<<"SAU-14">>, <<"Asir">>},
		{<<"SAU-06">>, <<"Hail">>},
		{<<"SAU-09">>, <<"Jazan">>},
		{<<"SAU-02">>, <<"Makkah">>},
		{<<"SAU-10">>, <<"Najran">>},
		{<<"SAU-07">>, <<"Tabuk">>}
	].

categories() ->
	ok.

banner_sizes() ->
	[
		{{1024, 768}, 1051},
		{{120, 20}, 1052},
		{{120, 600}, 1053},
		{{12, 12}, 1054},
		{{160, 600}, 1055},
		{{168, 28}, 1056},
		{{1800, 1000}, 1057},
		{{180, 150}, 1058},
		{{1, 1}, 1059},
		{{216, 36}, 1060},
		{{234, 60}, 1061},
		{{240, 400}, 1062},
		{{250, 250}, 1063},
		{{250, 360}, 1064},
		{{300, 1050}, 1065},
		{{300, 170}, 1066},
		{{300, 250}, 1067},
		{{300, 340}, 1068},
		{{300, 50}, 1069},
		{{300, 600}, 1070}
	].

users() ->
	[
		{1, {0, 0, <<"M">>}},
		{2, {1, 11, <<"M">>}},
		{3, {12, 18, <<"M">>}},
		{4, {19, 25, <<"M">>}},
		{5, {26, 39, <<"M">>}},
		{6, {40, 55, <<"M">>}},
		{7, {56, 64, <<"M">>}},
		{8, {65, 120, <<"M">>}},
		{9, {0, 0, <<"F">>}},
		{10, {1, 11, <<"F">>}},
		{11, {12, 18, <<"F">>}},
		{12, {19, 25, <<"F">>}},
		{13, {26, 39, <<"F">>}},
		{14, {40, 55, <<"F">>}},
		{15, {56, 64, <<"F">>}},
		{16, {65, 120, <<"F">>}},
		{17, {0, 0, <<"O">>}},
		{18, {1, 11, <<"O">>}},
		{19, {12, 18, <<"O">>}},
		{20, {19, 25, <<"O">>}},
		{21, {26, 39, <<"O">>}},
		{22, {40, 55, <<"O">>}},
		{23, {56, 64, <<"O">>}},
		{24, {65, 120, <<"O">>}}
	].