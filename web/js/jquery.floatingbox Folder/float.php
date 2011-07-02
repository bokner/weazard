
<html>
	<head>
		<title>Floating Box Demo PHPletter.com</title>
		<style type="text/css">
			html,body{margin:0; padding:0}
			#content, #box, #rightBox{ border:1px solid red}
			#body{width:760px;}
			#box, #rightBox{width:260px; padding:16px;}
			#content{width:500px; padding:4px 16px;  margin-left:300px}
		</style>
		<script type="text/javascript" src="/javascript/jquery.js"></script>
		<script type="text/javascript" src="/javascript/jquery.floating.js"></script>
		<script type="text/javascript">

			$(document).ready(
				function()
				{
					$('#box').floating({targetX:'left', targetY:'bottom'});
					$('#rightBox').floating({targetX:'right', targetY:'top'});		
				}
			);
			
		</script>
	</head>
	<body>
		
		<div id="body">
			<div id="box">
				<h1>Control Panel</h1>
				<p>
				Welcome to <a href="http://www.phpletter.com/">Phpletter.com</a> Open Source Centre!</p>

<p>The <a href="http://www.phpletter.com/">Phpletter.com</a> Open Source Development Team is dedicated to developing web2.0 ready PHP Based Projects, powered by Jquery Javascript Library .</p>

<p>Our goal is to provide you with easy to use powerful tools that allow you to maintain the contents, images and files of your web site, and provide PHP developers with time saving web based applications.</p>
				</p>
			</div>
			<div id="content">
				<?php
					$str = '<p>
				Welcome to <a href="http://www.phpletter.com/">Phpletter.com</a> Open Source Centre!</p>

<p>The <a href="http://www.phpletter.com/">Phpletter.com</a> Open Source Development Team is dedicated to developing web2.0 ready PHP Based Projects, powered by Jquery Javascript Library .</p>

<p>Our goal is to provide you with easy to use powerful tools that allow you to maintain the contents, images and files of your web site, and provide PHP developers with time saving web based applications.</p>';
					for($i = 0; $i < 20; $i++)
					{
						echo '<p>' . substr($str, rand(0, 40), rand(strlen($str)/2, strlen($str))) . '</p>';
					}
				?>
			</div>
			<div id="rightBox">
				<h1>Control Panel</h1>
				<p>
				need any Web-based Information System?<br> Please <a href="http://www.phpletter.com">Contact Us</a><br>
				We are specialized in <br>
				<ul>
					<li>Website Design</li>
					<li>Survey System Creation</li>
					<li>E-commerce Site Development</li>
				</ul>
				<button onclick="javascript:$('#box').floatingPosition('left', 'top');">Top Left</button><br>
				<button onclick="javascript:$('#box').floatingPosition('right', 'top');">Top Right</button><br>
				<button onclick="javascript:$('#box').floatingPosition('left', 'bottom');">Bottom Left</button><br>
				<button onclick="javascript:$('#box').floatingPosition('right', 'bottom');">Bottom Right</button>
				</p>
			</di>
		</div>
	</body>
</html>