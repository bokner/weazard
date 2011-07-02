jQuery.fn.spinner = function(config) {
	var frame  = 1;
	var target = $(this);
	var frames = 1;
	
	// Set the height
	if(config.height) {
		target.height(config.height);
	}
	
	// Set the width	
	if(config.width) {
		target.width(config.width);
	}
	
	// Set or get the spinner image
	if(config.image) {
		target.css("background-image","url("+config.image+")");
		target.css("background-position","0px 0px");
		target.css("background-repeat","no-repeat");
	} else {
		config.image = target.css("background-image");
	}
	
	// Determine how many frames exist
	img = new Image();
	img.src = config.image;
	img.onload = function() {
		frames = img.width/config.width;
	};
	
	// Set the frame speed
	if(!config.speed) {
		config.speed = 25;
	}
	
	// Update the drawing area by adjusting the background-image
	function spinnerRedraw() {
		// If we've reached the last frame, loop back around
		if(frame >= frames) {
			frame = 1;
		}
		
		// Set the background-position for this frame
		pos = "-"+(frame*config.width)+"px 0px";
		target.css("background-position",pos);
		
		// Increment the frame count
		frame++;
	}
	
	// Kick off the animation
	var animation = setInterval(spinnerRedraw,config.speed);
	
	// Call $.spinnerStop to halt the spinner
	$.spinnerStop = function() {
	    clearInterval(animation);
	};
}

