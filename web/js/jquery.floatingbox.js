/**
*	@auth Logan Cai
*	@email cailongqun#yahoo.com.cn  (replace # with @)
*	@ this plugin is based on the nice javascript provided by http://www.jtricks.com/javascript/navigation/floating.html
*   @version 1.0
*	
*/
function floatingBox(box, options, index)
{
    this.targetX = -250;
    this.targetY =  10;
    this.interval = 50;
    this.menu = box;
    this.index = index;
    this.options = options;
    this.hasInner = typeof(window.innerWidth) == 'number';
    this.hasElement =  document.documentElement
        && document.documentElement.clientWidth;    
	this.move = function ()
	{//move function start
		jQuery(this.menu).css('left', this.nextX + 'px').css('top', this.nextY + 'px');
	};  
	this.computeShifts = function ()
	{
	    var de = document.documentElement;	
	    this.shiftX =
	        this.hasInner
	        ? pageXOffset
	        : this.hasElement
	          ? de.scrollLeft
	          : document.body.scrollLeft;
	    if (this.targetX < 0)
	    {
	        if (this.hasElement && this.hasInner)
	        {
	            // Handle Opera 8 problems
	            this.shiftX +=
	                de.clientWidth > window.innerWidth
	                ? window.innerWidth
	                : de.clientWidth
	        }
	        else
	        {
	            this.shiftX +=
	                this.hasElement
	                ? de.clientWidth
	                : this.hasInner
	                  ? window.innerWidth
	                  : document.body.clientWidth;
	        }
	    }
	
	    this.shiftY = 
	        this.hasInner
	        ? pageYOffset
	        : this.hasElement
	          ? de.scrollTop
	          : document.body.scrollTop;
	    if (this.targetY < 0)
	    {
	        if (this.hasElement && this.hasInner)
	        {
	            // Handle Opera 8 problems
	            this.shiftY +=
	                de.clientHeight > window.innerHeight
	                ? window.innerHeight
	                : de.clientHeight
	        }
	        else
	        {
	            this.shiftY +=
	                this.hasElement
	                ? document.documentElement.clientHeight
	                : this.hasInner
	                  ? window.innerHeight
	                  : document.body.clientHeight;
	        }
	    }
	};
	this.doFloat = function()
	{
	    var stepX, stepY;
	
	    this.computeShifts();
	
	    stepX = (this.shiftX + 
	        this.targetX - this.nextX) * .07;
	    if (Math.abs(stepX) < .5)
	    {
	        stepX = this.shiftX +
	            this.targetX - this.nextX;
	    }
	
	    stepY = (this.shiftY + 
	        this.targetY - this.nextY) * .07;
	    if (Math.abs(stepY) < .5)
	    {
	        stepY = this.shiftY + 
	            this.targetY - this.nextY;
	    }
	
	    if (Math.abs(stepX) > 0 ||
	        Math.abs(stepY) > 0)
	    {
	        this.nextX += stepX;
	        this.nextY += stepY;
	        this.move();
	    }
		
	    setTimeout('funcFloating[' + this.index + '].func()', this.interval);
	};	
	this.initSecondary = function()
	{
	    this.computeShifts();
	    this.nextX = this.shiftX +
	        this.targetX;
	    this.nextY = this.shiftY +
	        this.targetY;
	    this.move();
	};	
	this.position = function(targetX, targetY)
	{
		if(typeof(targetX) == 'number')
		{
			this.targetX = targetX;
		}else if(typeof(targetX) == 'string')
		{
			switch(targetX)
			{
				case 'left':
					this.targetX = 5;
					break;
				case 'right':
					this.targetX = -((jQuery(this.menu).outerWidth()) + 5);
					break;									
			}
			
		}	
		if(typeof(targetY) == 'number')
		{
			this.targetY = targetY;
		}else if(typeof(targetY) == 'string')
		{
			switch(targetY)
			{
				case 'top':
					this.targetY= 5;
					break;
				case 'bottom':

					this.targetY = -((jQuery(this.menu).outerHeight() + 5));
					
					break;								
			}
			
		}				
		
	};
	this.init = function()
	{
		jQuery(this.menu).css('position', 'absolute');
		if(typeof(this.options)  == 'object')
		{
			for(var i in this.options)
			{
				switch(i)
				{
					
					case 'targetX':
					case 'targetY':
						if(typeof(this.options[i]) == 'number')
						{
							this[i] = this.options[i];
						}else if(typeof(this.options[i]) == 'string')
						{
							switch(this.options[i])
							{
								case 'top':
									this.targetY= 5;
									break;
								case 'bottom':
									this.targetY = -((jQuery(this.menu).outerHeight() + 5));									
									break;
								case 'left':
									this.targetX = 5;
									break;
								case 'right':
									this.targetX = -((jQuery(this.menu).outerWidth()) + 5);
									break;									
							}
							
						}
						break;	
					default:
						this[i] = this.options[i];				
				}
			}
		}		
		
	    this.initSecondary();
	    this.doFloat();
	}
		      
};

// Some browsers init scrollbars only after
// full document load.
var funcFloating = {};
jQuery.fn.floating = function(options)
{
	return jQuery(this).each(
	
		function(i)
		{		
			var nextIndex = 0;
			for(var index in funcFloating)
			{
				nextIndex = parseInt(index);
			}	
			funcFloating[nextIndex + 1] = {};	
			funcFloating[nextIndex + 1].box = this;	
			funcFloating[nextIndex + 1].obj = new floatingBox(this, options, (nextIndex + 1));
			funcFloating[nextIndex + 1].func = function(){ funcFloating[nextIndex + 1].obj.doFloat(); };
			if (document.layers)
			{
				funcFloating[nextIndex + 1].obj.init();
			}else
			{
				
				funcFloating[nextIndex + 1].obj.init();
				funcFloating[nextIndex + 1].obj.initSecondary();
			}
			
		}
	);
};
jQuery.fn.floatingPosition = function(targetX, targetY)
{
	return jQuery(this).each(
	
		function(i)
		{	
			for(var j in funcFloating)
			{
				if(funcFloating[j].box == this)
				{
					funcFloating[j].obj.position(targetX, targetY);
				}
			}
			
		}
	);	
};