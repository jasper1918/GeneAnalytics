// JavaScript Document
var opts = {
      lines: 11, // The number of lines to draw
      length: 10, // The length of each line
      width: 6, // The line thickness
      radius: 14, // The radius of the inner circle
      corners: 1, // Corner roundness (0..1)
      rotate: 0, // The rotation offset
      color: "#000", // #rgb or #rrggbb
      speed: 1.8, // Rounds per second
      trail: 40, // Afterglow percentage
      shadow: false, // Whether to render a shadow
      hwaccel: false, // Whether to use hardware acceleration
      className: "spinner", // The CSS class to assign to the spinner
      zIndex: 2e9, // The z-index (defaults to 2000000000)
      top: 90 , // Top position relative to parent in px
      left: 'auto' // Left position relative to parent in px
    };
   var target = document.getElementById('spindiv');
   var spinner = new Spinner(opts).spin(target);
   
 
  function spin_stop()
  {
	spinner.stop();  
  }
  function spin_start()
  {
	spinner.spin(target);  
  }
  