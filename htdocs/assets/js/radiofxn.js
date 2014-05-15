// JavaScript Document

$(function() {
	$("#identifier").autocomplete({
		source: "myauto.php",
		minLength: 2,
		autoFocus: true
	});
       
	$('input[name="Idtype"]').click(function(){
		if(this.checked){
			if(this.value == "probe"){
				document.getElementById('identifier').value = '';
				$( "#identifier" ).autocomplete('option', 'source', "myauto.php")
			} else {
				document.getElementById('identifier').value = '';
				$( "#identifier" ).autocomplete('option', 'source', "myautosym.php")
			}
		}
	})
});

 $(function () {
     $.widget("ui.tooltip", $.ui.tooltip, {
         options: {
             content: function () {
                 return $(this).prop('title');
             }
         }
     });

     $('[rel=tooltip]').tooltip({
         position: {
             my: "center bottom-20",
             at: "center top",
             using: function (position, feedback) {
                 $(this).css(position);
                 $("<div>")
                     .addClass("arrow")
                     .addClass(feedback.vertical)
                     .addClass(feedback.horizontal)
                     .appendTo(this);
             }
         }
     });
 });


 /*
  $(function () {
      $(document).tooltip({
          position: {
              my: "center bottom-20",
              at: "center top",
              using: function (position, feedback) {
                  $(this).css(position);
                  $("<div>")
                      .addClass("arrow")
                      .addClass(feedback.vertical)
                      .addClass(feedback.horizontal)
                      .appendTo(this);
              }
          }
      });
  });*/
$( "#clickme" ).click(function() {
  $( "#book" ).hide( "slow", function() {
    alert( "Animation complete." );
  });
});

function xmlhttpPost(strURL) {
    var xmlHttpReq = false;
    var self = this;
    // Mozilla/Safari
    if (window.XMLHttpRequest) {
        self.xmlHttpReq = new XMLHttpRequest();
    }
    // IE
    else if (window.ActiveXObject) {
        self.xmlHttpReq = new ActiveXObject("Microsoft.XMLHTTP");
    }
    self.xmlHttpReq.open('POST', strURL, true);
    self.xmlHttpReq.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
    self.xmlHttpReq.onreadystatechange = function() {
        if (self.xmlHttpReq.readyState == 4) {
            updatepage(self.xmlHttpReq.responseText);
        }
    }
    
}
