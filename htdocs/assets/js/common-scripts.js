/*---LEFT BAR ACCORDION----*/
$(function() {
    $('#nav-accordion').dcAccordion({
        eventType: 'click',
        autoClose: true,
        saveState: true,
        disableLink: true,
        speed: 'slow',
        showCount: false,
        autoExpand: true,
//        cookie: 'dcjq-accordion-1',
        classExpand: 'dcjq-current-parent'
    });
});

var Script = function () {

//    sidebar dropdown menu auto scrolling

    jQuery('#sidebar .sub-menu > a').click(function () {
        var o = ($(this).offset());
        diff = 250 - o.top;
        if(diff>0)
            $("#sidebar").scrollTo("-="+Math.abs(diff),500);
        else
            $("#sidebar").scrollTo("+="+Math.abs(diff),500);
    });

//    sidebar toggle

    $(function() {
        function responsiveView() {
            var wSize = $(window).width();
            if (wSize <= 768) {
                $('#container').addClass('sidebar-close');
                $('#sidebar > ul').hide();
            }

            if (wSize > 768) {
                $('#container').removeClass('sidebar-close');
                $('#sidebar > ul').show();
            }
        }
        $(window).on('load', responsiveView);
        $(window).on('resize', responsiveView);
    });

    $('.icon-reorder').click(function () {
        if ($('#sidebar > ul').is(":visible") === true) {
            $('#main-content').css({
                'margin-left': '0px'
            });
            $('#sidebar').css({
                'margin-left': '-210px'
            });
            $('#sidebar > ul').hide();
            $("#container").addClass("sidebar-closed");
        } else {
            $('#main-content').css({
                'margin-left': '210px'
            });
            $('#sidebar > ul').show();
            $('#sidebar').css({
                'margin-left': '0'
            });
            $("#container").removeClass("sidebar-closed");
        }
    });

// custom scrollbar
    $("#sidebar").niceScroll({styler:"fb",cursorcolor:"#e8403f", cursorwidth: '3', cursorborderradius: '10px', background: '#404040', spacebarenabled:false, cursorborder: ''});

    $("html").niceScroll({styler:"fb",cursorcolor:"#e8403f", cursorwidth: '6', cursorborderradius: '10px', background: '#404040', spacebarenabled:false,  cursorborder: '', zindex: '1000'});

// widget tools

    jQuery('.panel .tools .icon-chevron-down').click(function () {
        var el = jQuery(this).parents(".panel").children(".panel-body");
        if (jQuery(this).hasClass("icon-chevron-down")) {
            jQuery(this).removeClass("icon-chevron-down").addClass("icon-chevron-up");
            el.slideUp(200);
        } else {
            jQuery(this).removeClass("icon-chevron-up").addClass("icon-chevron-down");
            el.slideDown(200);
        }
    });

    jQuery('.panel .tools .icon-remove').click(function () {
        jQuery(this).parents(".panel").parent().remove();
    });


//    tool tips

    $('.tooltips').tooltip();

//    popovers

    $('.popovers').popover();



// custom bar chart

    if ($(".custom-bar-chart")) {
        $(".bar").each(function () {
            var i = $(this).find(".value").html();
            $(this).find(".value").html("");
            $(this).find(".value").animate({
                height: i
            }, 2000)
        })
    }


}();

//tooltips
/*$('#identifier').tooltip({
  placement: 'right'
  ,title: 'Hg-U133A probe or Symbol'
  ,trigger: 'focus'
 });

 $('#subtype').tooltip({
  placement: 'right'
  ,title: 'Subtype classifying algorithm'
  ,trigger: 'focus'
 });
 
  $('#months').tooltip({
  placement: 'right'
  ,title: 'Number of months to plot for the desired end point'
  ,trigger: 'focus'
 });
 
  $('#split').tooltip({
  placement: 'right'
  ,title: 'Cut points for classifying data into groups. If tertile or quartile only the upper and lower groups are used.'
  ,trigger: 'focus'
 });
 
  $('#tamoxifen').tooltip({
  placement: 'right'
  ,title: 'Include tamoxifen treated patients or compare the effect of this treatment.'
  ,trigger: 'focus'
 });
 
 $('#chemo').tooltip({
  placement: 'right'
  ,title: 'Include chemo treated patients or compare the effect of this treatment.'
  ,trigger: 'focus'
 });
 
 $('#surv').tooltip({
  placement: 'right'
  ,title: 'End point to use for survival analysis'
  ,trigger: 'focus'
 });
 
 
  $('#mylabel').tooltip({
  placement: 'right'
  ,title: 'name of set'
  ,trigger: 'focus'
 });
 
   $('#metric').tooltip({
  placement: 'right'
  ,title: 'how to model'
  ,trigger: 'focus'
 });
 
   $('#pam50subtype').tooltip({
  placement: 'right'
  ,title: 'subtype to query within the PAM50 subtype classes'
  ,trigger: 'focus'
 });
 */

$(function() {
    $('.pophere').tooltip({
	placement: 'right'
  ,trigger: 'focus'	
});
});