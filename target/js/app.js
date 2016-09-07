document.firefoxSVGReload = function(id){
	$('#' + id).removeClass('svgFig');
	//To force rerender of svg viewbox for firefox
	setTimeout(function(){$('#' + id).addClass('svgFig');}, 500);
};

//Changes selected button colors
$('.figToggle').each(function(){
  $('.figToggle').on('click', function(){
    $('.figToggle').removeClass('showing');
    $(this).addClass('showing');
    //toggles the figs based on which button is active
    if($('#figArea').hasClass('showing')){
      $('#futureSuitabilityFig').fadeOut(1000, function() {
        $('#futureSuitabilityFigArea').fadeIn(1000);
      });
    }else{
      $('#futureSuitabilityFigArea').fadeOut(1000, function() {
        $('#futureSuitabilityFig').fadeIn(1000);
      });
    }
  });
});

//Toggles text when toggleInfo is clicked
  $('.toggleInfo').on('click', function(){
    $('#drawConclusionsText').toggle('slow');
  });

var vizlab = document.vizlab || {};
vizlab.scrollTimer = null;
$(window).scroll(function(){
  if (vizlab.scrollTimer) {
      clearTimeout(vizlab.scrollTimer);
  }
  vizlab,scrollTimer = setTimeout(vizlab.chapterScroll, 250);
});

vizlab.chapters = ["#intro", "#walleyeDecline", "#walleyeBass", "#lakeWarming", "#futureWarming", "#futureSuitability", "#fishManagement", "#footer"];
vizlab.chapterTriggers = {};

vizlab.inview = function (el) {
    var rect = el.getBoundingClientRect();

    return rect.bottom > 0 &&
        rect.right > 0 &&
        rect.left < (window.innerWidth || document.documentElement.clientWidth) &&
        rect.top < (window.innerHeight || document.documentElement.clientHeight);
};

vizlab.chapterScroll = function() {
  $.each(vizlab.chapters, function(index, value) {
    if (vizlab.inview($(value)[0])) {
      if (!vizlab.chapterTriggers[value]) {
        vizlab.chapterTriggers[value] = true; // trigger
        ga('send', 'event', 'chapter', 'scrolled to ' + value);
      }
    }
  })
};

vizlab.clicklink = function(url) {
  ga('send', 'event', 'outbound', 'click', url, {
     'transport': 'beacon',
     'hitCallback': function(){document.location = url;}
   });
};
