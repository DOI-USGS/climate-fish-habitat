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

