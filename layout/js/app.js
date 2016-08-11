document.firefoxSVGReload = function(id){
	$('#' + id).removeClass('svgFig');
	//To force rerender of svg viewbox for firefox
	setTimeout(function(){$('#' + id).addClass('svgFig');}, 500);
};
        