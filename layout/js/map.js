$(document).ready(function(){

  //Clicking X closes popUp
  $('.escapeMe').on('click', function(){
    $('.popUp').hide();
  });
  //Reveals popUp
  $('#aboutMap').on('click', function(event){
    event.stopPropagation();
    $('#readFirst').show();
    $('#faqsPopUp').hide();
  });
  $('#faqs').on('click', function(event){
    event.stopPropagation();
    $('#faqsPopUp').show();
  });
  //clicking off the popUp hides it
  $(document).on('click', function(){
    $('.popUp').hide();
  });
  //clicking on the popUp does not hides it
  $('.popUp').on('click', function(e){
    e.stopPropagation();
    return false;
  });

//the map and where it centers and sets zoom level
var map = L.map('map').setView([44.514198, -89.740264], 8);

//the background layer
var Carto_Positron = L.tileLayer('https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png', {
  attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, &copy; <a href="https://cartodb.com/attributions">CartoDB</a>',
  subdomains: 'abcd',
  minZoom: 0,
  maxZoom: 20,
  ext: 'png'
});
Carto_Positron.addTo(map);

//the three different layers we want to have in the map
var url = 'https://cida.usgs.gov/nwc/geoserver/wms?service=wms&tiled=true';

//the better wms javascript for getfeatureinfo from wms
L.TileLayer.BetterWMS = L.TileLayer.WMS.extend({

  onAdd: function(map) {
    // Triggered when the layer is added to a map.
    //   Register a click listener, then do all the upstream WMS things
    L.TileLayer.WMS.prototype.onAdd.call(this, map);
    map.on('click', this.getFeatureInfo, this);
  },

  onRemove: function(map) {
    // Triggered when the layer is removed from a map.
    //   Unregister a click listener, then do all the upstream WMS things
    L.TileLayer.WMS.prototype.onRemove.call(this, map);
    map.off('click', this.getFeatureInfo, this);
  },

  getFeatureInfo: function(evt) {
    // Make an AJAX request to the server and hope for the best
    var url = this.getFeatureInfoUrl(evt.latlng),
      showResults = L.Util.bind(this.showGetFeatureInfo, this);
    $.ajax({
      url: url,
      success: function(data, status, xhr) {
        var err = typeof data === 'string' ? null : data;
        showResults(err, evt.latlng, data);
        sendAnalytics(evt.latlng, data);
      },
      error: function(xhr, status, error) {
        showResults(error);
      }
    });
  },

  getFeatureInfoUrl: function(latlng) {
    // Construct a GetFeatureInfo request URL given a point
    var point = this._map.latLngToContainerPoint(latlng, this._map.getZoom()),
      size = this._map.getSize(),

      params = {
        request: 'GetFeatureInfo',
        service: 'WMS',
        srs: 'EPSG:4326',
        styles: this.wmsParams.styles,
        transparent: this.wmsParams.transparent,
        version: this.wmsParams.version,
        format: this.wmsParams.format,
        bbox: this._map.getBounds().toBBoxString(),
        height: size.y,
        width: size.x,
        layers: this.wmsParams.layers,
        query_layers: this.wmsParams.layers,
        info_format: 'text/html'
      };

    params[params.version === '1.3.0' ? 'i' : 'x'] = Math.floor(point.x);
    params[params.version === '1.3.0' ? 'j' : 'y'] = Math.floor(point.y);

    return this._url + L.Util.getParamString(params, this._url, true);
  },

  showGetFeatureInfo: function(err, latlng, content) {
    if (err) {
      console.log(err);
      return;
    } // do nothing if there's an error

    // Otherwise show the content in a popup, or something.
    // Check content.length() to see if it's more than 658ish characters, because actual features have more than that, empty responses from Geoserver have about that many characters

    if (content.length > 660) {
      L.popup({
          maxWidth: 800
        })
        .setLatLng(latlng)
        .setContent(content)
        .openOn(this._map);
        //IE fix for popup headers
        $('.featureInfo th:nth-child(2)').html('Lake Name');
        $('.featureInfo th:nth-child(3)').html('Time Period');
        $('.featureInfo th:nth-child(4)').html('Lake Class');
        $('.featureInfo th:nth-child(5)').html('Bass Probability');
        $('.featureInfo th:nth-child(6)').html('Walleye Probability');
    }
  }
});

//betterWMSing the three layers
L.tileLayer.betterWms = function(url, options) {
  return new L.TileLayer.BetterWMS(url, options);
};

// when the user changes baselayers, close the popup
map.on('baselayerchange', function(a) {

        map.closePopup();
    });


//putting the layers on the map with betterwms
//early 1989-2014
var early = L.tileLayer.betterWms(url, {
  layers: 'vizlab:predicted_species_1989-2014',
  transparent: true,
  format: 'image/png'
});
early.addTo(map);

//mid 2040-2064
var mid = L.tileLayer.betterWms(url, {
  layers: 'vizlab:predicted_species_2040-2064',
  transparent: true,
  format: 'image/png'
});

//late 2065-2089
var late = L.tileLayer.betterWms(url, {
  layers: 'vizlab:predicted_species_2065-2089',
  transparent: true,
  format: 'image/png'
});

/*
 * L.Control.WMSLegend is used to add a WMS Legend to the map
 */

L.Control.WMSLegend = L.Control.extend({
  options: {
    position: 'bottomright',
    uri: ''
  },

  onAdd: function() {
    var controlClassName = 'leaflet-control-wms-legend',
      legendClassName = 'wms-legend',
      stop = L.DomEvent.stopPropagation;
    this.container = L.DomUtil.create('div', controlClassName);
    this.img = L.DomUtil.create('img', legendClassName, this.container);
    this.img.src = uri;
    this.img.alt = 'Legend';

    L.DomEvent
      .on(this.img, 'click', this._click, this)
      .on(this.container, 'click', this._click, this)
      .on(this.img, 'mousedown', stop)
      .on(this.img, 'dblclick', stop)
      .on(this.img, 'click', L.DomEvent.preventDefault)
      .on(this.img, 'click', stop);
    this.height = null;
    this.width = null;
    return this.container;
  },
  _click: function(e) {
    L.DomEvent.stopPropagation(e);
    L.DomEvent.preventDefault(e);
    // toggle legend visibility
    var style = window.getComputedStyle(this.img);
    if (style.display === 'none') {
      this.container.style.height = this.height + 'px';
      this.container.style.width = this.width + 'px';
      this.img.style.display = this.displayStyle;
    } else {
      if (this.width === null && this.height === null) {
        // Only do inside the above check to prevent the container
        // growing on successive uses
        this.height = this.container.offsetHeight;
        this.width = this.container.offsetWidth;
      }
      this.displayStyle = this.img.style.display;
      this.img.style.display = 'none';
      this.container.style.height = '20px';
      this.container.style.width = '20px';
    }
  },
});

L.wmsLegend = function(uri) {
  var wmsLegendControl = new L.Control.WMSLegend;
  wmsLegendControl.options.uri = uri;
  map.addControl(wmsLegendControl);
  return wmsLegendControl;
};

uri = "https://cida.usgs.gov/nwc/geoserver/wms?service=wms&request=GetLegendGraphic&format=image%2Fpng&width=20&height=20&layer=vizlab:predicted_species_1989-2014";
L.wmsLegend(uri);

// layer toggle
// Use basemaps instead of overlays because basemaps allows only one to be on at a time, and we always want one to be on. swap basemaps with overlays if you want checkboxes. collapsed false refers to the toggle not being hidden initially
var basemaps = {
  "1989-2014": early,
  "2040-2064": mid,
  "2065-2089": late
};
var layerControl2 = L.control.layers(basemaps, null, {
  collapsed: false
});
map.addControl(layerControl2);
});

var sendAnalytics = function(latlng, data) {
  // ignoring latlng, but leaving here if we want to log those clicks
  var lakeid = data.match(/predicted_species_\d{4}-\d{4}\.(\d*)/)[1];
  ga('send', 'event', 'map', 'click', 'lake', lakeid);
}
