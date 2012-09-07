var LatLng = google.maps.LatLng;
var LatLngBounds = google.maps.LatLngBounds;

$(function() {
    _.mixin(_.str);

    var flight = new volare.Flight(records, 'red');

    var map = new volare.Map($('#map'));
    map.addFlight(flight);

    var altitudeGraph = new volare.AltitudeGraph($('#altitude'));
    altitudeGraph.addFlight(flight);
});
