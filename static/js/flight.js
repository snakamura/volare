var LatLng = google.maps.LatLng;
var LatLngBounds = google.maps.LatLngBounds;

$(function() {
    _.mixin(_.str);

    var map = new volare.Map($('#map'));
    var altitudeGraph = new volare.AltitudeGraph($('#altitude'));

    function addFlight(flight) {
        map.addFlight(flight);
        altitudeGraph.addFlight(flight);
    }

    $.getJSON('', function(flight) {
        addFlight(new volare.Flight(flight, 'red'));
    });
    $.getJSON('/flights/2', function(flight) {
        addFlight(new volare.Flight(flight, 'blue'));
    });
});
