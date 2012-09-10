var LatLng = google.maps.LatLng;
var LatLngBounds = google.maps.LatLngBounds;

$(function() {
    _.mixin(_.string.exports());

    var flights = new volare.Flights();
    var player = new volare.Player(flights, $('#player'));
    var map = new volare.Map(flights, $('#map'));
    var altitudeGraph = new volare.AltitudeGraph(flights, $('#altitude'));
    var chart = new volare.Chart(flights, $('#chart'));

    $.getJSON('', function(flight) {
        flights.addFlight(new volare.Flight(flight, 'red'));
    });
    $.getJSON('/flights/2', function(flight) {
        flights.addFlight(new volare.Flight(flight, 'blue'));
    });
});
