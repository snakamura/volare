var LatLng = google.maps.LatLng;
var LatLngBounds = google.maps.LatLngBounds;

$(function() {
    _.mixin(_.str);

    var flights = new volare.Flights();
    var player = new volare.Player(flights);
    var map = new volare.Map(flights, $('#map'));
    var altitudeGraph = new volare.AltitudeGraph(flights, $('#altitude'));

    var play = $('#play');
    play.on('click', function() {
        player.play();
    });

    $.getJSON('', function(flight) {
        flights.addFlight(new volare.Flight(flight, 'red'));
    });
    $.getJSON('/flights/2', function(flight) {
        flights.addFlight(new volare.Flight(flight, 'blue'));
    });
});
