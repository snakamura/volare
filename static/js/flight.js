var LatLng = google.maps.LatLng;
var LatLngBounds = google.maps.LatLngBounds;

$(function() {
    _.mixin(_.str);

    var flights = new volare.Flights();
    var map = new volare.Map(flights, $('#map'));
    var altitudeGraph = new volare.AltitudeGraph(flights, $('#altitude'));

    var play = $('#play');
    var timer = null;
    play.on('click', function() {
        var time = flights.start;
        timer = setInterval(function() {
            if (time > flights.end) {
                clearInterval(timer);
                timer = null;
            }
            else {
                map.setCurrentTime(time);
                altitudeGraph.setCurrentTime(time);
                time = new Date(time.getTime() + 10*1000);
            }
        }, 100);
    });

    $.getJSON('', function(flight) {
        flights.addFlight(new volare.Flight(flight, 'red'));
    });
    $.getJSON('/flights/2', function(flight) {
        flights.addFlight(new volare.Flight(flight, 'blue'));
    });
});
