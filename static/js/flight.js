var LatLng = google.maps.LatLng;
var LatLngBounds = google.maps.LatLngBounds;

$(function() {
    _.mixin(_.str);

    var map = new volare.Map($('#map'));
    var altitudeGraph = new volare.AltitudeGraph($('#altitude'));

    var play = $('#play');
    var timer = null;
    play.on('click', function() {
        var time = altitudeGraph.start;
        timer = setInterval(function() {
            if (time > altitudeGraph.end) {
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
