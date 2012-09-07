var LatLng = google.maps.LatLng;
var LatLngBounds = google.maps.LatLngBounds;

$(function() {
    _.mixin(_.str);

    var flight = new volare.Flight(records, 'red');

    var map = new google.maps.Map($('#map')[0], {
        mapTypeId: google.maps.MapTypeId.TERRAIN
    });
    map.fitBounds(flight.getBounds());

    var path = new google.maps.MVCArray(_.map(records, function(record) {
        return new LatLng(record.latitude, record.longitude);
    }));
    var polyline = new google.maps.Polyline({
        map: map,
        path: path,
        strokeColor: 'red'
    });

    var altitudeGraph = new volare.AltitudeGraph($('#altitude'));
    altitudeGraph.addFlight(flight);
});
