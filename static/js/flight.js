var LatLng = google.maps.LatLng;
var LatLngBounds = google.maps.LatLngBounds;

$(function() {
    _.mixin(_.str);

    var minLatitude = records[0].latitude;
    var maxLatitude = records[0].latitude;
    var minLongitude = records[0].longitude;
    var maxLongitude = records[0].longitude;
    _.each(records, function(record) {
        minLatitude = Math.min(minLatitude, record.latitude);
        maxLatitude = Math.max(maxLatitude, record.latitude);
        minLongitude = Math.min(minLongitude, record.longitude);
        maxLongitude = Math.max(maxLongitude, record.longitude);
    });

    var map = new google.maps.Map($('#map')[0], {
        mapTypeId: google.maps.MapTypeId.TERRAIN
    });
    map.fitBounds(new LatLngBounds(new LatLng(maxLatitude, minLongitude), new LatLng(minLatitude, maxLongitude)));

    var path = new google.maps.MVCArray(_.map(records, function(record) {
        return new LatLng(record.latitude, record.longitude);
    }));
    var polyline = new google.maps.Polyline({
        map: map,
        path: path,
        strokeColor: 'red'
    });

    var altitudeGraph = new volare.AltitudeGraph($('#altitude'));
    altitudeGraph.addFlight(new volare.Flight(records, 'red'));
});
