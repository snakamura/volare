var LatLng = google.maps.LatLng;
var LatLngBounds = google.maps.LatLngBounds;

$(function() {
    var options = {
        center: new LatLng(records[0].latitude, records[0].longitude),
        zoom: 14,
        mapTypeId: google.maps.MapTypeId.TERRAIN
    };
    var map = new google.maps.Map($('#map')[0], options);

    var minLatitude = records[0].latitude;
    var maxLatitude = records[0].latitude;
    var minLongitude = records[0].longitude;
    var maxLongitude = records[0].longitude;
    $.each(records, function(index, record) {
        minLatitude = Math.min(minLatitude, record.latitude);
        maxLatitude = Math.max(maxLatitude, record.latitude);
        minLongitude = Math.min(minLongitude, record.longitude);
        maxLongitude = Math.max(maxLongitude, record.longitude);
    });
    map.fitBounds(new LatLngBounds(new LatLng(maxLatitude, minLongitude), new LatLng(minLatitude, maxLongitude)));

    var path = new google.maps.MVCArray($.map(records, function(record) {
        return new LatLng(record.latitude, record.longitude);
    }));
    var polyline = new google.maps.Polyline({
        map: map,
        path: path,
        strokeColor: 'red'
    });
})
