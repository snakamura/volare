var LatLng = google.maps.LatLng;
var LatLngBounds = google.maps.LatLngBounds;

$(function() {
    var minLatitude = records[0].latitude;
    var maxLatitude = records[0].latitude;
    var minLongitude = records[0].longitude;
    var maxLongitude = records[0].longitude;
    var maxAltitude = records[0].altitude;
    $.each(records, function(index, record) {
        minLatitude = Math.min(minLatitude, record.latitude);
        maxLatitude = Math.max(maxLatitude, record.latitude);
        minLongitude = Math.min(minLongitude, record.longitude);
        maxLongitude = Math.max(maxLongitude, record.longitude);
        maxAltitude = Math.max(maxAltitude, record.altitude);
    });
    maxAltitude *= 1.1;

    var map = new google.maps.Map($('#map')[0], {
        mapTypeId: google.maps.MapTypeId.TERRAIN
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

    var start = new Date(records[0].time);
    var duration = new Date(records[records.length - 1].time) - start;

    var canvas = $('#altitude');
    var width = canvas.width();
    var height = canvas.height();

    var canvasElem = canvas[0];
    canvasElem.width = width;
    canvasElem.height = height;

    var context = canvasElem.getContext('2d');
    context.strokeStyle = 'red';
    context.lineWidth = 2;
    context.beginPath();
    context.moveTo(0, height - records[0].altitude/maxAltitude*height);
    $.each(records, function(index, record) {
        context.lineTo((new Date(record.time) - start)/duration*width, height - record.altitude/maxAltitude*height);
    });
    context.stroke();
})
