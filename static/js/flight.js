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
    context.strokeStyle = 'gray';
    context.lineWidth = 0.5;
    context.beginPath();
    var time = new Date(start);
    time.setMinutes(Math.floor(time.getMinutes()/10)*10);
    time.setSeconds(0);
    time = time.getTime() + 10*60*1000;
    var timeStep = 10*60*1000;
    for (; time < start.getTime() + duration; time += timeStep) {
        context.moveTo((time - start)/duration*width, 0);
        context.lineTo((time - start)/duration*width, height);
    }
    var altitudeStep = 200;
    for (var n = 0; n < maxAltitude; n += altitudeStep) {
        context.moveTo(0, height - n/maxAltitude*height);
        context.lineTo(width, height - n/maxAltitude*height);
    }
    context.stroke();
    context.strokeStyle = 'red';
    context.lineWidth = 2;
    context.beginPath();
    context.moveTo(0, height - records[0].altitude/maxAltitude*height);
    $.each(records, function(index, record) {
        context.lineTo((new Date(record.time) - start)/duration*width, height - record.altitude/maxAltitude*height);
    });
    context.stroke();
})
