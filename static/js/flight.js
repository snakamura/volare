var LatLng = google.maps.LatLng;
var LatLngBounds = google.maps.LatLngBounds;

$(function() {
    _.mixin(_.str);

    var minLatitude = records[0].latitude;
    var maxLatitude = records[0].latitude;
    var minLongitude = records[0].longitude;
    var maxLongitude = records[0].longitude;
    var maxAltitude = records[0].altitude;
    _.each(records, function(record) {
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

    var path = new google.maps.MVCArray(_.map(records, function(record) {
        return new LatLng(record.latitude, record.longitude);
    }));
    var polyline = new google.maps.Polyline({
        map: map,
        path: path,
        strokeColor: 'red'
    });

    var start = new Date(records[0].time);
    var end = new Date(records[records.length - 1].time);
    var duration = end - start;

    function formatTime(time) {
        var date = new Date(time);
        return _.sprintf("%02d:%02d", date.getHours(), date.getMinutes());
    }

    function formatAltitude(altitude) {
        return altitude + 'm';
    }

    var canvas = $('#altitude');
    var width = canvas.width();
    var height = canvas.height();
    var margin = {
        top: 0,
        left: 50,
        bottom: 15,
        right: 0
    };
    function getX(time) {
        return (time - start)/duration*(width - (margin.left + margin.right)) + margin.left;
    }
    function getY(altitude) {
        return height - altitude/maxAltitude*(height - (margin.top + margin.bottom)) - margin.bottom;
    }

    var canvasElem = canvas[0];
    canvasElem.width = width;
    canvasElem.height = height;

    var context = canvasElem.getContext('2d');
    context.strokeStyle = 'gray';
    context.lineWidth = 0.5;

    var top = margin.top;
    var bottom = height - margin.bottom;
    var left = margin.left;
    var right = width - margin.right;

    context.beginPath();
    var time = new Date(start);
    time.setMinutes(Math.floor(time.getMinutes()/10)*10);
    time.setSeconds(0);
    time = time.getTime() + 10*60*1000;
    var timeStep = 10*60*1000;
    context.moveTo(left, top);
    context.lineTo(left, bottom);
    context.textAlign = 'center';
    var lowestY = getY(0);
    var highestY = getY(maxAltitude);
    for (; time < start.getTime() + duration; time += timeStep) {
        var x = getX(time);
        context.moveTo(x, lowestY);
        context.lineTo(x, highestY);
        context.fillText(formatTime(time), x, height - margin.bottom + 12);
    }

    var altitudeStep = 200;
    context.textAlign = 'end';
    var startX = getX(start);
    var endX = getX(end);
    for (var altitude = 0; altitude < maxAltitude; altitude += altitudeStep) {
        var y = getY(altitude);
        context.moveTo(startX, y);
        context.lineTo(endX, y);
        context.fillText(formatAltitude(altitude), margin.left - 4, y + 5);
    }
    context.stroke();

    context.strokeStyle = 'red';
    context.lineWidth = 2;
    context.beginPath();
    context.moveTo(getX(start), getY(records[0].altitude));
    _.each(records, function(record) {
        context.lineTo(getX(new Date(record.time)), getY(record.altitude));
    });
    context.stroke();
})
