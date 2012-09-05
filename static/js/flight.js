$(function() {
    var options = {
        center: new google.maps.LatLng(records[0].latitude, records[0].longitude),
        zoom: 14,
        mapTypeId: google.maps.MapTypeId.TERRAIN
    };
    var map = new google.maps.Map($('#map')[0], options);

    var path = new google.maps.MVCArray($.map(records, function(record) {
        return new google.maps.LatLng(Math.round(record.latitude*10000)/10000, Math.round(record.longitude*10000)/10000);
    }));
    var polyline = new google.maps.Polyline({
        map: map,
        path: path,
        strokeColor: 'red'
    });
})
