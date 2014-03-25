$(function() {
    var map = $('#map');
    var m = new google.maps.Map(map[0], {
        mapTypeId: google.maps.MapTypeId.HYBRID
    });

    function layout() {
        map.width($(document).width());
        var mapPosition = map.position();
        map.height($(document).height() - mapPosition.top);
    }
    $(window).on('resize', layout);
    layout();

    $.getJSON('', function(waypoint) {
        var bounds = null;
        _.each(waypoint.items, function(item) {
            var position = new google.maps.LatLng(item.latitude, item.longitude);
            var label = item.name;
            if (item.name !== item.description) {
                label += ' (' + item.description + ')';
            }
            var marker = new MarkerWithLabel({
                map: m,
                position: position,
                title: item.name,
                labelContent: label,
                labelAnchor: new google.maps.Point(-15, 35),
                labelClass: 'label'
            });

            if (bounds == null)
                bounds = new google.maps.LatLngBounds(position, position);
            else
                bounds.extend(position);
        });

        m.fitBounds(bounds);
    });

    var showName = $('#show_name');
    var editName = $('#edit_name');
    var inputName = $('#edit_name input');
    function startEditingName() {
        showName.hide();
        editName.show();
        inputName.focus();
    }
    function finishEditingName() {
        var name = inputName.val();
        $('#name').text(name);
        editName.hide();
        showName.show();

        var data = {
            name: name
        };
        $.putJSON('', data, function(flight) {
        });
    }
    $('#show_name span.edit').on('click', function(event) {
        event.preventDefault();
        startEditingName();
    });
    $('#edit_name span.save').on('click', finishEditingName);
    inputName.on('keyup', function(event) {
        if (event.keyCode == 0x0d)
            finishEditingName();
    });

    $('#show_name span.delete').on('click', function(event) {
        if (confirm('Are you sure to delete these waypoints?')) {
            $.deleteJSON('', function() {
                document.location.href = '/waypoints';
            });
        }
    });
});
