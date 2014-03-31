$(function() {
    var waypoints = new Waypoints();
    $(waypoints).on('waypoint_added', function(event, waypoint, index) {
        var $tr = $('<tr>' +
                    '<td class="name"><a></a></td>' +
                    '</tr>');
        $tr.find('.name a').attr('href', '/waypoints/' + waypoint.id).text(waypoint.name);

        var $rows = $('#waypoints tbody tr');
        if ($rows.length > index)
            $($rows[index]).before($tr);
        else
            $('#waypoints tbody').append($tr);
    });

    $.getJSON('/waypoints', function(ws) {
        _.each(ws, _.bind(waypoints.addWaypoint, waypoints));
    });

    function addFiles(files) {
        _.each(files, function(file) {
            var reader = new FileReader();
            $(reader).on('loadend', function(event) {
                $.postJSON('/waypoints', {
                    name: common.basename(file.name),
                    wpt: reader.result
                }, function(waypoint) {
                    if (files.length == 1)
                        document.location.href = '/waypoints/' + waypoint.id;
                    else
                        waypoints.addWaypoint(waypoint);
                });
            });
            reader.readAsText(file);
        });
    }

    $('#add_waypoint').on('change', function(event) {
        addFiles(event.target.files);
        return false;
    });

    var $dropTarget = $('#waypoints');
    $dropTarget.on('dragenter', function(event) {
        event.preventDefault();
        event.originalEvent.dataTransfer.dropEffect = 'copy';
    });
    $dropTarget.on('dragleave', function(event) {
        event.preventDefault();
    });
    $dropTarget.on('dragover', function(event) {
        event.preventDefault();
    });
    $dropTarget.on('drop', function(event) {
        event.preventDefault();
        addFiles(event.originalEvent.dataTransfer.files);
    });


    function Waypoints() {
        this._waypoints = [];
    }

    Waypoints.prototype.addWaypoint = function(waypoint) {
        var index = _.sortedIndex(this._waypoints, waypoint, function(waypoint) {
            return waypoint.name;
        });
        this._waypoints.splice(index, 0, waypoint);
        $(this).trigger('waypoint_added', [waypoint, index]);
    };
});
