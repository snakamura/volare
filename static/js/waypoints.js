$(function() {
    function addFiles(files) {
        _.each(files, function(file) {
            var reader = new FileReader();
            $(reader).on('loadend', function(event) {
                var data = {
                    name: common.basename(file.name),
                    wpt: reader.result
                };
                $.postJSON('/waypoints', data, function(waypoint) {
                    if (files.length == 1)
                        document.location.href = '/waypoints/' + waypoint.id;
//                    else
//                        flights.addFlight(flight);
                });
            });
            reader.readAsText(file);
        });
    }

    $('#add_waypoint').on('change', function(event) {
        addFiles(event.target.files);
        return false;
    });

    var dropTarget = $('#waypoints');
    dropTarget.on('dragenter', function(event) {
        event.preventDefault();
        event.originalEvent.dataTransfer.dropEffect = 'copy';
    });
    dropTarget.on('dragleave', function(event) {
        event.preventDefault();
    });
    dropTarget.on('dragover', function(event) {
        event.preventDefault();
    });
    dropTarget.on('drop', function(event) {
        event.preventDefault();
        addFiles(event.originalEvent.dataTransfer.files);
    });
});
