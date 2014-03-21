$(function() {
    var flights = new Flights();
    $(flights).on('flight_added', function(event, flight, index) {
        var tr = $('<tr>' +
                   '<td class="name"><a></a></td>' +
                   '<td class="time"></td>' +
                   '<td class="duration"></td>' +
                   '</tr>');
        tr.find('.name a').attr('href', '/flights/' + flight.id).text(flight.name);
        tr.find('.time').text(common.formatTime(new Date(flight.time)));
        tr.find('.duration').text(common.formatDuration(flight.duration));

        var rows = $('#flights tbody tr');
        if (rows.length > index)
            $(rows[index]).before(tr);
        else
            $('#flights tbody').append(tr);
    });

    $.getJSON('/flights', function(fs) {
        _.each(fs, _.bind(flights.addFlight, flights));
    });

    function addFiles(files) {
        _.each(files, function(file) {
            var reader = new FileReader();
            $(reader).on('loadend', function(event) {
                var data = {
                    name: common.basename(file.name),
                    igc: reader.result
                };
                $.postJSON('/flights', data, function(flight) {
                    if (files.length == 1)
                        document.location.href = '/flights/' + flight.id;
                    else
                        flights.addFlight(flight);
                });
            });
            reader.readAsText(file);
        });
    }

    $('#add_flight').on('change', function(event) {
        addFiles(event.target.files);
        return false;
    });

    var dropTarget = $('#flights');
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


    function Flights() {
        this._flights = [];
    }

    Flights.prototype.addFlight = function(flight) {
        flight.time = new Date(flight.time);

        var index = _.sortedIndex(this._flights, flight, function(flight) {
            return -flight.time.getTime();
        });
        this._flights.splice(index, 0, flight);
        $(this).trigger('flight_added', [flight, index]);
    };
});
