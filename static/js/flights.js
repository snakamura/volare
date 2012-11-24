$(function() {
    _.mixin(_.string.exports());

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
        $($('#flights tr')[index]).after(tr);
    });

    $.getJSON('/flights', function(fs) {
        _.each(fs, _.bind(flights.addFlight, flights));
    });

    $('#add_flight').on('change', function(event) {
        _.each(event.target.files, function(file) {
            var reader = new FileReader();
            $(reader).on('loadend', function(event) {
                var data = {
                    name: file.name,
                    igc: reader.result
                };
                $.postJSON('/flights', data, insertFlight);
            });
            reader.readAsText(file);
        });
        return false;
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
