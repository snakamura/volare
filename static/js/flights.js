$(function() {
    _.mixin(_.string.exports());

    var flights = $.getJSON('/flights', function(flights) {
        _.each(flights, insertFlight);
    });

    function insertFlight(flight) {
        var tr = $('<tr><td class="time"></td><td class="name"><a></a></td></tr>');
        tr.find('.time').text(common.formatTime(new Date(flight.time)));
        tr.find('.name a').attr('href', '/flights/' + flight.id).text(flight.name);
        $('#flights').append(tr);
    }

    var addFlight = $('#add_flight');
    addFlight.on('change', function(event) {
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
});
