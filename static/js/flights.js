$(function() {
    var flights = $.getJSON('/flights', function(flights) {
        _.each(flights, insertFlight);
    });

    function insertFlight(flight) {
        var li = $('<li><a></a></li>');
        li.find('a').attr('href', '/flights/' + flight.id).text(flight.name);
        $('#flights').append(li);
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
