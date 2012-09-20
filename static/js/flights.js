$(function() {
    var addFlight = $('#add_flight');
    addFlight.on('change', function(event) {
        _.each(event.target.files, function(file) {
            var reader = new FileReader();
            $(reader).on('loadend', function(event) {
                var req = {
                    name: file.name,
                    igc: reader.result
                };
                $.postJSON('/flights', req, function(flight) {
                    var li = $('<li><a></a></li>');
                    li.find('a').attr('href', '/flights/' + flight.id).text(flight.name);
                    $('#flights').append(li);
                });
            });
            reader.readAsText(file);
        });
        return false;
    });
});
