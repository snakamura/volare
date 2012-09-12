$(function() {
    _.mixin(_.string.exports());

    var flights = new volare.Flights();
    var player = new volare.Player(flights, $('#player'));
    var map = new volare.Map(flights, $('#map'));
    var altitudeGraph = new volare.AltitudeGraph(flights, $('#altitude'));
    var speedGraph = new volare.SpeedGraph(flights, $('#speed'));
    var chart = new volare.Chart(flights, $('#chart'));

    var colors = [
        'red',
        'blue',
        'green',
        'yellow',
        'aqua',
        'fuchsia',
        'lime',
        'maroon',
        'navy',
        'olive',
        'purple',
        'silver',
        'teal'
    ];

    $.getJSON('/flights', function(fs) {
        var f = $('#flights');
        _.each(fs, function(flight) {
            var e = $('<div><input type="checkbox"><span></span></div>');
            e.find('span').text(flight.name);
            e.find('input').on('change', function(event) {
                if (event.target.checked) {
                    $.getJSON('/flights/' + flight.id, function(flight) {
                        flights.addFlight(new volare.Flight(flight, colors[flights.getCount() % colors.length]));
                    });
                }
                else {
                }
            });
            f.append(e);

        });
    });
});
