$(function() {
    var flights = new volare.Flights();
    var player = new volare.Player(flights, $('#player'));
    var map = new volare.Map(flights, $('#map'));
    map.setTrackType(volare.Map.TrackType.ALTITUDE);
    var altitudeGraph = new volare.AltitudeGraph(flights, $('#altitude'));
    var speedGraph = new volare.SpeedGraph(flights, $('#speed'));
    var chart = new volare.Chart(flights, $('#chart'));
    var optionsControl = new volare.OptionsControl(flights, map, $('#options'));
    var waypointControl = new volare.WaypointControl(map, $('#waypoint'));
    var weatherControl = new volare.WeatherControl(map, $('#weather'));

    $(flights).on('flight_added', function(event, flight) {
        $('#time').text(common.formatTime(flight.getTime()));
        $('#duration').text(common.formatDuration(flight.getDuration()));
        $('#max_altitude').text(common.formatAltitude(flight.getMaxAltitude()));
    });
    flights.addFlight(flightId, 'red');

    common.makeNameEditable(function(name) {
        $.putJSON('', {
            name: name
        }, function(flight) {
        });
    });

    $('#show_name span.delete').on('click', function(event) {
        if (confirm('Are you sure to delete this flight?')) {
            $.deleteJSON('', function() {
                document.location.href = '/flights';
            });
        }
    });

    $('#new_workspace').on('click', function(event) {
        $.postJSON('/workspaces', {
            name: $('#name').text()
        }, function(workspace) {
            $.postJSON('/workspaces/' + workspace.id + '/flights', {
                flightIds: [flights.getPrimaryFlight().getId()]
            }, function(flights) {
                document.location.href = '/workspaces/' + workspace.id;
            });
        });
    });

    volare.setupLayout(flights, $('#map'), $('#sidebar'), $('#chart'));
});
