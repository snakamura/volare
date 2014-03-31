$(function() {
    var flights = new volare.Flights();
    var player = new volare.Player(flights, $('#player'));
    var map = new volare.Map(flights, $('#map'));
    map.setUseGradientColorTrack(true);
    var altitudeGraph = new volare.AltitudeGraph(flights, $('#altitude'));
    var speedGraph = new volare.SpeedGraph(flights, $('#speed'));
    var chart = new volare.Chart(flights, $('#chart'));
    var optionsControl = new volare.OptionsControl(map, $('#options'));
    var waypointControl = new volare.WaypointControl(map, $('#waypoint'));
    var weatherControl = new volare.WeatherControl(map, $('#weather'));

    $.getJSON('', function(flight) {
        var f = new volare.Flight(flight, 'red');
        $('#time').text(common.formatTime(f.getTime()));
        $('#duration').text(common.formatDuration(f.getDuration()));
        $('#max_altitude').text(common.formatAltitude(f.getMaxAltitude()));
        flights.addFlight(f);
    });

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
