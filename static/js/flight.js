$(function() {
    var flights = new volare.Flights();
    var player = new volare.Player(flights, $('#player'));
    var map = new volare.Map(flights, $('#map'));
    var altitudeGraph = new volare.AltitudeGraph(flights, $('#altitude'));
    var speedGraph = new volare.SpeedGraph(flights, $('#speed'));
    var chart = new volare.Chart(flights, $('#chart'));
    var weatherControl = new volare.WeatherControl(map, $('#weather'));

    $.getJSON('', function(flight) {
        var f = new volare.Flight(flight, 'red');
        $('#time').text(common.formatTime(f.getTime()));
        $('#duration').text(common.formatDuration(f.getDuration()));
        $('#max_altitude').text(common.formatAltitude(f.getMaxAltitude()));
        flights.addFlight(f);
    });

    var showName = $('#show_name');
    var editName = $('#edit_name');
    var inputName = $('#edit_name input');
    function startEditingName() {
        showName.hide();
        editName.show();
        inputName.focus();
    }
    function finishEditingName() {
        var name = inputName.val();
        $('#name').text(name);
        editName.hide();
        showName.show();

        var data = {
            name: name
        };
        $.putJSON('', data, function(flight) {
        });
    }
    $('#show_name span.edit').on('click', function(event) {
        event.preventDefault();
        startEditingName();
    });
    $('#edit_name span.save').on('click', finishEditingName);
    inputName.on('keyup', function(event) {
        if (event.keyCode == 0x0d)
            finishEditingName();
    });

    $('#show_name span.delete').on('click', function(event) {
        if (confirm('Are you sure to delete this flight?')) {
            $.deleteJSON('', {}, function() {
                document.location.href = '/flights';
            });
        }
    });

    volare.setupLayout(flights, $('#map'), $('#sidebar'), $('#chart'));
});
