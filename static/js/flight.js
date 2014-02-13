$(function() {
    var flights = new volare.Flights();
    var player = new volare.Player(flights, $('#player'));
    var map = new volare.Map(flights, $('#map'));
    var altitudeGraph = new volare.AltitudeGraph(flights, $('#altitude'));
    var speedGraph = new volare.SpeedGraph(flights, $('#speed'));
    var chart = new volare.Chart(flights, $('#chart'));

    function makeItem(selector, flags) {
        return {
            selector: selector,
            flags: flags
        };
    }
    var items = [
        makeItem('#msm', volare.Map.MSM),
        makeItem('#msm_wind', volare.Map.MSM_WIND),
        makeItem('#msm_temperature', volare.Map.MSM_TEMPERATURE),
        makeItem('#msm_cloud_amount', volare.Map.MSM_CLOUD_AMOUNT),
        makeItem('#amedas', volare.Map.AMEDAS),
        makeItem('#amedas_wind', volare.Map.AMEDAS_WIND),
        makeItem('#amedas_temperature', volare.Map.AMEDAS_TEMPERATURE),
        makeItem('#amedas_sunshine', volare.Map.AMEDAS_SUNSHINE)
    ];
    _.each(items, function(item) {
        $(item.selector).on('click', function(event) {
            map.setWeatherFlags($(event.target).prop('checked') ? item.flags : 0, item.flags);
        });
    });
    $(map).on('weatherFlags_changed', function(event, flags) {
        $('#msm').prop('checked', flags & volare.Map.MSM);
        $('#msm_wind').prop('checked', flags & volare.Map.MSM_WIND);
        $('#msm_temperature').prop('checked', flags & volare.Map.MSM_TEMPERATURE);
        $('#msm_cloud_amount').prop('checked', flags & volare.Map.MSM_CLOUD_AMOUNT);
        $('#amedas').prop('checked', flags & volare.Map.AMEDAS);
        $('#amedas_wind').prop('checked', flags & volare.Map.AMEDAS_WIND);
        $('#amedas_temperature').prop('checked', flags & volare.Map.AMEDAS_TEMPERATURE);
        $('#amedas_sunshine').prop('checked', flags & volare.Map.AMEDAS_SUNSHINE);
    });

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
    $('#show_name a').on('click', function(event) {
        event.preventDefault();
        startEditingName();
    });
    $('#edit_name button').on('click', finishEditingName);
    inputName.on('keyup', function(event) {
        if (event.keyCode == 0x0d)
            finishEditingName();
    });

    $('#show_name button').on('click', function(event) {
        if (confirm('Are you sure to delete this flight?')) {
            $.deleteJSON('', {}, function() {
                document.location.href = '/flights';
            });
        }
    });

    volare.setupLayout(flights, $('#map'), $('#sidebar'), $('#chart'));
});
