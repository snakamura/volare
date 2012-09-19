$(function() {
    _.mixin(_.string.exports());

    var flights = new volare.Flights();
    var player = new volare.Player(flights, $('#player'));
    var map = new volare.Map(flights, $('#map'));
    var altitudeGraph = new volare.AltitudeGraph(flights, $('#altitude'));
    var speedGraph = new volare.SpeedGraph(flights, $('#speed'));
    var chart = new volare.Chart(flights, $('#chart'));

    $('#add_flight').button().on('click', function() {
        var dialog = $('<div><div class="loading">Loading...</div><form><input type="hidden" name="_token"></form></div>');
        var form = dialog.find('form');
        form.find('input').prop('value', token);
        dialog.dialog({
            title: 'Flights',
            modal: true,
            buttons: [
                {
                    text: 'OK',
                    click: function() {
                        $.post('/workspaces/' + workspaceId + '/flights', form.serialize(), function(flightIds) {
                            _.each(flightIds, addFlight);
                            dialog.dialog('close');
                        }, 'json');
                    }
                },
                {
                    text: 'Cancel',
                    click: function() {
                        dialog.dialog('close');
                    }
                }
            ],
            close: function() {
                dialog.dialog('destroy');
                dialog.remove();
            }
        });
        $.getJSON('/workspaces/' + workspaceId + '/candidates', function(flights) {
            _.each(flights, function(flight) {
                var e = $('<div><label><input type="checkbox" name="flights"><span></span></label></div>');
                e.find('input').prop('value', flight.id);
                e.find('span').text(flight.name);
                form.append(e);
            });
            dialog.find('div.loading').remove();
        });
    });

    function addFlight(flightId) {
        $.getJSON('/flights/' + flightId, function(flight) {
            flights.addFlight(new volare.Flight(flight));
        });
    }

    $.getJSON('/workspaces/' + workspaceId + '/flights', function(fs) {
        _.each(fs, function(flight) {
            addFlight(flight.id);
        });
    });
});
