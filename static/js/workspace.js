$(function() {
    var flights = new volare.Flights();
    var player = new volare.Player(flights, $('#player'));
    var map = new volare.Map(flights, $('#map'));
    var altitudeGraph = new volare.AltitudeGraph(flights, $('#altitude'));
    var speedGraph = new volare.SpeedGraph(flights, $('#speed'));
    var chart = new volare.Chart(flights, $('#chart'));
    var optionsControl = new volare.OptionsControl(map, $('#options'));
    var waypointControl = new volare.WaypointControl(map, $('#waypoint'));
    var routeControl = new volare.RouteControl(map, $('#route'));
    var weatherControl = new volare.WeatherControl(map, $('#weather'));

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
        $.putJSON('', data, function(workspace) {
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
        if (confirm('Are you sure to delete this workspace?')) {
            $.deleteJSON('', function() {
                document.location.href = '/workspaces';
            });
        }
    });

    $('#add_flight').on('click', function() {
        var modal = $('#add_flight_modal');
        var body = modal.find('.modal-body');
        body.text('Loading...');
        modal.modal({
            backdrop: 'static'
        });
        $.getJSON('/workspaces/' + workspaceId + '/candidates', function(flights) {
            body.text('');
            var ul = $('<ul></ul>');
            body.append(ul);
            _.each(flights, function(flight) {
                var e = $('<li><label><input type="checkbox" name="flights"><span></span></label></li>');
                e.find('input').prop('value', flight.id);
                e.find('span').text(flight.name);
                ul.append(e);
            });
        });
    });
    $('#add_flight_modal .btn-primary').on('click', function() {
        var modal = $('#add_flight_modal');
        var flightIds = _.map(modal.find('input:checked[type=checkbox]'), function(checkbox) {
            return parseInt(checkbox.value, 10);
        });
        if (flightIds.length === 0) {
            modal.modal('hide');
            return;
        }

        var data = {
            flightIds: flightIds
        };
        $.postJSON('/workspaces/' + workspaceId + '/flights', data, function(flights) {
            _.each(flights, function(flight) {
                addFlight(flight.id, flight.color);
            });
            modal.modal('hide');
        });
    });

    $('#remove_flight').on('click', function() {
        var modal = $('#remove_flight_modal');
        var body = modal.find('.modal-body');
        body.text('');
        var ul = $('<ul></ul>');
        body.append(ul);
        flights.eachFlight(function(flight) {
            var e = $('<li><label><input type="checkbox" name="flights"><span></span></label></li>');
            e.find('input').prop('value', flight.getId());
            e.find('span').text(flight.getName());
            ul.append(e);
        });
        modal.modal({
            backdrop: 'static'
        });
    });
    $('#remove_flight_modal .btn-primary').on('click', function() {
        var modal = $('#remove_flight_modal');
        var flightIds = _.map(modal.find('input:checked[type=checkbox]'), function(checkbox) {
            return parseInt(checkbox.value, 10);
        });
        if (flightIds.length === 0) {
            modal.modal('hide');
            return;
        }

        _.each(flightIds, function(flightId) {
            $.deleteJSON('/workspaces/' + workspaceId + '/flights/' + flightId, function() {
                flights.removeFlight(flightId);
            });
        });

        modal.modal('hide');
    });

    function addFlight(flightId, color) {
        $.getJSON('/flights/' + flightId, function(flight) {
            flights.addFlight(new volare.Flight(flight, color));
        });
    }

    $(map).on('route_changed', function() {
        // TODO
        // Remove an old route
        var route = map.getRoute();
        if (route) {
            if (!route.id) {
                var r = _.map(route.items, function(routeItem) {
                    return {
                        waypointItemId: routeItem.waypointItem.id,
                        radius: routeItem.radius
                    };
                });
                $.postJSON('/routes', r, function(route) {
                    var data = {
                        routeId: route.id
                    };
                    $.putJSON('', data, function(workspace) {
                    });
                });
            }
        }
        else {
            var data = {
                routeId: null
            };
            $.putJSON('', data, function(workspace) {
                console.log(workspace);
            });
        }
    });

    $.getJSON('/workspaces/' + workspaceId, function(workspace) {
        if (workspace.route) {
            $.getJSON('/routes/' + workspace.route, function(route) {
                map.setRoute(route);
            });
        }
    });
    $.getJSON('/workspaces/' + workspaceId + '/flights', function(fs) {
        _.each(fs, function(flight) {
            addFlight(flight.id, flight.color);
        });
    });

    volare.setupLayout(flights, $('#map'), $('#sidebar'), $('#chart'));
});
