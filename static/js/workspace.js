(function() {
    var workspace = angular.module('volare.workspace', ['volare.name']);

    workspace.controller('WorkspaceController', ['$scope', '$http', '$name', function($scope, $http, $name) {
        var flights = new volare.Flights();
        flights.setInterval(10);
        var player = new volare.Player(flights, $('#player'));
        var map = new volare.Map(flights, $('#map'));
        var altitudeGraph = new volare.AltitudeGraph(flights, $('#altitude'));
        var groundSpeedGraph = new volare.GroundSpeedGraph(flights, $('#ground_speed'));
        var verticalSpeedGraph = new volare.VerticalSpeedGraph(flights, $('#vertical_speed'));
        var chart = new volare.Chart(flights, $('#chart'));
        var optionsControl = new volare.OptionsControl(flights, map, $('#options'));
        var waypointControl = new volare.WaypointControl(map, $('#waypoint'));
        var routeControl = new volare.RouteControl(map, $('#route'));
        var weatherControl = new volare.WeatherControl(map, $('#weather'));

        $scope.name = $name;
        $scope.addFlight = function() {
            var $modal = $('#add_flight_modal');
            var $body = $modal.find('.modal-body');
            $body.text('Loading...');
            $modal.modal({
                backdrop: 'static'
            });
            $.getJSON('/workspaces/' + workspaceId + '/candidates', function(flights) {
                $body.empty();
                var $ul = $('<ul></ul>');
                $body.append($ul);
                _.each(flights, function(flight) {
                    var $e = $('<li><label><input type="checkbox" name="flights"><span></span></label></li>');
                    $e.find('input').prop('value', flight.id);
                    $e.find('span').text(flight.name);
                    $ul.append($e);
                });
            });
        };
        $('#add_flight_modal .btn-primary').on('click', function() {
            var $modal = $('#add_flight_modal');
            var flightIds = _.map($modal.find('input:checked[type=checkbox]'), function(checkbox) {
                return parseInt(checkbox.value, 10);
            });
            if (flightIds.length === 0) {
                $modal.modal('hide');
                return;
            }

            $.postJSON('/workspaces/' + workspaceId + '/flights', {
                flightIds: flightIds
            }, function(fs) {
                _.each(fs, function(flight) {
                    flights.addFlight(flight.id, flight.color);
                });
                $modal.modal('hide');
            });
        });

        $scope.removeFlight = function() {
            var $modal = $('#remove_flight_modal');
            var $body = $modal.find('.modal-body');
            $body.empty();
            var $ul = $('<ul></ul>');
            $body.append($ul);
            flights.eachFlight(function(flight) {
                var $e = $('<li><label><input type="checkbox" name="flights"><span></span></label></li>');
                $e.find('input').prop('value', flight.getId());
                $e.find('span').text(flight.getName());
                $ul.append($e);
            });
            $modal.modal({
                backdrop: 'static'
            });
        };
        $('#remove_flight_modal .btn-primary').on('click', function() {
            var $modal = $('#remove_flight_modal');
            var flightIds = _.map($modal.find('input:checked[type=checkbox]'), function(checkbox) {
                return parseInt(checkbox.value, 10);
            });
            if (flightIds.length === 0) {
                $modal.modal('hide');
                return;
            }

            _.each(flightIds, function(flightId) {
                $.deleteJSON('/workspaces/' + workspaceId + '/flights/' + flightId, function() {
                    flights.removeFlight(flightId);
                });
            });

            $modal.modal('hide');
        });

        $(map).on('route_changed', function() {
            var route = map.getRoute();
            if (route) {
                if (!route.getId()) {
                    var r = _.map(route.getItems(), function(routeItem) {
                        return {
                            waypointItemId: routeItem.getWaypointItem().getId(),
                            radius: routeItem.getRadius()
                        };
                    });
                    $.postJSON('/routes', r, function(route) {
                        $.putJSON('', {
                            routeId: route.id
                        }, function(workspace) {
                        });
                    });
                }
            }
            else {
                $.putJSON('', {
                    routeId: null
                }, function(workspace) {
                });
            }
        });

        $.getJSON('/workspaces/' + workspaceId, function(workspace) {
            if (workspace.route) {
                $.getJSON('/routes/' + workspace.route, function(route) {
                    map.setRoute(volare.Route.wrap(route));
                });
            }
        });
        $.getJSON('/workspaces/' + workspaceId + '/flights', function(fs) {
            _.each(fs, function(flight) {
                flights.addFlight(flight.id, flight.color);
            });
        });

        volare.setupLayout(flights, $('#map'), $('#sidebar'), $('#chart'));
    }]);

    workspace.controller('WorkspaceNameController', ['$scope', '$http', function($scope, $http) {
        $scope.update = function(name) {
            var self = this;
            $http.put('', {
                name: name
            }).success(function(workspace) {
                self.name = workspace.name;
            });
        };
        $scope.delete = function() {
            if (confirm('Are you sure to delete this workspace?')) {
                $http.delete('').success(function() {
                    document.location.href = '/workspaces';
                });
            }
        };
    }]);
}());
