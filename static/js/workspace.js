(function() {
    var workspace = angular.module('volare.workspace', ['ui.bootstrap', 'volare.name']);

    workspace.controller('WorkspaceController', ['$scope', '$http', '$modal', '$name', function($scope, $http, $modal, $name) {
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
            var modal = $modal.open({
                templateUrl: 'addFlight.html',
                controller: 'WorkspaceAddFlightController',
                backdrop: 'static',
                resolve: {
                    $workspaceId: function() {
                        return workspaceId;
                    }
                }
            });
            modal.result.then(function(flightIds) {
                if (flightIds.length !== 0) {
                    $http.post('/workspaces/' + workspaceId + '/flights', {
                        flightIds: flightIds
                    }).success(function(fs) {
                        _.each(fs, function(flight) {
                            flights.addFlight(flight.id, flight.color);
                        });
                    });
                }
            });
        };
        $scope.removeFlight = function() {
            var modal = $modal.open({
                templateUrl: 'removeFlight.html',
                controller: 'WorkspaceRemoveFlightController',
                backdrop: 'static',
                resolve: {
                    $flights: function() {
                        return flights.mapFlight(function(flight) {
                            return {
                                id: flight.getId(),
                                name: flight.getName()
                            };
                        });
                    }
                }
            });
            modal.result.then(function(flightIds) {
                if (flightIds.length !== 0) {
                    _.each(flightIds, function(flightId) {
                        $http.delete('/workspaces/' + workspaceId + '/flights/' + flightId).success(function() {
                            flights.removeFlight(flightId);
                        });
                    });
                }
            });
        };

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

    workspace.controller('WorkspaceAddFlightController', ['$scope', '$http', '$workspaceId', function($scope, $http, $workspaceId) {
        $scope.flights = [];
        $scope.ok = function() {
            var flightIds = _.pluck(_.filter(this.flights, function(flight) {
                return flight.selected;
            }), 'id');
            this.$close(flightIds);
        };
        $scope.cancel = function() {
            this.$dismiss('cancel');
        };

        $http.get('/workspaces/' + $workspaceId + '/candidates').success(function(flights) {
            $scope.flights = flights;
        });
    }]);

    workspace.controller('WorkspaceRemoveFlightController', ['$scope', '$flights', function($scope, $flights) {
        $scope.flights = $flights;
        $scope.ok = function() {
            var flightIds = _.pluck(_.filter(this.flights, function(flight) {
                return flight.selected;
            }), 'id');
            this.$close(flightIds);
        };
        $scope.cancel = function() {
            this.$dismiss('cancel');
        };
    }]);
}());
