/* global $id, $name */

define(['require',
        'lodash',
        'jquery',
        'angular',
        'angular-ui-bootstrap',
        'bootstrap',
        'volare/volare',
        'volare/common',
        'volare/name',
        'volare/chart',
        'volare/player',
        'text!volare/workspace.css'],
       function(require, _, $, angular, __aub, __b, volare, common, __n, __c, __p, css) {
    'use strict';

    common.loadCssInline(css);

    var workspace = angular.module('volare.workspace', ['ui.bootstrap', 'volare.name', 'volare.chart', 'volare.player']);

    workspace.controller('WorkspaceController', ['$scope', '$http', '$modal', function($scope, $http, $modal) {
        var flights = new volare.Flights();
        flights.setInterval(10);
        var map = new volare.Map(flights, $('#map'));
        var altitudeGraph = new volare.AltitudeGraph(flights, $('#altitude'));
        var groundSpeedGraph = new volare.GroundSpeedGraph(flights, $('#ground_speed'));
        var verticalSpeedGraph = new volare.VerticalSpeedGraph(flights, $('#vertical_speed'));
        var optionsControl = new volare.OptionsControl(flights, map, $('#options'));
        var waypointControl = new volare.WaypointControl(map, $('#waypoint'));
        var routeControl = new volare.RouteControl(map, $('#route'));
        var weatherControl = new volare.WeatherControl(map, $('#weather'));

        $scope.name = $name;
        $scope.flights = flights;
        $scope.addFlight = function() {
            var modal = $modal.open({
                templateUrl: 'addFlight.html',
                controller: 'WorkspaceAddFlightController',
                backdrop: 'static',
                resolve: {
                    $workspaceId: function() {
                        return $id;
                    }
                }
            });
            modal.result.then(function(flightIds) {
                if (flightIds.length !== 0) {
                    $http.post('/workspaces/' + $id + '/flights', {
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
                        $http.delete('/workspaces/' + $id + '/flights/' + flightId).success(function() {
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
                    $http.post('/routes', r).success(function(route) {
                        $http.put('', {
                            routeId: route.id
                        }).success(function(workspace) {
                        });
                    });
                }
            }
            else {
                $http.put('', {
                    routeId: null
                }).success(function(workspace) {
                });
            }
        });

        $http.get('/workspaces/' + $id).success(function(workspace) {
            if (workspace.route) {
                $http.get('/routes/' + workspace.route).success(function(route) {
                    map.setRoute(volare.Route.wrap(route));
                });
            }
        });
        $http.get('/workspaces/' + $id + '/flights').success(function(fs) {
            _.each(fs, function(flight) {
                flights.addFlight(flight.id, flight.color);
            });
        });

        volare.setupLayout(flights, $('#map'), $('#sidebar'), $('.chart'));
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
            if (window.confirm('Are you sure to delete this workspace?')) {
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

    return workspace;
});
