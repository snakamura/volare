/* global $id, $name */

define([
    'require',
    'lodash',
    'jquery',
    'angular',
    'volare/volare',
    'text!./workspace.css',
    'text!./workspaceAddFlight.html',
    'text!./workspaceRemoveFlight.html',
    'angular-ui-bootstrap',
    'bootstrap',
    'volare/components/chart',
    'volare/components/graph/altitude',
    'volare/components/graph/groundSpeed',
    'volare/components/graph/verticalSpeed',
    'volare/components/name',
    'volare/components/options',
    'volare/components/player',
    'volare/components/route',
    'volare/components/waypoint',
    'volare/components/weather',
    'volare/util'
], function(require, _, $, angular, volare, css, workspaceAddFlightTemplate, workspaceRemoveFlightTemplate) {
    'use strict';

    var workspace = angular.module('volare.workspace', [
        'ui.bootstrap',
        'volare.components.chart',
        'volare.components.graph.altitude',
        'volare.components.graph.groundSpeed',
        'volare.components.graph.verticalSpeed',
        'volare.components.name',
        'volare.components.options',
        'volare.components.player',
        'volare.components.route',
        'volare.components.waypoint',
        'volare.components.weather',
        'volare.util'
    ]);

    workspace.controller('WorkspaceController', ['$scope', '$http', '$modal', 'util', function($scope, $http, $modal, util) {
        util.loadCssInline(css);

        var flights = new volare.Flights();
        flights.setInterval(10);
        var map = new volare.Map(flights, $('#map'));

        $scope.name = $name;
        $scope.flights = flights;
        $scope.map = map;
        $scope.addFlight = function() {
            var modal = $modal.open({
                template: workspaceAddFlightTemplate,
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
                template: workspaceRemoveFlightTemplate,
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
        $scope.add = function() {
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
        $scope.remove = function() {
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
