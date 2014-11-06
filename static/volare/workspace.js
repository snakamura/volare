/* global workspaceId, workspaceName */

define([
    'require',
    'lodash',
    'jquery',
    'angular',
    'text!./workspace.css',
    'text!./workspaceAddFlight.html',
    'text!./workspaceRemoveFlight.html',
    'angular-ui-bootstrap',
    'bootstrap',
    'volare/components/chart',
    'volare/components/graph/altitude',
    'volare/components/graph/groundSpeed',
    'volare/components/graph/verticalSpeed',
    'volare/components/map',
    'volare/components/name',
    'volare/components/options',
    'volare/components/player',
    'volare/components/route',
    'volare/components/waypoint',
    'volare/components/weather',
    'volare/layout',
    'volare/model',
    'volare/util'
], function(require, _, $, angular, css, workspaceAddFlightTemplate, workspaceRemoveFlightTemplate) {
    'use strict';

    var workspace = angular.module('volare.workspace', [
        'ui.bootstrap',
        'volare.components.chart',
        'volare.components.graph.altitude',
        'volare.components.graph.groundSpeed',
        'volare.components.graph.verticalSpeed',
        'volare.components.map',
        'volare.components.name',
        'volare.components.options',
        'volare.components.player',
        'volare.components.route',
        'volare.components.waypoint',
        'volare.components.weather',
        'volare.layout',
        'volare.model',
        'volare.util'
    ]);

    workspace.controller('WorkspaceController', ['$scope', '$http', '$modal', 'layout', 'model', 'util', function($scope, $http, $modal, layout, model, util) {
        util.loadCssInline(css);

        var flights = new model.Flights();
        flights.setInterval(10);

        $scope.name = workspaceName;
        $scope.flights = flights;
        $scope.addFlight = function() {
            var modal = $modal.open({
                template: workspaceAddFlightTemplate,
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
                        $http.delete('/workspaces/' + workspaceId + '/flights/' + flightId).success(function() {
                            flights.removeFlight(flightId);
                        });
                    });
                }
            });
        };

        $($scope.map).on('route_changed', function() {
            var route = $scope.map.getRoute();
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

        $http.get('/workspaces/' + workspaceId).success(function(workspace) {
            if (workspace.route) {
                $http.get('/routes/' + workspace.route).success(function(route) {
                    $scope.map.setRoute(model.Route.wrap(route));
                });
            }
        });
        $http.get('/workspaces/' + workspaceId + '/flights').success(function(fs) {
            _.each(fs, function(flight) {
                flights.addFlight(flight.id, flight.color);
            });
        });

        layout.setupLayout(flights, $('#map'), $('#sidebar'), $('#chart'));
    }]);

    workspace.controller('WorkspaceNameController', ['$scope', '$http', '$window', function($scope, $http, $window) {
        $scope.update = function(name) {
            var self = this;
            $http.put('', {
                name: name
            }).success(function(workspace) {
                self.name = workspace.name;
            });
        };
        $scope.delete = function() {
            if ($window.confirm('Are you sure to delete this workspace?')) {
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
