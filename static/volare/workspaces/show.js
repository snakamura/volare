/* global workspaceId, workspaceName */

define([
    'require',
    'lodash',
    'jquery',
    'angular',
    'text!./show.css',
    'text!./addFlight.html',
    'text!./removeFlight.html',
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

    var module = angular.module('volare.workspace', [
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

    module.controller('WorkspaceController', ['$scope', '$http', '$modal', 'layout', 'Map', 'model', 'util', function($scope, $http, $modal, layout, Map, model, util) {
        util.loadCssInline(css);

        var flights = new model.Flights();
        flights.setInterval(10);

        $scope.name = workspaceName;
        $scope.flights = flights;
        $scope.map = {};
        $scope.addFlight = function() {
            var modal = $modal.open({
                template: workspaceAddFlightTemplate,
                controller: 'WorkspaceAddFlightController',
                backdrop: 'static',
                resolve: {
                    workspaceId: function() {
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
                            flights.loadFlight(flight.id, flight.color, true);
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
                    flights: function() {
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

        $scope.$watch('map.route', function(newRoute, oldRoute) {
            if (newRoute) {
                if (!newRoute.getId()) {
                    var r = _.map(newRoute.getItems(), function(routeItem) {
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
            else if (oldRoute) {
                $http.put('', {
                    routeId: null
                }).success(function(workspace) {
                });
            }
        });

        $http.get('/workspaces/' + workspaceId).success(function(workspace) {
            if (workspace.route) {
                $http.get('/routes/' + workspace.route).success(function(route) {
                    $scope.map.route = model.Route.wrap(route);
                });
            }
        });
        $http.get('/workspaces/' + workspaceId + '/flights').success(function(fs) {
            _.each(fs, function(flight) {
                flights.loadFlight(flight.id, flight.color, true);
            });
        });

        layout.setupLayout(flights, $('#map'), $('#sidebar'), $('#chart'));
    }]);

    module.controller('WorkspaceNameController', ['$scope', '$http', '$window', '$document', function($scope, $http, $window, $document) {
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
                    $document[0].location.href = '/workspaces';
                });
            }
        };
    }]);

    module.controller('WorkspaceAddFlightController', ['$scope', '$http', 'workspaceId', function($scope, $http, workspaceId) {
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

        $http.get('/workspaces/' + workspaceId + '/candidates').success(function(flights) {
            $scope.flights = flights;
        });
    }]);

    module.controller('WorkspaceRemoveFlightController', ['$scope', 'flights', function($scope, flights) {
        $scope.flights = flights;
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

    return module;
});
