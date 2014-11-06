/* global flightId, flightName */

define([
    'jquery',
    'angular',
    'text!./flight.css',
    'bootstrap',
    'volare/filters',
    'volare/components/chart',
    'volare/components/graph/altitude',
    'volare/components/graph/groundSpeed',
    'volare/components/graph/verticalSpeed',
    'volare/components/map',
    'volare/components/name',
    'volare/components/options',
    'volare/components/player',
    'volare/components/waypoint',
    'volare/components/weather',
    'volare/layout',
    'volare/model',
    'volare/util'
], function($, angular, css) {
    'use strict';

    var module = angular.module('volare.flight', [
        'volare.components.chart',
        'volare.components.graph.altitude',
        'volare.components.graph.groundSpeed',
        'volare.components.graph.verticalSpeed',
        'volare.components.map',
        'volare.components.name',
        'volare.components.options',
        'volare.components.player',
        'volare.components.waypoint',
        'volare.components.weather',
        'volare.filters',
        'volare.layout',
        'volare.model',
        'volare.util'
    ]);

    module.controller('FlightController', ['$scope', '$http', '$document', 'layout', 'Map', 'model', 'util', function($scope, $http, $document, layout, Map, model, util) {
        util.loadCssInline(css);

        var flights = new model.Flights();

        $scope.name = flightName;
        $scope.flights = flights;
        $scope.newWorkspace = function() {
            $http.post('/workspaces', {
                name: this.name
            }).success(function(workspace) {
                $http.post('/workspaces/' + workspace.id + '/flights', {
                    flightIds: [flights.getPrimaryFlight().getId()]
                }).success(function(flights) {
                    $document[0].location.href = '/workspaces/' + workspace.id;
                });
            });
        };

        $scope.$watch('map', function(map) {
            if (map)
                map.setTrackType(Map.TrackType.ALTITUDE);
        });

        flights.addFlight(flightId, 'red');

        layout.setupLayout(flights, $('#map'), $('#sidebar'), $('#chart'));
    }]);

    module.controller('FlightNameController', ['$scope', '$http', '$window', '$document', function($scope, $http, $window, $document) {
        $scope.update = function(name) {
            var self = this;
            $http.put('', {
                name: name
            }).success(function(flight) {
                self.name = flight.name;
            });
        };
        $scope.delete = function() {
            if ($window.confirm('Are you sure to delete this flight?')) {
                $http.delete('').success(function() {
                    $document[0].location.href = '/flights';
                });
            }
        };
    }]);

    module.controller('FlightPropertiesController', ['$scope', function($scope) {
        $($scope.flights).on('flight_added', function(event, flight) {
            $scope.time = flight.getTime();
            $scope.duration = flight.getDuration();
            $scope.maxAltitude = flight.getMaxAltitude();
        });
    }]);

    return module;
});
