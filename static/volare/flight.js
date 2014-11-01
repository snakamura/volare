/* global $id, $name */

define([
    'jquery',
    'angular',
    'volare/volare',
    'volare/common',
    'volare/name',
    'volare/components/chart',
    'volare/components/options',
    'volare/components/player',
    'volare/components/weather',
    'text!./flight.css'
], function($, angular, volare, common, __n, __c, __o, __p, __w, css) {
    'use strict';

    common.loadCssInline(css);

    var flight = angular.module('volare.flight', [
        'volare.name',
        'volare.components.chart',
        'volare.components.options',
        'volare.components.player',
        'volare.components.weather'
    ]);

    flight.controller('FlightController', ['$scope', '$http', function($scope, $http) {
        var flights = new volare.Flights();
        var map = new volare.Map(flights, $('#map'));
        map.setTrackType(volare.Map.TrackType.ALTITUDE);
        var altitudeGraph = new volare.AltitudeGraph(flights, $('#altitude'));
        var groundSpeedGraph = new volare.GroundSpeedGraph(flights, $('#ground_speed'));
        var verticalSpeedGraph = new volare.VerticalSpeedGraph(flights, $('#vertical_speed'));
        var waypointControl = new volare.WaypointControl(map, $('#waypoint'));

        $scope.name = $name;
        $scope.flights = flights;
        $scope.map = map;
        $scope.newWorkspace = function() {
            $http.post('/workspaces', {
                name: this.name
            }).success(function(workspace) {
                $http.post('/workspaces/' + workspace.id + '/flights', {
                    flightIds: [flights.getPrimaryFlight().getId()]
                }).success(function(flights) {
                    document.location.href = '/workspaces/' + workspace.id;
                });
            });
        };

        $(flights).on('flight_added', function(event, flight) {
            $('#time').text(common.formatTime(flight.getTime()));
            $('#duration').text(common.formatDuration(flight.getDuration()));
            $('#max_altitude').text(common.formatAltitude(flight.getMaxAltitude()));
        });
        flights.addFlight($id, 'red');

        volare.setupLayout(flights, $('#map'), $('#sidebar'), $('.chart'));
    }]);

    flight.controller('FlightNameController', ['$scope', '$http', function($scope, $http) {
        $scope.update = function(name) {
            var self = this;
            $http.put('', {
                name: name
            }).success(function(flight) {
                self.name = flight.name;
            });
        };
        $scope.delete = function() {
            if (window.confirm('Are you sure to delete this flight?')) {
                $http.delete('').success(function() {
                    document.location.href = '/flights';
                });
            }
        };
    }]);

    return flight;
});
