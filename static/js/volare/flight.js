define(function(require) {
    var angular = require('angular');
    require('bootstrap');
    var common = require('volare/common');
    require('volare/name');
    var volare = require('volare/volare');

    var flight = angular.module('volare.flight', ['volare.name']);

    flight.controller('FlightController', ['$scope', '$http', function($scope, $http) {
        var flights = new volare.Flights();
        var player = new volare.Player(flights, $('#player'));
        var map = new volare.Map(flights, $('#map'));
        map.setTrackType(volare.Map.TrackType.ALTITUDE);
        var altitudeGraph = new volare.AltitudeGraph(flights, $('#altitude'));
        var groundSpeedGraph = new volare.GroundSpeedGraph(flights, $('#ground_speed'));
        var verticalSpeedGraph = new volare.VerticalSpeedGraph(flights, $('#vertical_speed'));
        var chart = new volare.Chart(flights, $('#chart'));
        var optionsControl = new volare.OptionsControl(flights, map, $('#options'));
        var waypointControl = new volare.WaypointControl(map, $('#waypoint'));
        var weatherControl = new volare.WeatherControl(map, $('#weather'));

        $scope.name = $name;
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

        volare.setupLayout(flights, $('#map'), $('#sidebar'), $('#chart'));
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
            if (confirm('Are you sure to delete this flight?')) {
                $http.delete('').success(function() {
                    document.location.href = '/flights';
                });
            }
        };
    }]);

    return flight;
});
