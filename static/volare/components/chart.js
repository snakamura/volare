define([
    'lodash',
    'jquery',
    'angular',
    'volare/common',
    'volare/volare',
    'text!./chart.css',
    'text!./chart.html'
], function(_, $, angular, common, volare, css, template) {
    'use strict';

    common.loadCssInline(css);

    var chart = angular.module('volare.components.chart', []);

    chart.directive('volareChart', [function() {
        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                modelFlights: '=flights'
            },
            controller: function($scope) {
                var modelFlights = $scope.modelFlights;

                $scope.flights = [];

                $scope.visible = true;
                $scope.changeVisible = function(visible) {
                    _.each(this.flights, function(flight) {
                        flight.visible = visible;
                    });
                };

                $scope.$watch(function(scope) {
                    return _.map(scope.flights, function(flight) {
                        return _.pick(flight, ['id', 'visible']);
                    });
                }, function(flights) {
                    _.each(flights, function(flight) {
                        var modelFlight = modelFlights.getFlight(flight.id);
                        modelFlight.setVisible(flight.visible);
                    });
                }, true);

                function update(propertiesOnly) {
                    if (!propertiesOnly) {
                        $scope.flights = modelFlights.mapFlight(function(modelFlight) {
                            return {
                                id: modelFlight.getId(),
                                visible: modelFlight.isVisible(),
                                name: modelFlight.getName(),
                                color: modelFlight.getColor()
                            };
                        });
                    }

                    var time = modelFlights.getCurrentTime();
                    modelFlights.eachFlight(function(modelFlight, index) {
                        var position = modelFlight.getPositionAt(time);
                        var flight = $scope.flights[index];
                        flight.latitude = position.latitude;
                        flight.longitude = position.longitude;
                        flight.altitude = position.altutide;
                        flight.groundSpeed = modelFlight.getGroundSpeedAt(time);
                        flight.verticalSpeed = modelFlight.getVerticalSpeedAt(time);
                        flight.status = modelFlight.getStatusAt(time);
                        flight.ld = modelFlight.getLD(time);
                        flight.averageClimb = modelFlight.getAverageClimb(time);
                    });
                }

                $(modelFlights).on('flight_added', function(event, modelFlight, index) {
                    update(false);
                    // TODO
                    // Remove this when it uses $http to add a flight
                    $scope.$apply();
                });
                $(modelFlights).on('flight_removed', function(event, modelFlight) {
                    update(false);
                });
                $(modelFlights).on('currenttime_changed', function() {
                    update(true);
                });
                update();
            }
        };
    }]);

    chart.filter('position', function() {
        return function(position) {
            return _.sprintf('%.5f', position);
        };
    });

    chart.filter('altitude', function() {
        return function(altitude) {
            return _.numberFormat(altitude) + 'm';
        };
    });

    chart.filter('speed', function() {
        return function(speed) {
            return _.sprintf('%.1fkm/h', speed*3600/1000);
        };
    });

    chart.filter('verticalSpeed', function() {
        return function(speed) {
            return _.sprintf('%.1fm/s', speed);
        };
    });

    chart.filter('status', function() {
        return function(status) {
        switch (status) {
        case volare.Flight.STATUS_UNKNOWN:
            break;
        case volare.Flight.STATUS_CIRCLING:
            return 'Circling';
        case volare.Flight.STATUS_GLIDING:
            return 'Gliding';
        default:
            break;
        }
        return '-';
        };
    });

    chart.filter('ld', function() {
        return function(ld) {
            return !ld ? '-' : _.sprintf('%.1f', ld);
        };
    });

    chart.filter('averageClimb', function() {
        return function(averageClimb) {
            return !averageClimb ? '-' : _.sprintf('%.1fm/s', averageClimb);
        };
    });

    return chart;
});
