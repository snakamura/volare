define([
    'lodash',
    'underscore.string',
    'jquery',
    'angular',
    'text!./chart.css',
    'text!./chart.html',
    'volare/filters',
    'volare/model',
    'volare/util'
], function(_, _s, $, angular, css, template) {
    'use strict';

    var module = angular.module('volare.components.chart', [
        'volare.filters',
        'volare.model',
        'volare.util'
    ]);

    module.directive('volareChart', ['util', function(util) {
        util.loadCssInline(css);

        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                flights: '='
            },
            controller: 'ChartController'
        };
    }]);

    module.controller('ChartController', ['$scope', function($scope) {
        var flights = $scope.flights;

        $scope.rows = [];
        $scope.visible = true;
        $scope.changeVisible = function(visible) {
            _.each(this.rows, function(row) {
                row.visible = visible;
            });
        };

        $scope.$watch(function(scope) {
            return _.map(scope.rows, function(row) {
                return _.pick(row, ['id', 'visible', 'primary']);
            });
        }, function(rows) {
            var primaryFlight = null;
            _.each(rows, function(row) {
                var flight = flights.getFlight(row.id);
                flight.setVisible(row.visible);
                if (row.primary)
                    primaryFlight = flight;
            });
            flights.setPrimaryFlight(primaryFlight);
        }, true);

        function update(propertiesOnly) {
            if (!propertiesOnly) {
                var primaryFlight = flights.getPrimaryFlight();
                $scope.rows = flights.mapFlight(function(flight) {
                    return {
                        id: flight.getId(),
                        visible: flight.isVisible(),
                        primary: flight === primaryFlight ? 1 : 0,
                        name: flight.getName(),
                        color: flight.getColor()
                    };
                });
            }

            var time = flights.getCurrentTime();
            flights.eachFlight(function(flight, index) {
                var row = $scope.rows[index];
                row.position = flight.getPositionAt(time);
                row.groundSpeed = flight.getGroundSpeedAt(time);
                row.verticalSpeed = flight.getVerticalSpeedAt(time);
                row.status = flight.getStatusAt(time);
                row.ld = flight.getLD(time);
                row.averageClimb = flight.getAverageClimb(time);
            });
        }

        var visibleChangedListener = _.bind(update, null, false);
        $(flights).on('flight_added', function(event, flight) {
            $(flight).on('visible_changed', visibleChangedListener);
            update(false);
        });
        $(flights).on('flight_removed', function(event, flight) {
            $(flight).off('visible_changed', visibleChangedListener);
            update(false);
        });
        $(flights).on('currentTime_changed', _.bind(update, null, true));
        $(flights).on('primaryFlight_changed', _.bind(update, null, false));

        update(false);
    }]);

    module.filter('status', ['model', function(model) {
        return function(status) {
        switch (status) {
        case model.Flight.STATUS_UNKNOWN:
            break;
        case model.Flight.STATUS_CIRCLING:
            return 'Circling';
        case model.Flight.STATUS_GLIDING:
            return 'Gliding';
        default:
            break;
        }
        return '-';
        };
    }]);

    module.filter('ld', function() {
        return function(ld) {
            return ld ? _s.sprintf('%.1f', ld) : '-';
        };
    });

    module.filter('averageClimb', function() {
        return function(averageClimb) {
            return !averageClimb ? '-' : _s.sprintf('%.1fm/s', averageClimb);
        };
    });

    return module;
});
