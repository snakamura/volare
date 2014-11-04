define([
    'lodash',
    'angular',
    'text!./verticalSpeed.html',
    'volare/components/graph',
    'volare/components/graph/speed'
], function(_, angular, template) {
    'use strict';

    var verticalSpeed = angular.module('volare.components.graph.verticalSpeed', [
        'volare.components.graph',
        'volare.components.graph.speed'
    ]);

    verticalSpeed.directive('volareVerticalSpeedGraph', ['speedGraph', function(speedGraph) {
        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                flights: '='
            },
            controller: ['$scope', function($scope) {
                var flights = $scope.flights;

                function getRange() {
                    var min = -5;
                    var max = 5;
                    var steps = _.map(_.range(min, max, 1), function(value) {
                        return {
                            value: value,
                            label: _.numberFormat(value) + 'm/s',
                            primary: value === 0
                        };
                    });
                    return {
                        min: min,
                        max: max,
                        steps: steps
                    };
                }

                function getValue(flight, time) {
                    return flight.getVerticalSpeedAt(time);
                }

                speedGraph.init($scope, flights, 'currentVerticalSpeedGraphContext', getRange, getValue);
            }]
        };
    }]);

    return verticalSpeed;
});
