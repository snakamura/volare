define([
    'lodash',
    'angular',
    'text!./groundSpeed.html',
    'volare/components/graph',
    'volare/components/graph/speed'
], function(_, angular, template) {
    'use strict';

    var groundSpeed = angular.module('volare.components.graph.groundSpeed', [
        'volare.components.graph',
        'volare.components.graph.speed'
    ]);

    groundSpeed.directive('volareGroundSpeedGraph', ['speedGraph', function(speedGraph) {
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
                    var min = 0;
                    var max = 120;
                    var steps = _.map(_.range(min, max, 10), function(value) {
                        return {
                            value: value,
                            label: value % 20 === 0 ? _.numberFormat(value) + 'km/h' : '',
                            primary: value % 50 === 0
                        };
                    });
                    return {
                        min: min,
                        max: max,
                        steps: steps
                    };
                }

                function getValue(flight, time) {
                    return flight.getGroundSpeedAt(time)*3600/1000;
                }

                speedGraph.init($scope, flights, 'currentGroundSpeedGraphContext', getRange, getValue);
            }]
        };
    }]);

    return groundSpeed;
});
