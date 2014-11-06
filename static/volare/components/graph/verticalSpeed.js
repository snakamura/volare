define([
    'lodash',
    'underscore.string',
    'angular',
    'text!./verticalSpeed.html',
    'volare/components/graph',
    'volare/components/graph/speed',
    'volare/util'
], function(_, _s, angular, template) {
    'use strict';

    var module = angular.module('volare.components.graph.verticalSpeed', [
        'volare.components.graph',
        'volare.components.graph.speed',
        'volare.util'
    ]);

    module.directive('volareVerticalSpeedGraph', ['SpeedGraphController', 'util', function(SpeedGraphController, util) {
        function VerticalSpeedGraphController($scope) {
            SpeedGraphController.call(this, $scope, $scope.flights, 'currentVerticalSpeedGraphContext');
        }
        util.inherit(VerticalSpeedGraphController, SpeedGraphController);

        VerticalSpeedGraphController.prototype.getRange = function() {
            var min = -5;
            var max = 5;
            var steps = _.map(_.range(min, max, 1), function(value) {
                return {
                    value: value,
                    label: _s.numberFormat(value) + 'm/s',
                    primary: value === 0
                };
            });
            return {
                min: min,
                max: max,
                steps: steps
            };
        };

        VerticalSpeedGraphController.prototype.getValue = function(flight, time) {
            return flight.getVerticalSpeedAt(time);
        };

        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                flights: '='
            },
            controller: ['$scope', VerticalSpeedGraphController]
        };
    }]);

    return module;
});
