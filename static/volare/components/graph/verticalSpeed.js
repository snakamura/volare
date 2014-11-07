define([
    'lodash',
    'underscore.string',
    'angular',
    'text!./verticalSpeed.html',
    'volare/components/graph',
    'volare/components/graph/speed'
], function(_, _s, angular, template) {
    'use strict';

    var module = angular.module('volare.components.graph.verticalSpeed', [
        'volare.components.graph',
        'volare.components.graph.speed'
    ]);

    module.controller('VerticalSpeedGraphController', ['$scope', '$controller', function($scope, $controller) {
        function VerticalSpeedGraphController(scope) {
            this.init(scope.flights, 'currentVerticalSpeedGraphContext');
        }

        VerticalSpeedGraphController.prototype = $controller('SpeedGraphController', {
            $scope: $scope
        });

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

        return new VerticalSpeedGraphController($scope);
    }]);

    module.directive('volareVerticalSpeedGraph', function() {
        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                flights: '='
            },
            controller: 'VerticalSpeedGraphController'
        };
    });

    return module;
});
