define([
    'lodash',
    'underscore.string',
    'angular',
    'volare/components/graph',
    'volare/components/graph/speed',
    'volare/util'
], function(_, _s, angular) {
    'use strict';

    var module = angular.module('volare.components.graph.verticalSpeed', [
        'volare.components.graph',
        'volare.components.graph.speed',
        'volare.util'
    ]);

    module.controller('VerticalSpeedGraphController', ['$scope', 'SpeedGraphController', 'util', function($scope, SpeedGraphController, util) {
        function VerticalSpeedGraphController(scope) {
            SpeedGraphController.call(this, scope, scope.flights, 'currentVerticalSpeedGraphContext');
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

        return new VerticalSpeedGraphController($scope);
    }]);

    module.directive('volareVerticalSpeedGraph', function() {
        return {
            restrict: 'E',
            replace: true,
            template: '<div><volare-graph range="range" strokes="strokes"></volare-graph></div>',
            scope: {
                flights: '='
            },
            controller: 'VerticalSpeedGraphController'
        };
    });

    return module;
});
