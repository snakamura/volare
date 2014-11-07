define([
    'lodash',
    'underscore.string',
    'angular',
    'volare/components/graph',
    'volare/components/graph/speed',
    'volare/util'
], function(_, _s, angular) {
    'use strict';

    var module = angular.module('volare.components.graph.groundSpeed', [
        'volare.components.graph',
        'volare.components.graph.speed',
        'volare.util'
    ]);

    module.directive('volareGroundSpeedGraph', function() {
        return {
            restrict: 'E',
            replace: true,
            template: '<div><volare-graph range="range" strokes="strokes"></volare-graph></div>',
            scope: {
                flights: '='
            },
            controller: 'GroundSpeedGraphController'
        };
    });

    module.controller('GroundSpeedGraphController', ['$scope', 'SpeedGraphController', 'util', function($scope, SpeedGraphController, util) {
        function GroundSpeedGraphController(scope) {
            SpeedGraphController.call(this, scope, scope.flights, 'currentGroundSpeedGraphContext');
        }
        util.inherit(GroundSpeedGraphController, SpeedGraphController);

        GroundSpeedGraphController.prototype.getRange = function() {
            var min = 0;
            var max = 120;
            var steps = _.map(_.range(min, max, 10), function(value) {
                return {
                    value: value,
                    label: value % 20 === 0 ? _s.numberFormat(value) + 'km/h' : '',
                    primary: value % 50 === 0
                };
            });
            return {
                min: min,
                max: max,
                steps: steps
            };
        };

        GroundSpeedGraphController.prototype.getValue = function(flight, time) {
            return flight.getGroundSpeedAt(time)*3600/1000;
        };

        return new GroundSpeedGraphController($scope);
    }]);

    return module;
});
