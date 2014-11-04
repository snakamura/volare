define([
    'lodash',
    'jquery',
    'angular',
    'volare/components/graph',
    'text!./groundSpeed.html'
], function(_, $, angular, graph, template) {
    'use strict';

    var groundSpeed = angular.module('volare.components.graph.groundSpeed', [
        'volare.components.graph'
    ]);

    groundSpeed.directive('volareGroundSpeedGraph', ['graph', function(graph) {
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
                    var steps = _.map(_.range(min, max + 1, 10), function(value) {
                        return {
                            value: value,
                            label: _.numberFormat(value) + 'km/h',
                            primary: value % 50 === 0
                        };
                    });
                    return {
                        min: min,
                        max: max,
                        steps: steps
                    };
                }

                function getStrokes(currentTime, withContext, partial) {
                    var strokes = [];
                    flights.eachFlight(function(flight) {
                        if (flight.isVisible()) {
                            var graphContext = null;
                            if (withContext) {
                                graphContext = flight.getExtra('currentGroundSpeedGraphContext');
                                if (!graphContext) {
                                    graphContext = new GroundSpeedGraphContext();
                                    flight.setExtra('currentGroundSpeedGraphContext', graphContext);
                                }
                                if (!partial)
                                    graphContext.reset();
                            }

                            var stroke = {
                                color: flight.getColor(),
                                points: []
                            };

                            var startTime = null;
                            var endTime = null;
                            var step = 20*1000;
                            if (graphContext && graphContext.isSet()) {
                                var lastDrawnTime = graphContext.getTime();
                                if (currentTime.getTime() >= lastDrawnTime.getTime() + step) {
                                    startTime = lastDrawnTime;
                                    endTime = currentTime || flight.getEndTime();
                                }
                            }
                            else {
                                startTime = flight.getStartTime();
                                endTime = currentTime || flight.getEndTime();
                            }
                            if (startTime) {
                                stroke.points.push({
                                    time: startTime,
                                    value: flight.getGroundSpeedAt(startTime)*3600/1000
                                });
                                for (var time = startTime.getTime() + step; time < endTime.getTime(); time += step) {
                                    var t = new Date(time);
                                    stroke.points.push({
                                        time: t,
                                        value: flight.getGroundSpeedAt(t)*3600/1000
                                    });
                                }

                                if (graphContext)
                                    graphContext.set(new Date(time - step));
                            }

                            strokes.push(stroke);
                        }
                    });
                    return strokes;
                }

                graph.init($scope, flights, getRange, getStrokes);
            }]
        };
    }]);


    function GroundSpeedGraphContext() {
        this._set = false;
        this._time = null;
    }

    GroundSpeedGraphContext.prototype.isSet = function() {
        return this._set;
    };

    GroundSpeedGraphContext.prototype.getTime = function() {
        return this._time;
    };

    GroundSpeedGraphContext.prototype.set = function(time) {
        this._set = true;
        this._time = time;
    };

    GroundSpeedGraphContext.prototype.reset = function() {
        this._set = false;
    };


    return groundSpeed;
});
