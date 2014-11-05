define([
    'angular',
    'volare/components/graph'
], function(angular) {
    'use strict';

    var speed = angular.module('volare.components.graph.speed', [
        'volare.components.graph'
    ]);

    speed.factory('speedGraph', ['graph', function(graph) {
        return {
            init: function(scope, flights, contextName, getRange, getValue) {
                function getStrokes(currentTime, withContext, partial) {
                    var strokes = [];
                    flights.eachFlight(function(flight) {
                        if (flight.isVisible()) {
                            var graphContext = null;
                            if (withContext) {
                                graphContext = flight.getExtra(contextName);
                                if (!graphContext) {
                                    graphContext = new SpeedGraphContext();
                                    flight.setExtra(contextName, graphContext);
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
                                    value: getValue(flight, startTime)
                                });
                                for (var time = startTime.getTime() + step; time < endTime.getTime(); time += step) {
                                    var t = new Date(time);
                                    stroke.points.push({
                                        time: t,
                                        value: getValue(flight, t)
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

                graph.init(scope, flights, getRange, getStrokes);
            }
        };
    }]);


    function SpeedGraphContext() {
        this._set = false;
        this._time = null;
    }

    SpeedGraphContext.prototype.isSet = function() {
        return this._set;
    };

    SpeedGraphContext.prototype.getTime = function() {
        return this._time;
    };

    SpeedGraphContext.prototype.set = function(time) {
        this._set = true;
        this._time = time;
    };

    SpeedGraphContext.prototype.reset = function() {
        this._set = false;
    };


    return speed;
});
