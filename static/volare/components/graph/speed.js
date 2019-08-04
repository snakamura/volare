define([
    'angular',
    'volare/components/graph',
    'volare/util'
], function(angular) {
    'use strict';

    var module = angular.module('volare.components.graph.speed', [
        'volare.components.graph',
        'volare.util'
    ]);

    module.factory('SpeedGraphController', ['GraphController', 'util', function(GraphController, util) {
        function SpeedGraphController(scope, flights, name) {
            GraphController.call(this, scope, flights);

            this._name = name;
        }
        util.inherit(SpeedGraphController, GraphController);

        SpeedGraphController.prototype.getStrokes = function(currentTime, withContext, partial) {
            var flights = this.getFlights();

            var self = this;
            var strokes = [];
            flights.eachFlight(function(flight) {
                if (flight.isVisible()) {
                    var graphContext = null;
                    if (withContext) {
                        graphContext = flight.getExtra(self._name);
                        if (!graphContext) {
                            graphContext = new SpeedGraphContext();
                            flight.setExtra(self._name, graphContext);
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
                            value: self.getValue(flight, startTime)
                        });
                        for (var time = startTime.getTime() + step; time < endTime.getTime(); time += step) {
                            var t = new Date(time);
                            stroke.points.push({
                                time: t,
                                value: self.getValue(flight, t)
                            });
                        }
                        if (graphContext)
                            graphContext.set(new Date(time - step));
                    }
                    strokes.push(stroke);
                }
            });
            return strokes;
        };

        SpeedGraphController.prototype.getValue = function(flight, time) {
            throw 'This method must be overridden.';
        };

        return SpeedGraphController;
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


    return module;
});
