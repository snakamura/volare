define([
    'angular',
    'volare/components/graph'
], function(angular) {
    'use strict';

    var module = angular.module('volare.components.graph.speed', [
        'volare.components.graph'
    ]);

    module.controller('SpeedGraphController', ['$scope', '$controller', function($scope, $controller) {
        function SpeedGraphController(scope) {
        }

        SpeedGraphController.prototype = $controller('GraphController', {
            $scope: $scope
        });

        SpeedGraphController.prototype.init = function(flights, name) {
            this.constructor.prototype.constructor.prototype.init.call(this, flights);
            this._name = name;
        };

        SpeedGraphController.prototype.getStrokes = function(currentTime, withContext, partial) {
            var flights = this.getFlights();

            var strokes = [];
            flights.eachFlight(function(flight) {
                if (flight.isVisible()) {
                    var graphContext = null;
                    if (withContext) {
                        graphContext = flight.getExtra(this._name);
                        if (!graphContext) {
                            graphContext = new SpeedGraphContext();
                            flight.setExtra(this._name, graphContext);
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
                            value: this.getValue(flight, startTime)
                        });
                        for (var time = startTime.getTime() + step; time < endTime.getTime(); time += step) {
                            var t = new Date(time);
                            stroke.points.push({
                                time: t,
                                value: this.getValue(flight, t)
                            });
                        }
                        if (graphContext)
                            graphContext.set(new Date(time - step));
                    }
                    strokes.push(stroke);
                }
            }, this);
            return strokes;
        };

        SpeedGraphController.prototype.getValue = function(flight, time) {
            throw 'This method must be overridden.';
        };

        return new SpeedGraphController($scope);
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
