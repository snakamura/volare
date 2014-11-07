define([
    'lodash',
    'underscore.string',
    'angular',
    'text!./altitude.html',
    'volare/components/graph',
    'volare/util'
], function(_, _s, angular, template) {
    'use strict';

    var module = angular.module('volare.components.graph.altitude', [
        'volare.components.graph',
        'volare.util'
    ]);

    module.controller('AltitudeGraphController', ['$scope', 'GraphController', 'util', function($scope, GraphController, util) {
        function AltitudeGraphController(scope) {
            GraphController.call(this, scope, scope.flights);
        }
        util.inherit(AltitudeGraphController, GraphController);

        AltitudeGraphController.prototype.getRange = function() {
            var flights = this.getFlights();

            var min = 0;
            var max = flights.getMaxAltitude();
            var steps = _.map(_.range(min, max + 1, 100), function(value) {
                return {
                    value: value,
                    label: value % 200 === 0 ? _s.numberFormat(value) + 'm' : '',
                    primary: value % 1000 === 0
                };
            });
            return {
                min: min,
                max: max,
                steps: steps
            };
        };

        AltitudeGraphController.prototype.getStrokes = function(currentTime, withContext, partial) {
            var flights = this.getFlights();

            var strokes = [];
            flights.eachFlight(function(flight) {
                if (flight.isVisible()) {
                    var graphContext = null;
                    if (withContext) {
                        graphContext = flight.getExtra('currentAltitudeGraphContext');
                        if (!graphContext) {
                            graphContext = new AltitudeGraphContext();
                            flight.setExtra('currentAltitudeGraphContext', graphContext);
                        }
                        if (!partial)
                            graphContext.reset();
                    }
                    var stroke = {
                        color: flight.getColor(),
                        points: []
                    };
                    var startIndex = 0;
                    var startTime = null;
                    var startAltitude = 0;
                    if (graphContext && graphContext.isSet()) {
                        startIndex = graphContext.getIndex();
                        startTime = graphContext.getTime();
                        startAltitude = graphContext.getAltitude();
                    }
                    else {
                        startTime = flight.getStartTime();
                        startAltitude = flight.getRecord(0).altitude;
                    }
                    stroke.points.push({
                        time: startTime,
                        value: startAltitude
                    });
                    var lastTime = startTime;
                    var lastAltitude = startAltitude;
                    var n = startIndex;
                    for (; n < flight.getRecordCount(); ++n) {
                        var record = flight.getRecord(n);
                        var time = record.time;
                        if (currentTime && time > currentTime)
                            break;
                        stroke.points.push({
                            time: time,
                            value: record.altitude
                        });
                        lastTime = time;
                        lastAltitude = record.altitude;
                    }
                    if (currentTime && flight.getStartTime() <= currentTime) {
                        stroke.points.push({
                            time: currentTime,
                            value: lastAltitude
                        });
                    }
                    if (graphContext)
                        graphContext.set(lastAltitude, n > 0 ? n - 1 : n, lastTime);
                    strokes.push(stroke);
                }
            });
            return strokes;
        };

        return new AltitudeGraphController($scope);
    }]);

    module.directive('volareAltitudeGraph', ['$controller', function($controller) {
        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                flights: '='
            },
            controller: 'AltitudeGraphController'
        };
    }]);


    function AltitudeGraphContext() {
        this._set = false;
        this._altitude = 0;
        this._index = 0;
        this._time = null;
    }

    AltitudeGraphContext.prototype.isSet = function() {
        return this._set;
    };

    AltitudeGraphContext.prototype.getAltitude = function() {
        return this._altitude;
    };

    AltitudeGraphContext.prototype.getIndex = function() {
        return this._index;
    };

    AltitudeGraphContext.prototype.getTime = function() {
        return this._time;
    };

    AltitudeGraphContext.prototype.set = function(altitude, index, time) {
        this._set = true;
        this._altitude = altitude;
        this._index = index;
        this._time = time;
    };

    AltitudeGraphContext.prototype.reset = function() {
        this._set = false;
    };


    return module;
});
