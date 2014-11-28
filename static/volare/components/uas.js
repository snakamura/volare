define([
    'lodash',
    'underscore.string',
    'angular',
    'text!./uas.html'
], function(_, _s, angular, uasTemplate) {
    'use strict';

    var module = angular.module('volare.components.uas', []);

    module.directive('volareUas', [function() {
        return {
            restrict: 'E',
            replace: true,
            template: uasTemplate,
            scope: {
                station: '=',
                date: '='
            },
            controller: 'UASChartController',
            link: function(scope, element, attrs) {
                var width = element.width();
                var height = element.height();

                var margin = {
                    top: 10,
                    left: 50,
                    bottom: 25,
                    right: 10
                };

                var canvas = element.children('canvas')[0];
                canvas.width = width;
                canvas.height = height;

                var minTemperature = -90;
                var maxTemperature = 50;
                var minPressure = 100;
                var maxPressure = 1050;
                var pressures = _.take([maxPressure, 1000, 925, 850, 700, 500, 400, 300, 250, 200, 150, 100], function(p) {
                    return p >= minPressure;
                });

                function getX(temperature) {
                    return (temperature - minTemperature)/(maxTemperature - minTemperature)*(width - (margin.left + margin.right)) + margin.left;
                }

                function getY(pressure) {
                    return (Math.log(pressure) - Math.log(minPressure))/(Math.log(maxPressure) - Math.log(minPressure))*(height - (margin.top + margin.bottom)) + margin.top;
                }

                function clip(context) {
                    context.rect(margin.left, margin.top, width - (margin.left + margin.right), height - (margin.top + margin.bottom));
                    context.clip();
                }

                function drawGrid(context) {
                    context.save();
                    context.lineWidth = 0.1;
                    context.strokeStyle = 'black';
                    context.fillStyle = 'black';
                    context.font = 'bold 12px sans-serif';

                    var minX = getX(minTemperature);
                    var maxX = getX(maxTemperature);
                    var minY = getY(maxPressure);
                    var maxY = getY(minPressure);

                    context.textAlign = 'end';
                    context.textBaseline = 'middle';
                    _.each(pressures, function(p) {
                        context.strokeStyle = p === minPressure || p === maxPressure ? 'black' : 'gray';

                        var y = getY(p);
                        context.beginPath();
                        context.moveTo(minX, y);
                        context.lineTo(maxX, y);
                        context.stroke();

                        if (p !== maxPressure)
                            context.fillText(p, minX - 5, y);
                    });

                    var stepTemperature = 5;
                    context.textAlign = 'center';
                    context.textBaseline = 'top';
                    _.each(_.range(minTemperature, maxTemperature + 1, stepTemperature), function(t) {
                        var primary = t % (stepTemperature * 2) === 0;
                        context.strokeStyle = primary ? 'black' : 'gray';

                        var x = getX(t);
                        context.beginPath();
                        context.moveTo(x, minY);
                        context.lineTo(x, maxY);
                        context.stroke();

                        if (primary)
                            context.fillText(t, x, minY + 5);
                    });

                    context.restore();
                }

                var mixingRatio = {
                    t0: 273.16,
                    rv: 461.7,
                    lv: 2500000,
                    e0: 611.73,
                    ep: 0.622,
                    k: 273.15,
                    temperature: function(ratio, pressure) {
                        return 1 / (1 / this.t0 - this.rv / this.lv * Math.log(ratio / 1000 * pressure * 100 / (this.e0 * (this.ep + ratio / 1000)))) - this.k;
                    },
                    ratio: function(temperature, pressure) {
                        var v = Math.exp((((temperature + this.k) / this.t0) - 1) / ((temperature + this.k) * (this.rv / this.lv)));
                        return 10 * v * this.e0 * this.ep / (pressure - v * this.e0 / 100);
                    }
                };

                function drawMixingRatio(context, ratio, bold) {
                    context.save();
                    clip(context);
                    context.lineWidth = bold ? 1 : 0.5;
                    context.strokeStyle = bold ? 'purple' : 'violet';
                    context.fillStyle = bold ? 'purple' : 'violet';
                    context.textAlign = 'center';
                    context.textBaseline = 'bottom';

                    context.beginPath();
                    var x = getX(mixingRatio.temperature(ratio, _.head(pressures)));
                    var y = getY(_.head(pressures));
                    context.moveTo(x, y);
                    _.each(_.tail(pressures), function(p) {
                        context.lineTo(getX(mixingRatio.temperature(ratio, p)), getY(p));
                    });
                    context.stroke();

                    context.fillText(_s.sprintf('%.1f', ratio), x, y - 2);

                    context.restore();
                }

                function drawMixingRatios(context) {
                    var ratios = [0.1, 0.4, 1, 2, 4, 7, 10, 16, 24, 32];
                    _.each(ratios, function(r) {
                        drawMixingRatio(context, r, false);
                    });
                }

                var dryAdiabat = {
                    k: 273.15,
                    r: 0.28571,
                    temperature: function(temperatureAt1000, pressure) {
                        return (temperatureAt1000 + this.k) * Math.pow(pressure / 1000, this.r) - this.k;
                    },
                    temperatureAt1000: function(temperature, pressure) {
                        return (temperature + this.k) * Math.pow(1000 / pressure, this.r) - this.k;
                    }
                };

                function drawDryAdiabat(context, temperature, bold) {
                    context.save();
                    clip(context);
                    context.lineWidth = bold ? 1 : 0.5;
                    context.strokeStyle = bold ? 'green' : 'lightgreen';

                    context.beginPath();
                    _.each(pressures, function(p) {
                        context.lineTo(getX(dryAdiabat.temperature(temperature, p)), getY(p));
                    });
                    context.stroke();

                    context.restore();
                }

                function drawDryAdiabats(context) {
                    var temperatures = [-60, -40, -20, 0, 20, 40, 60, 80, 100];
                    _.each(temperatures, function(t) {
                        drawDryAdiabat(context, t, false);
                    });
                }

                function drawObservation(context, observation) {
                    if (!observation)
                        return;

                    var items = _.take(observation, function(items) {
                        return items.pressure >= minPressure;
                    });
                    var surfaceItem = _.head(items);

                    context.save();
                    clip(context);

                    context.lineWidth = 2;

                    context.strokeStyle = 'red';
                    context.beginPath();
                    context.moveTo(getX(surfaceItem.temperature), getY(surfaceItem.pressure));
                    _.each(_.tail(items), function(item) {
                        if (item.temperature)
                            context.lineTo(getX(item.temperature), getY(item.pressure));
                    });
                    context.stroke();

                    context.strokeStyle = 'blue';
                    context.beginPath();
                    context.moveTo(getX(surfaceItem.dewPoint), getY(surfaceItem.pressure));
                    _.each(_.tail(items), function(item) {
                        if (item.dewPoint)
                            context.lineTo(getX(item.dewPoint), getY(item.pressure));
                    });
                    context.stroke();

                    context.restore();

                    context.save();
                    context.fillStyle = 'black';
                    context.textAlign = 'left';
                    context.textBaseline = 'middle';
                    _.each(items, function(item) {
                        if (item.height)
                            context.fillText(_s.numberFormat(item.height) + 'm', getX(minTemperature) + 3, getY(item.pressure));
                    });

                    context.restore();

                    var ratio = mixingRatio.ratio(surfaceItem.dewPoint, surfaceItem.pressure);
                    drawMixingRatio(context, ratio, true);

                    var temperatureAt1000 = dryAdiabat.temperatureAt1000(surfaceItem.temperature, surfaceItem.pressure);
                    drawDryAdiabat(context, temperatureAt1000, true);
                }

                scope.$watch('observation', function(observation) {
                    var context = canvas.getContext('2d');
                    context.save();
                    context.clearRect(0, 0, width, height);

                    drawGrid(context);
                    drawMixingRatios(context);
                    drawDryAdiabats(context);
                    drawObservation(context, observation);

                    context.restore();
                });
            }
        };
    }]);

    module.controller('UASChartController', ['$scope', '$http', function($scope, $http) {
        var station = $scope.station;
        var date = $scope.date;

        var path = _s.sprintf('/uas/observation/%s/%04d/%02d/%02d/00', station.id,
            date.getUTCFullYear(), date.getUTCMonth() + 1, date.getUTCDate());
        $http.get(path).success(function(observation) {
            $scope.observation = observation;
        });
    }]);

    return module;
});
