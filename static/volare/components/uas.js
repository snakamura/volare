define([
    'lodash',
    'underscore.string',
    'angular',
    'text!./uas.css',
    'text!./uas.html',
    'text!./uasParams.html',
    'text!./uasDate.html',
    'text!./uasTime.html',
    'volare/util'
], function(_, _s, angular, css, template, paramsTemplate, dateTemplate, timeTemplate) {
    'use strict';

    var module = angular.module('volare.components.uas', [
        'volare.util'
    ]);

    module.directive('volareUas', ['util', 'dryAdiabat', 'mixingRatio', function(util, dryAdiabat, mixingRatio) {
        util.loadCssInline(css);

        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                station: '=',
                date: '=',
                range: '=?',
                params: '=?'
            },
            controller: 'UASChartController',
            link: function(scope, element, attrs) {
                var width = element.width();
                var height = element.height();

                var canvas = element.children('canvas')[0];
                canvas.width = width;
                canvas.height = height;

                function Range(width, height, range) {
                    _.extend(this, range);

                    var margin = {
                        top: 10,
                        left: 50,
                        bottom: 25,
                        right: 10
                    };
                    this._width = width - (margin.left + margin.right);
                    this._height = height - (margin.top + margin.bottom);
                    this._margin = margin;
                }

                Range.prototype.x = function(temperature) {
                    var min = this.temperature.min;
                    var max = this.temperature.max;
                    return (temperature - min)/(max - min)*this._width + this._margin.left;
                };

                Range.prototype.minX = function() {
                    return this._margin.left;
                };

                Range.prototype.maxX = function() {
                    return this._margin.left + this._width;
                };

                Range.prototype.y = function(pressure) {
                    var min = this.pressure.min;
                    var max = this.pressure.max;
                    return (Math.log(pressure) - Math.log(min))/(Math.log(max) - Math.log(min))*this._height + this._margin.top;
                };

                Range.prototype.minY = function() {
                    return this._margin.top + this._height;
                };

                Range.prototype.maxY = function() {
                    return this._margin.top;
                };


                function clip(context, range) {
                    context.beginPath();
                    context.rect(range.minX(), range.maxY(), range.maxX() - range.minX(), range.minY() - range.maxY());
                    context.clip();
                }

                function drawGrid(context, range) {
                    context.save();
                    context.lineWidth = 0.1;
                    context.strokeStyle = 'black';
                    context.fillStyle = 'black';
                    context.font = 'bold 12px sans-serif';

                    var minX = range.minX();
                    var maxX = range.maxX();
                    var minY = range.minY();
                    var maxY = range.maxY();

                    context.textAlign = 'end';
                    context.textBaseline = 'middle';
                    _.each(range.pressures, function(p) {
                        context.strokeStyle = p === range.pressure.min || p === range.pressure.max ? 'black' : 'gray';

                        var y = range.y(p);
                        context.beginPath();
                        context.moveTo(minX, y);
                        context.lineTo(maxX, y);
                        context.stroke();

                        if (p !== range.pressure.max)
                            context.fillText(p, minX - 5, y);
                    });

                    var stepTemperature = 5;
                    context.textAlign = 'center';
                    context.textBaseline = 'top';
                    _.each(_.range(range.temperature.min, range.temperature.max + 1, stepTemperature), function(t) {
                        var primary = t % (stepTemperature * 2) === 0;
                        context.strokeStyle = primary ? 'black' : 'gray';

                        var x = range.x(t);
                        context.beginPath();
                        context.moveTo(x, minY);
                        context.lineTo(x, maxY);
                        context.stroke();

                        if (primary)
                            context.fillText(t, x, minY + 5);
                    });

                    context.restore();
                }

                function drawHeight(context, range, pressure, height, color) {
                    context.save();
                    context.lineWidth = 2;
                    context.strokeStyle = color;
                    context.fillStyle = color;
                    context.textAlign = 'end';
                    context.textBaseline = 'bottom';

                    var y = range.y(pressure);
                    context.beginPath();
                    context.moveTo(range.minX(), y);
                    context.lineTo(range.maxX(), y);
                    context.stroke();

                    context.fillText(_s.numberFormat(height) + 'm', range.maxX() - 5, y - 2);

                    context.restore();
                }

                function drawDryAdiabat(context, range, temperature, bold) {
                    context.save();
                    clip(context, range);
                    context.lineWidth = bold ? 1 : 0.5;
                    context.strokeStyle = bold ? 'green' : 'lightgreen';

                    context.beginPath();
                    _.each(range.pressures, function(p) {
                        context.lineTo(range.x(dryAdiabat.temperature(temperature, p)), range.y(p));
                    });
                    context.stroke();

                    context.restore();
                }

                function drawDryAdiabats(context, range) {
                    var temperatures = [-60, -40, -20, 0, 20, 40, 60, 80, 100];
                    _.each(temperatures, function(t) {
                        drawDryAdiabat(context, range, t, false);
                    });
                }

                function drawMixingRatio(context, range, ratio, bold) {
                    context.save();
                    clip(context, range);
                    context.lineWidth = bold ? 1 : 0.5;
                    context.strokeStyle = bold ? 'purple' : 'violet';
                    context.fillStyle = bold ? 'purple' : 'violet';
                    context.textAlign = 'center';
                    context.textBaseline = 'bottom';

                    context.beginPath();
                    var x = range.x(mixingRatio.temperature(ratio, _.head(range.pressures)));
                    var y = range.y(_.head(range.pressures));
                    context.moveTo(x, y);
                    _.each(_.tail(range.pressures), function(p) {
                        context.lineTo(range.x(mixingRatio.temperature(ratio, p)), range.y(p));
                    });
                    context.stroke();

                    context.fillText(_s.sprintf('%.1f', ratio), x, y - 2);

                    context.restore();
                }

                function drawMixingRatios(context, range) {
                    var ratios = [0.1, 0.4, 1, 2, 4, 7, 10, 16, 24, 32];
                    _.each(ratios, function(r) {
                        drawMixingRatio(context, range, r, false);
                    });
                }

                function drawObservation(context, range, observation) {
                    if (!observation)
                        return;

                    var items = _.take(observation, function(item) {
                        return item.pressure >= range.pressure.min;
                    });
                    var surfaceItem = _.head(items);

                    context.save();
                    clip(context, range);

                    context.lineWidth = 2;

                    context.strokeStyle = 'red';
                    context.beginPath();
                    context.moveTo(range.x(surfaceItem.temperature), range.y(surfaceItem.pressure));
                    _.each(_.tail(items), function(item) {
                        if (item.temperature)
                            context.lineTo(range.x(item.temperature), range.y(item.pressure));
                    });
                    context.stroke();

                    context.strokeStyle = 'blue';
                    context.beginPath();
                    context.moveTo(range.x(surfaceItem.dewPoint), range.y(surfaceItem.pressure));
                    _.each(_.tail(items), function(item) {
                        if (item.dewPoint)
                            context.lineTo(range.x(item.dewPoint), range.y(item.pressure));
                    });
                    context.stroke();

                    context.restore();

                    context.save();
                    context.fillStyle = 'black';
                    context.textAlign = 'left';
                    context.textBaseline = 'middle';
                    _.each(items, function(item) {
                        if (item.height)
                            context.fillText(_s.numberFormat(item.height) + 'm', range.minX() + 3, range.y(item.pressure));
                    });

                    context.restore();

                    var surfaceDewPoint = (scope.params && scope.params.surfaceDewPoint) || surfaceItem.dewPoint;
                    var ratio = mixingRatio.ratio(surfaceDewPoint, surfaceItem.pressure);
                    drawMixingRatio(context, range, ratio, true);

                    var surfaceTemperature = (scope.params && scope.params.surfaceTemperature) || surfaceItem.temperature;
                    var temperatureAt1000 = dryAdiabat.temperatureAt1000(surfaceTemperature, surfaceItem.pressure);
                    drawDryAdiabat(context, range, temperatureAt1000, true);
                }

                function draw() {
                    var context = canvas.getContext('2d');
                    context.save();
                    context.clearRect(0, 0, width, height);

                    var range = new Range(width, height, scope.internalRange);
                    drawGrid(context, range);
                    drawMixingRatios(context, range);
                    drawDryAdiabats(context, range);
                    drawObservation(context, range, scope.observation);

                    if (scope.params && scope.params.lcl)
                        drawHeight(context, range, scope.params.lcl.pressure, scope.params.lcl.height, 'darkorange');
                    if (scope.params && scope.params.thermalTop)
                        drawHeight(context, range, scope.params.thermalTop.pressure, scope.params.thermalTop.height, 'firebrick');

                    context.restore();
                }

                scope.$watch('observation', draw);
                scope.$watch('params', draw, true);
                scope.$watch('internalRange', draw);
            }
        };
    }]);

    module.controller('UASChartController', ['$scope', '$http', 'dryAdiabat', 'mixingRatio', function($scope, $http, dryAdiabat, mixingRatio) {
        var station = $scope.station;
        var date = $scope.date;

        function getHeight(observation, pressure) {
            var items = _.filter(observation, 'height');
            var i = _.head(_.drop(_.take(_.zip(items, _.tail(items)), '1'), function(i) {
                return pressure < i[1].pressure || i[0].pressure < pressure;
            }));
            if (!i)
                return null;
            return Math.round(i[0].height + (i[1].height - i[0].height)*(i[0].pressure - pressure)/(i[0].pressure - i[1].pressure));
        }

        function lcl(surfaceTemperature, surfaceDewPoint, surfacePressure, pressures) {
            var temperatureAt1000 = dryAdiabat.temperatureAt1000(surfaceTemperature, surfacePressure);
            var ratio = mixingRatio.ratio(surfaceDewPoint, surfacePressure);
            return _.head(_.drop(_.map(_.take(_.zip(pressures, _.tail(pressures)), '1'), function(p) {
                var p1 = p[0];
                var p2 = p[1];
                var da1 = dryAdiabat.temperature(temperatureAt1000, p1);
                var da2 = dryAdiabat.temperature(temperatureAt1000, p2);
                var mr1 = mixingRatio.temperature(ratio, p1);
                var mr2 = mixingRatio.temperature(ratio, p2);
                var temperature = ((mr1 - mr2) * da1 - (da1 - da2) * mr1) / ((mr1 - mr2) - (da1 - da2));
                var pressure = (p1 - p2) / (da1 - da2) * (temperature - da1) + p1;
                return p2 <= pressure && pressure <= p1 ? pressure : null;
            }), _.isNull));
        }

        function thermalTop(surfaceTemperature, surfacePressure, observation) {
            var temperatureAt1000 = dryAdiabat.temperatureAt1000(surfaceTemperature, surfacePressure);
            return _.head(_.drop(_.map(_.take(_.zip(observation, _.tail(observation)), '1'), function(items) {
                var p1 = items[0].pressure;
                var p2 = items[1].pressure;
                var t1 = items[0].temperature;
                var t2 = items[1].temperature;
                var da1 = dryAdiabat.temperature(temperatureAt1000, p1);
                var da2 = dryAdiabat.temperature(temperatureAt1000, p2);
                var temperature = ((t1 - t2) * da1 - (da1 - da2) * t1) / ((t1 - t2) - (da1 - da2));
                var pressure = (p1 - p2) / (da1 - da2) * (temperature - da1) + p1;
                return p2 <= pressure && pressure <= p1 ? pressure : null;
            }), _.isNull));
        }

        function updateRange() {
            var range = $scope.range || {};
            var minPressure = (range.pressure && range.pressure.min) || 100;
            var maxPressure = (range.pressure && range.pressure.max) || 1050;
            $scope.internalRange = {
                temperature: {
                    min: (range.temperature && range.temperature.min) || -90,
                    max: (range.temperature && range.temperature.max) || 50
                },
                pressure: {
                    min: minPressure,
                    max: maxPressure
                },
                pressures: _.take([maxPressure, 1000, 925, 850, 700, 500, 400, 300, 250, 200, 150, 100], function(p) {
                    return p >= minPressure;
                })
            };
        }
        $scope.$watch('range', updateRange);
        updateRange();

        $scope.$watch('params', function() {
            var observation = $scope.observation;
            if (observation) {
                var surfaceItem = _.head(observation);
                var surfaceTemperature = ($scope.params && $scope.params.surfaceTemperature) || surfaceItem.temperature;
                var surfaceDewPoint = ($scope.params && $scope.params.surfaceDewPoint) || surfaceItem.dewPoint;
                var lclPressure = lcl(surfaceTemperature, surfaceDewPoint, surfaceItem.pressure, $scope.internalRange.pressures);
                $scope.params.lcl = lclPressure ? {
                    pressure: lclPressure,
                    height: getHeight(observation, lclPressure)
                } : null;

                var thermalTopPressure = thermalTop(surfaceTemperature, surfaceItem.pressure, observation);
                $scope.params.thermalTop = thermalTopPressure ? {
                    pressure: thermalTopPressure,
                    height: getHeight(observation, thermalTopPressure)
                } : null;
            }
        }, true);

        var path = _s.sprintf('/uas/%s/%04d/%02d/%02d/00', station.id,
            date.getUTCFullYear(), date.getUTCMonth() + 1, date.getUTCDate());
        $http.get(path).success(function(observation) {
            $scope.observation = observation;
            $scope.params = {
                surfaceTemperature: _.head(observation).temperature,
                surfaceDewPoint: _.head(observation).dewPoint
            };
        });
    }]);

    module.factory('dryAdiabat', function() {
        return {
            k: 273.15,
            r: 0.28571,
            temperature: function(temperatureAt1000, pressure) {
                return (temperatureAt1000 + this.k) * Math.pow(pressure / 1000, this.r) - this.k;
            },
            temperatureAt1000: function(temperature, pressure) {
                return (temperature + this.k) * Math.pow(1000 / pressure, this.r) - this.k;
            }
        };
    });

    module.factory('mixingRatio', function() {
        return {
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
    });

    module.directive('volareUasParams', [function() {
        return {
            restrict: 'E',
            replace: true,
            template: paramsTemplate,
            scope: {
                range: '=',
                params: '='
            },
            controller: 'UasParamsController',
            link: function(scope, element, attrs) {
            }
        };
    }]);

    module.controller('UasParamsController', ['$scope', function($scope) {
        $scope.pressure = '700';
        function updateRange() {
            if ($scope.pressure === '700') {
                $scope.range = {
                    pressure: {
                        min: _.parseInt($scope.pressure)
                    },
                    temperature: {
                        min: -20
                    }
                };
            }
            else {
                $scope.range = {
                };
            }
        }
        $scope.$watch('pressure', updateRange);
    }]);

    module.directive('volareUasDate', [function() {
        return {
            restrict: 'E',
            replace: true,
            template: dateTemplate,
            scope: {
                date: '='
            },
            controller: 'UasDateController',
            link: function(scope, element, attrs) {
            }
        };
    }]);

    module.controller('UasDateController', ['$scope', function($scope) {
        $scope.options = {
            showWeeks: false,
            showButtonBar: false
        };
        $scope.toggle = function($event) {
            $event.preventDefault();
            $event.stopPropagation();
            $scope.opened = !$scope.opened;
        };
    }]);

    module.directive('volareUasTime', [function() {
        return {
            restrict: 'E',
            replace: true,
            template: timeTemplate,
            scope: {
                time: '='
            },
            controller: 'UasTimeController',
            link: function(scope, element, attrs) {
            }
        };
    }]);

    module.controller('UasTimeController', ['$scope', function($scope) {
        $scope.toggle = function($event) {
            $event.preventDefault();
            $event.stopPropagation();
            $scope.opened = !$scope.opened;
        };
        $scope.select = function(time) {
            $scope.time = time;
        }
    }]);

    return module;
});
