define([
    'require',
    'lodash',
    'underscore.string',
    'jquery',
    'angular',
    'google',
    'text!./index.css',
    'text!./time.html',
    'jquery-ui',
    'volare/util',
    'volare/util/layout'
], function(require, _, _s, $, angular, google, css, timeTemplate) {
    'use strict';

    var module = angular.module('volare.forcast', [
        'volare.util',
        'volare.util.layout'
    ]);

    module.controller('ForcastController', ['$scope', '$http', 'util', function($scope, $http, util) {
        util.loadCssInline(css);

        $scope.bounds = null;
        $scope.hour = 0;
        $scope.time = null;

        function update() {
            var bounds = $scope.bounds;
            if (!bounds) {
                return;
            }

            var path = _s.sprintf('/msm/surface/latest/%d', $scope.hour);
            $http.get(path, {
                params: {
                    nwlat: bounds.getNorthEast().lat(),
                    nwlng: bounds.getSouthWest().lng(),
                    selat: bounds.getSouthWest().lat(),
                    selng: bounds.getNorthEast().lng()
                }
            }).success(function(data) {
                var timestamp = data.timestamp;
                $scope.time = new Date(Date.UTC(timestamp.year, timestamp.month - 1, timestamp.day, timestamp.hour + $scope.hour));
                $scope.items = data.items;
            });
        }

        $scope.$watch('bounds', update);
        $scope.$watch('hour', update);
    }]);

    module.directive('volareForcastTime', [function() {
        return {
            restrict: 'E',
            replace: true,
            template: timeTemplate,
            scope: {
                time: '=',
                hour: '='
            },
            link: function(scope, element, attrs) {
                var min = 0;
                var max = 33;

                var slider = element.find('.slider');
                slider.slider({
                    range: 'min',
                    min: min,
                    max: max
                }).on('slide', function(event, ui) {
                    scope.hour = ui.value;
                    scope.$apply();
                });

                scope.forward = function(fast) {
                    var step = fast ? 3 : 1;
                    scope.hour = Math.min(Math.floor((scope.hour + step) / step) * step, max);
                };
                scope.backward = function(fast) {
                    var step = fast ? 3 : 1;
                    scope.hour = Math.max(Math.ceil((scope.hour - step) / step) * step, min);
                };

                scope.$watch('hour', function() {
                    slider.slider('value', scope.hour);
                });
            }
        };
    }]);

    module.directive('volareForcast', ['util', function(util) {
        function MSMOverlay() {
            this._div = null;
            this._items = {};
        }
        util.inherit(MSMOverlay, google.maps.OverlayView);

        MSMOverlay.prototype.onAdd = function() {
            var div = $('<div class="weatherOverlay"></div>');

            var panes = this.getPanes();
            panes.overlayLayer.appendChild(div[0]);

            this._div = div;
        };

        MSMOverlay.prototype.onRemove = function() {
            this._div.remove();
            this._div = null;
        };

        MSMOverlay.prototype.draw = function() {
            var LatLng = google.maps.LatLng;

            var map = this.getMap();
            var bounds = map.getBounds();
            if (!bounds)
                return;

            var projection = this.getProjection();
            var sw = projection.fromLatLngToDivPixel(bounds.getSouthWest());
            var ne = projection.fromLatLngToDivPixel(bounds.getNorthEast());

            var div = this._div;
            div.css('left', sw.x + 'px');
            div.css('top', ne.y + 'px');
            div.css('width', (ne.x - sw.x) + 'px');
            div.css('height', (sw.y - ne.y) + 'px');

            if (!_.isEmpty(this._items)) {
                var self = this;
                _.each(this._items, function(item) {
                    var nw = projection.fromLatLngToDivPixel(new LatLng(item.latitude + self._getLatitudeStep()/2, item.longitude - self._getLongitudeStep()/2));
                    var se = projection.fromLatLngToDivPixel(new LatLng(item.latitude - self._getLatitudeStep()/2, item.longitude + self._getLongitudeStep()/2));
                    var width = se.x - nw.x;
                    var height = se.y - nw.y;

                    var elem = item.elem;
                    if (!elem) {
                        elem = $('<div class="item"><div class="cell"><img class="wind"><br><span class="temperature"></span></div></div>');
                        div.append(elem);

                        var windSpeed = Math.sqrt(Math.pow(item.northwardWind, 2) + Math.pow(item.eastwardWind, 2));
                        var windAngle = Math.atan2(item.northwardWind, item.eastwardWind);
                        var windIconIndex = MSMOverlay.windIconIndex(windSpeed);
                        var windImage = elem.find('.wind');
                        windImage[0].src = require.toUrl('../components/image/weather/wind/' + windIconIndex + '.png');
                        windImage.css('transform', 'rotate(' + (-windAngle*180/Math.PI) + 'deg)');

                        var temperatureDiv = elem.find('.temperature');
                        temperatureDiv.css('background-color', MSMOverlay.colorForTemperature(item.airTemperature, 0.5));
                        temperatureDiv.text(Math.round(item.airTemperature*10)/10);

                        elem.css('background-color', 'rgba(255, 255, 255, ' + item.cloudAmount/100*0.9 + ')');

                        item.elem = elem;
                    }

                    elem.css('left', nw.x + 'px');
                    elem.css('top', nw.y + 'px');
                    elem.css('width', width + 'px');
                    elem.css('height', height + 'px');

                    var cell = elem.find('.cell');
                    cell.css('width', width + 'px');
                    cell.css('height', height + 'px');
                });
            }
        };

        MSMOverlay.prototype.setItems = function(items, time) {
            var self = this;

            var oldItems = self._items;
            var newItems = {};
            _.each(items, function(item) {
                var key = self._getItemKey(item, time);
                var oldItem = oldItems[key];
                if (!_.isUndefined(oldItem)) {
                    item.elem = oldItem.elem;
                    delete oldItems[key];
                }
                newItems[key] = item;
            });
            _.each(oldItems, function(item) {
                var elem = item.elem;
                if (elem)
                    elem.remove();
            });
            self._items = newItems;

            this.draw();
        };

        MSMOverlay.windIconIndex = function(windSpeed) {
            return windSpeed <= 2 ? 1 : windSpeed <= 3 ? 2 : windSpeed <= 4 ? 3 : windSpeed <= 5 ? 4 : 5;
        };

        MSMOverlay.colorForTemperature = function(temperature, alpha) {
            if (temperature < 0)
                return 'rgba(0, 0, 255, ' + alpha + ')';
            else if (temperature < 5)
                return 'rgba(204, 204, 204, ' + alpha + ')';
            else if (temperature < 10)
                return 'rgba(0, 255, 255, ' + alpha + ')';
            else if (temperature < 15)
                return 'rgba(0, 204, 255, ' + alpha + ')';
            else if (temperature < 20)
                return 'rgba(51, 204, 0, ' + alpha + ')';
            else if (temperature < 25)
                return 'rgba(255, 255, 0, ' + alpha + ')';
            else if (temperature < 30)
                return 'rgba(255, 153, 51, ' + alpha + ')';
            else if (temperature < 35)
                return 'rgba(255, 0, 0, ' + alpha + ')';
            else
                return 'rgba(204, 0, 0, ' + alpha + ')';
        };

        MSMOverlay.prototype._getItemKey = function(item, time) {
            return time.toISOString() + ' ' + item.latitude + ' ' + item.longitude;
        };

        MSMOverlay.prototype._getLatitudeStep = function() {
            return MSMOverlay.SURFACE_LATITUDE_STEP;
        };

        MSMOverlay.prototype._getLongitudeStep = function() {
            return MSMOverlay.SURFACE_LONGITUDE_STEP;
        };

        MSMOverlay.SURFACE_LATITUDE_STEP = 0.05;
        MSMOverlay.SURFACE_LONGITUDE_STEP = 0.0625;

        return {
            restrict: 'E',
            replace: true,
            template: '<div class="map"></div>',
            scope: {
                items: '=',
                time: '=',
                bounds: '='
            },
            link: function(scope, element, attrs) {
                var map = new google.maps.Map(element[0], {
                    mapTypeId: google.maps.MapTypeId.HYBRID,
                    center: new google.maps.LatLng(35.685124, 139.752787),
                    zoom: 10
                });

                var msmOverlay = new MSMOverlay();
                msmOverlay.setMap(map);

                map.addListener('idle', function() {
                    scope.bounds = map.getBounds();
                    scope.$apply();
                });

                scope.$watch('items', function(items) {
                    msmOverlay.setItems(items, scope.time);
                });
            }
        };
    }]);

    return module;
});
