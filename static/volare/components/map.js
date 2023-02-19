/* jshint -W003 */
define([
    'require',
    'lodash',
    'underscore.string',
    'jquery',
    'angular',
    'google',
    'markerwithlabel',
    'text!./map.css',
    'text!./uasModal.html',
    'angular-ui-bootstrap',
    'volare/components/uas',
    'volare/util'
], function(require, _, _s, $, angular, google, markerWithLabel, css, uasModalTemplate) {
    'use strict';

    var module = angular.module('volare.components.map', [
        'ui.bootstrap',
        'volare.components.uas',
        'volare.util'
    ]);

    module.directive('volareMap', ['Map', 'util', function(Map, util) {
        util.loadCssInline(css);

        return {
            restrict: 'E',
            replace: true,
            template: '<div class="map"></div>',
            scope: {
                flights: '=',
                map: '='
            },
            controller: 'MapController',
            link: function(scope, element, attrs) {
                var map = new Map(scope.flights, element);

                function bind(name, get, set) {
                    if (_.isUndefined(scope.map[name]))
                        scope.map[name] = get();
                    else
                        set(scope.map[name]);
                    scope.$watch('map.' + name, set);
                }

                bind('trackType', function() {
                    return map.getTrackType();
                }, function(trackType) {
                    map.setTrackType(trackType);
                });
                bind('weatherFlags', function() {
                    return map.getWeatherFlags();
                }, function(weatherFlags) {
                    map.setWeatherFlags(weatherFlags, Map.WeatherFlag.ALL);
                });
                bind('waypoint', function() {
                    return map.getWaypoint();
                }, function(waypoint) {
                    map.setWaypoint(waypoint);
                });
                bind('route', function() {
                    return map.getRoute();
                }, function(route) {
                    map.setRoute(route);
                });
            }
        };
    }]);

    module.controller('MapController', ['$scope', function($scope) {
        // TODO
        // Unbound Map from flights and use this controller
    }]);

    module.factory('Map', ['$http', '$uibModal', 'util', function($http, $uibModal, util) {
        var LatLng = google.maps.LatLng;
        var LatLngBounds = google.maps.LatLngBounds;

        function Map(flights, element) {
            this._flights = flights;
            this._map = new google.maps.Map(element[0], {
                mapTypeId: google.maps.MapTypeId.HYBRID
            });
            this._weatherFlags = 0;
            this._weatherOverlays = {};
            this._weatherOverlays[Map.WeatherFlag.MSM.SURFACE.ALL] = new MSMSurfaceOverlay();
            this._weatherOverlays[Map.WeatherFlag.MSM.BAROMETRIC.ALL] = new MSMBarometricOverlay();
            this._weatherOverlays[Map.WeatherFlag.AMEDAS.ALL] = new AMEDASOverlay();
            this._weatherOverlays[Map.WeatherFlag.WINDAS.ALL] = new WINDASOverlay();
            this._weatherOverlays[Map.WeatherFlag.UAS.ALL] = new UASOverlay();
            this._trackType = Map.TrackType.SOLID;
            this._waypoint = null;
            this._waypointMakers = [];
            this._route = null;
            this._routeOverlays = [];

            var self = this;
            var visibleChangedListener = function(event) {
                var flight = event.target;
                self.updateTrack(flight, false);
            };

            $(this._flights).on('flight_added', function(event, flight) {
                self._map.fitBounds(self._flights.getBounds());

                var track = self._createTrack(flight);
                flight.setExtra('track', track);
                self.updateTrack(flight, false);

                self._updateCurrentTime();

                $(flight).on('visible_changed', visibleChangedListener);
            });
            $(this._flights).on('flight_removed', function(event, flight) {
                $(flight).off('visible_changed', visibleChangedListener);

                var track = flight.getExtra('track');
                track.clear();
                flight.setExtra('track', null);

                self._updateCurrentTime();
            });
            $(this._flights).on('properties_changed', function() {
                self._flights.eachFlight(function(flight) {
                    self.updateTrack(flight, false);
                });
            });
            $(this._flights).on('currentTime_changed', function(event, time) {
                self._flights.eachFlight(function(flight) {
                    self.updateTrack(flight, true);
                });

                var primaryFlight = self._flights.getPrimaryFlight();
                if (primaryFlight) {
                    var position = primaryFlight.getPositionAt(time);
                    var span = self._map.getBounds().toSpan();
                    var bounds = new LatLngBounds(new LatLng(position.latitude - span.lat()/10, position.longitude - span.lng()/10),
                                                  new LatLng(position.latitude + span.lat()/10, position.longitude + span.lng()/10));
                    self._map.panToBounds(bounds);
                }

                self._updateCurrentTime();
            });
        }

        Map.prototype.getWeatherFlags = function() {
            return this._weatherFlags;
        };

        Map.prototype.setWeatherFlags = function(flags, mask) {
            this._weatherFlags = (this._weatherFlags & ~mask) | (flags & mask);

            var self = this;
            _.each(this._weatherOverlays, function(weatherOverlay, mask) {
                weatherOverlay.setFlags(self._map, self._weatherFlags & mask);
            });

            $(this).trigger('weatherFlags_changed', this._weatherFlags);
        };

        Map.prototype.getTrackType = function() {
            return this._trackType;
        };

        Map.prototype.setTrackType = function(trackType) {
            this._trackType = trackType;

            var self = this;
            this._flights.eachFlight(function(flight) {
                var oldTrack = flight.getExtra('track');
                oldTrack.clear();

                var newTrack = self._createTrack(flight);
                flight.setExtra('track', newTrack);
                self.updateTrack(flight, false);
            });

            $(this).trigger('trackType_changed', this._trackType);

        };

        Map.prototype.getWaypoint = function() {
            return this._waypoint;
        };

        Map.prototype.setWaypoint = function(waypoint) {
            _.each(this._waypointMakers, function(marker) {
                marker.setMap(null);
            });

            var markers = [];
            if (waypoint) {
                var self = this;
                _.each(waypoint.getItems(), function(item) {
                    var marker = new markerWithLabel.MarkerWithLabel({
                        map: self._map,
                        position: item.getPosition(),
                        title: item.getName(),
                        labelContent: item.getLabel(),
                        labelAnchor: new google.maps.Point(-15, 35),
                        labelClass: 'label'
                    });
                    markers.push(marker);
                });
            }
            this._waypointMakers = markers;

            this._waypoint = waypoint;

            $(this).trigger('waypoint_changed', this._waypoint);
        };

        Map.prototype.getRoute = function() {
            return this._route;
        };

        Map.prototype.setRoute = function(route) {
            _.each(this._routeOverlays, function(overlay) {
                overlay.setMap(null);
            });

            var overlays = [];
            if (route) {
                var self = this;
                var path = [];
                _.each(route.getItems(), function(routeItem) {
                    var waypointItem = routeItem.getWaypointItem();
                    var position = waypointItem.getPosition();
                    var marker = new markerWithLabel.MarkerWithLabel({
                        map: self._map,
                        position: position,
                        title: waypointItem.getName(),
                        labelContent: waypointItem.getLabel(),
                        labelAnchor: new google.maps.Point(-15, 35),
                        labelClass: 'label'
                    });
                    overlays.push(marker);

                    var circle = new google.maps.Circle({
                        map: self._map,
                        center: position,
                        clickable: false,
                        radius: routeItem.getRadius(),
                        strokeColor: '#ff3300',
                        strokeWeight: 2,
                        fillOpacity: 0.01
                    });
                    overlays.push(circle);

                    path.push(position);
                });

                var polyline = new google.maps.Polyline({
                    map: this._map,
                    path: path,
                    strokeColor: '#ff3300',
                    strokeWeight: 2
                });
                overlays.push(polyline);
            }
            this._routeOverlays = overlays;

            this._route = route;

            $(this).trigger('route_changed', route);
        };

        Map.prototype._updateCurrentTime = function() {
            var time = this._flights.getCurrentTime() || this._flights.getStartTime();
            _.each(this._weatherOverlays, function(weatherOverlay) {
                weatherOverlay.setCurrentTime(time);
            });
        };

        Map.prototype._createTrack = function(flight) {
            switch (this._trackType) {
            case Map.TrackType.SOLID:
                return new SolidColorTrack(this._map, flight.getColor());
            case Map.TrackType.ALTITUDE:
                return new GradientColorAltitudeTrack(this._map, this._flights);
            case Map.TrackType.GROUND_SPEED:
                return new GradientColorGroundSpeedTrack(this._map, this._flights, flight);
            case Map.TrackType.VERTICAL_SPEED:
                return new GradientColorVerticalSpeedTrack(this._map, this._flights, flight);
            default:
                throw 'Never happens.';
            }
        };

        Map.prototype.updateTrack = function(flight, currentOnly) {
            var track = flight.getExtra('track');
            if (!track)
                return;

            var records = [];
            var currentRecords = [];

            if (flight.isVisible()) {
                records = flight.getRecords();

                var currentTime = this._flights.getCurrentTime();
                if (currentTime) {
                    var startTime = new Date(currentTime.getTime() - Map.TRACK_DURATION*1000);
                    var endTime = currentTime;
                    currentRecords = flight.getRecordsRange(startTime, endTime);
                }
                else {
                    currentRecords = records;
                }
            }

            if (!currentOnly)
                track.setRecords(records);
            track.setCurrentRecords(currentRecords);
        };


        Map.TrackType = {
            SOLID: 0,
            ALTITUDE: 1,
            GROUND_SPEED: 2,
            VERTICAL_SPEED: 3
        };
        Map.TRACK_DURATION = 10*60;

        Map.WeatherFlag = {
            MSM: {
                SURFACE: {
                    WIND: 0x01,
                    TEMPERATURE: 0x02,
                    CLOUD_AMOUNT: 0x04,
                    ALL: 0x00
                },
                BAROMETRIC: {
                    ALL: 0x00
                }
            },
            AMEDAS: {
                WIND: 0x00100000,
                TEMPERATURE: 0x00200000,
                SUNSHINE: 0x00400000,
                ALL: 0x00000000
            },
            WINDAS: {
                ALL: 0x00000000
            },
            UAS: {
                ALL: 0x80000000
            }
        };
        Map.WeatherFlag.MSM.SURFACE.ALL = Map.WeatherFlag.MSM.SURFACE.WIND |
                                          Map.WeatherFlag.MSM.SURFACE.TEMPERATURE |
                                          Map.WeatherFlag.MSM.SURFACE.CLOUD_AMOUNT;
        (function() {
            var flag = 0x10;
            _.each([1000, 975, 950, 925, 900, 850, 800, 700], function(airPressure) {
                var wind = flag;
                flag <<= 1;
                var temperature = flag;
                flag <<= 1;
                Map.WeatherFlag.MSM.BAROMETRIC[airPressure] = {
                    WIND: wind,
                    TEMPERATURE: temperature,
                    ALL: wind | temperature
                };
                Map.WeatherFlag.MSM.BAROMETRIC.ALL |= wind | temperature;
            });
        })();
        Map.WeatherFlag.AMEDAS.ALL = Map.WeatherFlag.AMEDAS.WIND |
                                     Map.WeatherFlag.AMEDAS.TEMPERATURE |
                                     Map.WeatherFlag.AMEDAS.SUNSHINE;
        (function() {
            var flag = 0x01000000;
            _.each([500, 1000, 1500, 2000, 2500, 3000], function(altitude) {
                var value = flag;
                flag <<= 1;
                Map.WeatherFlag.WINDAS[altitude] = value;
                Map.WeatherFlag.WINDAS.ALL |= value;
            });
        })();
        Map.WeatherFlag.ALL = Map.WeatherFlag.MSM.SURFACE.ALL |
                              Map.WeatherFlag.MSM.BAROMETRIC.ALL |
                              Map.WeatherFlag.AMEDAS.ALL |
                              Map.WeatherFlag.WINDAS.ALL |
                              Map.WeatherFlag.UAS.ALL;


        function Track() {
        }

        Track.prototype.clear = function() {
            throw 'This method must be overridden.';
        };

        Track.prototype.setRecords = function(records) {
            throw 'This method must be overridden.';
        };

        Track.prototype.setCurrentRecords = function(records) {
            throw 'This method must be overridden.';
        };

        Track.OPACITY_CURRENT = 1;
        Track.OPACITY_ALL = 0.3;


        function SolidColorTrack(map, color) {
            Track.call(this);

            this._polyline = new google.maps.Polyline({
                map: map,
                strokeColor: color,
                strokeOpacity: Track.OPACITY_ALL
            });
            this._currentPolyline = new google.maps.Polyline({
                map: map,
                strokeColor: color
            });
        }
        util.inherit(SolidColorTrack, Track);

        SolidColorTrack.prototype.clear = function() {
            this._polyline.setMap(null);
            this._currentPolyline.setMap(null);
        };

        SolidColorTrack.prototype.setRecords = function(records) {
            this._setRecords(this._polyline.getPath(), records);
        };

        SolidColorTrack.prototype.setCurrentRecords = function(records) {
            this._setRecords(this._currentPolyline.getPath(), records);
        };

        SolidColorTrack.prototype._setRecords = function(path, records) {
            path.clear();
            _.each(records, function(record) {
                path.push(new LatLng(record.latitude, record.longitude));
            });
        };


        function GradientColorTrack(map, flights) {
            Track.call(this);

            this._map = map;
            this._flights = flights;
            this._polylines = [];
            this._currentPolylines = [];
        }
        util.inherit(GradientColorTrack, Track);

        GradientColorTrack.prototype.clear = function() {
            _.each(this._polylines, function(polyline) {
                polyline.setMap(null);
            });
            _.each(this._currentPolylines, function(polyline) {
                polyline.setMap(null);
            });
        };

        GradientColorTrack.prototype.setRecords = function(records) {
            this._polylines = this._setRecords(this._polylines, Track.OPACITY_ALL, records);
        };

        GradientColorTrack.prototype.setCurrentRecords = function(records) {
            this._currentPolylines = this._setRecords(this._currentPolylines, Track.OPACITY_CURRENT, records);
        };

        GradientColorTrack.prototype.getMax = function(flights) {
            throw 'This method must be overridden.';
        };

        GradientColorTrack.prototype.getMin = function(flights) {
            throw 'This method must be overridden.';
        };

        GradientColorTrack.prototype.getValue = function(record) {
            throw 'This method must be overridden.';
        };

        GradientColorTrack.prototype._setRecords = function(polylines, opacity, records) {
            _.each(polylines, function(polyline) {
                polyline.setMap(null);
                polylines = [];
            });

            var self = this;
            var max = this.getMax(this._flights);
            var min = this.getMin(this._flights);
            var step = (max - min)/(GradientColorTrack.COLORS.length - 1);
            var previousColorIndex = null;
            var previousPolyline = null;
            _.each(records, function(record) {
                if (previousPolyline) {
                    previousPolyline.getPath().push(new LatLng(record.latitude, record.longitude));
                }

                function clip(index) {
                    return Math.min(Math.max(index, 0), GradientColorTrack.COLORS.length - 1);
                }

                var colorIndex = clip(Math.round((self.getValue(record) - min)/step));
                var polyline = previousPolyline;
                if (!previousPolyline || previousColorIndex !== colorIndex) {
                    polyline = new google.maps.Polyline({
                        map:self._map,
                        strokeColor: GradientColorTrack.COLORS[colorIndex],
                        strokeOpacity: opacity
                    });
                    polylines.push(polyline);
                    previousColorIndex = colorIndex;
                    previousPolyline = polyline;
                }
                polyline.getPath().push(new LatLng(record.latitude, record.longitude));
            });

            return polylines;
        };

        GradientColorTrack.COLORS = [
            '#FF0000',
            '#FF3500',
            '#FF6B00',
            '#FFA100',
            '#FFD600',
            '#F1FF00',
            '#BBFF00',
            '#86FF00',
            '#50FF00',
            '#1AFF00',
            '#00FF1A',
            '#00FF50',
            '#00FF86',
            '#00FFBB',
            '#00FFF1',
            '#00D6FF',
            '#00A1FF',
            '#006BFF',
            '#0035FF',
            '#0000FF'
        ];


        function GradientColorAltitudeTrack(map, flights) {
            GradientColorTrack.call(this, map, flights);
        }
        util.inherit(GradientColorAltitudeTrack, GradientColorTrack);

        GradientColorAltitudeTrack.prototype.getMax = function(flights) {
            return flights.getMaxAltitude();
        };

        GradientColorAltitudeTrack.prototype.getMin = function(flights) {
            return flights.getMinAltitude();
        };

        GradientColorAltitudeTrack.prototype.getValue = function(record) {
            return record.altitude;
        };


        function GradientColorGroundSpeedTrack(map, flights, flight) {
            GradientColorTrack.call(this, map, flights);

            this._flight = flight;
        }
        util.inherit(GradientColorGroundSpeedTrack, GradientColorTrack);

        GradientColorGroundSpeedTrack.prototype.getMax = function(flights) {
            return 80;
        };

        GradientColorGroundSpeedTrack.prototype.getMin = function(flights) {
            return 30;
        };

        GradientColorGroundSpeedTrack.prototype.getValue = function(record) {
            return this._flight.getGroundSpeedAt(record.time)*3600/1000;
        };


        function GradientColorVerticalSpeedTrack(map, flights, flight) {
            GradientColorTrack.call(this, map, flights);

            this._flight = flight;
        }
        util.inherit(GradientColorVerticalSpeedTrack, GradientColorTrack);

        GradientColorVerticalSpeedTrack.prototype.getMax = function(flights) {
            return 3;
        };

        GradientColorVerticalSpeedTrack.prototype.getMin = function(flights) {
            return -3;
        };

        GradientColorVerticalSpeedTrack.prototype.getValue = function(record) {
            return this._flight.getVerticalSpeedAt(record.time);
        };


        function WeatherOverlay() {
            this._currentTime = null;
            this._$div = null;
            this._idleListener = null;
        }
        util.inherit(WeatherOverlay, google.maps.OverlayView);

        WeatherOverlay.prototype.getCurrentTime = function() {
            return this._currentTime;
        };

        WeatherOverlay.prototype.setCurrentTime = function(time) {
            this._currentTime = time;

            if (this.getMap())
                this._update();
        };

        WeatherOverlay.prototype.onAdd = function() {
            var $div = $('<div class="weatherOverlay ' + this._getClassName() + '"></div>');

            var panes = this.getPanes();
            panes.overlayLayer.appendChild($div[0]);

            this._$div = $div;

            var map = this.getMap();
            var self = this;
            this._idleListener = map.addListener('idle', function() {
                self._update();
            });

            this._update();
        };

        WeatherOverlay.prototype.onRemove = function() {
            google.maps.event.removeListener(this._idleListener);
            this._idleListener = null;

            this._$div.remove();
            this._$div = null;

            this._clear();
        };

        WeatherOverlay.prototype._getClassName = function() {
            throw 'This method must be overridden.';
        };

        WeatherOverlay.prototype._clear = function() {
            throw 'This method must be overridden.';
        };

        WeatherOverlay.prototype._draw = function() {
            throw 'This method must be overridden.';
        };

        WeatherOverlay.prototype._update = function() {
            throw 'This method must be overridden.';
        };

        WeatherOverlay.prototype.draw = function() {
            this._draw();
        };

        WeatherOverlay.hoursEquals = function(time1, time2) {
            return time1.getUTCFullYear() === time2.getUTCFullYear() &&
                time1.getUTCMonth() === time2.getUTCMonth() &&
                time1.getUTCDate() === time2.getUTCDate() &&
                time1.getUTCHours() === time2.getUTCHours();
        };

        WeatherOverlay.tenMinutesEquals = function(time1, time2) {
            return WeatherOverlay.hoursEquals(time1, time2) &&
                Math.floor(time1.getUTCMinutes() / 10) === Math.floor(time2.getUTCMinutes() / 10);
        };

        WeatherOverlay.windIconIndex = function(windSpeed) {
            return windSpeed <= 2 ? 1 : windSpeed <= 3 ? 2 : windSpeed <= 4 ? 3 : windSpeed <= 5 ? 4 : 5;
        };

        WeatherOverlay.colorForTemperature = function(temperature, alpha) {
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


        function MSMOverlay() {
            WeatherOverlay.call(this);

            this._clear();
        }
        util.inherit(MSMOverlay, WeatherOverlay);

        MSMOverlay.prototype.setFlags = function(map, flags) {
            this._setFlags(flags);
            this.setMap(flags ? map : null);
        };

        MSMOverlay.prototype._getClassName = function() {
            return 'msm';
        };

        MSMOverlay.prototype._clear = function() {
            this._time = null;
            this._bounds = null;
            this._items = {};
        };

        MSMOverlay.prototype._draw = function() {
            var map = this.getMap();
            var bounds = map.getBounds();
            var projection = this.getProjection();
            var sw = projection.fromLatLngToDivPixel(bounds.getSouthWest());
            var ne = projection.fromLatLngToDivPixel(bounds.getNorthEast());

            var $div = this._$div;
            $div.css('left', sw.x + 'px');
            $div.css('top', ne.y + 'px');
            $div.css('width', (ne.x - sw.x) + 'px');
            $div.css('height', (sw.y - ne.y) + 'px');

            if (!_.isEmpty(this._items)) {
                var self = this;
                _.each(this._items, function(item) {
                    if (!self._isItemVisible(item)) {
                        return;
                    }

                    var nw = projection.fromLatLngToDivPixel(new LatLng(item.latitude + self._getLatitudeStep()/2, item.longitude - self._getLongitudeStep()/2));
                    var se = projection.fromLatLngToDivPixel(new LatLng(item.latitude - self._getLatitudeStep()/2, item.longitude + self._getLongitudeStep()/2));
                    var width = se.x - nw.x;
                    var height = se.y - nw.y;

                    var $elem = item.$elem;
                    if (!$elem) {
                        $elem = $('<div class="item"><div class="cell"><img class="wind"><br><span class="temperature"></span></div></div>');
                        $div.append($elem);

                        var windSpeed = Math.sqrt(Math.pow(item.northwardWind, 2) + Math.pow(item.eastwardWind, 2));
                        var windAngle = Math.atan2(item.northwardWind, item.eastwardWind);
                        var windIconIndex = WeatherOverlay.windIconIndex(windSpeed);
                        var $windImage = $elem.find('.wind');
                        $windImage[0].src = require.toUrl('./image/weather/wind/' + windIconIndex + '.png');
                        $windImage.css('transform', 'rotate(' + (-windAngle*180/Math.PI) + 'deg)');
                        $windImage.css('visibility', self._isWindVisible(item) ? 'visible' : 'hidden');

                        var $temperatureDiv = $elem.find('.temperature');
                        $temperatureDiv.css('background-color', WeatherOverlay.colorForTemperature(item.airTemperature, 0.5));
                        $temperatureDiv.text(Math.round(item.airTemperature*10)/10);
                        $temperatureDiv.css('visibility', self._isTemperatureVisible(item) ? 'visible' : 'hidden');

                        $elem.css('background-color', 'rgba(255, 255, 255, ' + (self._isCloudAmountVisible(item) ? item.cloudAmount/100*0.9 : 0) + ')');

                        item.$elem = $elem;
                    }

                    $elem.css('left', nw.x + 'px');
                    $elem.css('top', nw.y + 'px');
                    $elem.css('width', width + 'px');
                    $elem.css('height', height + 'px');

                    var $cell = $elem.find('.cell');
                    $cell.css('width', width + 'px');
                    $cell.css('height', height + 'px');
                });
            }
        };

        MSMOverlay.prototype._update = function() {
            var time = this.getCurrentTime();
            if (time) {
                var map = this.getMap();
                var bounds = map.getBounds();
                if (!this._bounds || !this._bounds.equals(bounds) ||
                    !this._time || !WeatherOverlay.hoursEquals(this._time, time)) {
                    var self = this;
                    var path = _s.sprintf('/msm/%s/%d/%d/%d/%d', this._getName(), time.getUTCFullYear(),
                        time.getUTCMonth() + 1, time.getUTCDate(), time.getUTCHours());
                    $http.get(path, {
                        params: {
                            nwlat: bounds.getNorthEast().lat(),
                            nwlng: bounds.getSouthWest().lng(),
                            selat: bounds.getSouthWest().lat(),
                            selng: bounds.getNorthEast().lng()
                        }
                    }).success(function(data) {
                        var items = data.items;
                        if (self._time && WeatherOverlay.hoursEquals(self._time, time)) {
                            var oldItems = self._items;
                            var newItems = {};
                            _.each(items, function(item) {
                                var key = self._getItemKey(item);
                                var oldItem = oldItems[key];
                                if (!_.isUndefined(oldItem)) {
                                    item.$elem = oldItem.$elem;
                                    delete oldItems[key];
                                }
                                newItems[key] = item;
                            });
                            _.each(oldItems, function(item) {
                                var $elem = item.$elem;
                                if ($elem)
                                    $elem.remove();
                            });
                            self._items = newItems;
                        }
                        else {
                            self._items = {};
                            _.each(items, function(item) {
                                var key = self._getItemKey(item);
                                self._items[key] = item;
                            });
                            self._$div.empty();
                        }
                        self._time = time;
                        self._bounds = bounds;
                        self._draw();
                    });
                }
            }
            else {
                this._items = {};
                this._time = null;
                this._bounds = null;
                this._$div.empty();
                this._draw();
            }
        };

        MSMOverlay.prototype._getName = function() {
            throw 'This method must be overridden.';
        };

        MSMOverlay.prototype._getItemKey = function(item) {
            throw 'This method must be overridden.';
        };

        MSMOverlay.prototype._setFlags = function(flags) {
            throw 'This method must be overridden.';
        };

        MSMOverlay.prototype._isItemVisible = function(item) {
            throw 'This method must be overridden.';
        };

        MSMOverlay.prototype._isWindVisible = function(item) {
            throw 'This method must be overridden.';
        };

        MSMOverlay.prototype._isTemperatureVisible = function(item) {
            throw 'This method must be overridden.';
        };

        MSMOverlay.prototype._isCloudAmountVisible = function(item) {
            throw 'This method must be overridden.';
        };

        MSMOverlay.prototype._getLatitudeStep = function() {
            throw 'This method must be overridden.';
        };

        MSMOverlay.prototype._getLongitudeStep = function() {
            throw 'This method must be overridden.';
        };


        function MSMSurfaceOverlay() {
            MSMOverlay.call(this);

            this._flags = Map.WeatherFlag.MSM.SURFACE.ALL;
        }
        util.inherit(MSMSurfaceOverlay, MSMOverlay);

        MSMSurfaceOverlay.prototype._getName = function() {
            return 'surface';
        };

        MSMSurfaceOverlay.prototype._getItemKey = function(item) {
            return item.latitude + ' ' + item.longitude;
        };

        MSMSurfaceOverlay.prototype._setFlags = function(flags) {
            this._flags = flags;
        };

        MSMSurfaceOverlay.prototype._isItemVisible = function(item) {
            return true;
        };

        MSMSurfaceOverlay.prototype._isWindVisible = function(item) {
            return (this._flags & Map.WeatherFlag.MSM.SURFACE.WIND) !== 0;
        };

        MSMSurfaceOverlay.prototype._isTemperatureVisible = function(item) {
            return (this._flags & Map.WeatherFlag.MSM.SURFACE.TEMPERATURE) !== 0;
        };

        MSMSurfaceOverlay.prototype._isCloudAmountVisible = function(item) {
            return (this._flags & Map.WeatherFlag.MSM.SURFACE.CLOUD_AMOUNT) !== 0;
        };

        MSMSurfaceOverlay.prototype._getLatitudeStep = function() {
            return MSMSurfaceOverlay.SURFACE_LATITUDE_STEP;
        };

        MSMSurfaceOverlay.prototype._getLongitudeStep = function() {
            return MSMSurfaceOverlay.SURFACE_LONGITUDE_STEP;
        };

        MSMSurfaceOverlay.SURFACE_LATITUDE_STEP = 0.05;
        MSMSurfaceOverlay.SURFACE_LONGITUDE_STEP = 0.0625;


        function MSMBarometricOverlay() {
            MSMOverlay.call(this);

            this._flags = Map.WeatherFlag.MSM.BAROMETRIC.ALL;
        }
        util.inherit(MSMBarometricOverlay, MSMOverlay);

        MSMBarometricOverlay.prototype._getName = function() {
            return 'barometric';
        };

        MSMBarometricOverlay.prototype._getItemKey = function(item) {
            return item.latitude + ' ' + item.longitude + ' ' + item.airPressure;
        };

        MSMBarometricOverlay.prototype._setFlags = function(flags) {
            this._flags = flags;
        };

        MSMBarometricOverlay.prototype._isItemVisible = function(item) {
            var flags = Map.WeatherFlag.MSM.BAROMETRIC[item.airPressure];
            return flags && this._flags & flags.ALL;
        };

        MSMBarometricOverlay.prototype._isWindVisible = function(item) {
            var flags = Map.WeatherFlag.MSM.BAROMETRIC[item.airPressure];
            return flags && this._flags & flags.WIND;
        };

        MSMBarometricOverlay.prototype._isTemperatureVisible = function(item) {
            var flags = Map.WeatherFlag.MSM.BAROMETRIC[item.airPressure];
            return flags && this._flags & flags.TEMPERATURE;
        };

        MSMBarometricOverlay.prototype._isCloudAmountVisible = function(item) {
            return false;
        };

        MSMBarometricOverlay.prototype._getLatitudeStep = function() {
            return MSMBarometricOverlay.BAROMETRIC_LATITUDE_STEP;
        };

        MSMBarometricOverlay.prototype._getLongitudeStep = function() {
            return MSMBarometricOverlay.BAROMETRIC_LONGITUDE_STEP;
        };

        MSMBarometricOverlay.BAROMETRIC_LATITUDE_STEP = 0.1;
        MSMBarometricOverlay.BAROMETRIC_LONGITUDE_STEP = 0.125;


        function AMEDASOverlay() {
            WeatherOverlay.call(this);

            this._flags = Map.WeatherFlag.AMEDAS.ALL;
            this._clear();
        }
        util.inherit(AMEDASOverlay, WeatherOverlay);

        AMEDASOverlay.prototype.setFlags = function(map, flags) {
            this._flags = flags;
            this.setMap(flags ? map : null);
        };

        AMEDASOverlay.prototype._getClassName = function() {
            return 'amedas';
        };

        AMEDASOverlay.prototype._clear = function() {
            this._time = null;
            this._bounds = null;
            this._items = {};
        };

        AMEDASOverlay.prototype._draw = function() {
            var map = this.getMap();
            var bounds = map.getBounds();
            var projection = this.getProjection();
            var sw = projection.fromLatLngToDivPixel(bounds.getSouthWest());
            var ne = projection.fromLatLngToDivPixel(bounds.getNorthEast());

            var $div = this._$div;
            $div.css('left', sw.x + 'px');
            $div.css('top', ne.y + 'px');
            $div.css('width', (ne.x - sw.x) + 'px');
            $div.css('height', (sw.y - ne.y) + 'px');

            if (!_.isEmpty(this._items)) {
                var self = this;
                var time = this.getCurrentTime();
                var amedasTime = new Date(time.getTime() + 10*60*1000);
                var minute = Math.floor((amedasTime.getUTCHours()*60 + amedasTime.getUTCMinutes())/10)*10;
                _.each(this._items, function(item) {
                    var $elem = item.$elem;

                    if (item.time !== minute)
                        return;

                    if ($elem) {
                        if (!$elem.parent())
                            $div.append($elem);
                    }
                    else {
                        $elem = $('<div class="item"><div class="cell"><img class="wind"><br><span class="temperature"></span></div></div>');
                        $div.append($elem);

                        var windIconIndex = WeatherOverlay.windIconIndex(item.windSpeed);
                        var $windImage = $elem.find('.wind');
                        var windAngle = AMEDASOverlay.windAngle(item.windDirection);
                        if (!_.isNull(windAngle)) {
                            $windImage[0].src = require.toUrl('./image/weather/wind/' + windIconIndex + '.png');
                            $windImage.css('transform', 'rotate(' + (-windAngle) + 'deg)');
                            $windImage.css('visibility', self._flags & Map.WeatherFlag.AMEDAS.WIND ? 'visible' : 'hidden');
                        }
                        else {
                            $windImage.css('display', 'none');
                        }

                        var $temperatureDiv = $elem.find('.temperature');
                        if (!_.isNull(item.temperature)) {
                            $temperatureDiv.css('background-color', WeatherOverlay.colorForTemperature(item.temperature, 1.0));
                            $temperatureDiv.text(Math.round(item.temperature*10)/10);
                            $temperatureDiv.css('visibility', self._flags & Map.WeatherFlag.AMEDAS.TEMPERATURE ? 'visible' : 'hidden');
                        }
                        else {
                            $temperatureDiv.css('display', 'none');
                        }

                        if (!_.isNull(item.sunshine)) {
                            $elem.css('background-color', self._flags & Map.WeatherFlag.AMEDAS.SUNSHINE ? AMEDASOverlay.colorForSunshine(item.sunshine) : 'rgba(255, 255, 255, 0)');
                        }

                        item.$elem = $elem;
                    }

                    var pos = projection.fromLatLngToDivPixel(new LatLng(item.latitude, item.longitude));
                    $elem.css('left', (pos.x - 14) + 'px');
                    $elem.css('top', (pos.y - 10) + 'px');
                });
            }
        };

        AMEDASOverlay.prototype._update = function() {
            var time = this.getCurrentTime();
            if (time) {
                var map = this.getMap();
                var bounds = map.getBounds();
                if (!this._bounds || !this._bounds.equals(bounds) ||
                    !this._time || !WeatherOverlay.hoursEquals(this._time, time)) {
                    var self = this;
                    var path = _s.sprintf('/amedas/%d/%d/%d/%d', time.getUTCFullYear(),
                        time.getUTCMonth() + 1, time.getUTCDate(), time.getUTCHours());
                    $http.get(path, {
                        params: {
                            nwlat: bounds.getNorthEast().lat(),
                            nwlng: bounds.getSouthWest().lng(),
                            selat: bounds.getSouthWest().lat(),
                            selng: bounds.getNorthEast().lng()
                        }
                    }).success(function(items) {
                        if (self._time && WeatherOverlay.hoursEquals(self._time, time)) {
                            var oldItems = self._items;
                            var newItems = {};
                            _.each(items, function(item) {
                                var key = item.latitude + ' ' + item.longitude + ' ' + item.time;
                                var oldItem = oldItems[key];
                                if (!_.isUndefined(oldItem)) {
                                    item.$elem = oldItem.$elem;
                                    delete oldItems[key];
                                }
                                newItems[key] = item;
                            });
                            _.each(oldItems, function(item) {
                                var $elem = item.$elem;
                                if ($elem)
                                    $elem.remove();
                            });
                            self._items = newItems;
                        }
                        else {
                            self._items = {};
                            _.each(items, function(item) {
                                var key = item.latitude + ' ' + item.longitude + ' ' + item.time;
                                self._items[key] = item;
                            });
                            self._$div.empty();
                        }
                        self._time = time;
                        self._bounds = bounds;
                        self._draw();
                    });
                }
                else if (!WeatherOverlay.tenMinutesEquals(this._time, time)) {
                    _.each(this._items, function(item) {
                        item.$elem = null;
                    });
                    this._time = time;
                    this._$div.empty();
                    this._draw();
                }
            }
            else {
                this._items = {};
                this._time = null;
                this._bounds = null;
                this._$div.empty();
                this._draw();
            }
        };

        AMEDASOverlay.windAngle = function(windDirection) {
            switch (windDirection) {
            case 'N':
                return -90;
            case 'NNE':
                return -112.5;
            case 'NE':
                return -135;
            case 'ENE':
                return -157.5;
            case 'E':
                return 180;
            case 'ESE':
                return 157.5;
            case 'SE':
                return 135;
            case 'SSE':
                return 112.5;
            case 'S':
                return 90;
            case 'SSW':
                return 67.5;
            case 'SW':
                return 45;
            case 'WSW':
                return 22.5;
            case 'W':
                return 0;
            case 'WNW':
                return -22.5;
            case 'NW':
                return -45;
            case 'NNW':
                return -67.5;
            case 'CALM':
                return null;
            default:
                return null;
            }
        };

        AMEDASOverlay.colorForSunshine = function(sunshine) {
            if (sunshine === 0)
                return 'rgba(204, 204, 204, 0.5)';
            else if (sunshine < 5)
                return 'rgba(51, 204, 0, 0.5)';
            else if (sunshine < 10)
                return 'rgba(255, 255, 0, 0.5)';
            else
                return 'rgba(255, 153, 51, 0.5)';
        };


        function WINDASOverlay() {
            WeatherOverlay.call(this);

            this._flags = Map.WeatherFlag.WINDAS.ALL;
            this._clear();
        }
        util.inherit(WINDASOverlay, WeatherOverlay);

        WINDASOverlay.prototype.setFlags = function(map, flags) {
            this._flags = flags;
            this.setMap(flags ? map : null);
        };

        WINDASOverlay.prototype._getClassName = function() {
            return 'windas';
        };

        WINDASOverlay.prototype._clear = function() {
            this._time = null;
            this._bounds = null;
            this._stations = [];
        };

        WINDASOverlay.prototype._draw = function() {
            var map = this.getMap();
            var bounds = map.getBounds();
            var projection = this.getProjection();
            var sw = projection.fromLatLngToDivPixel(bounds.getSouthWest());
            var ne = projection.fromLatLngToDivPixel(bounds.getNorthEast());

            var $div = this._$div;
            $div.empty();
            $div.css('left', sw.x + 'px');
            $div.css('top', ne.y + 'px');
            $div.css('width', (ne.x - sw.x) + 'px');
            $div.css('height', (sw.y - ne.y) + 'px');

            if (!_.isEmpty(this._stations)) {
                var self = this;
                var time = this.getCurrentTime();
                var windasTime = new Date(time.getTime() + 10*60*1000);
                var hour = windasTime.getUTCHours();
                var minute = Math.floor(windasTime.getUTCMinutes()/10)*10;
                _.each(this._stations, function(station) {
                    var pos = projection.fromLatLngToDivPixel(new LatLng(station.station.latitude, station.station.longitude));
                    _.each(station.observations, function(observation) {
                        if (observation.hour === hour && observation.minute === minute) {
                            _.each(self._filterItems(station.station, observation.items), function(item) {
                                var $elem = $('<div class="item"><div class="cell"><img class="wind"></div></div>');
                                $elem.css('left', (pos.x - 14) + 'px');
                                $elem.css('top', (pos.y - 10) + 'px');

                                var windSpeed = Math.sqrt(Math.pow(item.northwardWind, 2) + Math.pow(item.eastwardWind, 2));
                                var windAngle = Math.atan2(item.northwardWind, item.eastwardWind);
                                var windIconIndex = WeatherOverlay.windIconIndex(windSpeed);
                                var $windImage = $elem.find('.wind');
                                $windImage[0].src = require.toUrl('./image/weather/wind/' + windIconIndex + '.png');
                                $windImage.css('transform', 'rotate(' + (-windAngle*180/Math.PI) + 'deg)');

                                $div.append($elem);
                            });
                        }
                    });
                });
            }
        };

        WINDASOverlay.prototype._update = function() {
            var time = this.getCurrentTime();
            if (time) {
                var map = this.getMap();
                var bounds = map.getBounds();
                if (!this._bounds || !this._bounds.equals(bounds) ||
                    !this._time || !WeatherOverlay.hoursEquals(this._time, time)) {
                    var self = this;
                    var path = _s.sprintf('/windas/%d/%d/%d/%d', time.getUTCFullYear(),
                        time.getUTCMonth() + 1, time.getUTCDate(), time.getUTCHours());
                    $http.get(path, {
                        params: {
                            nwlat: bounds.getNorthEast().lat(),
                            nwlng: bounds.getSouthWest().lng(),
                            selat: bounds.getSouthWest().lat(),
                            selng: bounds.getNorthEast().lng()
                        }
                    }).success(function(stations) {
                        self._stations = stations;
                        self._time = time;
                        self._bounds = bounds;
                        self._draw();
                    });
                }
                else if (!WeatherOverlay.tenMinutesEquals(this._time, time)) {
                    this._time = time;
                    this._draw();
                }
            }
            else {
                this._stations = [];
                this._time = null;
                this._bounds = null;
                this._draw();
            }
        };

        WINDASOverlay.prototype._filterItems = function(station, items) {
            var self = this;

            var margin = 200;
            var altitudes = [500, 1000, 1500, 2000, 2500, 3000];
            var enabledAltitudes = _.filter(altitudes, function(a) {
                return (self._flags & Map.WeatherFlag.WINDAS[a]) !== 0;
            });
            return _.filter(_.map(enabledAltitudes, function(altitude) {
                var matchedItems = _.filter(items, function(item) {
                    var a = item.altitude + station.elevation;
                    return altitude - margin <= a && a < altitude + margin;
                });
                if (_.isEmpty(matchedItems)) {
                    return null;
                }
                else {
                    return _.first(_.sortBy(matchedItems, function(item) {
                        return Math.abs(item.altitude - altitude);
                    }));
                }
            }), function(item) {
                return item !== null;
            });
        };


        function UASOverlay() {
            WeatherOverlay.call(this);

            this._flags = Map.WeatherFlag.UAS.ALL;
            this._clear();
            this._$mouseTarget = null;
        }
        util.inherit(UASOverlay, WeatherOverlay);

        UASOverlay.prototype.onAdd = function() {
            UASOverlay.super_.onAdd.call(this);

            var self = this;
            var $mouseTarget = $('<div></div>');
            $mouseTarget.on('click', function(event) {
                _.each(self._$div.find('div.item'), function(elem) {
                    var offset = $(elem).offset();
                    var width = $(elem).width();
                    var height = $(elem).height();
                    if (offset.left <= event.pageX && event.pageX <= offset.left + width &&
                        offset.top <= event.pageY && event.pageY <= offset.top + height) {
                        $(elem).trigger(event);
                    }
                });
            });
            var panes = this.getPanes();
            panes.overlayMouseTarget.appendChild($mouseTarget[0]);

            this._$mouseTarget = $mouseTarget;
        };

        UASOverlay.prototype.onRemove = function() {
            this._$mouseTarget.remove();
            this._$mouseTarget = null;

            UASOverlay.super_.onRemove.call(this);
        };

        UASOverlay.prototype.setFlags = function(map, flags) {
            this._flags = flags;
            this.setMap(flags ? map : null);
        };

        UASOverlay.prototype._getClassName = function() {
            return 'uas';
        };

        UASOverlay.prototype._clear = function() {
            this._bounds = null;
            this._stations = [];
        };

        UASOverlay.prototype._draw = function() {
            var map = this.getMap();
            var bounds = map.getBounds();
            var projection = this.getProjection();
            var sw = projection.fromLatLngToDivPixel(bounds.getSouthWest());
            var ne = projection.fromLatLngToDivPixel(bounds.getNorthEast());

            var $div = this._$div;
            $div.empty();
            $div.css('left', sw.x + 'px');
            $div.css('top', ne.y + 'px');
            $div.css('width', (ne.x - sw.x) + 'px');
            $div.css('height', (sw.y - ne.y) + 'px');

            if (!_.isEmpty(this._stations)) {
                var self = this;
                _.each(this._stations, function(station) {
                    var pos = projection.fromLatLngToDivPixel(new LatLng(station.latitude, station.longitude));
                    var $elem = $('<div class="item"><div class="cell"></div></div>');
                    $elem.css('left', (pos.x - 14) + 'px');
                    $elem.css('top', (pos.y - 10) + 'px');
                    $elem.css('background', 'url(' + require.toUrl('./image/weather/uas.png') + ')');

                    $elem.on('click', function() {
                        self._open(station);
                    });

                    $div.append($elem);
                });
            }

            var $mouseTarget = this._$mouseTarget;
            $mouseTarget.css('left', sw.x + 'px');
            $mouseTarget.css('top', ne.y + 'px');
            $mouseTarget.css('width', (ne.x - sw.x) + 'px');
            $mouseTarget.css('height', (sw.y - ne.y) + 'px');
        };

        UASOverlay.prototype._update = function() {
            var map = this.getMap();
            var bounds = map.getBounds();
            if (!this._bounds || !this._bounds.equals(bounds)) {
                var self = this;
                $http.get('/uas', {
                    params: {
                        nwlat: bounds.getNorthEast().lat(),
                        nwlng: bounds.getSouthWest().lng(),
                        selat: bounds.getSouthWest().lat(),
                        selng: bounds.getNorthEast().lng()
                    }
                }).success(function(stations) {
                    self._stations = stations;
                    self._bounds = bounds;
                    self._draw();
                });
            }
        };

        UASOverlay.prototype._open = function(station) {
            var modal = $uibModal.open({
                template: uasModalTemplate,
                controller: 'UASModalController',
                backdrop: 'static',
                resolve: {
                    station: function() {
                        return station;
                    },
                    date: _.bind(this.getCurrentTime, this)
                }
            });
            modal.result.then(function() {
            });
        };


        return Map;
    }]);

    module.controller('UASModalController', ['$scope', 'station', 'date', function($scope, station, date) {
        $scope.station = station;
        $scope.date = new Date(Date.parse(_s.sprintf('%04d-%02d-%02dT00:00:00Z', date.getUTCFullYear(), date.getUTCMonth() + 1, date.getUTCDate())));

        $scope.close = function() {
            this.$dismiss('close');
        };
    }]);

    return module;
});
