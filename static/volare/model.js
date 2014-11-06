define([
    'lodash',
    'jquery',
    'angular',
    'google',
    'volare/util'
], function(_, $, angular, google) {
    'use strict';

    var module = angular.module('volare.model', [
        'volare.util'
    ]);

    module.factory('model', ['$http', 'util', function($http, util) {
        var LatLng = google.maps.LatLng;
        var LatLngBounds = google.maps.LatLngBounds;

        function Flights() {
            this._flights = [];
            this._interval = null;

            this._start = null;
            this._end = null;
            this._maxAltitude = 0;
            this._minAltitude = 0;

            this._currentTime = null;

            var self = this;
            this._visibleChangedListener = function() {
                self._clearProperties();
            };
        }

        Flights.prototype.getCount = function() {
            return this._flights.length;
        };

        Flights.prototype.getVisibleCount = function() {
            return _.reduce(this._flights, function(count, flight) {
                return count + (flight.isVisible() ? 1 : 0);
            }, 0);
        };

        Flights.prototype.eachFlight = function(iterator, context) {
            _.each(this._flights, iterator, context);
        };

        Flights.prototype.mapFlight = function(iterator, context) {
            return _.map(this._flights, iterator, context);
        };

        Flights.prototype.getFlight = function(id) {
            return _.find(this._flights, function(flight) {
                return flight.getId() === id;
            });
        };

        Flights.prototype.getPrimaryFlight = function() {
            return _.find(this._flights, function(flight) {
                return flight.isVisible();
            }) || null;
        };

        Flights.prototype.getStartTime = function() {
            if (!this._start) {
                this._start = _.reduce(this._flights, function(time, flight) {
                    if (flight.isVisible()) {
                        var start = flight.getStartTime();
                        return time && time < start ? time : start;
                    }
                    else {
                        return time;
                    }
                }, null);
            }
            return this._start;
        };

        Flights.prototype.getEndTime = function() {
            if (!this._end) {
                this._end = _.reduce(this._flights, function(time, flight) {
                    if (flight.isVisible()) {
                        var end = flight.getEndTime();
                        return time && time > end ? time : end;
                    }
                    else {
                        return time;
                    }
                }, null);
            }
            return this._end;
        };

        Flights.prototype.getDuration = function() {
            return this.getEndTime() - this.getStartTime();
        };

        Flights.prototype.getMaxAltitude = function() {
            if (this._maxAltitude === 0) {
                this._maxAltitude = _.reduce(this._flights, function(altitude, flight) {
                    if (flight.isVisible())
                        return Math.max(altitude, flight.getMaxAltitude());
                    else
                        return altitude;
                }, 0) + 100;
            }
            return this._maxAltitude;
        };

        Flights.prototype.getMinAltitude = function() {
            if (this._minAltitude === 0) {
                var minAltitude = _.reduce(this._flights, function(altitude, flight) {
                    if (flight.isVisible())
                        return Math.min(altitude, flight.getMinAltitude());
                    else
                        return altitude;
                }, 10000);
                this._minAltitude = Math.max(minAltitude - 100, 0);
            }
            return this._minAltitude;
        };

        Flights.prototype.getBounds = function() {
            return _.reduce(this._flights, function(bounds, flight) {
                var b = flight.getBounds();
                return bounds ? bounds.union(b) : b;
            }, null);
        };

        Flights.prototype.addFlight = function(id, color) {
            var self = this;
            var params = {};
            if (this._interval)
                params.interval = this._interval;
            $http.get('/flights/' + id, params).success(function(flight) {
                if (!color)
                    color =  self._getNextAvailableColor();

                var f = Flight.wrap(flight, color);
                var n = _.sortedIndex(self._flights, f, function(flight) {
                    return flight.getName();
                });
                self._flights.splice(n, 0, f);
                $(f).on('visible_changed', self._visibleChangedListener);
                self._clearProperties();
                $(self).trigger('flight_added', [f, n]);
            });
        };

        Flights.prototype.removeFlight = function(id) {
            var flight = _.find(this._flights, function(flight) {
                return flight.getId() === id;
            });
            if (!flight)
                return;

            this._flights = _.without(this._flights, flight);
            $(flight).off('visible_changed', this._visibleChangedListener);
            this._clearProperties();
            $(this).trigger('flight_removed', flight);
        };

        Flights.prototype.getCurrentTime = function() {
            return this._currentTime;
        };

        Flights.prototype.setCurrentTime = function(time, play) {
            if (time !== this._currentTime) {
                this._currentTime = time;
                $(this).trigger('currenttime_changed', [time, play]);
            }
        };

        Flights.prototype.getInterval = function() {
            return this._interval;
        };

        Flights.prototype.setInterval = function(interval) {
            if (interval !== this._interval) {
                this._interval = interval;
                this._reload();
                $(this).trigger('interval_changed', this._interval);
            }
        };

        Flights.prototype._reload = function() {
            var flights = this._flights.slice(0);
            var self = this;
            _.each(flights, function(flight) {
                self.removeFlight(flight.getId());
                self.addFlight(flight.getId(), flight.getColor());
            });
        };

        Flights.prototype._clearProperties = function() {
            this._start = null;
            this._end = null;
            this._maxAltitude = 0;
            this._minAltitude = 0;

            $(this).trigger('properties_changed');
        };

        Flights.prototype._getNextAvailableColor = function() {
            var colors = {};
            _.each(Flights.COLORS, function(color) {
                colors[color] = 0;
            });
            _.each(this._flights, function(flight) {
                var color = flight.getColor();
                colors[color] = (colors[color] || 0) + 1;
            });
            var u = [];
            _.each(colors, function(n, color) {
                u[n] = (u[n] || []).concat([color]);
            });
            return _.head(_.find(u, function(a) {
                return true;
            }));
        };

        Flights.COLORS = [
            'red',
            'blue',
            'green',
            'yellow',
            'aqua',
            'fuchsia',
            'lime',
            'maroon',
            'navy',
            'olive',
            'purple',
            'silver',
            'teal'
        ];


        function Flight() {
        }

        Flight.wrap = function(flight, color) {
            var f = util.wrap(Flight.prototype, flight);
            f._time = new Date(flight.time);
            _.each(f._records, function(record) {
                record.time = new Date(record.time);
            });

            f._color = color;
            f._visible = true;
            f._statuses = {};

            return f;
        };

        Flight.prototype.getId = function() {
            return this._id;
        };

        Flight.prototype.getName = function() {
            return this._name;
        };

        Flight.prototype.getTime = function() {
            return this._time;
        };

        Flight.prototype.getDuration = function() {
            return this._duration;
        };

        Flight.prototype.getColor = function() {
            return this._color;
        };

        Flight.prototype.setColor = function(color) {
            this._color = color;
        };

        Flight.prototype.isVisible = function() {
            return this._visible;
        };

        Flight.prototype.setVisible = function(visible) {
            if (visible !== this._visible) {
                this._visible = visible;
                $(this).trigger('visible_changed', visible);
            }
        };

        Flight.prototype.getStartTime = function() {
            return _.head(this._records).time;
        };

        Flight.prototype.getEndTime = function() {
            return _.last(this._records).time;
        };

        Flight.prototype.getBounds = function() {
            return new LatLngBounds(new LatLng(this._minLatitude, this._minLongitude),
                                    new LatLng(this._maxLatitude, this._maxLongitude));
        };

        Flight.prototype.getMaxAltitude = function() {
            return this._maxAltitude;
        };

        Flight.prototype.getMinAltitude = function() {
            return this._minAltitude;
        };

        Flight.prototype.getRecord = function(index) {
            return this._records[index];
        };

        Flight.prototype.getRecordCount = function() {
            return this._records.length;
        };

        Flight.prototype.getPositionAt = function(time) {
            var records = this._records;
            if (!time)
                return records[0];

            var index = this._getRecordIndexAt(time);
            if (index >= records.length)
                index = records.length - 1;
            return records[index];
        };

        Flight.prototype.getGroundSpeedAt = function(time) {
            var self = this;
            return this._getSpeedAt(time, Flight.GROUND_SPEED_SAMPLING_DURATION, function(n) {
                var s = self._records[n];
                var e = self._records[n + 1];
                return self._getDistance(n)/((e.time - s.time)/1000);
            });
        };

        Flight.prototype.getVerticalSpeedAt = function(time) {
            var self = this;
            return this._getSpeedAt(time, Flight.VERTICAL_SPEED_SAMPLING_DURATION, function(n) {
                var s = self._records[n];
                var e = self._records[n + 1];
                return (e.altitude - s.altitude)/((e.time - s.time)/1000);
            });
        };

        Flight.prototype.getStatusAt = function(time) {
            var self = this;
            function getStatus() {
                var range = self._getRecordIndexRange(time, Flight.STATUS_SAMPLING_DURATION);
                if (!range)
                    return Flight.STATUS_UNKNOWN;

                var directions = _.map(range, _.bind(self._getDirection, self));
                if (directions.length < 2)
                    return Flight.STATUS_UNKNOWN;

                var clockwise = Math.sin(directions[1] - directions[0]) >= 0;
                var circling = true;
                var firstHalf = true;
                var circle = 0;
                (function() {
                    for (var n = 1; n < directions.length && circling; ++n) {
                        var f = true;
                        if (clockwise) {
                            circling = Math.sin(directions[n] - directions[n - 1]) >= 0;
                            f = Math.sin(directions[n] - directions[0]) >= 0;
                        }
                        else {
                            circling = Math.sin(directions[n] - directions[n - 1]) <= 0;
                            f = Math.sin(directions[n] - directions[0]) <= 0;
                        }
                        if (f && !firstHalf)
                            ++circle;
                        firstHalf = f;
                    }
                }());
                if (circling && circle > 0)
                    return Flight.STATUS_CIRCLING;

                var gliding = true;
                (function() {
                    for (var n = 1; n < directions.length && gliding; ++n) {
                        gliding = Math.cos(directions[n] - directions[n - 1]) >= 0;
                    }
                }());
                if (gliding)
                    return Flight.STATUS_GLIDING;

                return Flight.STATUS_UNKNOWN;
            }

            var status = this._statuses[time];
            if (_.isUndefined(status)) {
                status = getStatus();
                this._statuses[time] = status;
            }
            return status;
        };

        Flight.prototype.getLD = function(time) {
            var status = this.getStatusAt(time);
            if (status !== Flight.STATUS_GLIDING)
                return null;

            var self = this;

            var start = time;
            (function() {
                while (true) {
                    var t = new Date(start.getTime() - 1000);
                    if (self.getStatusAt(t) !== status)
                        break;
                    start = t;
                }
            }());

            var end = time;
            (function() {
                while (true) {
                    var t = new Date(end.getTime() + 1000);
                    if (self.getStatusAt(t) !== status)
                        break;
                    end = t;
                }
            }());

            var startIndex = this._getRecordIndexAt(start);
            var endIndex = this._getRecordIndexAt(end);
            if (startIndex >= endIndex - 1)
                return null;

            var distance = 0;
            for (var n = startIndex; n < endIndex; ++n) {
                distance += this._getDistance(n);
            }
            if (distance === 0)
                return null;

            return distance/(this._records[startIndex].altitude - this._records[endIndex - 1].altitude);
        };

        Flight.prototype.getAverageClimb = function(time) {
            var status = this.getStatusAt(time);
            if (status !== Flight.STATUS_CIRCLING)
                return null;

            var self = this;

            var start = time;
            (function() {
                while (true) {
                    var t = new Date(start.getTime() - 1000);
                    if (self.getStatusAt(t) !== status)
                        break;
                    start = t;
                }
            })();

            var end = time;
            (function() {
                while (true) {
                    var t = new Date(end.getTime() + 1000);
                    if (self.getStatusAt(t) !== status)
                        break;
                    end = t;
                }
            }());

            var startIndex = this._getRecordIndexAt(start);
            var endIndex = this._getRecordIndexAt(end);
            if (startIndex >= endIndex - 1)
                return null;

            var startRecord = this._records[startIndex];
            var endRecord = this._records[endIndex - 1];
            return (endRecord.altitude - startRecord.altitude)/((endRecord.time.getTime() - startRecord.time.getTime())/1000);
        };

        Flight.prototype.getExtra = function(name) {
            return this._extra ? this._extra[name] : undefined;
        };

        Flight.prototype.setExtra = function(name, value) {
            if (!this._extra)
                this._extra = {};
            this._extra[name] = value;
        };

        Flight.prototype.updateTrack = function(track, currentTime, currentOnly) {
            if (this._visible) {
                var records = this._records;
                var currentRecords = records;
                if (currentTime) {
                    var start = this._getRecordIndexAt(new Date(currentTime.getTime() - Flight.TRACK_DURATION*1000));
                    var end = this._getRecordIndexAt(currentTime);
                    currentRecords = records.slice(start, end);
                }

                if (!currentOnly)
                    track.setRecords(records);
                track.setCurrentRecords(currentRecords);
            }
            else {
                if (!currentOnly)
                    track.setRecords([]);
                track.setCurrentRecords([]);
            }
        };

        Flight.prototype._getRecordIndexAt = function(time) {
            return _.sortedIndex(this._records, { time: time }, function(record) {
                return record.time;
            });
        };

        Flight.prototype._getSpeedAt = function(time, duration, f) {
            var range = this._getRecordIndexRange(time, duration);
            if (!range)
                return 0;

            var speeds = _.map(range, f);
            return _.reduce(speeds, function(a, n) { return a + n; }, 0)/speeds.length;
        };

        Flight.prototype._getRecordIndexRange = function(time, duration) {
            if (!time)
                return null;

            var records = this._records;
            var index = this._getRecordIndexAt(time);
            if (index <= 0 || records.length <= index)
                return null;

            var start = this._getRecordIndexAt(new Date(time.getTime() - duration*1000));
            var end = this._getRecordIndexAt(new Date(time.getTime() + duration*1000));
            if (end >= records.length)
                end = records.length - 1;
            if (start === end) {
                start = index - 1;
                end = index;
            }

            return _.range(start, end);
        };

        Flight.prototype._getDistance = function(index) {
            var distance = this._records[index].distance;
            if (_.isUndefined(distance)) {
                distance = Flight.distance(this._records[index], this._records[index + 1]);
                this._records[index].distance = distance;
            }
            return distance;
        };

        Flight.prototype._getDirection = function(index) {
            var direction = this._records[index].direction;
            if (_.isUndefined(direction)) {
                direction = Flight.direction(this._records[index], this._records[index + 1]);
                this._records[index].direction = direction;
            }
            return direction;
        };

        Flight.distance = function(p1, p2) {
            return Util.distance(p1.latitude, p1.longitude, p2.latitude, p2.longitude);
        };

        Flight.direction = function(p1, p2) {
            return Util.direction(p1.latitude, p1.longitude, p2.latitude, p2.longitude);
        };

        Flight.TRACK_DURATION = 10*60;
        Flight.GROUND_SPEED_SAMPLING_DURATION = 10;
        Flight.VERTICAL_SPEED_SAMPLING_DURATION = 5;
        Flight.STATUS_SAMPLING_DURATION = 30;

        Flight.STATUS_UNKNOWN = 0;
        Flight.STATUS_CIRCLING = 1;
        Flight.STATUS_GLIDING = 2;


        function Waypoint() {
        }

        Waypoint.wrap = function(waypoint) {
            var w = util.wrap(Waypoint.prototype, waypoint);
            w._items = _.map(waypoint.items, WaypointItem.wrap);
            return w;
        };

        Waypoint.prototype.getId = function() {
            return this._id;
        };

        Waypoint.prototype.getName = function() {
            return this._name;
        };

        Waypoint.prototype.getItems = function() {
            return this._items;
        };

        Waypoint.prototype.getItem = function(id) {
            return _.find(this._items, function(item) {
                return item.getId() === id;
            });
        };


        function WaypointItem(item) {
        }

        WaypointItem.wrap = function(item) {
            return util.wrap(WaypointItem.prototype, item);
        };

        WaypointItem.prototype.getId = function() {
            return this._id;
        };

        WaypointItem.prototype.getName = function() {
            return this._name;
        };

        WaypointItem.prototype.getLatitude = function() {
            return this._latitude;
        };

        WaypointItem.prototype.getLongitude = function() {
            return this._longitude;
        };

        WaypointItem.prototype.getAltitude = function() {
            return this._altitude;
        };

        WaypointItem.prototype.getDescription = function() {
            return this._description;
        };

        WaypointItem.prototype.getPosition = function() {
            return new LatLng(this._latitude, this._longitude);
        };

        WaypointItem.prototype.getLabel = function() {
            var label = this._name;
            if (this._name !== this._description)
                label += ' (' + this._description + ')';
            return label;
        };

        WaypointItem.distance = function(item1, item2) {
            return Util.distance(item1.getLatitude(), item1.getLongitude(), item2.getLatitude(), item2.getLongitude());
        };


        function Route() {
            this._id = null;
            this._items = [];
        }

        Route.wrap = function(route) {
            var r = util.wrap(Route.prototype, route);
            r._items = _.map(route.items, RouteItem.wrap);
            return r;
        };

        Route.prototype.getId = function() {
            return this._id;
        };

        Route.prototype.getItems = function() {
            return this._items;
        };

        Route.prototype.addItem = function(waypointItem, radius) {
            this._items.push(new RouteItem(waypointItem, radius));
        };


        function RouteItem(waypointItem, radius) {
            this._waypointItem = waypointItem;
            this._radius = radius;
        }

        RouteItem.wrap = function(item) {
            var i = util.wrap(RouteItem.prototype, item);
            i._waypointItem = WaypointItem.wrap(item.waypointItem);
            return i;
        };

        RouteItem.prototype.getWaypointItem = function() {
            return this._waypointItem;
        };

        RouteItem.prototype.getRadius = function() {
            return this._radius;
        };


        var Util = {};

        Util.distance = function(latitude1, longitude1, latitude2, longitude2) {
            var r = 6378137;
            var dx = (longitude1 - longitude2)/180*Math.PI;
            var y1 = latitude1/180*Math.PI;
            var y2 = latitude2/180*Math.PI;
            return r*Math.acos(Math.sin(y1)*Math.sin(y2) + Math.cos(y1)*Math.cos(y2)*Math.cos(dx));
        };

        Util.direction = function(latitude1, longitude1, latitude2, longitude2) {
            var y = Math.cos(latitude2)*Math.sin(longitude2 - longitude1);
            var x = Math.cos(latitude1)*Math.sin(latitude2) - Math.sin(latitude1)*Math.cos(latitude2)*Math.cos(longitude2 - longitude1);
            var t = Math.atan2(y, x);
            return t < 0 ? t + 2*Math.PI : t;
        };


        return {
            Flights: Flights,
            Flight: Flight,
            Waypoint: Waypoint,
            WaypointItem: WaypointItem,
            Route: Route,
            RouteItem: RouteItem
        };
    }]);

    return module;
});
