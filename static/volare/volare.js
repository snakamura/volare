define([
    'require',
    'lodash',
    'jquery',
    'google',
    'markerwithlabel',
    'volare/common',
    'text!./volare.css',
    'bootstrap'
], function(require, _, $, google, markerWithLabel, common, css) {
    'use strict';

    common.loadCssInline(css);

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
        $.getJSON('/flights/' + id, params, function(flight) {
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
        var f = common.wrap(Flight.prototype, flight);
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
        var w = common.wrap(Waypoint.prototype, waypoint);
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
        return common.wrap(WaypointItem.prototype, item);
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
        var r = common.wrap(Route.prototype, route);
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
        var i = common.wrap(RouteItem.prototype, item);
        i._waypointItem = WaypointItem.wrap(item.waypointItem);
        return i;
    };

    RouteItem.prototype.getWaypointItem = function() {
        return this._waypointItem;
    };

    RouteItem.prototype.getRadius = function() {
        return this._radius;
    };


    function Map(flights, $map) {
        this._flights = flights;
        this._map = new google.maps.Map($map[0], {
            mapTypeId: google.maps.MapTypeId.HYBRID
        });
        this._msmSurfaceOverlay = new MSMSurfaceOverlay(this._flights);
        this._msmBarometricOverlay = new MSMBarometricOverlay(this._flights);
        this._amedasOverlay = new AMEDASOverlay(this._flights);
        this._windasOverlay = new WINDASOverlay(this._flights);
        this._weatherFlags = 0;
        this._trackType = Map.TrackType.SOLID;
        this._waypoint = null;
        this._waypointMakers = [];
        this._route = null;
        this._routeOverlays = [];

        var self = this;
        var visibleChangedListener = function(event) {
            var flight = event.target;
            flight.updateTrack(flight.__track, self._flights.getCurrentTime());
        };

        $(this._flights).on('flight_added', function(event, flight) {
            self._map.fitBounds(self._flights.getBounds());

            var track = self._createTrack(flight);
            flight.updateTrack(track);
            flight.__track = track;

            $(flight).on('visible_changed', visibleChangedListener);
        });
        $(this._flights).on('flight_removed', function(event, flight) {
            $(flight).off('visible_changed', visibleChangedListener);
            flight.__track.clear();
            flight.__track = null;
        });
        $(this._flights).on('properties_changed', function() {
            self._flights.eachFlight(function(flight) {
                if (flight.__track)
                    flight.updateTrack(flight.__track, self._flights.getCurrentTime());
            });
        });
        $(this._flights).on('currenttime_changed', function(event, time) {
            self._flights.eachFlight(function(flight) {
                flight.updateTrack(flight.__track, time, true);
            });

            var primaryFlight = self._flights.getPrimaryFlight();
            if (primaryFlight) {
                var position = primaryFlight.getPositionAt(time);
                var span = self._map.getBounds().toSpan();
                var bounds = new LatLngBounds(new LatLng(position.latitude - span.lat()/10, position.longitude - span.lng()/10),
                                              new LatLng(position.latitude + span.lat()/10, position.longitude + span.lng()/10));
                self._map.panToBounds(bounds);
            }
        });
    }

    Map.prototype.getWeatherFlags = function() {
        return this._weatherFlags;
    };

    Map.prototype.setWeatherFlags = function(flags, mask) {
        this._weatherFlags = (this._weatherFlags & ~mask) | (flags & mask);

        this._msmSurfaceOverlay.setFlags(this._map, this._weatherFlags & Map.WeatherFlag.MSM.SURFACE.ALL);
        this._msmBarometricOverlay.setFlags(this._map, this._weatherFlags & Map.WeatherFlag.MSM.BAROMETRIC.ALL);
        this._amedasOverlay.setFlags(this._map, this._weatherFlags & Map.WeatherFlag.AMEDAS.ALL);
        this._windasOverlay.setFlags(this._map, this._weatherFlags & Map.WeatherFlag.WINDAS.ALL);

        $(this).trigger('weatherFlags_changed', this._weatherFlags);
    };

    Map.prototype.getTrackType = function() {
        return this._trackType;
    };

    Map.prototype.setTrackType = function(trackType) {
        this._trackType = trackType;

        var self = this;
        this._flights.eachFlight(function(flight) {
            flight.__track.clear();

            var track = self._createTrack(flight);
            flight.updateTrack(track);
            flight.__track = track;
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

    Map.TrackType = {
        SOLID: 0,
        ALTITUDE: 1,
        GROUND_SPEED: 2,
        VERTICAL_SPEED: 3
    };

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
                          Map.WeatherFlag.WINDAS.ALL;


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
    common.inherit(SolidColorTrack, Track);

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
    common.inherit(GradientColorTrack, Track);

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
    common.inherit(GradientColorAltitudeTrack, GradientColorTrack);

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
    common.inherit(GradientColorGroundSpeedTrack, GradientColorTrack);

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
    common.inherit(GradientColorVerticalSpeedTrack, GradientColorTrack);

    GradientColorVerticalSpeedTrack.prototype.getMax = function(flights) {
        return 3;
    };

    GradientColorVerticalSpeedTrack.prototype.getMin = function(flights) {
        return -3;
    };

    GradientColorVerticalSpeedTrack.prototype.getValue = function(record) {
        return this._flight.getVerticalSpeedAt(record.time);
    };


    function WeatherOverlay(flights) {
        var self = this;

        this._flights = flights;
        this._$div = null;
        this._idleListener = null;
        this._listener = function() {
            self._update();
        };
    }
    common.inherit(WeatherOverlay, google.maps.OverlayView);

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

        $(this._flights).on('flight_added', this._listener);
        $(this._flights).on('flight_removed', this._listener);
        $(this._flights).on('currenttime_changed', this._listener);

        this._update();
    };

    WeatherOverlay.prototype.onRemove = function() {
        $(this._flights).off('flight_added', this._listener);
        $(this._flights).off('flight_removed', this._listener);
        $(this._flights).off('currenttime_changed', this._listener);

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
            return 'rgb(0, 0, 255, ' + alpha + ')';
        else if (temperature < 5)
            return 'rgb(204, 204, 204, ' + alpha + ')';
        else if (temperature < 10)
            return 'rgb(0, 255, 255, ' + alpha + ')';
        else if (temperature < 15)
            return 'rgb(0, 204, 255, ' + alpha + ')';
        else if (temperature < 20)
            return 'rgb(51, 204, 0, ' + alpha + ')';
        else if (temperature < 25)
            return 'rgb(255, 255, 0, ' + alpha + ')';
        else if (temperature < 30)
            return 'rgb(255, 153, 51, ' + alpha + ')';
        else if (temperature < 35)
            return 'rgb(255, 0, 0, ' + alpha + ')';
        else
            return 'rgb(204, 0, 0, ' + alpha + ')';
    };


    function MSMOverlay(flights) {
        WeatherOverlay.call(this, flights);

        this._clear();
    }
    common.inherit(MSMOverlay, WeatherOverlay);

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

                var $cell = $elem.find('.cell');
                $cell.css('width', width + 'px');
                $cell.css('height', height + 'px');
            });
        }
    };

    MSMOverlay.prototype._update = function() {
        var time = this._flights.getCurrentTime() || this._flights.getStartTime();
        if (time) {
            var map = this.getMap();
            var bounds = map.getBounds();
            if (!this._bounds || !this._bounds.equals(bounds) ||
                !this._time || !WeatherOverlay.hoursEquals(this._time, time)) {
                var self = this;
                $.getJSON('/msm/' + this._getName() + '/' + time.getUTCFullYear() +
                          '/' + (time.getUTCMonth() + 1) +
                          '/' + time.getUTCDate() +
                          '/' + time.getUTCHours() +
                          '?nwlat=' + bounds.getNorthEast().lat() +
                          '&nwlng=' + bounds.getSouthWest().lng() +
                          '&selat=' + bounds.getSouthWest().lat() +
                          '&selng=' + bounds.getNorthEast().lng(), function(items) {
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


    function MSMSurfaceOverlay(flights) {
        MSMOverlay.call(this, flights);

        this._flags = Map.WeatherFlag.MSM.SURFACE.ALL;
    }
    common.inherit(MSMSurfaceOverlay, MSMOverlay);

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


    function MSMBarometricOverlay(flights) {
        MSMOverlay.call(this, flights);

        this._flags = Map.WeatherFlag.MSM.BAROMETRIC.ALL;
    }
    common.inherit(MSMBarometricOverlay, MSMOverlay);

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


    function AMEDASOverlay(flights) {
        WeatherOverlay.call(this, flights);

        this._flags = Map.WeatherFlag.AMEDAS.ALL;
        this._clear();
    }
    common.inherit(AMEDASOverlay, WeatherOverlay);

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
            var time = this._flights.getCurrentTime() || this._flights.getStartTime();
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
        var time = this._flights.getCurrentTime() || this._flights.getStartTime();
        if (time) {
            var map = this.getMap();
            var bounds = map.getBounds();
            if (!this._bounds || !this._bounds.equals(bounds) ||
                !this._time || !WeatherOverlay.hoursEquals(this._time, time)) {
                var self = this;
                $.getJSON('/amedas/' + time.getUTCFullYear() +
                          '/' + (time.getUTCMonth() + 1) +
                          '/' + time.getUTCDate() +
                          '/' + time.getUTCHours() +
                          '?nwlat=' + bounds.getNorthEast().lat() +
                          '&nwlng=' + bounds.getSouthWest().lng() +
                          '&selat=' + bounds.getSouthWest().lat() +
                          '&selng=' + bounds.getNorthEast().lng(), function(items) {
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
            return 'rgb(204, 204, 204, 0.5)';
        else if (sunshine < 5)
            return 'rgb(51, 204, 0, 0.5)';
        else if (sunshine < 10)
            return 'rgb(255, 255, 0, 0.5)';
        else
            return 'rgb(255, 153, 51, 0.5)';
    };


    function WINDASOverlay(flights) {
        WeatherOverlay.call(this, flights);

        this._flags = Map.WeatherFlag.WINDAS.ALL;
        this._clear();
    }
    common.inherit(WINDASOverlay, WeatherOverlay);

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
            var time = this._flights.getCurrentTime() || this._flights.getStartTime();
            var windasTime = new Date(time.getTime() + 10*60*1000);
            var hour = windasTime.getUTCHours();
            var minute = Math.floor(windasTime.getUTCMinutes()/10)*10;
            _.each(this._stations, function(station) {
                var pos = projection.fromLatLngToDivPixel(new LatLng(station.station.latitude, station.station.longitude));
                _.each(station.observations, function(observation) {
                    if (observation.hour === hour && observation.minute === minute) {
                        _.each(self.filterItems(station.station, observation.items), function(item) {
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
        var time = this._flights.getCurrentTime() || this._flights.getStartTime();
        if (time) {
            var map = this.getMap();
            var bounds = map.getBounds();
            if (!this._bounds || !this._bounds.equals(bounds) ||
                !this._time || !WeatherOverlay.hoursEquals(this._time, time)) {
                var self = this;
                $.getJSON('/windas/' + time.getUTCFullYear() +
                          '/' + (time.getUTCMonth() + 1) +
                          '/' + time.getUTCDate() +
                          '/' + time.getUTCHours() +
                          '?nwlat=' + bounds.getNorthEast().lat() +
                          '&nwlng=' + bounds.getSouthWest().lng() +
                          '&selat=' + bounds.getSouthWest().lat() +
                          '&selng=' + bounds.getNorthEast().lng(), function(stations) {
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

    WINDASOverlay.prototype.filterItems = function(station, items) {
        var self = this;

        var margin = 200;
        var altitudes = [500, 1000, 1500, 2000, 2500, 3000];
        var enabledAltitudes = _.filter(altitudes, function(a) {
            return (self._flags & Map.WeatherFlag.WINDAS[a]) !== 0;
        });
        return _.filter(_.map(enabledAltitudes, function(altitude) {
            var matchedItems = _.filter(items, function(item) {
                var a = item.altitude + station.height;
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


    function setupLayout(flights, $map, $sidebar, $chart) {
        function layout() {
            _.defer(function() {
                $map.width($(document).width() - ($sidebar.width() + 10));
                var mapPosition = $map.position();
                var chartPosition = $chart.position();
                $map.height(chartPosition.top - mapPosition.top - 20);
                $sidebar.height($map.height() + 10);
            });
        }
        $(flights).on('flight_added', layout);
        $(flights).on('flight_removed', layout);
        $(window).on('resize', layout);
        layout();
    }

    return {
        Flights: Flights,
        Flight: Flight,
        Waypoint: Waypoint,
        WaypointItem: WaypointItem,
        Route: Route,
        RouteItem: RouteItem,
        Map: Map,
        setupLayout: setupLayout
    };
});
