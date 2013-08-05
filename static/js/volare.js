var volare = volare || {};

(function() {
    var LatLng = google.maps.LatLng;
    var LatLngBounds = google.maps.LatLngBounds;

    var inherit = (function() {
        var Proxy = function() {
        };
        return function(clazz, parent) {
            Proxy.prototype = parent.prototype;
            clazz.prototype = new Proxy();
            clazz.super_ = parent.prototype;
            clazz.prototype.constructor = clazz;
        };
    })();

    function Flights() {
        this._flights = [];

        this._start = null;
        this._end = null;
        this._maxAltitude = 0;

        this._currentTime = null;
    }

    Flights.prototype.getCount = function() {
        return this._flights.length;
    };

    Flights.prototype.eachFlights = function(iterator, context) {
        _.each(this._flights, iterator, context);
    };

    Flights.prototype.getPrimaryFlight = function() {
        return this._flights.length > 0 ? this._flights[0] : null;
    };

    Flights.prototype.getStartTime = function() {
        if (!this._start) {
            this._start = _.reduce(this._flights, function(time, flight) {
                var start = flight.getStartTime();
                return time && time < start ? time : start;
            }, null);
        }
        return this._start;
    };

    Flights.prototype.getEndTime = function() {
        if (!this._end) {
            this._end = _.reduce(this._flights, function(time, flight) {
                var end = flight.getEndTime();
                return time && time > end ? time : end;
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
                return Math.max(altitude, flight.getMaxAltitude());
            }, 0) + 100;
        }
        return this._maxAltitude;
    };

    Flights.prototype.getBounds = function() {
        return _.reduce(this._flights, function(bounds, flight) {
            var b = flight.getBounds();
            return bounds ? bounds.union(b) : b;
        }, null);
    };

    Flights.prototype.addFlight = function(flight) {
        if (!flight.getColor())
            flight.setColor(this._getNextAvailableColor());

        var n = _.sortedIndex(this._flights, flight, function(flight) {
            return flight.getName();
        });
        this._flights.splice(n, 0, flight);
        this._clearProperties();
        $(this).trigger('flight_added', [flight, n]);
    };

    Flights.prototype.removeFlight = function(id) {
        var flight = _.find(this._flights, function(flight) {
            return flight.getId() === id;
        });
        if (!flight)
            return;

        this._flights = _.without(this._flights, flight);
        this._clearProperties();
        $(this).trigger('flight_removed', flight);
    };

    Flights.prototype.getCurrentTime = function() {
        return this._currentTime;
    };

    Flights.prototype.setCurrentTime = function(time, play) {
        this._currentTime = time;
        $(this).trigger('currenttime_changed', [time, play]);
    };

    Flights.prototype._clearProperties = function() {
        this._start = null;
        this._end = null;
        this._maxAltitude = 0;

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


    function Flight(flight, color) {
        var self = this;
        _.each(flight, function(value, key) {
            self['_' + key] = value;
        });
        self._time = new Date(self._time);
        _.each(this._records, function(record) {
            record.time = new Date(record.time);
        });

        this._color = color;
        this._visible = true;
        this._statuses = {};
    }

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
        this._visible = visible;
        $(this).trigger('visible_changed', visible);
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
            if (circling && circle > 0)
                return Flight.STATUS_CIRCLING;

            var gliding = true;
            for (var n = 1; n < directions.length && gliding; ++n) {
                gliding = Math.cos(directions[n] - directions[n - 1]) >= 0;
            }
            if (gliding)
                return Flight.STATUS_GLIDING;

            return Flight.STATUS_UNKNOWN;
        };

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

        var start = time;
        while (true) {
            var t = new Date(start.getTime() - 1000);
            if (this.getStatusAt(t) !== status)
                break;
            start = t;
        }

        var end = time;
        while (true) {
            var t = new Date(end.getTime() + 1000);
            if (this.getStatusAt(t) !== status)
                break;
            end = t;
        }

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

        var start = time;
        while (true) {
            var t = new Date(start.getTime() - 1000);
            if (this.getStatusAt(t) !== status)
                break;
            start = t;
        }

        var end = time;
        while (true) {
            var t = new Date(end.getTime() + 1000);
            if (this.getStatusAt(t) !== status)
                break;
            end = t;
        }

        var startIndex = this._getRecordIndexAt(start);
        var endIndex = this._getRecordIndexAt(end);
        if (startIndex >= endIndex - 1)
            return null;

        var startRecord = this._records[startIndex];
        var endRecord = this._records[endIndex - 1];
        return (endRecord.altitude - startRecord.altitude)/((endRecord.time.getTime() - startRecord.time.getTime())/1000);
    };

    Flight.prototype.updatePolyline = function(polyline, currentTime) {
        var path = polyline.getPath();
        path.clear();

        if (this._visible) {
            var records = this._records;
            if (currentTime) {
                var start = this._getRecordIndexAt(new Date(currentTime.getTime() - Flight.TRACK_DURATION*1000));
                var end = this._getRecordIndexAt(currentTime);
                records = records.slice(start, end);
            }

            _.each(records, function(record) {
                path.push(new LatLng(record.latitude, record.longitude));
            });
        }
    };

    Flight.prototype.drawAltitude = function(graph, context, currentTime, altitudeGraphContext) {
        if (this._visible) {
            context.strokeStyle = this._color;
            context.lineWidth = 2;

            var startIndex = 0;
            var startTime = null;
            var startAltitude = 0;
            if (altitudeGraphContext && altitudeGraphContext.isSet()) {
                startIndex = altitudeGraphContext.getIndex();
                startTime = altitudeGraphContext.getTime();
                startAltitude = altitudeGraphContext.getAltitude();
            }
            else {
                startTime = this.getStartTime();
                startAltitude = this._records[0].altitude;
            }

            context.beginPath();
            context.moveTo(graph.getX(startTime), graph.getY(startAltitude));

            var lastTime = startTime;
            var lastAltitude = startAltitude;
            var n = startIndex;
            for (; n < this._records.length; ++n) {
                var record = this._records[n];
                var time = record.time;
                if (currentTime && time > currentTime)
                    break;

                context.lineTo(graph.getX(time), graph.getY(record.altitude));

                lastTime = time;
                lastAltitude = record.altitude;
            }
            if (currentTime && this.getStartTime() <= currentTime)
                context.lineTo(graph.getX(currentTime), graph.getY(lastAltitude));
            context.stroke();

            if (altitudeGraphContext)
                altitudeGraphContext.set(lastAltitude, n > 0 ? n - 1 : n, lastTime);
        }
    };

    Flight.prototype.drawGroundSpeed = function(graph, context, currentTime, speedGraphContext) {
        if (this._visible) {
            context.strokeStyle = this._color;
            context.lineWidth = 2;

            var step = 20*1000;
            var startTime = null;
            var endTime = null;
            if (speedGraphContext && speedGraphContext.isSet()) {
                var lastDrawnGroundSpeedTime = speedGraphContext.getTime();
                if (currentTime.getTime() >= lastDrawnGroundSpeedTime.getTime() + step) {
                    startTime = lastDrawnGroundSpeedTime;
                    endTime = currentTime || this.getEndTime();
                }
            }
            else {
                startTime = this.getStartTime();
                endTime = currentTime || this.getEndTime();
            }
            if (startTime) {
                context.beginPath();
                context.moveTo(graph.getX(startTime), graph.getY(this.getGroundSpeedAt(startTime)*3600/1000));
                var time = startTime.getTime() + step;
                for (; time < endTime.getTime(); time += step) {
                    var t = new Date(time);
                    context.lineTo(graph.getX(t), graph.getY(this.getGroundSpeedAt(t)*3600/1000));
                }
                context.stroke();

                if (speedGraphContext)
                    speedGraphContext.set(new Date(time - step));
            }
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
        var r = 6378137;
        var dx = (p1.longitude - p2.longitude)/180*Math.PI;
        var y1 = p1.latitude/180*Math.PI;
        var y2 = p2.latitude/180*Math.PI;
        return r*Math.acos(Math.sin(y1)*Math.sin(y2) + Math.cos(y1)*Math.cos(y2)*Math.cos(dx));
    };

    Flight.direction = function(p1, p2) {
        var y = Math.cos(p2.latitude)*Math.sin(p2.longitude - p1.longitude);
        var x = Math.cos(p1.latitude)*Math.sin(p2.latitude) - Math.sin(p1.latitude)*Math.cos(p2.latitude)*Math.cos(p2.longitude - p1.longitude);
        var t = Math.atan2(y, x);
        return t < 0 ? t + 2*Math.PI : t;
    };

    Flight.TRACK_DURATION = 10*60;
    Flight.GROUND_SPEED_SAMPLING_DURATION = 10;
    Flight.VERTICAL_SPEED_SAMPLING_DURATION = 5;
    Flight.STATUS_SAMPLING_DURATION = 30;

    Flight.STATUS_UNKNOWN = 0;
    Flight.STATUS_CIRCLING = 1;
    Flight.STATUS_GLIDING = 2;


    function Player(flights, player) {
        this._flights = flights;
        this._player = player;

        this._timer = null;

        var self = this;

        this._player.html('<div>' +
                          '<button class="play">Play</button>' +
                          '<button class="stop">Stop</button>' +
                          '<span class="time"></span>' +
                          '</div>' +
                          '<div class="slider"></div>');

        this._player.find('.play').button().on('click', _.bind(this.play, this));
        this._player.find('.stop').button().on('click', _.bind(this.stop, this));
        this._player.find('.slider').slider({
            range: 'min',
            min: 0,
            max: 100
        }).on('slide', function(event, ui) {
            self._flights.setCurrentTime(new Date(ui.value));
        });

        $(this._flights).on('currenttime_changed', function() {
            self._updateButtons();
            self._updateSliderValue();
            self._updateTime();
        });
        $(this._flights).on('properties_changed', function() {
            self._updateSliderRange();
            self._updateTime();
        });

        this._updateButtons();
        this._updateSliderRange();
        this._updateTime();
    }

    Player.prototype.play = function() {
        if (!this._timer) {
            var self = this;
            this._flights.setCurrentTime(this._flights.getCurrentTime() || this._flights.getStartTime());
            this._timer = setInterval(function() {
                var time = new Date(self._flights.getCurrentTime().getTime() + 10*1000);
                if (time > self._flights.getEndTime())
                    self.stop();
                else
                    self._flights.setCurrentTime(time, true);
            }, 100);
        }
        else {
            clearInterval(this._timer);
            this._timer = null;
        }

        this._updateButtons();
    };

    Player.prototype.stop = function() {
        if (this._timer) {
            clearInterval(this._timer);
            this._timer = null;
        }
        this._flights.setCurrentTime(null);

        this._updateButtons();
    };

    Player.prototype._updateButtons = function() {
        this._player.find('.play').button('option', 'label', this._timer ? 'Pause' : 'Play');
        this._player.find('.stop').button(this._timer || this._flights.getCurrentTime() ? 'enable' : 'disable');
    };

    Player.prototype._updateSliderRange = function() {
        var slider = this._player.find('.slider');
        slider.slider('option', 'min', this._flights.getStartTime() ? this._flights.getStartTime().getTime() : 0);
        slider.slider('option', 'max', this._flights.getEndTime() ? this._flights.getEndTime().getTime() : 0);

        this._updateSliderValue();
    };

    Player.prototype._updateSliderValue = function() {
        var slider = this._player.find('.slider');
        var time = this._flights.getCurrentTime();
        slider.slider('value', time ? time.getTime() : slider.slider('option', 'min'));
    };

    Player.prototype._updateTime = function() {
        var timeLabel = this._player.find('.time');
        var time = this._flights.getCurrentTime() || this._flights.getStartTime();
        timeLabel.text(Player.formatTime(time));
    };

    Player.formatTime = function(time) {
        if (time)
            return _.sprintf("%02d:%02d:%02d", time.getHours(), time.getMinutes(), time.getSeconds());
        else
            return '';
    };


    function Map(flights, map) {
        this._flights = flights;
        this._map = new google.maps.Map(map[0], {
            mapTypeId: google.maps.MapTypeId.TERRAIN
        });

        var msmOverlay = new MSMOverlay(this._flights);
        msmOverlay.setMap(this._map);

        var amedasOverlay = new AMEDASOverlay(this._flights);
        amedasOverlay.setMap(this._map);

        var self = this;

        $(this._flights).on('flight_added', function(event, flight) {
            self._map.fitBounds(self._flights.getBounds());

            var polyline = new google.maps.Polyline({
                map: self._map,
                strokeColor: flight.getColor(),
                strokeOpacity: 0.3
            });
            flight.updatePolyline(polyline);
            flight.__polyline = polyline;

            var currentPolyline = new google.maps.Polyline({
                map: self._map,
                strokeColor: flight.getColor()
            });
            flight.updatePolyline(currentPolyline);
            flight.__currentPolyline = currentPolyline;

            $(flight).on('visible_changed', function() {
                flight.updatePolyline(flight.__polyline);
                flight.updatePolyline(flight.__currentPolyline, self._flights.getCurrentTime());
            });
        });
        $(this._flights).on('flight_removed', function(event, flight) {
            flight.__polyline.setMap(null);
            flight.__polyline = null;

            flight.__currentPolyline.setMap(null);
            flight.__currentPolyline = null;
        });
        $(this._flights).on('currenttime_changed', function(event, time) {
            self._flights.eachFlights(function(flight) {
                flight.updatePolyline(flight.__currentPolyline, time);
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


    function WeatherOverlay(flights) {
        var self = this;

        this._flights = flights;
        this._div = null;
        this._idleListener = null;
        this._listener = function() {
            self._update();
        };
    }

    WeatherOverlay.prototype = new google.maps.OverlayView();

    WeatherOverlay.prototype.onAdd = function() {
        var div = $('<div class="weather ' + this._getClassName() + '"></div>');

        var panes = this.getPanes();
        panes.overlayLayer.appendChild(div[0]);

        this._div = div;

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

        this._div.remove();
        this._div = null;
    };

    WeatherOverlay.prototype.draw = function() {
        this._draw();
    };

    WeatherOverlay.hoursEquals = function(time1, time2) {
        return time1.getUTCFullYear() == time2.getUTCFullYear() &&
            time1.getUTCMonth() == time2.getUTCMonth() &&
            time1.getUTCDate() == time2.getUTCDate() &&
            time1.getUTCHours() == time2.getUTCHours();
    };

    WeatherOverlay.tenMinutesEquals = function(time1, time2) {
        return WeatherOverlay.hoursEquals(time1, time2) &&
            Math.floor(time1.getUTCMinutes() / 10) == Math.floor(time2.getUTCMinutes() / 10);
    };

    WeatherOverlay.windIconIndex = function(windSpeed) {
        return windSpeed <= 2 ? 1 : windSpeed <= 3 ? 2 : windSpeed <= 4 ? 3 : windSpeed <= 5 ? 4 : 5;
    };

    WeatherOverlay.colorForTemperature = function(temperature) {
        if (temperature < 0)
            return 'rgb(0, 0, 255, 0.5)';
        else if (temperature < 5)
            return 'rgb(204, 204, 204, 0.5)';
        else if (temperature < 10)
            return 'rgb(0, 255, 255, 0.5)';
        else if (temperature < 15)
            return 'rgb(0, 204, 255, 0.5)';
        else if (temperature < 20)
            return 'rgb(51, 204, 0, 0.5)';
        else if (temperature < 25)
            return 'rgb(255, 255, 0, 0.5)';
        else if (temperature < 30)
            return 'rgb(255, 153, 51, 0.5)';
        else if (temperature < 35)
            return 'rgb(255, 0, 0, 0.5)';
        else
            return 'rgb(204, 0, 0, 0.5)';
    };


    function MSMOverlay(flights) {
        WeatherOverlay.call(this, flights);

        this._time = null;
        this._bounds = null;
        this._items = {};
    }

    MSMOverlay.prototype = new WeatherOverlay();

    MSMOverlay.prototype._getClassName = function() {
        return 'msm';
    };

    MSMOverlay.prototype._draw = function() {
        var map = this.getMap();
        var bounds = map.getBounds();
        var projection = this.getProjection();
        var sw = projection.fromLatLngToDivPixel(bounds.getSouthWest());
        var ne = projection.fromLatLngToDivPixel(bounds.getNorthEast());

        var div = this._div;
        div.css('left', sw.x + 'px');
        div.css('top', ne.y + 'px');
        div.css('width', (ne.x - sw.x) + 'px');
        div.css('height', (sw.y - ne.y) + 'px');

        if (!_.isEmpty(this._items)) {
            _.each(this._items, function(item) {
                var nw = projection.fromLatLngToDivPixel(new LatLng(item.latitude + MSMOverlay.SURFACE_LATITUDE_STEP/2, item.longitude - MSMOverlay.SURFACE_LONGITUDE_STEP/2));
                var se = projection.fromLatLngToDivPixel(new LatLng(item.latitude - MSMOverlay.SURFACE_LATITUDE_STEP/2, item.longitude + MSMOverlay.SURFACE_LONGITUDE_STEP/2));
                var width = se.x - nw.x;
                var height = se.y - nw.y;

                var elem = item.elem;
                if (!elem) {
                    elem = $('<div class="item"><div class="cell"><img class="wind"><br><span class="temperature"></span></div></div>');
                    div.append(elem);

                    var windSpeed = Math.sqrt(Math.pow(item.northwardWind, 2) + Math.pow(item.eastwardWind, 2));
                    var windAngle = Math.atan2(item.northwardWind, item.eastwardWind);
                    var windIconIndex = WeatherOverlay.windIconIndex(windSpeed);
                    var windImage = elem.find('.wind');
                    windImage[0].src = '/static/image/msm/wind/' + windIconIndex + '.png';
                    windImage.css('transform', 'rotate(' + (-windAngle*180/Math.PI) + 'deg)');

                    var temperatureDiv = elem.find('.temperature');
                    temperatureDiv.css('background-color', WeatherOverlay.colorForTemperature(item.airTemperature));
                    temperatureDiv.text(Math.round(item.airTemperature*10)/10);

                    elem.css('background-color', 'rgba(255, 255, 255, ' + item.cloudAmount/100*0.9 + ')');

                    item.elem = elem;
                }

                elem.css('left', nw.x + 'px');
                elem.css('top', nw.y + 'px');

                var cell = elem.find('.cell');
                cell.css('width', width + 'px');
                cell.css('height', height + 'px');
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
                $.getJSON('/msm/surface/' + time.getUTCFullYear() +
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
                            var key = item.latitude + ' ' + item.longitude;
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
                    }
                    else {
                        self._items = {};
                        _.each(items, function(item) {
                            var key = item.latitude + ' ' + item.longitude;
                            self._items[key] = item;
                        });
                        self._div.empty();
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
            this._div.empty();
            this._draw();
        }
    };

    MSMOverlay.SURFACE_LATITUDE_STEP = 0.05;
    MSMOverlay.SURFACE_LONGITUDE_STEP = 0.0625;


    function AMEDASOverlay(flights) {
        WeatherOverlay.call(this, flights);

        this._time = null;
        this._bounds = null;
        this._items = {};
    }

    AMEDASOverlay.prototype = new WeatherOverlay();

    AMEDASOverlay.prototype._getClassName = function() {
        return 'amedas';
    };

    AMEDASOverlay.prototype._draw = function() {
        var map = this.getMap();
        var bounds = map.getBounds();
        var projection = this.getProjection();
        var sw = projection.fromLatLngToDivPixel(bounds.getSouthWest());
        var ne = projection.fromLatLngToDivPixel(bounds.getNorthEast());

        var div = this._div;
        div.css('left', sw.x + 'px');
        div.css('top', ne.y + 'px');
        div.css('width', (ne.x - sw.x) + 'px');
        div.css('height', (sw.y - ne.y) + 'px');

        if (!_.isEmpty(this._items)) {
            var time = this._flights.getCurrentTime() || this._flights.getStartTime();
            var minute = Math.floor((time.getUTCHours()*60 + time.getMinutes())/10)*10;
            _.each(this._items, function(item) {
                var elem = item.elem;

                if (item.time != minute)
                    return;

                if (elem) {
                    if (!elem[0].parentNode)
                        div.append(elem);
                }
                else {
                    elem = $('<div class="item"><div class="cell"><img class="wind"><br><span class="temperature"></span></div></div>');
                    div.append(elem);

                    var windIconIndex = WeatherOverlay.windIconIndex(item.windSpeed);
                    var windImage = elem.find('.wind');
                    var windAngle = AMEDASOverlay.windAngle(item.windDirection);
                    if (!_.isNull(windAngle)) {
                        windImage[0].src = '/static/image/msm/wind/' + windIconIndex + '.png';
                        windImage.css('transform', 'rotate(' + (-windAngle) + 'deg)');
                    }
                    else {
                        windImage.css('display', 'none');
                    }

                    var temperatureDiv = elem.find('.temperature');
                    temperatureDiv.css('background-color', WeatherOverlay.colorForTemperature(item.temperature));
                    temperatureDiv.text(Math.round(item.temperature*10)/10);

                    item.elem = elem;
                }

                var pos = projection.fromLatLngToDivPixel(new LatLng(item.latitude, item.longitude));
                elem.css('left', (pos.x - 14) + 'px');
                elem.css('top', (pos.y - 10) + 'px');
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
                    }
                    else {
                        self._items = {};
                        _.each(items, function(item) {
                            var key = item.latitude + ' ' + item.longitude + ' ' + item.time;
                            self._items[key] = item;
                        });
                        self._div.empty();
                    }
                    self._time = time;
                    self._bounds = bounds;
                    self._draw();
                });
            }
            else if (!WeatherOverlay.tenMinutesEquals(this._time, time)) {
                this._time = time;
                this._div.empty();
                this._draw();
            }
        }
        else {
            this._items = {};
            this._time = null;
            this._bounds = null;
            this._div.empty();
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


    function Graph(flights, graph) {
        var gridCanvas = $('<canvas></canvas>');
        graph.append(gridCanvas);

        var canvas = $('<canvas></canvas>');
        graph.append(canvas);

        var currentCanvas = $('<canvas></canvas>');
        graph.append(currentCanvas);

        this._flights = flights;
        this._width = graph.width();
        this._height = graph.height();

        var self = this;
        _.each(graph.children('canvas'), function(canvas) {
            canvas.width = self._width;
            canvas.height = self._height;
        });

        this._gridContext = gridCanvas[0].getContext('2d');
        this._context = canvas[0].getContext('2d');
        this._context.globalAlpha = 0.3;
        this._currentContext = currentCanvas[0].getContext('2d');

        $(this._flights).on('flight_added', function(event, flight) {
            self._refresh();
            $(flight).on('visible_changed', function() {
                self._refresh();
            });
        });
        $(this._flights).on('flight_removed', function(event, flight) {
            self._refresh();
        });
        $(this._flights).on('currenttime_changed', function(event, time, play) {
            if (!play)
                self._currentContext.clearRect(0, 0, self._width, self._height);
            self._drawFlights(self._currentContext, self._flights.getCurrentTime(), true, play);
        });
    }

    Graph.prototype.getContext = function(time) {
        return this._context;
    };

    Graph.prototype.getCurrentContext = function(time) {
        return this._currentContext;
    };

    Graph.prototype.getX = function(time) {
        return (time - this._flights.getStartTime())/this._flights.getDuration()*(this._width - (Graph.MARGIN.left + Graph.MARGIN.right)) + Graph.MARGIN.left;
    };

    Graph.prototype.getY = function(value) {
        return this._height - (value - this._getMin())/this._getMax()*(this._height - (Graph.MARGIN.top + Graph.MARGIN.bottom)) - Graph.MARGIN.bottom;
    };

    Graph.prototype._refresh = function() {
        this._gridContext.clearRect(0, 0, this._width, this._height);
        this._context.clearRect(0, 0, this._width, this._height);
        this._currentContext.clearRect(0, 0, this._width, this._height);

        if (this._flights.getCount() > 0) {
            this._drawGrid();
            this._drawFlights(this._context, null, false, false);
            this._drawFlights(this._currentContext, null, false, false);
        }
    };

    Graph.prototype._drawGrid = function() {
        var startX = this.getX(this._flights.getStartTime());
        var endX = this.getX(this._flights.getEndTime());
        var lowestY = this.getY(this._getMin());
        var highestY = this.getY(this._getMax());

        var context = this._gridContext;

        context.lineWidth = 0.1;
        context.strokeStyle = 'black';
        context.beginPath();
        context.moveTo(startX, lowestY);
        context.lineTo(startX, highestY);
        context.stroke();

        var time = new Date(this._flights.getStartTime());
        time.setMinutes(Math.floor(time.getMinutes()/10)*10);
        time.setSeconds(0);
        time = time.getTime() + 10*60*1000;
        context.textAlign = 'center';
        for (; time < this._flights.getEndTime().getTime(); time += Graph.TIME_STEP) {
            var t = new Date(time);

            var b = t.getMinutes() == 0 || t.getMinutes() == 30;
            context.strokeStyle = b ? 'black' : 'gray';

            var x = this.getX(time);
            context.beginPath();
            context.moveTo(x, lowestY);
            context.lineTo(x, highestY);
            context.stroke();
            if (b)
                context.fillText(Graph.formatTime(t), x, this._height - Graph.MARGIN.bottom + 12);
        }

        context.textAlign = 'end';
        var step = this._getValueStep();
        for (var value = this._getMin(); value < this._getMax(); value += step) {
            var b = this._isPrimaryValue(value);
            context.strokeStyle = b ? 'black' : 'gray';

            var y = this.getY(value);
            context.beginPath();
            context.moveTo(startX, y);
            context.lineTo(endX, y);
            context.stroke();

            context.fillText(this._formatValue(value), Graph.MARGIN.left - 4, y + 5);
        }

        context.stroke();
    };

    Graph.prototype._drawFlights = function(context, currentTime, withGraphContext, partial) {
    };

    Graph.prototype._getMin = function() {
        return 0;
    };

    Graph.prototype._getMax = function() {
        return 1000;
    };

    Graph.prototype._isPrimaryValue = function(value) {
        return false;
    };

    Graph.prototype._getValueStep = function() {
        return (this._getMax() - this._getMin())/10;
    };

    Graph.prototype._formatValue = function(value) {
        return value;
    };

    Graph.MARGIN = {
        top: 0,
        left: 50,
        bottom: 15,
        right: 0
    };
    Graph.TIME_STEP = 10*60*1000;

    Graph.formatTime = function(time) {
        return _.sprintf("%02d:%02d", time.getHours(), time.getMinutes());
    }


    function AltitudeGraph(flights, canvas) {
        Graph.call(this, flights, canvas);
    }
    inherit(AltitudeGraph, Graph);

    AltitudeGraph.prototype._drawFlights = function(context, currentTime, withGraphContext, partial) {
        var self = this;
        this._flights.eachFlights(function(flight) {
            var graphContext = null;
            if (withGraphContext) {
                flight.__currentAltitudeGraphContext = flight.__currentAltitudeGraphContext || new AltitudeGraphContext();
                graphContext = flight.__currentAltitudeGraphContext;
                if (!partial)
                    graphContext.reset();
            }
            flight.drawAltitude(self, context, currentTime, graphContext);
        });
    }

    AltitudeGraph.prototype._getMax = function() {
        return this._flights.getMaxAltitude();
    };

    AltitudeGraph.prototype._isPrimaryValue = function(value) {
        return value % 1000 === 0;
    };

    AltitudeGraph.prototype._getValueStep = function() {
        return AltitudeGraph.ALTITUDE_STEP;
    };

    AltitudeGraph.prototype._formatValue = function(value) {
        return _.numberFormat(value) + 'm';
    };

    AltitudeGraph.ALTITUDE_STEP = 200;


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


    function SpeedGraph(flights, canvas) {
        Graph.call(this, flights, canvas);
    }
    inherit(SpeedGraph, Graph);

    SpeedGraph.prototype._drawFlights = function(context, currentTime, withGraphContext, partial) {
        var self = this;
        this._flights.eachFlights(function(flight) {
            var graphContext = null;
            if (withGraphContext) {
                flight.__currentSpeedGraphContext = flight.__currentSpeedGraphContext || new SpeedGraphContext();
                graphContext = flight.__currentSpeedGraphContext;
                if (!partial)
                    graphContext.reset();
            }
            flight.drawGroundSpeed(self, context, currentTime, graphContext);
        });
    }

    SpeedGraph.prototype._getMax = function() {
        return 120;
    };

    SpeedGraph.prototype._isPrimaryValue = function(value) {
        return value % 50 === 0;
    };

    SpeedGraph.prototype._getValueStep = function() {
        return SpeedGraph.SPEED_STEP;
    };

    SpeedGraph.prototype._formatValue = function(value) {
        return value + 'km/h';
    };

    SpeedGraph.SPEED_STEP = 10;


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


    function Chart(flights, chart) {
        this._flights = flights;
        this._chart = chart;

        this._chart.html('<table><thead>' +
                           '<tr>' +
                             '<th class="visible"></th>' +
                             '<th class="name">Name</th>' +
                             '<th class="color">Color</th>' +
                             '<th class="latitude">Latitude</th>' +
                             '<th class="longitude">Longitude</th>' +
                             '<th class="altitude">Altitude</th>' +
                             '<th class="ground_speed">Ground Speed</th>' +
                             '<th class="vertical_speed">Vertical Speed</th>' +
                             '<th class="status">Status</th>' +
                             '<th class="ld">L/D</th>' +
                             '<th class="average_climb">Average Climb</th>' +
                           '</tr>' +
                         '</thead><tbody></tbody></table>');

        var self = this;
        var row = _.template('<tr class="flight_<%- getId() %>">' +
                               '<td><input type="checkbox"></td>' +
                               '<td class="name"><%- getName() %></td>' +
                               '<td class="color"><div style="background:<%- getColor() %>"></div></td>' +
                               '<td class="latitude"></td>' +
                               '<td class="longitude"></td>' +
                               '<td class="altitude"></td>' +
                               '<td class="ground_speed"></td>' +
                               '<td class="vertical_speed"></td>' +
                               '<td class="status"></td>' +
                               '<td class="ld"></td>' +
                               '<td class="average_climb"></td>' +
                             '</tr>');
        $(this._flights).on('flight_added', function(event, flight, index) {
            var tr = $(row(flight));
            tr.find('input').on('change', function(event) {
                flight.setVisible(event.target.checked);
            });
            var tbody = self._chart.find('tbody');
            if (tbody.find('tr')[index])
                $(tbody.find('tr')[index]).before(tr);
            else
                tbody.append(tr);
            self._update();
        });
        $(this._flights).on('flight_removed', function(event, flight) {
            var tr = self._chart.find('.flight_' + flight.getId());
            tr.remove();
        });
        $(this._flights).on('currenttime_changed', _.bind(this._update, this));
    }

    Chart.prototype._update = function() {
        var time = this._flights.getCurrentTime();
        var self = this;
        this._flights.eachFlights(function(flight) {
            var position = flight.getPositionAt(time);
            var tr = self._chart.find('.flight_' + flight.getId());
            tr.find('input').prop('checked', flight.isVisible());
            tr.find('td.latitude').text(Chart.formatPosition(position.latitude));
            tr.find('td.longitude').text(Chart.formatPosition(position.longitude));
            tr.find('td.altitude').text(Chart.formatAltitude(position.altitude));
            tr.find('td.ground_speed').text(Chart.formatSpeed(flight.getGroundSpeedAt(time)));
            tr.find('td.vertical_speed').text(Chart.formatVerticalSpeed(flight.getVerticalSpeedAt(time)));
            tr.find('td.status').text(Chart.formatStatus(flight.getStatusAt(time)));
            tr.find('td.ld').text(Chart.formatLD(flight.getLD(time)));
            tr.find('td.average_climb').text(Chart.formatAverageClimb(flight.getAverageClimb(time)));
        });
    };

    Chart.formatPosition = function(position) {
        return _.sprintf('%.5f', position);
    };

    Chart.formatAltitude = function(altitude) {
        return _.numberFormat(altitude) + 'm';
    };

    Chart.formatSpeed = function(speed) {
        return _.sprintf('%.1fkm/h', speed*3600/1000);
    };

    Chart.formatVerticalSpeed = function(speed) {
        return _.sprintf('%.1fm/s', speed);
    };

    Chart.formatStatus = function(status) {
        switch (status) {
        case Flight.STATUS_UNKNOWN:
            break;
        case Flight.STATUS_CIRCLING:
            return 'Circling';
        case Flight.STATUS_GLIDING:
            return 'Gliding';
        default:
            break;
        };
        return '-';
    };

    Chart.formatLD = function(ld) {
        if (!ld)
            return '-';
        else
            return _.sprintf('%.1f', ld);
    };

    Chart.formatAverageClimb = function(averageClimb) {
        if (!averageClimb)
            return '-';
        else
            return this.formatVerticalSpeed(averageClimb);
    };


    function setupLayout(flights, map, sidebar, chart) {
        function layout() {
            map.width($(document).width() - (sidebar.width() + 20));
            var mapPosition = map.position();
            var chartPosition = chart.position();
            map.height(chartPosition.top - mapPosition.top - 10);
        }
        $(flights).on('flight_added', layout);
        $(flights).on('flight_removed', layout);
        $(window).on('resize', layout);
        layout();
    }

    volare.Flights = Flights;
    volare.Flight = Flight;
    volare.Player = Player;
    volare.Map = Map;
    volare.AltitudeGraph = AltitudeGraph;
    volare.SpeedGraph = SpeedGraph;
    volare.Chart = Chart;
    volare.setupLayout = setupLayout;
})();
