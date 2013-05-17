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
        this._polyline = null;
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
        var range = this._getRecordIndexRange(time, Flight.STATUS_SAMPLING_DURATION);
        if (!range)
            return Flight.STATUS_UNKNOWN;

        var directions = _.map(range, _.bind(this._getDirection, this));
        if (directions.length < 2)
            return Flight.STATUS_UNKNOWN;

        var gliding = true;
        for (var n = 1; n < directions.length && gliding; ++n) {
            gliding = Math.cos(directions[n] - directions[n - 1]) >= 0;
        }
        if (gliding)
            return Flight.STATUS_GLIDING;

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

        return Flight.STATUS_UNKNOWN;
    };

    Flight.prototype.setPolyline = function(map, currentTime) {
        if (this._visible) {
            var records = this._records;
            if (currentTime) {
                var start = this._getRecordIndexAt(new Date(currentTime.getTime() - Flight.TRACK_DURATION*1000));
                var end = this._getRecordIndexAt(currentTime);
                records = records.slice(start, end);
            }

            if (this._polyline) {
                var path = this._polyline.getPath();
                path.clear();
                _.each(records, function(record) {
                    path.push(new LatLng(record.latitude, record.longitude));
                });
            }
            else {
                var path = new google.maps.MVCArray(_.map(records, function(record) {
                    return new LatLng(record.latitude, record.longitude);
                }));
                this._polyline = new google.maps.Polyline({
                    map: map,
                    path: path,
                    strokeColor: this._color
                });
            }
        }
        else {
            this.removePolyline();
        }
    };

    Flight.prototype.removePolyline = function() {
        if (this._polyline) {
            this._polyline.setMap(null);
            this._polyline = null;
        }
    };

    Flight.prototype.drawAltitude = function(graph, currentTime, partial) {
        if (this._visible) {
            var context = graph.getContext();

            context.strokeStyle = this._color;
            context.lineWidth = 2;

            var startIndex = 0;
            var startTime = null;
            var startAltitude = 0;
            if (partial) {
                startIndex = this.lastDrawnAltitudeIndex;
                startTime = new Date(this.lastDrawnAltitudeTime);
                startAltitude = this.lastDrawnAltitude;
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

            this.lastDrawnAltitude = lastAltitude;
            this.lastDrawnAltitudeIndex = n > 0 ? n - 1 : n;
            this.lastDrawnAltitudeTime = lastTime.getTime();
        }
    };

    Flight.prototype.drawGroundSpeed = function(graph, currentTime, partial) {
        if (this._visible) {
            var context = graph.getContext();

            context.strokeStyle = this._color;
            context.lineWidth = 2;

            var step = 20*1000;
            var startTime = null;
            var endTime = null;
            if (partial) {
                if (currentTime.getTime() >= this.lastDrawnGroundSpeedTime + step) {
                    startTime = new Date(this.lastDrawnGroundSpeedTime);
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

                this.lastDrawnGroundSpeedTime = time - step;
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

        var self = this;
        $(this._flights).on('flight_added', function(event, flight) {
            self._map.fitBounds(self._flights.getBounds());
            flight.setPolyline(self._map);
            $(flight).on('visible_changed', function() {
                flight.setPolyline(self._map, self._flights.getCurrentTime());
            });
        });
        $(this._flights).on('flight_removed', function(event, flight) {
            flight.removePolyline();
        });
        $(this._flights).on('currenttime_changed', function(event, time) {
            self._flights.eachFlights(function(flight) {
                flight.setPolyline(self._map, time);
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


    function Graph(flights, canvas) {
        this._flights = flights;
        this._canvas = canvas;
        this._width = canvas.width();
        this._height = canvas.height();

        var canvasElem = this._canvas[0];
        canvasElem.width = this._width;
        canvasElem.height = this._height;

        this._context = canvasElem.getContext('2d');

        var self = this;
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
            if (play)
                self._drawFlights(true);
            else
                self._refresh();
        });
    }

    Graph.prototype.getContext = function() {
        return this._context;
    };

    Graph.prototype.getX = function(time) {
        return (time - this._flights.getStartTime())/this._flights.getDuration()*(this._width - (Graph.MARGIN.left + Graph.MARGIN.right)) + Graph.MARGIN.left;
    };

    Graph.prototype.getY = function(value) {
        return this._height - (value - this._getMin())/this._getMax()*(this._height - (Graph.MARGIN.top + Graph.MARGIN.bottom)) - Graph.MARGIN.bottom;
    };

    Graph.prototype._refresh = function() {
        this._context.clearRect(0, 0, this._width, this._height);

        if (this._flights.getCount() > 0) {
            this._drawGrid();
            this._drawFlights();
        }
    };

    Graph.prototype._drawGrid = function() {
        var startX = this.getX(this._flights.getStartTime());
        var endX = this.getX(this._flights.getEndTime());
        var lowestY = this.getY(this._getMin());
        var highestY = this.getY(this._getMax());

        var context = this._context;

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

    Graph.prototype._drawFlights = function() {
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

    AltitudeGraph.prototype._drawFlights = function(partial) {
        var self = this;
        this._flights.eachFlights(function(flight) {
            flight.drawAltitude(self, self._flights.getCurrentTime(), partial);
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


    function SpeedGraph(flights, canvas) {
        Graph.call(this, flights, canvas);
    }
    inherit(SpeedGraph, Graph);

    SpeedGraph.prototype._drawFlights = function(partial) {
        var self = this;
        this._flights.eachFlights(function(flight) {
            flight.drawGroundSpeed(self, self._flights.getCurrentTime(), partial);
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


    function Chart(flights, chart) {
        this._flights = flights;
        this._chart = chart;

        this._chart.html('<table><thead>' +
                           '<tr>' +
                             '<th></th>' +
                             '<th>Name</th>' +
                             '<th>Color</th>' +
                             '<th>Latitude</th>' +
                             '<th>Longitude</th>' +
                             '<th>Altitude</th>' +
                             '<th>Ground Speed</th>' +
                             '<th>Vertical Speed</th>' +
                             '<th>Status</th>' +
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
        return 'Unknown';
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
