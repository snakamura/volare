var volare = volare || {};

(function() {
    var LatLng = google.maps.LatLng;
    var LatLngBounds = google.maps.LatLngBounds;

    function Flights() {
        this.flights = [];

        this.start = null;
        this.end = null;
        this.duration = 0;
        this.maxAltitude = 0;

        this.currentTime = null;
    }

    Flights.prototype.getFlights = function() {
        return this.flights;
    };

    Flights.prototype.addFlight = function(flight) {
        this.flights.push(flight);

        var start = flight.getStart();
        var end = flight.getEnd();

        this.start = this.start == null || this.start > start ? start : this.start;
        this.end = this.end == null || this.end < end ? end : this.end;
        this.duration = this.end - this.start;
        this.maxAltitude = Math.max(this.maxAltitude, flight.getMaxAltitude() + 100);

        $(this).trigger('flight_added', flight);
    };

    Flights.prototype.setCurrentTime = function(time) {
        this.currentTime = time;
        $(this).trigger('currenttime_changed', time);
    };


    function Flight(flight, color) {
        this.flight = flight;
        this.color = color;
        this.polyline = null;
    }

    Flight.prototype.getStart = function() {
        return new Date(this.flight.records[0].time);
    };

    Flight.prototype.getEnd = function() {
        return new Date(this.flight.records[this.flight.records.length - 1].time);
    };

    Flight.prototype.getBounds = function() {
        return new LatLngBounds(new LatLng(this.flight.maxLatitude, this.flight.minLongitude),
                                new LatLng(this.flight.minLatitude, this.flight.maxLongitude));
    };

    Flight.prototype.getMaxAltitude = function() {
        return this.flight.maxAltitude;
    };

    Flight.prototype.setPolyline = function(map, currentTime) {
        if (this.polyline) {
            this.polyline.setMap(null);
            this.polyline = null;
        }

        var records = this.flight.records;
        if (currentTime) {
            var startTime = new Date(currentTime.getTime() - 10*60*1000);
            var start = -1;
            var end = 0;
            for (var n = 0; n < records.length; ++n) {
                var record = records[n];
                var time = new Date(record.time);
                if (start == -1 && time > startTime)
                    start = n;
                if (time > currentTime) {
                    end = n;
                    break;
                }
            }
            records = records.slice(start, end + 1);
        }

        var path = new google.maps.MVCArray(_.map(records, function(record) {
            return new LatLng(record.latitude, record.longitude);
        }));
        this.polyline = new google.maps.Polyline({
            map: map,
            path: path,
            strokeColor: this.color
        });
    };

    Flight.prototype.drawAltitude = function(graph, currentTime) {
        var context = graph.context;

        context.strokeStyle = this.color;
        context.lineWidth = 2;

        context.beginPath();
        context.moveTo(graph.getX(new Date(this.flight.records[0].time)),
                       graph.getY(this.flight.records[0].altitude));
        _.every(this.flight.records, function(record) {
            var time = new Date(record.time);
            if (currentTime != null && time > currentTime)
                return false;

            context.lineTo(graph.getX(time),
                           graph.getY(record.altitude));

            return true;
        });
        context.stroke();
    };


    function Map(flights, map) {
        this.flights = flights;
        this.map = new google.maps.Map(map[0], {
            mapTypeId: google.maps.MapTypeId.TERRAIN
        });

        var self = this;
        $(this.flights).on('flight_added', function(event, flight) {
            self.map.fitBounds(flight.getBounds());
            flight.setPolyline(self.map);
        });
        $(this.flights).on('currenttime_changed', function(event, time) {
            _.each(self.flights.getFlights(), function(flight) {
                flight.setPolyline(self.map, time);
            });
        });
    }


    function AltitudeGraph(flights, canvas) {
        this.flights = flights;

        this.canvas = canvas;
        this.width = canvas.width();
        this.height = canvas.height();

        var canvasElem = canvas[0];
        canvasElem.width = this.width;
        canvasElem.height = this.height;

        this.context = canvasElem.getContext('2d');

        var self = this;
        $(this.flights).on('flight_added', function(event, flight) {
            self._refresh();
        });
        $(this.flights).on('currenttime_changed', function(event, time) {
            self._refresh();
        });
    }

    AltitudeGraph.prototype.getX = function(time) {
        return (time - this.flights.start)/this.flights.duration*(this.width - (AltitudeGraph.MARGIN.left + AltitudeGraph.MARGIN.right)) + AltitudeGraph.MARGIN.left;
    };

    AltitudeGraph.prototype.getY = function(altitude) {
        return this.height - altitude/this.flights.maxAltitude*(this.height - (AltitudeGraph.MARGIN.top + AltitudeGraph.MARGIN.bottom)) - AltitudeGraph.MARGIN.bottom;
    };

    AltitudeGraph.prototype._refresh = function() {
        this.context.clearRect(0, 0, this.width, this.height);

        this._drawGrid();
        this._drawFlights();
    };

    AltitudeGraph.prototype._drawGrid = function() {
        var startX = this.getX(this.flights.start);
        var endX = this.getX(this.flights.end);
        var lowestY = this.getY(0);
        var highestY = this.getY(this.flights.maxAltitude);

        var context = this.context;

        context.strokeStyle = 'gray';
        context.lineWidth = 0.5;

        context.beginPath();

        context.moveTo(startX, lowestY);
        context.lineTo(startX, highestY);

        var time = new Date(this.flights.start);
        time.setMinutes(Math.floor(time.getMinutes()/10)*10);
        time.setSeconds(0);
        time = time.getTime() + 10*60*1000;
        context.textAlign = 'center';
        for (; time < this.flights.end.getTime(); time += AltitudeGraph.TIME_STEP) {
            var x = this.getX(time);
            context.moveTo(x, lowestY);
            context.lineTo(x, highestY);
            context.fillText(AltitudeGraph.formatTime(time), x,
                             this.height - AltitudeGraph.MARGIN.bottom + 12);
        }

        context.textAlign = 'end';
        for (var altitude = 0; altitude < this.flights.maxAltitude; altitude += AltitudeGraph.ALTITUDE_STEP) {
            var y = this.getY(altitude);
            context.moveTo(startX, y);
            context.lineTo(endX, y);
            context.fillText(AltitudeGraph.formatAltitude(altitude),
                             AltitudeGraph.MARGIN.left - 4, y + 5);
        }

        context.stroke();
    };

    AltitudeGraph.prototype._drawFlights = function() {
        var self = this;
        _.each(this.flights.getFlights(), function(flight) {
            flight.drawAltitude(self, self.flights.currentTime);
        });
    }

    AltitudeGraph.MARGIN = {
        top: 0,
        left: 50,
        bottom: 15,
        right: 0
    };
    AltitudeGraph.TIME_STEP = 10*60*1000;
    AltitudeGraph.ALTITUDE_STEP = 200;

    AltitudeGraph.formatTime = function(time) {
        var date = new Date(time);
        return _.sprintf("%02d:%02d", date.getHours(), date.getMinutes());
    }

    AltitudeGraph.formatAltitude = function(altitude) {
        return altitude + 'm';
    }


    volare.Flights = Flights;
    volare.Flight = Flight;
    volare.Map = Map;
    volare.AltitudeGraph = AltitudeGraph;
})();
