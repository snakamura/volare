var volare = volare || {};

(function() {
    var LatLng = google.maps.LatLng;
    var LatLngBounds = google.maps.LatLngBounds;

    function Flight(flight, color) {
        this.flight = flight;
        this.color = color;
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

    Flight.prototype.getPolyline = function() {
        var path = new google.maps.MVCArray(_.map(this.flight.records, function(record) {
            return new LatLng(record.latitude, record.longitude);
        }));
        return new google.maps.Polyline({
            path: path,
            strokeColor: this.color
        });
    };

    Flight.prototype.drawAltitude = function(graph) {
        var context = graph.context;

        context.strokeStyle = this.color;
        context.lineWidth = 2;

        context.beginPath();
        context.moveTo(graph.getX(new Date(this.flight.records[0].time)),
                       graph.getY(this.flight.records[0].altitude));
        _.each(this.flight.records, function(record) {
            context.lineTo(graph.getX(new Date(record.time)),
                           graph.getY(record.altitude));
        });
        context.stroke();
    };


    function Map(map) {
        this.map = new google.maps.Map(map[0], {
            mapTypeId: google.maps.MapTypeId.TERRAIN
        });
    }

    Map.prototype.addFlight = function(flight) {
        this.map.fitBounds(flight.getBounds());
        flight.getPolyline().setMap(this.map);
    };


    function AltitudeGraph(canvas) {
        this.canvas = canvas;
        this.width = canvas.width();
        this.height = canvas.height();

        var canvasElem = canvas[0];
        canvasElem.width = this.width;
        canvasElem.height = this.height;

        this.context = canvasElem.getContext('2d');

        this.flights = [];
        this.start = null;
        this.end = null;
        this.duration = 0;
        this.maxAltitude = 0;
    }

    AltitudeGraph.prototype.addFlight = function(flight) {
        this.flights.push(flight);

        var start = flight.getStart();
        var end = flight.getEnd();

        this.start = this.start == null || this.start > start ? start : this.start;
        this.end = this.end == null || this.end < end ? end : this.end;
        this.duration = this.end - this.start;
        this.maxAltitude = Math.max(this.maxAltitude, flight.getMaxAltitude() + 100);

        this._refresh();
    };

    AltitudeGraph.prototype.getX = function(time) {
        return (time - this.start)/this.duration*(this.width - (AltitudeGraph.MARGIN.left + AltitudeGraph.MARGIN.right)) + AltitudeGraph.MARGIN.left;
    };

    AltitudeGraph.prototype.getY = function(altitude) {
        return this.height - altitude/this.maxAltitude*(this.height - (AltitudeGraph.MARGIN.top + AltitudeGraph.MARGIN.bottom)) - AltitudeGraph.MARGIN.bottom;
    };

    AltitudeGraph.prototype._refresh = function() {
        this.context.clearRect(0, 0, this.width, this.height);

        this._drawGrid();
        this._drawFlights();
    };

    AltitudeGraph.prototype._drawGrid = function() {
        var startX = this.getX(this.start);
        var endX = this.getX(this.end);
        var lowestY = this.getY(0);
        var highestY = this.getY(this.maxAltitude);

        var context = this.context;

        context.strokeStyle = 'gray';
        context.lineWidth = 0.5;

        context.beginPath();

        context.moveTo(startX, lowestY);
        context.lineTo(startX, highestY);

        var time = new Date(this.start);
        time.setMinutes(Math.floor(time.getMinutes()/10)*10);
        time.setSeconds(0);
        time = time.getTime() + 10*60*1000;
        context.textAlign = 'center';
        for (; time < this.start.getTime() + this.duration; time += AltitudeGraph.TIME_STEP) {
            var x = this.getX(time);
            context.moveTo(x, lowestY);
            context.lineTo(x, highestY);
            context.fillText(AltitudeGraph.formatTime(time), x,
                             this.height - AltitudeGraph.MARGIN.bottom + 12);
        }

        context.textAlign = 'end';
        for (var altitude = 0; altitude < this.maxAltitude; altitude += AltitudeGraph.ALTITUDE_STEP) {
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
        _.each(this.flights, function(flight) {
            flight.drawAltitude(self);
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


    volare.Flight = Flight;
    volare.Map = Map;
    volare.AltitudeGraph = AltitudeGraph;
})();
