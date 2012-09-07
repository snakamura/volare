var volare = volare || {};

(function() {
    function Flight(records, color) {
        this.records = records;
        this.color = color;
    }

    Flight.prototype.drawAltitude = function(graph) {
        var context = graph.context;

        context.strokeStyle = this.color;
        context.lineWidth = 2;

        context.beginPath();
        context.moveTo(graph.getX(new Date(records[0].time)),
                       graph.getY(records[0].altitude));
        _.each(this.records, function(record) {
            context.lineTo(graph.getX(new Date(record.time)),
                           graph.getY(record.altitude));
        });
        context.stroke();
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

        var records = flight.records;
        this.start = new Date(records[0].time);
        this.end = new Date(records[records.length - 1].time);
        this.duration = this.end - this.start;
        this.maxAltitude = _.max(records, function(record) {
            return record.altitude;
        }).altitude + 100;

        this._drawGrid();
        this._drawFlights();
    };

    AltitudeGraph.prototype.getX = function(time) {
        return (time - this.start)/this.duration*(this.width - (AltitudeGraph.MARGIN.left + AltitudeGraph.MARGIN.right)) + AltitudeGraph.MARGIN.left;
    };

    AltitudeGraph.prototype.getY = function(altitude) {
        return this.height - altitude/this.maxAltitude*(this.height - (AltitudeGraph.MARGIN.top + AltitudeGraph.MARGIN.bottom)) - AltitudeGraph.MARGIN.bottom;
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
    volare.AltitudeGraph = AltitudeGraph;
})();
