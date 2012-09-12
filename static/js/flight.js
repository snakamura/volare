$(function() {
    _.mixin(_.string.exports());

    var flights = new volare.Flights();
    var player = new volare.Player(flights, $('#player'));
    var map = new volare.Map(flights, $('#map'));
    var altitudeGraph = new volare.AltitudeGraph(flights, $('#altitude'));
    var speedGraph = new volare.SpeedGraph(flights, $('#speed'));
    var chart = new volare.Chart(flights, $('#chart'));

    $.getJSON('', function(flight) {
        flights.addFlight(new volare.Flight(flight, 'red'));
    });
    $.getJSON('/flights/1', function(flight) {
        flights.addFlight(new volare.Flight(flight, 'blue'));
    });
});
