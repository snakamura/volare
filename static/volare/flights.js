define([
    'lodash',
    'jquery',
    'angular',
    'volare/filters',
    'volare/util',
    'volare/util/file'
], function(_, $, angular) {
    'use strict';

    var flights = angular.module('volare.flights', [
        'volare.filters',
        'volare.util',
        'volare.util.file'
    ]);

    function initFlight(flight) {
        flight.time = new Date(flight.time);
    }

    flights.controller('FlightsController', ['$scope', '$http', function($scope, $http) {
        $scope.flights = [];
        $scope.addFlight = function(flight) {
            initFlight(flight);

            var index = _.sortedIndex(this.flights, flight, function(flight) {
                return -flight.time.getTime();
            });
            this.flights.splice(index, 0, flight);
        };

        $http.get('/flights').success(function(flights) {
            _.each(flights, initFlight);
            $scope.flights = flights;
        });
    }]);

    flights.controller('FlightsUploadController', ['$scope', '$http', 'util', function($scope, $http, util) {
        $scope.addFiles = function(files) {
            var self = this;
            _.each(files, function(file) {
                var reader = new FileReader();
                $(reader).on('loadend', function(event) {
                    $http.post('/flights', {
                        name: util.basename(file.name),
                        igc: reader.result
                    }).success(function(flight) {
                        if (files.length === 1)
                            document.location.href = '/flights/' + flight.id;
                        else
                            self.$parent.addFlight(flight);
                    });
                });
                reader.readAsText(file);
            });
        };
    }]);

    return flights;
});
