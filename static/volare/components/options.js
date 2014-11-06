define([
    'lodash',
    'jquery',
    'angular',
    'text!./options.css',
    'text!./options.html',
    'volare/components/map',
    'volare/util'
], function(_, $, angular, css, template) {
    'use strict';

    var options = angular.module('volare.components.options', [
        'volare.components.map',
        'volare.util'
    ]);

    options.directive('volareOptions', ['util', function(util) {
        util.loadCssInline(css);

        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                flights: '=',
                map: '='
            },
            controller: ['$scope', 'trackType', function($scope, trackType) {
                var flights = $scope.flights;

                $scope.TrackType = trackType;
                $scope.$watch('map', function(map) {
                    $scope.$watch('trackType', function(trackType) {
                        map.setTrackType(_.parseInt(trackType));
                    });

                    function updateTrackType() {
                        $scope.trackType = map.getTrackType();
                    }

                    $(map).on('trackType_changed', updateTrackType);

                    updateTrackType();
                });
                $scope.$watch('thinOut', function(thinOut) {
                    flights.setInterval(thinOut ? 10 : 0);
                });

                function updateThinOut() {
                    $scope.thinOut = flights.getInterval() ? 1 : 0;
                }
                $(flights).on('interval_changed', updateThinOut);
                updateThinOut();
            }]
        };
    }]);

    return options;
});
