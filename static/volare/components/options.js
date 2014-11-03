define([
    'lodash',
    'jquery',
    'angular',
    'volare/volare',
    'volare/common',
    'text!./options.css',
    'text!./options.html'
], function(_, $, angular, volare, common, css, template) {
    'use strict';

    common.loadCssInline(css);

    var options = angular.module('volare.components.options', []);

    options.directive('volareOptions', [function() {
        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                modelFlights: '=flights',
                map: '='
            },
            controller: ['$scope', function($scope) {
                var modelFlights = $scope.modelFlights;
                var map = $scope.map;

                $scope.TrackType = volare.Map.TrackType;

                $scope.$watch('trackType', function(trackType) {
                    map.setTrackType(_.parseInt(trackType));
                });
                $scope.$watch('thinOut', function(thinOut) {
                    modelFlights.setInterval(thinOut ? 10 : 0);
                });

                function updateTrackType() {
                    $scope.trackType = map.getTrackType();
                }

                function updateThinOut() {
                    $scope.thinOut = modelFlights.getInterval() ? 1 : 0;
                }

                $(map).on('trackType_changed', updateTrackType);
                $(modelFlights).on('interval_changed', updateThinOut);

                updateTrackType();
                updateThinOut();
            }]
        };
    }]);

    return options;
});
