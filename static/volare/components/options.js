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

    var module = angular.module('volare.components.options', [
        'volare.components.map',
        'volare.util'
    ]);

    module.directive('volareOptions', ['util', function(util) {
        util.loadCssInline(css);

        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                flights: '=',
                mapTrackType: '=trackType'
            },
            controller: 'OptionsController'
        };
    }]);

    module.controller('OptionsController', ['$scope', 'Map', function($scope, Map) {
        var flights = $scope.flights;

        $scope.TrackType = Map.TrackType;

        $scope.$watch('trackType', function(trackType) {
            $scope.mapTrackType = _.parseInt(trackType);
        });
        $scope.$watch('mapTrackType', function(mapTrackType) {
            $scope.trackType = mapTrackType;
        });
        $scope.trackType = $scope.mapTrackType;

        $scope.$watch('thinOut', function(thinOut) {
            flights.setInterval(thinOut ? 10 : 0);
        });
        function updateThinOut() {
            $scope.thinOut = flights.getInterval() ? 1 : 0;
        }
        $(flights).on('interval_changed', updateThinOut);
        updateThinOut();
    }]);

    return module;
});
