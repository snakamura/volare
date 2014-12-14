/* global station, date */

define([
    'lodash',
    'underscore.string',
    'angular',
    'text!./uas.css',
    'angular-ui-bootstrap',
    'volare/components/uas',
    'volare/filters',
    'volare/util'
], function(_, _s, angular, css) {
    'use strict';

    var module = angular.module('volare.uas', [
        'ui.bootstrap',
        'volare.components.uas',
        'volare.filters',
        'volare.util'
    ]);

    module.controller('UASController', ['$scope', '$document', 'util', function($scope, $document, util) {
        util.loadCssInline(css);

        $scope.station = station;
        $scope.date = date;
        $scope.selectedStation = station;
        $scope.selectedDate = date;
        $scope.selectedTime = date.getUTCHours();

        function update() {
            var currentStation = $scope.station;
            var currentDate = $scope.date;
            var station = $scope.selectedStation;
            var date = $scope.selectedDate;
            var time = $scope.selectedTime;
            if (currentStation.id !== station.id ||
                currentDate.getUTCFullYear() !== date.getUTCFullYear() ||
                currentDate.getUTCMonth() !== date.getUTCMonth() ||
                currentDate.getUTCDate() !== date.getUTCDate() ||
                currentDate.getUTCHours() !== time)
                $document[0].location.href = _s.sprintf('/uas/%d/%04d/%02d/%02d/%02d', station.id, date.getUTCFullYear(), date.getUTCMonth() + 1, date.getUTCDate(), time);
        }
        $scope.$watch('selectedStation', update);
        $scope.$watch('selectedDate', update);
        $scope.$watch('selectedTime', update);
    }]);

    return module;
});
