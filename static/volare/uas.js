/* global station, date */

define([
    'lodash',
    'angular',
    'text!./uas.css',
    'volare/components/uas',
    'volare/util'
], function(_, angular, css) {
    'use strict';

    var module = angular.module('volare.uas', [
        'volare.components.uas',
        'volare.util'
    ]);

    module.controller('UASController', ['$scope', 'util', function($scope, util) {
        util.loadCssInline(css);

        $scope.station = station;
        $scope.date = date;

        $scope.pressure = '700';
        function updateRange() {
            if ($scope.pressure === '700') {
                $scope.range = {
                    pressure: {
                        min: _.parseInt($scope.pressure)
                    },
                    temperature: {
                        min: -20
                    }
                };
            }
            else {
                delete $scope.range;
            }
        }
        $scope.$watch('pressure', updateRange);
        updateRange();
    }]);

    return module;
});
