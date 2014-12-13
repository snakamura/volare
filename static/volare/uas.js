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
    }]);

    return module;
});
