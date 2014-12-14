define([
    'lodash',
    'underscore.string',
    'angular',
    'angular-ui-bootstrap',
    'volare/filters'
], function(_, _s, angular) {
    'use strict';

    var module = angular.module('volare.uasstations', [
        'ui.bootstrap',
        'volare.filters'
    ]);

    module.controller('UASStationsController', ['$scope', '$http', '$document', function($scope, $http, $document) {
        $scope.date = new Date();
        $scope.time = 0;
        $scope.click = function(station) {
            var date = $scope.date;
            $document[0].location.href = _s.sprintf('/uas/%d/%04d/%02d/%02d/%02d', station.id, date.getFullYear(), date.getMonth() + 1, date.getDate(), $scope.time);
        };

        $http.get('').success(function(stations) {
            $scope.stations = stations;
        });
    }]);

    module.controller('UASStationsDateController', ['$scope', function($scope) {
        $scope.options = {
            showWeeks: false
        };
        $scope.toggle = function($event) {
            $event.preventDefault();
            $event.stopPropagation();
            $scope.opened = !$scope.opened;
        };
    }]);

    module.controller('UASStationsTimeController', ['$scope', function($scope) {
        $scope.toggle = function($event) {
            $event.preventDefault();
            $event.stopPropagation();
            $scope.opened = !$scope.opened;
        };
        $scope.select = function(time) {
            $scope.$parent.time = time;
        }
    }]);

    return module;
});
