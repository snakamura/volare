define([
    'underscore.string',
    'angular',
    'text!./uas.html'
], function(_s, angular, uasTemplate) {
    'use strict';

    var module = angular.module('volare.components.uas', []);

    module.directive('volareUas', [function() {
        return {
            restrict: 'E',
            replace: true,
            template: uasTemplate,
            scope: {
                station: '=',
                date: '='
            },
            controller: 'UASController',
            link: function(scope, element, attrs) {
                scope.$watch('observation', function(observation) {
                    // TODO
                });
            }
        };
    }]);

    module.controller('UASController', ['$scope', '$http', function($scope, $http) {
        var station = $scope.station;
        var date = $scope.date;

        var path = _s.sprintf('/uas/observation/%s/%04d/%02d/%02d/00', station.id,
            date.getUTCFullYear(), date.getUTCMonth() + 1, date.getUTCDate());
        $http.get(path).success(function(observation) {
            $scope.observation = observation;
        });
    }]);

    return module;
});
