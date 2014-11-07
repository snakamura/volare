define([
    'lodash',
    'jquery',
    'angular',
    'text!./waypoint.css',
    'text!./waypoint.html',
    'volare/model',
    'volare/util'
], function(_, $, angular, css, template) {
    'use strict';

    var module = angular.module('volare.components.waypoint', [
        'volare.model',
        'volare.util'
    ]);

    module.directive('volareWaypoint', ['util', function(util) {
        util.loadCssInline(css);

        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                mapWaypoint: '=waypoint'
            },
            controller: 'WaypointController'
        };
    }]);

    module.controller('WaypointController', ['$scope', '$http', 'model', function($scope, $http, model) {
        $scope.waypoints = [];
        $scope.waypoint = null;

        $scope.$watch('waypoint', function(waypoint) {
            if (waypoint) {
                $http.get('/waypoints/' + waypoint.id).success(function(waypoint) {
                    $scope.mapWaypoint = model.Waypoint.wrap(waypoint);
                });
            }
            else {
                $scope.mapWaypoint = null;
            }
        });
        $scope.$watch('mapWaypoint', function(mapWaypoint) {
            $scope.waypoint = mapWaypoint ? _.find($scope.waypoints, function(waypoint) {
                return waypoint.id === mapWaypoint.getId();
            }) : null;
        });

        $http.get('/waypoints').success(function(waypoints) {
            $scope.waypoints = waypoints;
        });
    }]);

    return module;
});
