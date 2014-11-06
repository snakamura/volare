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

    module.directive('volareWaypoint', ['$http', 'model', 'util', function($http, model, util) {
        util.loadCssInline(css);

        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                map: '='
            },
            controller: ['$scope', function($scope) {
                $scope.waypoints = [];
                $scope.waypoint = null;

                $scope.$watch('map', function(map) {
                    $scope.$watch('waypoint', function(waypoint) {
                        if (waypoint) {
                            $http.get('/waypoints/' + waypoint.id).success(function(waypoint) {
                                map.setWaypoint(model.Waypoint.wrap(waypoint));
                            });
                        }
                        else {
                            map.setWaypoint(null);
                        }
                    });
                    $(map).on('waypoint_changed', function(event, waypoint) {
                        $scope.waypoint = waypoint ? _.find($scope.waypoints, function(w) {
                            return w.id === waypoint.getId();
                        }) : null;
                    });
                });

                $http.get('/waypoints').success(function(waypoints) {
                    $scope.waypoints = waypoints;
                });
            }]
        };
    }]);

    return module;
});
