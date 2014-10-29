define(['lodash',
        'jquery',
        'angular',
        'volare/common',
        'volare/file'],
       function(_, $, angular, common, __) {
    'use strict';

    var waypoints = angular.module('volare.waypoints', ['volare.file']);

    waypoints.controller('WaypointsController', ['$scope', '$http', function($scope, $http) {
        $scope.waypoints = [];
        $scope.addWaypoint = function(waypoint) {
            var index = _.sortedIndex(this.waypoints, waypoint, function(waypoint) {
                return waypoint.name;
            });
            this.waypoints.splice(index, 0, waypoint);
        };

        $http.get('/waypoints').success(function(waypoints) {
            $scope.waypoints = waypoints;
        });
    }]);

    waypoints.controller('WaypointsUploadController', ['$scope', '$http', function($scope, $http) {
        $scope.addFiles = function(files) {
            var self = this;
            _.each(files, function(file) {
                var reader = new FileReader();
                $(reader).on('loadend', function(event) {
                    $http.post('/waypoints', {
                        name: common.basename(file.name),
                        wpt: reader.result
                    }).success(function(waypoint) {
                        if (files.length === 1)
                            document.location.href = '/waypoints/' + waypoint.id;
                        else
                            self.$parent.addWaypoint(waypoint);
                    });
                });
                reader.readAsText(file);
            });
        };
    }]);

    return waypoints;
});
