define([
    'lodash',
    'jquery',
    'angular',
    'volare/util',
    'volare/util/file'
], function(_, $, angular) {
    'use strict';

    var module = angular.module('volare.waypoints', [
        'volare.util',
        'volare.util.file'
    ]);

    module.controller('WaypointsController', ['$scope', '$http', function($scope, $http) {
        $scope.waypoints = [];
        $scope.addWaypoint = function(waypoint) {
            var index = _.sortedIndexBy(this.waypoints, waypoint, function(waypoint) {
                return waypoint.name;
            });
            this.waypoints.splice(index, 0, waypoint);
        };

        $http.get('/waypoints').success(function(waypoints) {
            $scope.waypoints = waypoints;
        });
    }]);

    module.controller('WaypointsUploadController', ['$scope', '$http', '$document', 'util', function($scope, $http, $document, util) {
        $scope.addFiles = function(files) {
            var self = this;
            _.each(files, function(file) {
                var reader = new FileReader();
                $(reader).on('loadend', function(event) {
                    $http.post('/waypoints', {
                        name: util.basename(file.name),
                        wpt: reader.result
                    }).success(function(waypoint) {
                        if (files.length === 1)
                            $document[0].location.href = '/waypoints/' + waypoint.id;
                        else
                            self.addWaypoint(waypoint);
                    });
                });
                reader.readAsText(file);
            });
        };
    }]);

    return module;
});
