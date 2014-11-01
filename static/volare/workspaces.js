define([
    'angular'
], function(angular) {
    'use strict';

    var workspaces = angular.module('volare.workspaces', []);

    workspaces.controller('WorkspacesController', ['$scope', '$http', function($scope, $http) {
        $scope.workspaces = [];

        $http.get('/workspaces').success(function(workspaces) {
            $scope.workspaces = workspaces;
        });
    }]);

    return workspaces;
});
