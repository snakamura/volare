define([
    'angular'
], function(angular) {
    'use strict';

    var module = angular.module('volare.workspaces', []);

    module.controller('WorkspacesController', ['$scope', '$http', function($scope, $http) {
        $scope.workspaces = [];

        $http.get('/workspaces').success(function(workspaces) {
            $scope.workspaces = workspaces;
        });
    }]);

    return module;
});
