define([
    'lodash',
    'angular',
    'text!./name.css',
    'text!./name.html',
    'volare/util'
], function(_, angular, css, template) {
    'use strict';

    var module = angular.module('volare.components.name', [
        'volare.util'
    ]);

    module.directive('volareName', ['util', function(util) {
        util.loadCssInline(css);

        return {
            restrict: 'E',
            replace: true,
            transclude: true,
            template: template,
            scope: {
                name: '@',
                update: '&',
                delete: '&'
            },
            controller: 'NameController',
            link: function(scope, element, attrs) {
                var inputName = element.find('input');

                scope.$watch('editing', function(editing) {
                    if (editing)
                        _.defer(_.bind(inputName.focus, inputName));
                });
            }
        };
    }]);

    module.controller('NameController', ['$scope', function($scope) {
        $scope.editing = false;
        $scope.edit = function() {
            $scope.editing = true;
        };
        $scope.save = function() {
            $scope.editing = false;
            $scope.update({
                name: $scope.name
            });
        };
        $scope.keydown = function(event) {
            if (event.keyCode === 0x0d)
                this.save();
        };
    }]);

    return module;
});
