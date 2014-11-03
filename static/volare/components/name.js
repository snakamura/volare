define([
    'lodash',
    'angular',
    'volare/common',
    'text!./name.css',
    'text!./name.html'
], function(_, angular, common, css, template) {
    'use strict';

    common.loadCssInline(css);

    var name = angular.module('volare.components.name', []);

    name.directive('volareName', [function() {
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
            controller: function($scope) {
                $scope.editing = false;
                $scope.edit = function() {
                    $scope.editing = true;
                };
                $scope.save = function() {
                    $scope.editing = false;
                    $scope.update({
                        $name: $scope.name
                    });
                };
                $scope.keydown = function(event) {
                    if (event.keyCode === 0x0d)
                        this.save();
                };
            },
            link: function(scope, element, attrs) {
                var inputName = element.find('input');

                scope.$watch('editing', function(editing) {
                    if (editing)
                        _.defer(_.bind(inputName.focus, inputName));
                });
            }
        };
    }]);

    return name;
});
