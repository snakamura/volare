define([
    'angular',
    'text!./file.css',
    'text!./file.html',
    'volare/util'
], function(angular, css, template) {
    'use strict';

    var file = angular.module('volare.util.file', [
        'volare.util'
    ]);

    file.directive('volareFile', ['$parse', 'util', function($parse, util) {
        util.loadCssInline(css);

        return {
            restrict: 'E',
            replace: true,
            transclude: true,
            template: template,
            link: function(scope, element, attrs) {
                var changeHandler = $parse(attrs.change);

                var input = element.find('input');
                input.attr('accept', attrs.accept);
                input.on('change', function(event) {
                    changeHandler(scope, {
                        $event: event,
                        $files: event.target.files
                    });
                });
            }
        };
    }]);

    file.directive('volareDrop', ['$parse', function($parse) {
        return {
            restrict: 'A',
            link: function(scope, element, attrs) {
                var handler = $parse(attrs.volareDrop);
                element.on('dragenter', function(event) {
                    event.preventDefault();
                    event.originalEvent.dataTransfer.dropEffect = 'copy';
                });
                element.on('dragleave', function(event) {
                    event.preventDefault();
                });
                element.on('dragover', function(event) {
                    event.preventDefault();
                });
                element.on('drop', function(event) {
                    event.preventDefault();
                    handler(scope, {
                        $event: event,
                        $files: event.originalEvent.dataTransfer.files
                    });
                });
            }
        };
    }]);

    return file;
});
