define([
    'jquery',
    'angular'
], function($, angular) {
    'use strict';

    var module = angular.module('volare.util.layout', []);

    module.directive('volareFill', ['$window', '$document', function($window, $document) {
        return {
            link: function(scope, element, attrs) {
                function layout() {
                    element.width($document.width());
                    var mapPosition = element.position();
                    element.height($document.height() - mapPosition.top);
                }
                $($window).on('resize', layout);
                layout();
            }
        };
    }]);

    return module;
});
