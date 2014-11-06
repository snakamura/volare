define([
    'lodash',
    'jquery',
    'angular',
    'text!./layout.css',
    'volare/util'
], function(_, $, angular, css) {
    'use strict';

    var module = angular.module('volare.layout', [
        'volare.util'
    ]);

    module.factory('layout', ['$window', '$document', 'util', function($window, $document, util) {
        return {
            setupLayout: function(flights, map, sidebar, chart) {
                util.loadCssInline(css);

                function layout() {
                    _.defer(function() {
                        map.width($document.width() - (sidebar.width() + 10));
                        var mapPosition = map.position();
                        var chartPosition = chart.position();
                        map.height(chartPosition.top - mapPosition.top - 20);
                        sidebar.height(map.height() + 10);
                    });
                }
                $(flights).on('flight_added', layout);
                $(flights).on('flight_removed', layout);
                $($window).on('resize', layout);
                layout();
            }
        };
    }]);

    return module;
});
