define([
    'lodash',
    'jquery',
    'volare/common',
    'text!./volare.css'
], function(_, $, common, css) {
    'use strict';

    common.loadCssInline(css);

    function setupLayout(flights, $map, $sidebar, $chart) {
        function layout() {
            _.defer(function() {
                $map.width($(document).width() - ($sidebar.width() + 10));
                var mapPosition = $map.position();
                var chartPosition = $chart.position();
                $map.height(chartPosition.top - mapPosition.top - 20);
                $sidebar.height($map.height() + 10);
            });
        }
        $(flights).on('flight_added', layout);
        $(flights).on('flight_removed', layout);
        $(window).on('resize', layout);
        layout();
    }

    return {
        setupLayout: setupLayout
    };
});
