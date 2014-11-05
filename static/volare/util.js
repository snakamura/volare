define([
    'jquery',
    'angular'
], function($, angular) {
    'use strict';

    var util = angular.module('volare.util', []);

    util.factory('util', function() {
        return {
            basename: function(name) {
                return name.replace(/\.[^.]*$/, '');
            },

            loadCssInline: function(css) {
                var style = $('<style>');
                style.text(css);
                $('head').append(style);
            }
        };
    });

    return util;
});
