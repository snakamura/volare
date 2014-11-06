define([
    'jquery',
    'angular'
], function($, angular) {
    'use strict';

    var util = angular.module('volare.util', []);

    util.factory('util', function() {
        return {
            inherit: (function() {
                var Proxy = function() {
                };
                return function(clazz, parent) {
                    Proxy.prototype = parent.prototype;
                    clazz.prototype = new Proxy();
                    clazz.super_ = parent.prototype;
                    clazz.prototype.constructor = clazz;
                };
            }()),

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
