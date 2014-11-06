define([
    'lodash',
    'jquery',
    'angular'
], function(_, $, angular) {
    'use strict';

    var module = angular.module('volare.util', []);

    module.factory('util', function() {
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

            wrap: function(proto, o) {
                var w = Object.create(proto);
                _.each(o, function(value, key) {
                    w['_' + key] = value;
                });
                return w;
            },

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

    return module;
});
