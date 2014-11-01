define([
    'lodash',
    'underscore.string',
    'jquery'
], function(_, _s, $) {
    'use strict';

    _.mixin(_s.exports());

    $.ajaxSetup({
        accept: 'application/json; charset=utf-8'
    });

    $.ajaxJSON = function(method, url, data, success) {
        $.ajax({
            type: method,
            url: url,
            contentType: 'application/json; charset=utf-8',
            data: JSON.stringify(data),
            dataType: 'json'
        }).done(success);
    };

    $.postJSON = function(url, data, success) {
        return $.ajaxJSON('POST', url, data, success);
    };

    $.putJSON = function(url, data, success) {
        return $.ajaxJSON('PUT', url, data, success);
    };

    $.deleteJSON = function(url, success) {
        return $.ajaxJSON('DELETE', url, {}, success);
    };

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

        formatDistance: function(distance) {
            return _.sprintf('%.1f', distance/1000);
        },

        loadCss: function(url) {
            var link = $('<link>', {
                type: 'text/css',
                rel: 'stylesheet',
                href: url
            });
            $('head').append(link);
        },

        loadCssInline: function(css) {
            var style = $('<style>');
            style.text(css);
            $('head').append(style);
        }
    };
});
