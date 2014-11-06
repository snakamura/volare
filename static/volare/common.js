define([
    'lodash',
    'jquery'
], function(_, $) {
    'use strict';

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
        wrap: function(proto, o) {
            var w = Object.create(proto);
            _.each(o, function(value, key) {
                w['_' + key] = value;
            });
            return w;
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
