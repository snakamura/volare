(function() {
    'use strict';

    require.config({
        baseUrl: '/static',
        paths: {
            'angular': '//ajax.googleapis.com/ajax/libs/angularjs/1.3.0/angular.min',
            'angular-ui-bootstrap': '//cdnjs.cloudflare.com/ajax/libs/angular-ui-bootstrap/0.11.2/ui-bootstrap-tpls.min',
            'async': '//cdnjs.cloudflare.com/ajax/libs/requirejs-plugins/1.0.3/async.min',
            'bootstrap': '//netdna.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min',
            'domReady': '//cdnjs.cloudflare.com/ajax/libs/require-domReady/2.0.1/domReady.min',
            'google': 'ext/google',
            'jquery': '//code.jquery.com/jquery-2.1.1.min',
            'jquery-ui': '//ajax.googleapis.com/ajax/libs/jqueryui/1.11.2/jquery-ui.min',
            'lodash': '//cdnjs.cloudflare.com/ajax/libs/lodash.js/2.4.1/lodash.min',
            'markerwithlabel': 'lib/easy-markerwithlabel/markerwithlabel',
            'text': '//cdnjs.cloudflare.com/ajax/libs/require-text/2.0.12/text.min',
            'underscore.string': '//cdnjs.cloudflare.com/ajax/libs/underscore.string/2.3.3/underscore.string.min'
        },
        shim: {
            'angular': {
                deps: ['jquery'],
                exports: 'angular'
            },
            'angular-ui-bootstrap': {
                deps: ['angular']
            },
            'bootstrap': {
                deps: ['jquery']
            },
            'markerwithlabel': {
                deps: ['google'],
                init: function() {
                    return {
                        /* global MarkerWithLabel */
                        MarkerWithLabel: MarkerWithLabel
                    };
                }
            }
        }
    });

    define('../config', {
        bootstrap: function(module) {
            require(['angular', 'domReady'], function(angular, domReady) {
                domReady(function() {
                    angular.bootstrap(document, [module.name]);
                });
            });
        }
    });
}());
