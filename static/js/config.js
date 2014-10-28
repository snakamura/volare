require.config({
    baseUrl: '/static',
    paths: {
        'angular': '//ajax.googleapis.com/ajax/libs/angularjs/1.3.0/angular.min',
        'angular-ui-bootstrap': 'lib/angular-ui-bootstrap-bower/ui-bootstrap-tpls',
        'async': 'lib/requirejs-plugins/async',
        'bootstrap': '//netdna.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min',
        'domReady': 'lib/domReady/domReady',
        'jquery': '//code.jquery.com/jquery-2.1.1.min',
        'jquery-ui': '//ajax.googleapis.com/ajax/libs/jqueryui/1.11.2/jquery-ui.min',
        'markerwithlabel': 'lib/easy-markerwithlabel/markerwithlabel',
        'underscore': 'lib/underscore/underscore',
        'underscore.string': 'lib/underscore.string/underscore.string'
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
                    MarkerWithLabel: MarkerWithLabel
                }
            }
        }
    }
});

function bootstrap(module) {
    require(['angular', 'domReady'], function(angular, domReady) {
        domReady(function() {
            angular.bootstrap(document, [module.name]);
        });
    });
}

define('./config', function() {
});
