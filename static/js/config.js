require.config({
    baseUrl: '/static/js',
    paths: {
        'angular': '//ajax.googleapis.com/ajax/libs/angularjs/1.3.0/angular.min',
        'angular-ui-bootstrap': 'lib/angular-ui-bootstrap-bower/ui-bootstrap-tpls',
        'bootstrap': '//netdna.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min',
        'domReady': 'lib/domReady/domReady',
        'jquery': '//code.jquery.com/jquery-2.1.1.min',
        'jquery-ui': '//ajax.googleapis.com/ajax/libs/jqueryui/1.11.2/jquery-ui.min',
        'underscore': 'lib/underscore/underscore',
        'underscore.string': 'lib/underscore.string/underscore.string',
        'volare': 'volare'
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
        }
    }
});

function bootstrap(name) {
    require([name], function(module) {
        require(['angular', 'domReady'], function(angular, domReady) {
            domReady(function() {
                angular.bootstrap(document, [module.name]);
            });
        });
    });
};
