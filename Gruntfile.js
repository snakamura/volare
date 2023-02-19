module.exports = function(grunt) {
    var fs = require('fs');
    var glob = require('glob');
    var _ = require('lodash');

    var mainModules = _.map(glob.sync('static/js/**/*.js'), function(file) {
        var name = file.replace(/static\//, '').replace(/\..*$/, '');
        return {
            'name': name
        }
    });

    grunt.initConfig({
        bowerTargetDir: 'static/lib',
        requirejsTargetDir: 'static_build',
        requirejsModules: mainModules,

        bower: {
            install: {
                options: {
                    targetDir: '<%= bowerTargetDir %>',
                    layout: 'byType',
                    install: true,
                    copy: true,
                    verbose: false,
                    cleanTargetDir: true,
                    cleanBowerDir: false
                }
            }
        },

        requirejs: {
            build: {
                options: {
                    baseUrl: 'static',
                    mainConfigFile: 'static/config.js',
                    dir: '<%= requirejsTargetDir %>',
                    findNestedDependencies: true,
                    removeCombined: true,
                    optimizeCss: 'standard',
                    preserveLicenseComments: false,
                    fileExclusionRegExp: /\.git/,
                    wrapShim: true,
                    stubModules: [
                        'text'
                    ],
                    paths: {
                        'angular': 'empty:',
                        'angular-ui-bootstrap': 'empty:',
                        'async': 'empty:',
                        'bootstrap': 'empty:',
                        'domReady': 'empty:',
                        'jquery': 'empty:',
                        'jquery-ui': 'empty:',
                        'lodash': 'empty:',
                        'text': 'lib/requirejs-text/text',
                        'underscore.string': 'empty:'
                    },
                    modules: '<%= requirejsModules %>'
                }
            }
        },

        jshint: {
            all: {
                src: [
                    'static/*.js',
                    'static/ext/**/*.js',
                    'static/js/**/*.js',
                    'static/volare/**/*.js'
                ],
                options: {
                    jshintrc: true
                }
            }
        },

        csslint: {
            all: {
                src: [
                    'static/css/*.css',
                    'static/volare/**/*.css'
                ],
                options: {
                    csslintrc: '.csslintrc',
                },
            }
        },

        clean: {
            bower: [
                'bower_components',
                '<%= bowerTargetDir %>'
            ],
            requirejs: [
                '<%= requirejsTargetDir %>'
            ],
            build: [
                '<%= requirejsTargetDir %>/build.txt',
                '<%= requirejsTargetDir %>/lib/{angular,requirejs-plugins}',
                '<%= requirejsTargetDir %>/volare/*.{css,html}'
            ]
        }
    });

    require('load-grunt-tasks')(grunt);

    grunt.registerTask('build', ['jshint', 'csslint', 'requirejs:build', 'clean:build']);
};
