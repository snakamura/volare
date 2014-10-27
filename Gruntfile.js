module.exports = function(grunt) {
    var fs = require('fs');
    var _ = require('underscore');

    var mainJavascriptFiles = fs.readdirSync('static/js');
    var mainModules = _.map(_.filter(mainJavascriptFiles, function(file) {
        return file !== 'config.js';
    }), function(file) {
        var name = file.replace(/\..*$/, '');
        return {
            'name': 'js/' + name
        }
    });

    grunt.initConfig({
        bower: {
            install: {
                options: {
                    targetDir: 'static/lib',
                    layout: 'byType',
                    install: true,
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
                    mainConfigFile: 'static/js/config.js',
                    dir: 'static_build',
                    findNestedDependencies: true,
                    removeCombined: true,
                    optimizeCss: 'standard',
                    preserveLicenseComments: false,
                    fileExclusionRegExp: /\.git/,
                    paths: {
                        'angular': 'empty:',
                        'bootstrap': 'empty:',
                        'jquery': 'empty:',
                        'jquery-ui': 'empty:'
                    },
                    modules: mainModules
                }
            }
        }
    });

    grunt.loadNpmTasks('grunt-bower-task');
    grunt.loadNpmTasks('grunt-contrib-requirejs');
};
