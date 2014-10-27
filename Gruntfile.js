module.exports = function(grunt) {
    var requirejs = {};
    ['flight', 'flights', 'workspace', 'workspaces', 'waypoint', 'waypoints'].forEach(function(page) {
        requirejs[page] = {
            options: {
                name: 'js/' + page,
                baseUrl: 'static',
                mainConfigFile: 'static/js/config.js',
                out: 'static_build/js/' + page + '.js',
                findNestedDependencies: true
            }
        };
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

        requirejs: requirejs
    });

    grunt.loadNpmTasks('grunt-bower-task');
    grunt.loadNpmTasks('grunt-contrib-requirejs');
};
