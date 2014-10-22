module.exports = function(grunt) {
    grunt.initConfig({
        bower: {
            install: {
                options: {
                    targetDir: 'static/js/lib',
                    layout: 'byType',
                    install: true,
                    verbose: false,
                    cleanTargetDir: true,
                    cleanBowerDir: false
                }
            }
        }
    });

    grunt.loadNpmTasks('grunt-bower-task');
    grunt.registerTask('default', ['bower:install']);
};
