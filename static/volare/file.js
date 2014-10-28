define(['require',
        'angular',
        'volare/common',
        'text!volare/file.css'],
       function(require, angular, common, css) {
    common.loadCssInline(css);

    var file = angular.module('volare.file', []);

    file.directive('volareFile', ['$parse', function($parse) {
        return {
            restrict: 'E',
            replace: true,
            transclude: true,
            templateUrl: require.toUrl('./file.html'),
            link: function(scope, element, attrs) {
                var changeHandler = $parse(attrs['change']);

                var input = element.find('input');
                input.attr('accept', attrs['accept']);
                input.on('change', function(event) {
                    changeHandler(scope, {
                        $event: event,
                        $files: event.target.files
                    });
                });
            }
        };
    }]);

    file.directive('volareDrop', ['$parse', function($parse) {
        return {
            link: function(scope, element, attrs) {
                var handler = $parse(attrs['volareDrop']);
                element.on('dragenter', function(event) {
                    event.preventDefault();
                    event.originalEvent.dataTransfer.dropEffect = 'copy';
                });
                element.on('dragleave', function(event) {
                    event.preventDefault();
                });
                element.on('dragover', function(event) {
                    event.preventDefault();
                });
                element.on('drop', function(event) {
                    event.preventDefault();
                    handler(scope, {
                        $event: event,
                        $files: event.originalEvent.dataTransfer.files
                    });
                });
            }
        };
    }]);

    return file;
});
