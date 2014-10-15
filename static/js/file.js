(function() {
    var file = angular.module('volare.file', []);

    file.directive('volareFile', ['$parse', function($parse) {
        return {
            link: function(scope, element, attrs) {
                var handler = $parse(attrs['volareFile']);
                element.on('change', function(event) {
                    handler(scope, {
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

}());
