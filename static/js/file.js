(function() {
    var file = angular.module('volare.file', []);

    file.directive('volareFile', ['$parse', function($parse) {
        return {
            link: function(scope, element, attrs) {
                var handler = $parse(attrs['volareFile'])
                element.bind('change', function(event) {
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
                var handler = $parse(attrs['volareDrop'])
                element.bind('dragenter', function(event) {
                    event.preventDefault();
                    event.originalEvent.dataTransfer.dropEffect = 'copy';
                });
                element.bind('dragleave', function(event) {
                    event.preventDefault();
                });
                element.bind('dragover', function(event) {
                    event.preventDefault();
                });
                element.bind('drop', function(event) {
                    console.log(event.originalEvent.dataTransfer.files);
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
