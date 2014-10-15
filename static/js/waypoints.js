(function() {
    var app = angular.module('Waypoints', []);

    app.controller('WaypointsController', ['$scope', '$http', function($scope, $http) {
        function addFiles(files) {
            _.each(files, function(file) {
                var reader = new FileReader();
                $(reader).on('loadend', function(event) {
                    $http.post('/waypoints', {
                        name: common.basename(file.name),
                        wpt: reader.result
                    }).success(function(waypoint) {
                        if (files.length == 1)
                            document.location.href = '/waypoints/' + waypoint.id;
                        else
                            $scope.addWaypoint(waypoint);
                    });
                });
                reader.readAsText(file);
            });
        }

        $scope.waypoints = [];
        $scope.addWaypoint = function(waypoint) {
            var index = _.sortedIndex(this.waypoints, waypoint, function(waypoint) {
                return waypoint.name;
            });
            this.waypoints.splice(index, 0, waypoint);
        };
        $scope.addFiles = addFiles;

        $http.get('/waypoints').success(function(waypoints) {
            $scope.waypoints = waypoints;
        });

        var $dropTarget = $('#waypoints');
        $dropTarget.on('dragenter', function(event) {
            event.preventDefault();
            event.originalEvent.dataTransfer.dropEffect = 'copy';
        });
        $dropTarget.on('dragleave', function(event) {
            event.preventDefault();
        });
        $dropTarget.on('dragover', function(event) {
            event.preventDefault();
        });
        $dropTarget.on('drop', function(event) {
            event.preventDefault();
            addFiles(event.originalEvent.dataTransfer.files);
        });
    }]);

    app.directive('volareFile', ['$parse', '$timeout', function($parse, $timeout) {
        return {
            link: function(scope, element, attrs) {
                var handler = $parse(attrs['volareFile'])
                element.bind('change', function(event) {
                    $timeout(function() {
                        handler(scope, {
                            $event: event,
                            $files: event.target.files
                        });
                    });
                });
            }
        };
    }]);
}());
