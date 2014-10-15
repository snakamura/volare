(function() {
    var app = angular.module('Waypoints', []);

    app.controller('WaypointsController', ['$scope', '$http', function($scope, $http) {
        $scope.waypoints = [];
        $scope.addWaypoint = function(waypoint) {
            var index = _.sortedIndex(this.waypoints, waypoint, function(waypoint) {
                return waypoint.name;
            });
            this.waypoints.splice(index, 0, waypoint);
        };
        $scope.addFiles = function(files) {
            var self = this;
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
                            self.addWaypoint(waypoint);
                    });
                });
                reader.readAsText(file);
            });
        };

        $http.get('/waypoints').success(function(waypoints) {
            $scope.waypoints = waypoints;
        });
    }]);

    app.directive('volareFile', ['$parse', function($parse) {
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

    app.directive('volareDrop', ['$parse', function($parse) {
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
