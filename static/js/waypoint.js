(function() {
    var waypoint = angular.module('volare.waypoint', ['volare.name']);

    waypoint.controller('WaypointController', ['$scope', '$http', function($scope, $http) {
        $scope.items = [];
        $http.get('').success(function(waypoint) {
            $scope.name = waypoint.name;
            $scope.items = waypoint.items;
        });
    }]);

    waypoint.controller('WaypointNameController', ['$scope', '$http', function($scope, $http) {
        $scope.update = function(name) {
            var self = this;
            $http.put('', {
                name: name
            }).success(function(waypoint) {
                self.name = waypoint.name;
            });
        };
        $scope.delete = function() {
            if (confirm('Are you sure to delete these waypoints?')) {
                $http.delete('').success(function() {
                    document.location.href = '/waypoints';
                });
            }
        };
    }]);

    waypoint.directive('volareWaypoint', [function() {
        return {
            restrict: 'E',
            replace: true,
            template: '<div></div>',
            link: function(scope, element, attrs) {
                var map = new google.maps.Map(element[0], {
                    mapTypeId: google.maps.MapTypeId.HYBRID
                });

                var markers = [];
                scope.$watch('items', function(items) {
                    _.each(markers, function(marker) {
                        marker.setMap(null);
                    });
                    markers = [];

                    if (items.length !== 0) {
                        var bounds = null;
                        _.each(scope.items, function(item) {
                            var position = new google.maps.LatLng(item.latitude, item.longitude);
                            var label = item.name;
                            if (item.name !== item.description) {
                                label += ' (' + item.description + ')';
                            }
                            var marker = new MarkerWithLabel({
                                map: map,
                                position: position,
                                title: item.name,
                                labelContent: label,
                                labelAnchor: new google.maps.Point(-15, 35),
                                labelClass: 'label'
                            });
                            markers.push(marker);

                            if (!bounds)
                                bounds = new google.maps.LatLngBounds(position, position);
                            else
                                bounds.extend(position);
                        });

                        map.fitBounds(bounds);
                    }
                });

                function layout() {
                    element.width($(document).width());
                    var mapPosition = element.position();
                    element.height($(document).height() - mapPosition.top);
                }
                $(window).on('resize', layout);
                layout();
            }
        };
    }]);
}());
