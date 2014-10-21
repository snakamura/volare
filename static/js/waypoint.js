(function() {
    var waypoint = angular.module('volare.waypoint', ['volare.name']);

    waypoint.controller('WaypointNameController', ['$scope', '$http', '$name', function($scope, $http, $name) {
        $scope.name = $name;
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
}());

$(function() {
    var $map = $('#map');
    var map = new google.maps.Map($map[0], {
        mapTypeId: google.maps.MapTypeId.HYBRID
    });

    function layout() {
        $map.width($(document).width());
        var mapPosition = $map.position();
        $map.height($(document).height() - mapPosition.top);
    }
    $(window).on('resize', layout);
    layout();

    $.getJSON('', function(waypoint) {
        var bounds = null;
        _.each(waypoint.items, function(item) {
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

            if (!bounds)
                bounds = new google.maps.LatLngBounds(position, position);
            else
                bounds.extend(position);
        });

        map.fitBounds(bounds);
    });
});
