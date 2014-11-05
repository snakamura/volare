define([
    'lodash',
    'underscore.string',
    'jquery',
    'angular',
    'volare/volare',
    'text!./route.css',
    'text!./route.html',
    'text!./routeEditItems.html',
    'angular-ui-bootstrap',
    'volare/util'
], function(_, _s, $, angular, volare, css, template, routeEditItemsTemplate) {
    'use strict';

    var route = angular.module('volare.components.route', [
        'volare.util'
    ]);

    route.directive('volareRoute', ['$modal', 'util', function($modal, util) {
        util.loadCssInline(css);

        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                map: '='
            },
            controller: ['$scope', function($scope) {
                var map = $scope.map;

                $scope.route = map.getRoute();
                $scope.editRoute = function() {
                    var modal = $modal.open({
                        template: routeEditItemsTemplate,
                        controller: 'RouteEditItemsController',
                        backdrop: 'static'
                    });
                    modal.result.then(function(route) {
                        map.setRoute(route);
                    });
                };

                $(map).on('route_changed', function() {
                    $scope.route = map.getRoute();
                });
            }]
        };
    }]);

    route.controller('RouteEditItemsController', ['$scope', '$http', function($scope, $http) {
        function addItem() {
            $scope.items.push({
                waypointItem: null,
                radius: 400
            });
        }

        $scope.waypoint = null;
        $scope.waypointItems = [];
        $scope.items = [];
        $scope.addItem = function() {
            if (_.last(this.items).waypointItem)
                addItem();
        };
        $scope.save = function() {
            var route = new volare.Route();
            _.each(this.items, function(item) {
                if (item.waypointItem) {
                    route.addItem(item.waypointItem, item.radius);
                }
            });
            this.$close(route.getItems().length !== 0 ? route : null);
        };
        $scope.cancel = function() {
            this.$dismiss('cancel');
        };

        $scope.$watch('waypoint', function(waypoint) {
            this.items = [];
            if (waypoint) {
                $http.get('/waypoints/' + waypoint.id).success(function(waypoint) {
                    $scope.waypointItems = volare.Waypoint.wrap(waypoint).getItems();
                    addItem();
                });
            }
        });

        $http.get('/waypoints').success(function(waypoints) {
            $scope.waypoints = waypoints;
        });
    }]);

    route.filter('route', function() {
        function formatDistance(distance) {
            return _s.sprintf('%.1fkm', distance/1000);
        }
        return function(route) {
            var s = '';
            if (route) {
                var items = route.getItems();
                var totalDistance = 0;
                s += items[0].getWaypointItem().getName();
                for (var n = 1; n < items.length; ++n) {
                    var item = items[n];
                    var distance = volare.WaypointItem.distance(items[n - 1].getWaypointItem(), item.getWaypointItem());
                    s += ' - (';
                    s += formatDistance(distance);
                    s += ') - ';
                    s += item.getWaypointItem().getName();
                    totalDistance += distance;
                }
                s += ' (' + formatDistance(totalDistance) + ')';
            }
            return s;
        };
    });

    return route;
});
