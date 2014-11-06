define([
    'lodash',
    'underscore.string',
    'jquery',
    'angular',
    'text!./route.css',
    'text!./route.html',
    'text!./routeEditItems.html',
    'angular-ui-bootstrap',
    'volare/model',
    'volare/util'
], function(_, _s, $, angular, css, template, routeEditItemsTemplate) {
    'use strict';

    var module = angular.module('volare.components.route', [
        'volare.model',
        'volare.util'
    ]);

    module.directive('volareRoute', ['$modal', 'util', function($modal, util) {
        util.loadCssInline(css);

        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                map: '='
            },
            controller: ['$scope', function($scope) {
                $scope.$watch('map', function(map) {
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
                });
            }]
        };
    }]);

    module.controller('RouteEditItemsController', ['$scope', '$http', 'model', function($scope, $http, model) {
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
            var route = new model.Route();
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
                    $scope.waypointItems = model.Waypoint.wrap(waypoint).getItems();
                    addItem();
                });
            }
        });

        $http.get('/waypoints').success(function(waypoints) {
            $scope.waypoints = waypoints;
        });
    }]);

    module.filter('route', ['model', function(model) {
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
                    var distance = model.WaypointItem.distance(items[n - 1].getWaypointItem(), item.getWaypointItem());
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
    }]);

    return module;
});
