define([
    'lodash',
    'underscore.string',
    'angular',
    'text!./uas.html'
], function(_, _s, angular, uasTemplate) {
    'use strict';

    var module = angular.module('volare.components.uas', []);

    module.directive('volareUas', [function() {
        return {
            restrict: 'E',
            replace: true,
            template: uasTemplate,
            scope: {
                station: '=',
                date: '='
            },
            controller: 'UASController',
            link: function(scope, element, attrs) {
                var width = element.width();
                var height = element.height();

                var margin = {
                    top: 0,
                    left: 50,
                    bottom: 15,
                    right: 0
                };

                var canvas = element.children('canvas')[0];
                canvas.width = width;
                canvas.height = height;

                var context = canvas.getContext('2d');

                var minTemperature = -90;
                var maxTemperature = 50;
                var minPressure = 100;
                var maxPressure = 1050;

                function getX(temperature) {
                    return (temperature - minTemperature)/(maxTemperature - minTemperature)*width;
                }

                function getY(pressure) {
                    return (Math.log(pressure) - Math.log(minPressure))/(Math.log(maxPressure) - Math.log(minPressure))*height;
                }

                function drawObservation(observation) {
                    if (!observation)
                        return;

                    var items = observation;

                    context.lineWidth = 2;
                    context.strokeStyle = 'red';
                    context.beginPath();
                    context.moveTo(getX(_.head(items).temperature), getY(_.head(items).pressure));
                    _.each(_.tail(items), function(item) {
                        if (item.temperature)
                            context.lineTo(getX(item.temperature), getY(item.pressure));
                    });
                    context.stroke();

                    context.strokeStyle = 'blue';
                    context.beginPath();
                    context.moveTo(getX(_.head(items).dewPoint), getY(_.head(items).pressure));
                    _.each(_.tail(items), function(item) {
                        if (item.dewPoint)
                            context.lineTo(getX(item.dewPoint), getY(item.pressure));
                    });
                    context.stroke();
                }

                scope.$watch('observation', drawObservation);
            }
        };
    }]);

    module.controller('UASController', ['$scope', '$http', function($scope, $http) {
        var station = $scope.station;
        var date = $scope.date;

        var path = _s.sprintf('/uas/observation/%s/%04d/%02d/%02d/00', station.id,
            date.getUTCFullYear(), date.getUTCMonth() + 1, date.getUTCDate());
        $http.get(path).success(function(observation) {
            $scope.observation = observation;
        });
    }]);

    return module;
});
