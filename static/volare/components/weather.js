define([
    'lodash',
    'underscore.string',
    'jquery',
    'angular',
    'text!./weather.css',
    'text!./weather.html',
    'volare/components/map',
    'volare/util'
], function(_, _s, $, angular, css, template) {
    'use strict';

    var module = angular.module('volare.components.weather', [
        'volare.components.map',
        'volare.util'
    ]);

    module.directive('volareWeather', ['util', function(util) {
        util.loadCssInline(css);

        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                mapWeatherFlags: '=weatherFlags'
            },
            controller: 'WeatherController'
        };
    }]);

    module.controller('WeatherController', ['$scope', 'Map', function($scope, Map) {
        var WeatherFlag = Map.WeatherFlag;

        $scope.WeatherFlag = WeatherFlag;
        $scope.msmAltitudes = [1000, 975, 950, 925, 900, 850, 800, 700];
        $scope.windasAltitudes = [1000, 1500, 2000, 2500, 3000];

        $scope.check = function(flags) {
            return _.any(flags);
        };
        $scope.toggle = function(mask) {
            if ($scope.mapWeatherFlags & mask)
                $scope.mapWeatherFlags &= ~mask;
            else
                $scope.mapWeatherFlags |= mask;
        };

        $scope.$watch('flags', function(flags) {
            $scope.mapWeatherFlags = fromFlags(flags, WeatherFlag);
        }, true);
        function updateFlags() {
            $scope.flags = toFlags($scope.mapWeatherFlags, WeatherFlag);
        }
        $scope.$watch('mapWeatherFlags', updateFlags);
        updateFlags();
    }]);


    function fromFlags(flags, flag) {
        if (_.isObject(flag)) {
            return _.reduce(_.omit(flag, 'ALL'), function(weatherFlags, v, k) {
                return weatherFlags | fromFlags(flags[_s.camelize(k.toLowerCase())], v);
            }, 0);
        }
        else {
            return flags ? flag : 0;
        }
    }

    function toFlags(weatherFlags, flag) {
        if (_.isObject(flag)) {
            return _.transform(_.omit(flag, 'ALL'), function(o, v, k) {
                o[_s.camelize(k.toLowerCase())] = toFlags(weatherFlags, v);
            });
        }
        else {
            return (weatherFlags & flag) !== 0;
        }
    }


    return module;
});
