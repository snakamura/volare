define([
    'lodash',
    'jquery',
    'angular',
    'volare/volare',
    'volare/common',
    'text!./weather.css',
    'text!./weather.html'
], function(_, $, angular, volare, common, css, template) {
    'use strict';

    common.loadCssInline(css);

    var weather = angular.module('volare.components.weather', []);

    weather.directive('volareWeather', [function() {
        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                map: '='
            },
            controller: function($scope) {
                var map = $scope.map;

                var WeatherFlag = volare.Map.WeatherFlag;

                $scope.WeatherFlag = WeatherFlag;
                $scope.msmAltitudes = [1000, 975, 950, 925, 900, 850, 800, 700];
                $scope.windasAltitudes = [1000, 1500, 2000, 2500, 3000];

                $scope.check = function(flags) {
                    return _.any(flags);
                };
                $scope.toggle = function(mask) {
                    map.setWeatherFlags(map.getWeatherFlags() & mask ? 0 : mask, mask);
                };

                function fromFlags(flags, flag) {
                    if (_.isObject(flag)) {
                        return _.reduce(_.omit(flag, 'ALL'), function(weatherFlags, v, k) {
                            return weatherFlags | fromFlags(flags[_.camelize(k.toLowerCase())], v);
                        }, 0);
                    }
                    else {
                        return flags ? flag : 0;
                    }
                }

                function toFlags(weatherFlags, flag) {
                    if (_.isObject(flag)) {
                        return _.transform(_.omit(flag, 'ALL'), function(o, v, k) {
                            o[_.camelize(k.toLowerCase())] = toFlags(weatherFlags, v);
                        });
                    }
                    else {
                        return (weatherFlags & flag) !== 0;
                    }
                }

                $scope.$watch('flags', function(flags) {
                    map.setWeatherFlags(fromFlags(flags, WeatherFlag), WeatherFlag.ALL);
                }, true);

                function updateFlags() {
                    $scope.flags = toFlags(map.getWeatherFlags(), WeatherFlag);
                }

                $(map).on('weatherFlags_changed', updateFlags);

                updateFlags();
            }
        };
    }]);

    return weather;
});
