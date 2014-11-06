define([
    'lodash',
    'jquery',
    'angular',
    'text!./player.css',
    'text!./player.html',
    'jquery-ui',
    'volare/filters',
    'volare/util'
], function(_, $, angular, css, template) {
    'use strict';

    var module = angular.module('volare.components.player', [
        'volare.filters',
        'volare.util'
    ]);

    module.directive('volarePlayer', ['$interval', 'util', function($interval, util) {
        util.loadCssInline(css);

        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                flights: '='
            },
            controller: ['$scope', function($scope) {
                var flights = $scope.flights;
                var timer = null;

                $scope.play = function() {
                    if (!timer) {
                        var self = this;
                        flights.setCurrentTime(flights.getCurrentTime() || flights.getStartTime());
                        timer = $interval(function() {
                            var time = new Date(flights.getCurrentTime().getTime() + 10*1000);
                            if (time > flights.getEndTime())
                                self.stop();
                            else
                                flights.setCurrentTime(time, true);
                        }, 100);

                        this.playing = true;
                    }
                    else {
                        $interval.cancel(timer);
                        timer = null;

                        this.playing = false;
                    }
                };
                $scope.stop = function() {
                    if (timer) {
                        $interval.cancel(timer);
                        timer = null;
                    }
                    flights.setCurrentTime(null);

                    this.playing = false;
                };

                $scope.$watch('time', function(time) {
                    flights.setCurrentTime(time);
                });

                function updateTime() {
                    $scope.time = flights.getCurrentTime();
                }

                function updateProperties() {
                    $scope.start = flights.getStartTime();
                    $scope.end = flights.getEndTime();
                }

                $(flights).on('currenttime_changed', updateTime);
                $(flights).on('properties_changed', updateProperties);

                updateProperties();
                updateTime();
            }],
            link: function(scope, element, attrs) {
                var slider = element.find('.slider');
                slider.slider({
                    range: 'min',
                    min: 0,
                    max: 100
                }).on('slide', function(event, ui) {
                    scope.time = new Date(ui.value);
                    scope.$apply();
                });

                function updateStopButton() {
                    element.find('.stop').prop('disabled', !scope.playing && !scope.time);
                }

                scope.$watch('start', function(start) {
                    slider.slider('option', 'min', start ? start.getTime() : 0);
                });
                scope.$watch('end', function(end) {
                    slider.slider('option', 'max', end ? end.getTime() : 0);
                });
                scope.$watch('time', function(time) {
                    slider.slider('value', time ? time.getTime() : slider.slider('option', 'min'));
                    updateStopButton();
                });
                scope.$watch('playing', function(playing) {
                    element.find('.play span').addClass(playing ? 'glyphicon-pause' : 'glyphicon-play');
                    element.find('.play span').removeClass(playing ? 'glyphicon-play' : 'glyphicon-pause');
                    updateStopButton();
                });
            }
        };
    }]);

    return module;
});
