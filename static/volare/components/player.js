define([
    'lodash',
    'jquery',
    'angular',
    'volare/common',
    'text!./player.css',
    'text!./player.html',
    'jquery-ui',
    'volare/filters'
], function(_, $, angular, common, css, template) {
    'use strict';

    common.loadCssInline(css);

    var player = angular.module('volare.components.player', [
        'volare.filters'
    ]);

    player.directive('volarePlayer', ['$interval', function($interval) {
        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                modelFlights: '=flights'
            },
            controller: function($scope) {
                var modelFlights = $scope.modelFlights;
                var timer = null;

                $scope.play = function() {
                    if (!timer) {
                        var self = this;
                        modelFlights.setCurrentTime(modelFlights.getCurrentTime() || modelFlights.getStartTime());
                        timer = $interval(function() {
                            var time = new Date(modelFlights.getCurrentTime().getTime() + 10*1000);
                            if (time > modelFlights.getEndTime())
                                self.stop();
                            else
                                modelFlights.setCurrentTime(time, true);
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
                    modelFlights.setCurrentTime(null);

                    this.playing = false;
                };

                $scope.$watch('time', function(time) {
                    modelFlights.setCurrentTime(time);
                });

                function updateTime() {
                    $scope.time = modelFlights.getCurrentTime();
                }

                function updateProperties() {
                    $scope.start = modelFlights.getStartTime();
                    $scope.end = modelFlights.getEndTime();
                }

                $(modelFlights).on('currenttime_changed', updateTime);
                $(modelFlights).on('properties_changed', updateProperties);

                updateProperties();
                updateTime();
            },
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

    return player;
});
