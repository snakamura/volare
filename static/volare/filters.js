define([
    'lodash',
    'underscore.string',
    'angular',
], function(_, _s, angular) {
    'use strict';

    var filters = angular.module('volare.filters', []);

    filters.filter('time', function() {
        return function(time) {
            return time ? _s.sprintf('%02d:%02d:%02d', time.getHours(), time.getMinutes(), time.getSeconds()) : '';
        };
    });

    filters.filter('duration', function() {
        return function(duration) {
            return duration ? _s.sprintf('%02d:%02d:%02d', Math.floor(duration/(60*60)), (duration/60)%60, duration%60) : '';
        };
    });

    filters.filter('position', function() {
        return function(position) {
            return _s.sprintf('%.5f', position);
        };
    });

    filters.filter('altitude', function() {
        return function(altitude) {
            return _s.numberFormat(altitude) + 'm';
        };
    });

    filters.filter('speed', function() {
        return function(speed) {
            return _s.sprintf('%.1fkm/h', speed*3600/1000);
        };
    });

    filters.filter('verticalSpeed', function() {
        return function(speed) {
            return _s.sprintf('%.1fm/s', speed);
        };
    });

    return filters;
});
