define([
    'lodash',
    'angular'
], function(_, angular) {
    'use strict';

    var filters = angular.module('volare.filters', []);

    filters.filter('time', function() {
        return function(time) {
            return time ? _.sprintf('%02d:%02d:%02d', time.getHours(), time.getMinutes(), time.getSeconds()) : '';
        };
    });

    filters.filter('duration', function() {
        return function(duration) {
            return duration ? _.sprintf('%02d:%02d:%02d', Math.floor(duration/(60*60)), (duration/60)%60, duration%60) : '';
        };
    });

    filters.filter('position', function() {
        return function(position) {
            return _.sprintf('%.5f', position);
        };
    });

    filters.filter('altitude', function() {
        return function(altitude) {
            return _.numberFormat(altitude) + 'm';
        };
    });

    filters.filter('speed', function() {
        return function(speed) {
            return _.sprintf('%.1fkm/h', speed*3600/1000);
        };
    });

    filters.filter('verticalSpeed', function() {
        return function(speed) {
            return _.sprintf('%.1fm/s', speed);
        };
    });

    return filters;
});
