define([
    'angular',
    'google',
    'volare/util/layout'
], function(angular, google) {
    'use strict';

    var module = angular.module('volare.forcast', [
        'volare.util.layout'
    ]);

    module.controller('ForcastController', [function() {
    }]);

    module.directive('volareForcast', [function() {
        return {
            restrict: 'E',
            replace: true,
            template: '<div></div>',
            link: function(scope, element, attrs) {
                var map = new google.maps.Map(element[0], {
                    mapTypeId: google.maps.MapTypeId.HYBRID,
                    center: new google.maps.LatLng(35.685124, 139.752787),
                    zoom: 6
                });
            }
        };
    }]);

    return module;
});
