/* global googleApiKey */

define(['async!http://maps.google.com/maps/api/js?v=3&key=' + googleApiKey + '&sensor=false'], function() {
    'use strict';
    return window.google;
});
