/* global googleApiKey */

define(['async!http://maps.google.com/maps/api/js?v=3&key=' + googleApiKey], function() {
    'use strict';
    return window.google;
});
