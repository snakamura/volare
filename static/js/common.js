var common = common || {};

$(function() {
    _.mixin(_.string.exports());

    $.ajaxSetup({
        accept: 'application/json; charset=utf-8'
    });

    $.ajaxJSON = function(method, url, data, success) {
        $.ajax({
            type: method,
            url: url,
            contentType: 'application/json; charset=utf-8',
            data: JSON.stringify(data),
            dataType: 'json'
        }).done(success);
    };

    $.postJSON = function(url, data, success) {
        return $.ajaxJSON('POST', url, data, success);
    };

    $.putJSON = function(url, data, success) {
        return $.ajaxJSON('PUT', url, data, success);
    };

    $.deleteJSON = function(url, data, success) {
        return $.ajaxJSON('DELETE', url, data, success);
    };

    common.formatTime = function(time) {
        return _.sprintf('%04d-%02d-%02d %02d:%02d:%02d', time.getFullYear(),
                         time.getMonth() + 1, time.getDate(), time.getHours(),
                         time.getMinutes(), time.getSeconds());
    };

    common.formatDuration = function(duration) {
        return _.sprintf('%02d:%02d:%02d', Math.floor(duration/(60*60)), (duration/60)%60, duration%60);
    };

    common.formatAltitude = function(altitude) {
        return _.numberFormat(altitude) + 'm';
    };
});
