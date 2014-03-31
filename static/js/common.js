var common = common || {};

(function() {
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

    $.deleteJSON = function(url, success) {
        return $.ajaxJSON('DELETE', url, {}, success);
    };

    common.inherit = (function() {
        var Proxy = function() {
        };
        return function(clazz, parent) {
            Proxy.prototype = parent.prototype;
            clazz.prototype = new Proxy();
            clazz.super_ = parent.prototype;
            clazz.prototype.constructor = clazz;
        };
    })();

    common.basename = function(name) {
        return name.replace(/\.[^.]*$/, '');
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

    common.makeNameEditable = function(update) {
        var showName = $('#show_name');
        var editName = $('#edit_name');
        var inputName = $('#edit_name input');

        function startEditingName() {
            showName.hide();
            editName.show();
            inputName.focus();
        }

        function finishEditingName() {
            var name = inputName.val();
            $('#name').text(name);
            editName.hide();
            showName.show();

            update(name);
        }

        $('#show_name span.edit').on('click', startEditingName);
        $('#edit_name span.save').on('click', finishEditingName);
        inputName.on('keyup', function(event) {
            if (event.keyCode == 0x0d)
                finishEditingName();
        });
    };
})();
