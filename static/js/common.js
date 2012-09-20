$(function() {
    $.ajaxSetup({
        accept: 'application/json; charset=utf-8'
    });

    $.postJSON = function(url, data, success) {
        $.ajax({
            type: 'POST',
            url: url,
            contentType: 'application/json; charset=utf-8',
            data: JSON.stringify(data),
            dataType: 'json'
        }).done(success);
    };
});
