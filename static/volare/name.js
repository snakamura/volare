define(['angular',
        'volare/common',
        'text!volare/name.css',
        'text!volare/name.html'],
       function(angular, common, css, template) {
    common.loadCssInline(css);

    var name = angular.module('volare.name', []);

    name.directive('volareName', ['$parse', function($parse) {
        return {
            restrict: 'E',
            replace: true,
            transclude: true,
            template: template,
            link: function(scope, element, attrs) {
                var updateHandler = $parse(attrs['update']);
                var deleteHandler = $parse(attrs['delete']);

                var showName = element.find('.show_name');
                var displayName = showName.find('.name');
                var editName = element.find('.edit_name');
                var inputName = editName.find('input');

                scope.$watch('name', function(name) {
                    displayName.text(name);
                    inputName.val(name);
                });

                function startEditingName() {
                    showName.hide();
                    editName.show();
                    inputName.focus();
                }

                function finishEditingName() {
                    editName.hide();
                    showName.show();

                    var name = inputName.val();
                    updateHandler(scope, {
                        $name: name
                    });
                }

                showName.find('span.edit').on('click', startEditingName);
                showName.find('span.delete').on('click', function() {
                    deleteHandler(scope, {});
                });
                editName.find('span.save').on('click', finishEditingName);
                inputName.on('keyup', function(event) {
                    if (event.keyCode == 0x0d)
                        finishEditingName();
                });
            }
        };
    }]);

    return name;
});
