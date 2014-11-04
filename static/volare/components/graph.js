define([
    'lodash',
    'angular',
    'volare/common',
    'text!./graph.css',
    'text!./graph.html'
], function(_, angular, common, css, template) {
    'use strict';

    common.loadCssInline(css);

    var graph = angular.module('volare.components.graph', []);

    graph.directive('volareGraph', [function() {
        return {
            restrict: 'E',
            replace: true,
            template: template,
            scope: {
                range: '=',
                strokes: '='
            },
            link: function(scope, element, attrs) {
                var width = element.width();
                var height = element.height();

                var margin = {
                    top: 0,
                    left: 50,
                    bottom: 15,
                    right: 0
                };
                var timeStep = 10*60*1000;
                var opacityAll = 0.3;

                function formatTime(time) {
                    return _.sprintf('%02d:%02d', time.getHours(), time.getMinutes());
                }

                var canvases = element.children('canvas');
                _.each(canvases, function(canvas) {
                    canvas.width = width;
                    canvas.height = height;
                });

                var gridContext = canvases[0].getContext('2d');
                var context = canvases[1].getContext('2d');
                context.globalAlpha = opacityAll;
                var currentContext = canvases[2].getContext('2d');

                scope.getX = function(time) {
                    var duration = this.range.end - this.range.start;
                    return (time - this.range.start)/duration*(width - (margin.left + margin.right)) + margin.left;
                };

                scope.getY = function(value) {
                    return height - (value - this.range.min)/(this.range.max - this.range.min)*(height - (margin.top + margin.bottom)) - margin.bottom;
                };

                scope.drawGrid = function(context) {
                    var startX = this.getX(scope.range.start);
                    var endX = this.getX(scope.range.end);
                    var lowestY = this.getY(scope.range.min);
                    var highestY = this.getY(scope.range.max);

                    context.lineWidth = 0.1;
                    context.strokeStyle = 'black';
                    context.beginPath();
                    context.moveTo(startX, lowestY);
                    context.lineTo(startX, highestY);
                    context.stroke();

                    var time = new Date(scope.range.start);
                    time.setMinutes(Math.floor(time.getMinutes()/10)*10);
                    time.setSeconds(0);
                    time = time.getTime() + 10*60*1000;
                    context.textAlign = 'center';
                    for (; time < scope.range.end.getTime(); time += timeStep) {
                        var t = new Date(time);

                        var b = t.getMinutes() === 0 || t.getMinutes() === 30;
                        context.strokeStyle = b ? 'black' : 'gray';

                        var x = this.getX(time);
                        context.beginPath();
                        context.moveTo(x, lowestY);
                        context.lineTo(x, highestY);
                        context.stroke();
                        if (b)
                            context.fillText(formatTime(t), x, height - margin.bottom + 12);
                    }

                    context.textAlign = 'end';
                    _.each(scope.range.steps, function(step) {
                        context.strokeStyle = step.primary ? 'black' : 'gray';

                        var y = this.getY(step.value);
                        context.beginPath();
                        context.moveTo(startX, y);
                        context.lineTo(endX, y);
                        context.stroke();

                        context.fillText(step.label, margin.left - 4, y + 5);
                    }, this);

                    context.stroke();
                };

                scope.drawStrokes = function(context, strokes) {
                    _.each(strokes, function(stroke) {
                        this.drawStroke(context, stroke);
                    }, this);
                };

                scope.drawStroke = function(context, stroke) {
                    if (stroke.points.length >= 2) {
                        context.strokeStyle = stroke.color;
                        context.lineWidth = 2;

                        context.beginPath();
                        context.moveTo(this.getX(stroke.points[0].time), this.getY(stroke.points[0].value));
                        for (var n = 1; n < stroke.points.length; ++n) {
                            context.lineTo(this.getX(stroke.points[n].time), this.getY(stroke.points[n].value));
                        }
                        context.stroke();
                    }
                };

                scope.$watch('range', function(range) {
                    gridContext.clearRect(0, 0, width, height);
                    if (range.start !== range.end)
                        scope.drawGrid(gridContext);
                });
                scope.$watch('strokes.all', function(strokes) {
                    context.clearRect(0, 0, width, height);
                    if (strokes.length !== 0)
                        scope.drawStrokes(context, strokes);
                });
                scope.$watch('strokes.current', function(strokes) {
                    currentContext.clearRect(0, 0, width, height);
                    if (strokes.length !== 0)
                        scope.drawStrokes(currentContext, strokes);
                });
                scope.$watch('strokes.diff', function(strokes) {
                    if (strokes.length !== 0)
                        scope.drawStrokes(currentContext, strokes);
                });
            }
        };
    }]);

    return graph;
});
