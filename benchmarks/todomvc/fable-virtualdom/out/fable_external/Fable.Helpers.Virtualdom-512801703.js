define(["exports", "fable-core", "virtual-dom"], function (exports, _fableCore, _virtualDom) {
    "use strict";

    Object.defineProperty(exports, "__esModule", {
        value: true
    });
    exports.renderer = exports.render = exports.createTree = exports.App = exports.Html = undefined;

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    var Html = exports.Html = function ($exports) {
        var Types = $exports.Types = function ($exports) {
            var MouseEvent = $exports.MouseEvent = function MouseEvent($arg0, $arg1, $arg2) {
                _classCallCheck(this, MouseEvent);

                this.altKey = $arg0;
                this.screenX = $arg1;
                this.screenY = $arg2;
            };

            _fableCore.Util.setInterfaces(MouseEvent.prototype, [], "Fable.Helpers.Virtualdom.Html.Types.MouseEvent");

            var KeyboardEvent = $exports.KeyboardEvent = function KeyboardEvent($arg0, $arg1) {
                _classCallCheck(this, KeyboardEvent);

                this.code = $arg0;
                this.keyCode = $arg1;
            };

            _fableCore.Util.setInterfaces(KeyboardEvent.prototype, [], "Fable.Helpers.Virtualdom.Html.Types.KeyboardEvent");

            var EventHandlerBinding = $exports.EventHandlerBinding = function EventHandlerBinding() {
                _classCallCheck(this, EventHandlerBinding);

                this.Case = arguments[0];
                this.Fields = [];

                for (var i = 1; i < arguments.length; i++) {
                    this.Fields[i - 1] = arguments[i];
                }
            };

            _fableCore.Util.setInterfaces(EventHandlerBinding.prototype, [], "Fable.Helpers.Virtualdom.Html.Types.EventHandlerBinding");

            var Attribute = $exports.Attribute = function Attribute() {
                _classCallCheck(this, Attribute);

                this.Case = arguments[0];
                this.Fields = [];

                for (var i = 1; i < arguments.length; i++) {
                    this.Fields[i - 1] = arguments[i];
                }
            };

            _fableCore.Util.setInterfaces(Attribute.prototype, [], "Fable.Helpers.Virtualdom.Html.Types.Attribute");

            var Node = $exports.Node = function Node() {
                _classCallCheck(this, Node);

                this.Case = arguments[0];
                this.Fields = [];

                for (var i = 1; i < arguments.length; i++) {
                    this.Fields[i - 1] = arguments[i];
                }
            };

            _fableCore.Util.setInterfaces(Node.prototype, [], "Fable.Helpers.Virtualdom.Html.Types.Node");

            return $exports;
        }({});

        var Tags = $exports.Tags = function ($exports) {
            return $exports;
        }({});

        var Attributes = $exports.Attributes = function ($exports) {
            return $exports;
        }({});

        var Events = $exports.Events = function ($exports) {
            return $exports;
        }({});

        var Svg = $exports.Svg = function ($exports) {
            var svgNS = $exports.svgNS = function () {
                return new Types.Attribute("Property", ["namespace", "http://www.w3.org/2000/svg"]);
            };

            return $exports;
        }({});

        return $exports;
    }({});

    var App = exports.App = function ($exports) {
        var AppState = $exports.AppState = function AppState($arg0, $arg1, $arg2) {
            _classCallCheck(this, AppState);

            this.Model = $arg0;
            this.View = $arg1;
            this.Update = $arg2;
        };

        _fableCore.Util.setInterfaces(AppState.prototype, [], "Fable.Helpers.Virtualdom.App.AppState");

        var AppEvents = $exports.AppEvents = function AppEvents() {
            _classCallCheck(this, AppEvents);

            this.Case = arguments[0];
            this.Fields = [];

            for (var i = 1; i < arguments.length; i++) {
                this.Fields[i - 1] = arguments[i];
            }
        };

        _fableCore.Util.setInterfaces(AppEvents.prototype, [], "Fable.Helpers.Virtualdom.App.AppEvents");

        var RenderState = $exports.RenderState = function RenderState() {
            _classCallCheck(this, RenderState);

            this.Case = arguments[0];
            this.Fields = [];

            for (var i = 1; i < arguments.length; i++) {
                this.Fields[i - 1] = arguments[i];
            }
        };

        _fableCore.Util.setInterfaces(RenderState.prototype, [], "Fable.Helpers.Virtualdom.App.RenderState");

        var App = $exports.App = function App($arg0, $arg1, $arg2, $arg3, $arg4, $arg5, $arg6) {
            _classCallCheck(this, App);

            this.AppState = $arg0;
            this.JsCalls = $arg1;
            this.Node = $arg2;
            this.CurrentTree = $arg3;
            this.Subscribers = $arg4;
            this.NodeSelector = $arg5;
            this.RenderState = $arg6;
        };

        _fableCore.Util.setInterfaces(App.prototype, [], "Fable.Helpers.Virtualdom.App.App");

        var ScheduleMessage = $exports.ScheduleMessage = function ScheduleMessage() {
            _classCallCheck(this, ScheduleMessage);

            this.Case = arguments[0];
            this.Fields = [];

            for (var i = 1; i < arguments.length; i++) {
                this.Fields[i - 1] = arguments[i];
            }
        };

        _fableCore.Util.setInterfaces(ScheduleMessage.prototype, [], "Fable.Helpers.Virtualdom.App.ScheduleMessage");

        var AppMessage = $exports.AppMessage = function AppMessage() {
            _classCallCheck(this, AppMessage);

            this.Case = arguments[0];
            this.Fields = [];

            for (var i = 1; i < arguments.length; i++) {
                this.Fields[i - 1] = arguments[i];
            }
        };

        _fableCore.Util.setInterfaces(AppMessage.prototype, [], "Fable.Helpers.Virtualdom.App.AppMessage");

        var Renderer = $exports.Renderer = function Renderer($arg0, $arg1, $arg2, $arg3) {
            _classCallCheck(this, Renderer);

            this.Render = $arg0;
            this.Diff = $arg1;
            this.Patch = $arg2;
            this.CreateElement = $arg3;
        };

        _fableCore.Util.setInterfaces(Renderer.prototype, [], "Fable.Helpers.Virtualdom.App.Renderer");

        var createApp = $exports.createApp = function (appState) {
            return new App(appState, new _fableCore.List(), null, null, new Map(), null, new RenderState("NoRequest"));
        };

        var withStartNode = $exports.withStartNode = function (selector, app) {
            var NodeSelector = selector;
            return new App(app.AppState, app.JsCalls, app.Node, app.CurrentTree, app.Subscribers, NodeSelector, app.RenderState);
        };

        var withSubscriber = $exports.withSubscriber = function (subscriberId, subscriber, app) {
            var subsribers = function (table) {
                return new Map(table).set(subscriberId, subscriber);
            }(app.Subscribers);

            return new App(app.AppState, app.JsCalls, app.Node, app.CurrentTree, subsribers, app.NodeSelector, app.RenderState);
        };

        var createScheduler = $exports.createScheduler = function () {
            return _fableCore.MailboxProcessor.start(function (inbox) {
                var loop = function (unitVar0) {
                    return function (builder_) {
                        return builder_.delay(function (unitVar) {
                            return builder_.bind(inbox.receive(), function (_arg1) {
                                var message = _arg1;
                                var milliseconds = message.Fields[0];
                                var cb = message.Fields[1];
                                window.setTimeout(cb, milliseconds);
                                return builder_.returnFrom(loop());
                            });
                        });
                    }(_fableCore.Async);
                };

                return loop();
            });
        };

        var start = $exports.start = function (renderer, app) {
            var renderTree = function (view) {
                return function (handler) {
                    return function (model) {
                        return renderer.Render(handler)(view(model));
                    };
                };
            };

            var startElem = function () {
                var matchValue = app.NodeSelector;

                if (matchValue != null) {
                    var sel = matchValue;
                    return document.body.querySelector(sel);
                } else {
                    return document.body;
                }
            }();

            var scheduler = createScheduler();
            return _fableCore.MailboxProcessor.start(function (inbox) {
                var post = function (message) {
                    inbox.post(new AppMessage("Message", message));
                };

                var notifySubscribers = function (subs) {
                    return function (model) {
                        _fableCore.Map.iter(function (key, handler) {
                            handler(model);
                        }, subs);
                    };
                };

                var loop = function (state) {
                    return function (builder_) {
                        return builder_.delay(function (unitVar) {
                            var matchValue = [state.Node, state.CurrentTree];

                            if (matchValue[0] != null) {
                                if (matchValue[1] != null) {
                                    var currentTree = matchValue[1];
                                    var rootNode = matchValue[0];
                                    return builder_.bind(inbox.receive(), function (_arg1) {
                                        var message = _arg1;

                                        if (message.Case === "Message") {
                                            var msg = message.Fields[0];
                                            notifySubscribers(state.Subscribers)(new AppEvents("ActionReceived", msg));
                                            var patternInput = state.AppState.Update(state.AppState.Model)(msg);
                                            var model_ = patternInput[0];
                                            var jsCalls = patternInput[1];

                                            var renderState = function () {
                                                var matchValue_1 = state.RenderState;

                                                if (matchValue_1.Case === "InProgress") {
                                                    return new RenderState("InProgress");
                                                } else {
                                                    scheduler.post(new ScheduleMessage("PingIn", 1000 / 60, function (unitVar0) {
                                                        inbox.post(new AppMessage("Draw"));
                                                    }));
                                                    return new RenderState("InProgress");
                                                }
                                            }();

                                            return builder_.returnFrom(loop(function () {
                                                var AppState_1 = function () {
                                                    var inputRecord = state.AppState;
                                                    return new AppState(model_, inputRecord.View, inputRecord.Update);
                                                }();

                                                return new App(AppState_1, _fableCore.List.append(state.JsCalls, jsCalls), state.Node, state.CurrentTree, state.Subscribers, state.NodeSelector, renderState);
                                            }()));
                                        } else {
                                            if (message.Case === "Draw") {
                                                var matchValue_1 = state.RenderState;

                                                if (matchValue_1.Case === "NoRequest") {
                                                    throw "Shouldn't happen";
                                                    return builder_.zero();
                                                } else {
                                                    notifySubscribers(state.Subscribers)(new AppEvents("DrawStarted"));
                                                    var model = state.AppState.Model;
                                                    var jsCalls = state.JsCalls;
                                                    var tree = renderTree(state.AppState.View)(post)(model);
                                                    var patches = renderer.Diff(currentTree)(tree);
                                                    renderer.Patch(rootNode)(patches);

                                                    _fableCore.Seq.iter(function (i) {
                                                        i();
                                                    }, jsCalls);

                                                    notifySubscribers(state.Subscribers)(new AppEvents("ModelChanged", model, state.AppState.Model));
                                                    return builder_.returnFrom(loop(function () {
                                                        var RenderState_1 = new RenderState("NoRequest");
                                                        var CurrentTree = tree;
                                                        var JsCalls = new _fableCore.List();
                                                        return new App(state.AppState, JsCalls, state.Node, CurrentTree, state.Subscribers, state.NodeSelector, RenderState_1);
                                                    }()));
                                                }
                                            } else {
                                                return builder_.returnFrom(loop(state));
                                            }
                                        }
                                    });
                                } else {
                                    throw "Shouldn't happen";
                                    return builder_.zero();
                                }
                            } else {
                                var tree = renderTree(state.AppState.View)(post)(state.AppState.Model);
                                var rootNode = renderer.CreateElement(tree);
                                startElem.appendChild(rootNode);
                                return builder_.returnFrom(loop(function () {
                                    var CurrentTree = tree;
                                    var Node = rootNode;
                                    return new App(state.AppState, state.JsCalls, Node, CurrentTree, state.Subscribers, state.NodeSelector, state.RenderState);
                                }()));
                            }
                        });
                    }(_fableCore.Async);
                };

                return loop(app);
            });
        };

        return $exports;
    }({});

    var createTree = exports.createTree = function (handler, tag, attributes, children) {
        var toAttrs = function (attrs) {
            var elAttributes = function (_arg2) {
                return _arg2.tail == null ? null : function () {
                    var v = _arg2;
                    return ["attributes", _fableCore.Util.createObj(v)];
                }();
            }(_fableCore.List.choose(function (x) {
                return x;
            }, _fableCore.List.map(function (_arg1) {
                return _arg1.Case === "Attribute" ? function () {
                    var v = _arg1.Fields[0][1];
                    var k = _arg1.Fields[0][0];
                    return [k, v];
                }() : null;
            }, attrs)));

            var props = _fableCore.List.map(function (_arg4) {
                return _arg4.Case === "Style" ? function () {
                    var style = _arg4.Fields[0];
                    return ["style", _fableCore.Util.createObj(style)];
                }() : _arg4.Case === "Property" ? function () {
                    var v = _arg4.Fields[0][1];
                    var k = _arg4.Fields[0][0];
                    return [k, v];
                }() : _arg4.Case === "EventHandlerBinding" ? function () {
                    var binding = _arg4.Fields[0];

                    if (binding.Case === "KeyboardEventHandler") {
                        var f = binding.Fields[0][1];
                        var ev = binding.Fields[0][0];
                        return [ev, function ($var1) {
                            return handler(f($var1));
                        }];
                    } else {
                        if (binding.Case === "EventHandler") {
                            var f = binding.Fields[0][1];
                            var ev = binding.Fields[0][0];
                            return [ev, function ($var2) {
                                return handler(f($var2));
                            }];
                        } else {
                            var f = binding.Fields[0][1];
                            var ev = binding.Fields[0][0];
                            return [ev, function ($var3) {
                                return handler(f($var3));
                            }];
                        }
                    }
                }() : function () {
                    throw "Shouldn't happen";
                }();
            }, _fableCore.List.filter(function (_arg3) {
                return _arg3.Case === "Attribute" ? false : true;
            }, attrs));

            return function (x) {
                return x;
            }(_fableCore.Util.createObj(elAttributes != null ? function () {
                var x = elAttributes;
                return _fableCore.List.ofArray([x], props);
            }() : props));
        };

        var elem = (0, _virtualDom.h)(tag, toAttrs(attributes), Array.from(children));
        return elem;
    };

    var render = exports.render = function (handler, node) {
        var $target0 = function (attrs, nodes, tag) {
            return createTree(handler, tag, attrs, _fableCore.List.map(function (node_1) {
                return render(handler, node_1);
            }, nodes));
        };

        if (node.Case === "Svg") {
            return $target0(node.Fields[0][1], node.Fields[1], node.Fields[0][0]);
        } else {
            if (node.Case === "VoidElement") {
                var tag = node.Fields[0][0];
                var attrs = node.Fields[0][1];
                return createTree(handler, tag, attrs, new _fableCore.List());
            } else {
                if (node.Case === "Text") {
                    var str = node.Fields[0];
                    return str;
                } else {
                    if (node.Case === "WhiteSpace") {
                        var str = node.Fields[0];
                        return str;
                    } else {
                        return $target0(node.Fields[0][1], node.Fields[1], node.Fields[0][0]);
                    }
                }
            }
        }
    };

    var renderer = exports.renderer = function () {
        return new App.Renderer(function (handler) {
            return function (node) {
                return render(handler, node);
            };
        }, function (tree1) {
            return function (tree2) {
                return (0, _virtualDom.diff)(tree1, tree2);
            };
        }, function (node) {
            return function (patches) {
                return (0, _virtualDom.patch)(node, patches);
            };
        }, function (e) {
            return (0, _virtualDom.create)(e);
        });
    };
});
//# sourceMappingURL=Fable.Helpers.Virtualdom-512801703.js.map