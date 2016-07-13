define(["exports", "fable-core", "./fable_external/Fable.Helpers.Virtualdom-512801703"], function (exports, _fableCore, _FableHelpers) {
    "use strict";

    Object.defineProperty(exports, "__esModule", {
        value: true
    });
    exports.todoApp = exports.initModel = exports.initList = exports.Storage = exports.todoView = exports.todoMain = exports.itemList = exports.listItem = exports.todoHeader = exports.todoFooter = exports.filters = exports.filter = exports.filterToTextAndUrl = exports.todoUpdate = exports.TodoAction = exports.TodoModel = exports.Item = exports.Filter = undefined;

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    var Filter = exports.Filter = function Filter() {
        _classCallCheck(this, Filter);

        this.Case = arguments[0];
        this.Fields = [];

        for (var i = 1; i < arguments.length; i++) {
            this.Fields[i - 1] = arguments[i];
        }
    };

    _fableCore.Util.setInterfaces(Filter.prototype, [], "Virtualdom.Filter");

    var Item = exports.Item = function Item($arg0, $arg1, $arg2, $arg3) {
        _classCallCheck(this, Item);

        this.Name = $arg0;
        this.Done = $arg1;
        this.Id = $arg2;
        this.IsEditing = $arg3;
    };

    _fableCore.Util.setInterfaces(Item.prototype, [], "Virtualdom.Item");

    var TodoModel = exports.TodoModel = function TodoModel($arg0, $arg1, $arg2) {
        _classCallCheck(this, TodoModel);

        this.Items = $arg0;
        this.Input = $arg1;
        this.Filter = $arg2;
    };

    _fableCore.Util.setInterfaces(TodoModel.prototype, [], "Virtualdom.TodoModel");

    var TodoAction = exports.TodoAction = function TodoAction() {
        _classCallCheck(this, TodoAction);

        this.Case = arguments[0];
        this.Fields = [];

        for (var i = 1; i < arguments.length; i++) {
            this.Fields[i - 1] = arguments[i];
        }
    };

    _fableCore.Util.setInterfaces(TodoAction.prototype, [], "Virtualdom.TodoAction");

    var todoUpdate = exports.todoUpdate = function (model, msg) {
        var checkAllWith = function (v) {
            var items_ = _fableCore.List.map(function (i) {
                return new Item(i.Name, v, i.Id, i.IsEditing);
            }, model.Items);

            return new TodoModel(items_, model.Input, model.Filter);
        };

        var updateItem = function (i) {
            return function (model_1) {
                var items_ = _fableCore.List.map(function (i_) {
                    return i_.Id !== i.Id ? i_ : i;
                }, model_1.Items);

                return new TodoModel(items_, model_1.Input, model_1.Filter);
            };
        };

        var model_ = msg.Case === "ChangeInput" ? function () {
            var v = msg.Fields[0];
            return new TodoModel(model.Items, v, model.Filter);
        }() : msg.Case === "MarkAsDone" ? function () {
            var i = msg.Fields[0];

            var items_ = _fableCore.List.map(function (i_) {
                return _fableCore.Util.compareTo(i_, i) !== 0 ? i_ : function () {
                    var Done = true;
                    return new Item(i.Name, Done, i.Id, i.IsEditing);
                }();
            }, model.Items);

            return new TodoModel(items_, model.Input, model.Filter);
        }() : msg.Case === "CheckAll" ? checkAllWith(true) : msg.Case === "UnCheckAll" ? checkAllWith(false) : msg.Case === "Destroy" ? function () {
            var i = msg.Fields[0];

            var items_ = _fableCore.List.filter(function (i_) {
                return i_.Id !== i.Id;
            }, model.Items);

            return new TodoModel(items_, model.Input, model.Filter);
        }() : msg.Case === "ToggleItem" ? function () {
            var i = msg.Fields[0];
            return updateItem(function () {
                var Done = !i.Done;
                return new Item(i.Name, Done, i.Id, i.IsEditing);
            }())(model);
        }() : msg.Case === "SetActiveFilter" ? function () {
            var f = msg.Fields[0];
            return new TodoModel(model.Items, model.Input, f);
        }() : msg.Case === "ClearCompleted" ? function () {
            var items_ = _fableCore.List.filter(function (i) {
                return !i.Done;
            }, model.Items);

            return new TodoModel(items_, model.Input, model.Filter);
        }() : msg.Case === "EditItem" ? function () {
            var i = msg.Fields[0];
            return updateItem(function () {
                var IsEditing = true;
                return new Item(i.Name, i.Done, i.Id, IsEditing);
            }())(model);
        }() : msg.Case === "SaveItem" ? function () {
            var str = msg.Fields[1];
            var i = msg.Fields[0];
            return updateItem(function () {
                var IsEditing = false;
                return new Item(str, i.Done, i.Id, IsEditing);
            }())(model);
        }() : msg.Case === "Nop" ? model : function () {
            var maxId = model.Items.tail == null ? 1 : _fableCore.Seq.max(_fableCore.List.map(function (x) {
                return x.Id;
            }, model.Items));

            var item_ = function () {
                var Id = maxId + 1;
                return new Item(model.Input, false, Id, false);
            }();

            return new TodoModel(_fableCore.List.ofArray([item_], model.Items), "", model.Filter);
        }();
        var jsCalls = msg.Case === "EditItem" ? function () {
            var i = msg.Fields[0];
            return _fableCore.List.ofArray([function (unitVar0) {
                document.getElementById("item-" + i.Id.toString()).focus();
            }]);
        }() : new _fableCore.List();
        return [model_, jsCalls];
    };

    var filterToTextAndUrl = exports.filterToTextAndUrl = function (_arg1) {
        return _arg1.Case === "Completed" ? ["Completed", "completed"] : _arg1.Case === "Active" ? ["Active", "active"] : ["All", ""];
    };

    var filter = exports.filter = function (activeFilter, f) {
        var linkClass = _fableCore.Util.compareTo(f, activeFilter) === 0 ? "selected" : "";
        var patternInput = filterToTextAndUrl(f);
        var url = patternInput[1];
        var fText = patternInput[0];
        return function () {
            var tagName = "li";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("MouseEventHandler", ["onclick", function (_arg1) {
                    return new TodoAction("SetActiveFilter", f);
                }]))])], children);
            };
        }()(_fableCore.List.ofArray([function () {
            var tagName = "a";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["href", "#/" + url]), new _FableHelpers.Html.Types.Attribute("Attribute", ["class", linkClass])])], children);
            };
        }()(_fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Text", fText)]))]));
    };

    var filters = exports.filters = function (model) {
        return function () {
            var tagName = "ul";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "filters"])])], children);
            };
        }()(_fableCore.List.map(function () {
            var activeFilter = model.Filter;
            return function (f) {
                return filter(activeFilter, f);
            };
        }(), _fableCore.List.ofArray([new Filter("All"), new Filter("Active"), new Filter("Completed")])));
    };

    var todoFooter = exports.todoFooter = function (model) {
        var clearVisibility = _fableCore.Seq.exists(function (i) {
            return i.Done;
        }, model.Items) ? "" : "none";

        var activeCount = _fableCore.List.filter(function (i) {
            return !i.Done;
        }, model.Items).length.toString();

        return function () {
            var tagName = "footer";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "footer"]), new _FableHelpers.Html.Types.Attribute("Style", _fableCore.List.ofArray([["display", "block"]]))])], children);
            };
        }()(_fableCore.List.ofArray([function () {
            var tagName = "span";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "todo-count"])])], children);
            };
        }()(_fableCore.List.ofArray([function () {
            var tagName = "strong";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, new _fableCore.List()], children);
            };
        }()(_fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Text", activeCount)])), new _FableHelpers.Html.Types.Node("Text", " items left")])), filters(model), function () {
            var tagName = "button";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "clear-completed"]), new _FableHelpers.Html.Types.Attribute("Style", _fableCore.List.ofArray([["display", clearVisibility]])), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("MouseEventHandler", ["onclick", function (_arg1) {
                    return new TodoAction("ClearCompleted");
                }]))])], children);
            };
        }()(_fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Text", "Clear completed")]))]));
    };

    var todoHeader = exports.todoHeader = function (model) {
        return function () {
            var tagName = "header";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "header"])])], children);
            };
        }()(_fableCore.List.ofArray([function () {
            var tagName = "h1";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, new _fableCore.List()], children);
            };
        }()(_fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Text", "todos")])), new _FableHelpers.Html.Types.Node("VoidElement", ["input", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "new-todo"]), new _FableHelpers.Html.Types.Attribute("Attribute", ["id", "new-todo"]), new _FableHelpers.Html.Types.Attribute("Property", ["value", model]), new _FableHelpers.Html.Types.Attribute("Property", ["placeholder", "What needs to be done?"]), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("EventHandler", ["oninput", function (x) {
            console.debug(function () {
                var clo1 = _fableCore.String.fsFormat("%A")(function (x) {
                    return x;
                });

                return function (arg10) {
                    return clo1(arg10);
                };
            }()(x));
            return new TodoAction("ChangeInput", x.target.value);
        }])), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("KeyboardEventHandler", ["onkeydown", function (x) {
            return x.keyCode === 13 ? new TodoAction("AddItem") : new TodoAction("Nop");
        }]))])])]));
    };

    var listItem = exports.listItem = function (item) {
        var itemChecked = item.Done ? "true" : "";
        var editClass = item.IsEditing ? "editing" : "";
        return function () {
            var tagName = "li";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", (item.Done ? "completed " : " ") + editClass])])], children);
            };
        }()(_fableCore.List.ofArray([function () {
            var tagName = "div";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "view"]), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("MouseEventHandler", ["ondblclick", function (x) {
                    return new TodoAction("EditItem", item);
                }]))])], children);
            };
        }()(_fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("VoidElement", ["input", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Property", ["className", "toggle"]), new _FableHelpers.Html.Types.Attribute("Property", ["type", "checkbox"]), new _FableHelpers.Html.Types.Attribute("Property", ["checked", itemChecked]), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("MouseEventHandler", ["onclick", function (e) {
            return new TodoAction("ToggleItem", item);
        }]))])]), function () {
            var tagName = "label";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, new _fableCore.List()], children);
            };
        }()(_fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Text", item.Name)])), function () {
            var tagName = "button";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "destroy"]), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("MouseEventHandler", ["onclick", function (e) {
                    return new TodoAction("Destroy", item);
                }]))])], children);
            };
        }()(new _fableCore.List())])), new _FableHelpers.Html.Types.Node("VoidElement", ["input", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "edit"]), new _FableHelpers.Html.Types.Attribute("Attribute", ["value", item.Name]), new _FableHelpers.Html.Types.Attribute("Property", ["id", "item-" + item.Id.toString()]), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("EventHandler", ["onblur", function (e) {
            return new TodoAction("SaveItem", item, e.target.value);
        }]))])])]));
    };

    var itemList = exports.itemList = function (items, activeFilter) {
        var filterItems = function (i) {
            return activeFilter.Case === "Completed" ? i.Done : activeFilter.Case === "Active" ? !i.Done : true;
        };

        return function () {
            var tagName = "ul";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "todo-list"])])], children);
            };
        }()(function (list) {
            return _fableCore.List.map(function (item) {
                return listItem(item);
            }, list);
        }(function (list) {
            return _fableCore.List.filter(filterItems, list);
        }(items)));
    };

    var todoMain = exports.todoMain = function (model) {
        var items = model.Items;

        var allChecked = _fableCore.Seq.exists(function (i) {
            return !i.Done;
        }, items);

        return function () {
            var tagName = "section";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "main"]), new _FableHelpers.Html.Types.Attribute("Style", _fableCore.List.ofArray([["style", "block"]]))])], children);
            };
        }()(_fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("VoidElement", ["input", _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Property", ["id", "toggle-all"]), new _FableHelpers.Html.Types.Attribute("Attribute", ["class", "toggle-all"]), new _FableHelpers.Html.Types.Attribute("Property", ["type", "checkbox"]), new _FableHelpers.Html.Types.Attribute("Property", ["checked", !allChecked ? "true" : ""]), new _FableHelpers.Html.Types.Attribute("EventHandlerBinding", new _FableHelpers.Html.Types.EventHandlerBinding("MouseEventHandler", ["onclick", function (e) {
            return allChecked ? new TodoAction("CheckAll") : new TodoAction("UnCheckAll");
        }]))])]), function () {
            var tagName = "label";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Attribute", ["for", "toggle-all"])])], children);
            };
        }()(_fableCore.List.ofArray([new _FableHelpers.Html.Types.Node("Text", "Mark all as complete")])), itemList(items, model.Filter)]));
    };

    var todoView = exports.todoView = function (model) {
        return function () {
            var tagName = "section";
            return function (children) {
                return new _FableHelpers.Html.Types.Node("Element", [tagName, _fableCore.List.ofArray([new _FableHelpers.Html.Types.Attribute("Property", ["class", "todoapp"])])], children);
            };
        }()(_fableCore.List.ofArray([todoHeader(model.Input)], model.Items.tail == null ? new _fableCore.List() : _fableCore.List.ofArray([todoMain(model), todoFooter(model)])));
    };

    var Storage = exports.Storage = function ($exports) {
        var STORAGE_KEY = "vdom-storage";

        var fetch = $exports.fetch = function () {
            return function (arg00) {
                return JSON.parse(arg00);
            }(function (_arg1) {
                return _arg1 == null ? "[]" : function () {
                    var x = _arg1;
                    return x;
                }();
            }(localStorage.getItem(STORAGE_KEY)));
        };

        var save = $exports.save = function (todos) {
            localStorage.setItem(STORAGE_KEY, JSON.stringify(todos));
        };

        return $exports;
    }({});

    var initList = exports.initList = _fableCore.List.ofArray(Storage.fetch());

    var initModel = exports.initModel = function () {
        var Filter_1 = new Filter("All");
        return new TodoModel(initList, "", Filter_1);
    }();

    var todoApp = exports.todoApp = _FableHelpers.App.withStartNode("#todo", _FableHelpers.App.createApp(new _FableHelpers.App.AppState(initModel, function (model) {
        return todoView(model);
    }, function (model) {
        return function (msg) {
            return todoUpdate(model, msg);
        };
    })));

    _FableHelpers.App.start((0, _FableHelpers.renderer)(), todoApp);
});
//# sourceMappingURL=virtualdom.js.map