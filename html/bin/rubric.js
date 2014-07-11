var __extends = this.__extends || function (d, b) {
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
}
function escape(input) {
    var output = "";
    for(var i = 0; i < input.length; i++) {
        var c = input.charAt(i);
        if(c == '"') {
            output += "\\\"";
        } else {
            output += c;
        }
    }
    return output;
}
function quote(toquote) {
    return "\"" + escape(toquote) + "\"";
}
function concat(map0, map1) {
    var newMap = {
    };
    for(var key in map0) {
        newMap[key] = map0[key];
    }
    for(var key in map1) {
        newMap[key] = map1[key];
    }
    return newMap;
}
var CaptainTeach;
(function (CaptainTeach) {
    (function (Rubric) {
        var elementConstructors = {
        };
        var Rubric = (function () {
            function Rubric() {
                this.elements = new Array();
            }
            Rubric.fromJson = function fromJson(json) {
                var newRubric = new Rubric();
                var rubric = json.rubric;
                for(var i = 0; i < rubric.length; i++) {
                    var el_json = rubric[i];
                    if(!("class" in el_json)) {
                        throw ("Could not create rubric from: " + rubric);
                    }
                    if(!(el_json.class in elementConstructors)) {
                        throw ("Could not create rubric from: " + rubric);
                    }
                    var fromJson = elementConstructors[el_json.class];
                    var element = fromJson(el_json);
                    newRubric.append(element);
                }
                return newRubric;
            }
            Rubric.prototype.append = function (el) {
                this.elements.push(el);
                el.register(this);
                return this;
            };
            Rubric.prototype.toJson = function () {
                var array = new Array(this.elements.length);
                for(var i = 0; i < this.elements.length; i++) {
                    array[i] = this.elements[i].toJson();
                }
                var json = {
                    "rubric": array
                };
                return json;
            };
            Rubric.prototype.onchange = function () {
                save(this.toJson(), function () {
                });
            };
            Rubric.prototype.attach = function (id) {
                var element = document.getElementById(id);
                for(var i = 0; i < this.elements.length; i++) {
                    var e = this.elements[i];
                    e.toDOM(element);
                }
            };
            return Rubric;
        })();        
        var BasicElement = (function () {
            function BasicElement(prompt, id) {
                this.prompt = prompt;
                this.id = id;
                this.rubric = null;
            }
            BasicElement.clazz = "BasicElement";
            BasicElement.prototype.getClass = function () {
                return BasicElement.clazz;
            };
            BasicElement.fromJson = function fromJson(element) {
                if(!("id" in element && "prompt" in element)) {
                    throw ("Could not create " + BasicElement.clazz + " from" + element);
                }
                var el = new BasicElement(element.prompt, element.id);
                return el;
            }
            BasicElement.prototype.toJson = function () {
                var json = {
                    "class": this.getClass()
                };
                return concat(json, this.innerJson());
            };
            BasicElement.prototype.innerJson = function () {
                var json = {
                    "prompt": this.prompt,
                    "id": this.id
                };
                return json;
            };
            BasicElement.prototype.getId = function () {
                return this.id;
            };
            BasicElement.prototype.getPrompt = function () {
                return this.prompt;
            };
            BasicElement.prototype.toDOM = function (parent) {
                var rubricItem = document.createElement('div');
                parent.appendChild(rubricItem);
                rubricItem.className = "rubric-item instruction";
                var prompt = document.createElement('p');
                prompt.innerHTML = this.prompt;
                rubricItem.appendChild(prompt);
                return rubricItem;
            };
            BasicElement.prototype.notify = function () {
                if(this.rubric == null) {
                    return;
                }
                this.rubric.onchange();
            };
            BasicElement.prototype.register = function (rubric) {
                this.rubric = rubric;
            };
            return BasicElement;
        })();        
        var LikertElement = (function (_super) {
            __extends(LikertElement, _super);
            function LikertElement(prompt, id, minLabel, maxLabel, rangeSize) {
                        _super.call(this, prompt, id);
                this.minLabel = minLabel;
                this.maxLabel = maxLabel;
                this.rangeSize = rangeSize;
                this.selected = -1;
            }
            LikertElement.clazz = "LikertElement";
            LikertElement.fromJson = function fromJson(element) {
                if(!("maxLabel" in element && "minLabel" in element && "rangeSize" in element && "selected" in element && "id" in element && "prompt" in element)) {
                    throw ("Could not create FreeFormElement from" + element);
                }
                var el = new LikertElement(element.prompt, element.id, element.minLabel, element.maxLabel, element.rangeSize);
                el.selected = element.selected;
                return el;
            }
            LikertElement.prototype.getClass = function () {
                return LikertElement.clazz;
            };
            LikertElement.prototype.innerJson = function () {
                var inner = _super.prototype.innerJson.call(this);
                var json = {
                    "minLabel": this.minLabel,
                    "maxLabel": this.maxLabel,
                    "rangeSize": this.rangeSize,
                    "selected": this.selected
                };
                var merged = concat(inner, json);
                return concat(inner, json);
            };
            LikertElement.prototype.getOnChange = function (_this, input) {
                return function () {
                    _this.selected = input;
                    _this.notify();
                }
            };
            LikertElement.prototype.toDOM = function (parent) {
                var rubricItem = _super.prototype.toDOM.call(this, parent);
                rubricItem.className = "rubric-item likert question";
                var list = document.createElement('ul');
                rubricItem.appendChild(list);
                var min = document.createElement('li');
                list.appendChild(min);
                min.innerHTML = this.minLabel;
                for(var i = 0; i < this.rangeSize; i++) {
                    var item = document.createElement('li');
                    var input = document.createElement('input');
                    input.setAttribute('type', "radio");
                    input.setAttribute('name', this.getId());
                    input.setAttribute('value', "" + i);
                    input.onclick = this.getOnChange(this, i);
                    if(i == this.selected) {
                        input.setAttribute('checked', "true");
                    }
                    item.appendChild(input);
                    list.appendChild(item);
                }
                var max = document.createElement('li');
                list.appendChild(max);
                max.innerHTML = this.maxLabel;
                return rubricItem;
            };
            return LikertElement;
        })(BasicElement);        
        var FreeFormElement = (function (_super) {
            __extends(FreeFormElement, _super);
            function FreeFormElement(prompt, id) {
                        _super.call(this, prompt, id);
                this.content = "";
            }
            FreeFormElement.clazz = "FreeFormElement";
            FreeFormElement.fromJson = function fromJson(element) {
                if(!("content" in element && "id" in element && "prompt" in element)) {
                    throw ("Could not create FreeFormElement from" + element);
                }
                var el = new FreeFormElement(element.prompt, element.id);
                el.content = element.content;
                return el;
            }
            FreeFormElement.prototype.getClass = function () {
                return FreeFormElement.clazz;
            };
            FreeFormElement.prototype.innerJson = function () {
                var inner = _super.prototype.innerJson.call(this);
                var val = {
                    "content": this.content
                };
                return concat(inner, val);
            };
            FreeFormElement.prototype.getOnChange = function (_this, input) {
                return function () {
                    _this.content = input;
                    if(_this.autosave == null) {
                        _this.autosave = function () {
                            _this.notify();
                            _this.autosave = null;
                            console.log("Saved.");
                        };
                        window.setTimeout(_this.autosave, 2000);
                    }
                }
            };
            FreeFormElement.prototype.toDOM = function (parent) {
                var rubricItem = _super.prototype.toDOM.call(this, parent);
                rubricItem.className = "rubric-item free-response question";
                var content = document.createElement('textarea');
                var _this = this;
                content.onkeyup = function (_) {
                    _this.getOnChange(_this, (content).value)();
                    content.style.height = "";
                    content.style.height = Math.min(content.scrollHeight) + "px";
                };
                content.onchange = function () {
                    _this.content = (content).value;
                    content.style.height = "";
                    content.style.height = Math.min(content.scrollHeight) + "px";
                    _this.notify();
                };
                rubricItem.appendChild(content);
                content.setAttribute('name', this.getId());
                (content).value = this.content;
                content.onkeyup(null);
                return rubricItem;
            };
            return FreeFormElement;
        })(BasicElement);        
        elementConstructors[BasicElement.clazz] = BasicElement.fromJson;
        elementConstructors[LikertElement.clazz] = LikertElement.fromJson;
        elementConstructors[FreeFormElement.clazz] = FreeFormElement.fromJson;
        window.onload = function () {
            load(function (json) {
                var rubric = Rubric.fromJson(json);
                rubric.attach("rubric");
                console.log("Loaded.");
            });
        };
    })(CaptainTeach.Rubric || (CaptainTeach.Rubric = {}));
    var Rubric = CaptainTeach.Rubric;

})(CaptainTeach || (CaptainTeach = {}));

