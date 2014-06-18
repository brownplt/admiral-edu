var __extends = this.__extends || function (d, b) {
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
}
var CaptainTeach;
(function (CaptainTeach) {
    (function (Rubric) {
        var BasicElement = (function () {
            function BasicElement(prompt, id) {
                this.prompt = prompt;
                this.id = id;
                this.rubric = null;
            }
            BasicElement.prototype.getId = function () {
                return this.id;
            };
            BasicElement.prototype.getPrompt = function () {
                return this.prompt;
            };
            BasicElement.prototype.toDOM = function () {
                var rubricItem = document.createElement('div');
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
            LikertElement.prototype.getOnChange = function (_this, input) {
                return function () {
                    _this.selected = input;
                    _this.notify();
                }
            };
            LikertElement.prototype.toDOM = function () {
                var rubricItem = _super.prototype.toDOM.call(this);
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
            }
            FreeFormElement.prototype.getOnChange = function (_this, input) {
                return function () {
                    _this.content = input;
                    if(_this.autosave == null) {
                        _this.autosave = function () {
                            _this.notify();
                            _this.autosave = null;
                        };
                        window.setTimeout(_this.autosave, 5000);
                    }
                }
            };
            FreeFormElement.prototype.toDOM = function () {
                var rubricItem = _super.prototype.toDOM.call(this);
                rubricItem.className = "rubric-item free-response question";
                var content = document.createElement('textarea');
                content.setAttribute('name', this.getId());
                content.onkeyup = this.getOnChange(this, (content).value);
                var _this = this;
                content.onchange = function () {
                    _this.content = (content).value;
                    _this.notify();
                };
                rubricItem.appendChild(content);
                return rubricItem;
            };
            return FreeFormElement;
        })(BasicElement);        
        window.onload = function () {
            var rubric = document.getElementById('rubric');
            var basic = new BasicElement("When reviewing a file, you can leave feedback at a specific location by clicking on a line number.", "prompt");
            var likert1 = new LikertElement("This code correctly implements the desired behavior.", "behavior", "Disagree", "Agree", 9);
            var likert2 = new LikertElement("This code is structured well.", "structure", "Disagree", "Agree", 9);
            var freeform = new FreeFormElement("Additional Comments", "feedback");
            rubric.appendChild(basic.toDOM());
            rubric.appendChild(likert1.toDOM());
            rubric.appendChild(likert2.toDOM());
            rubric.appendChild(freeform.toDOM());
        };
    })(CaptainTeach.Rubric || (CaptainTeach.Rubric = {}));
    var Rubric = CaptainTeach.Rubric;

})(CaptainTeach || (CaptainTeach = {}));

