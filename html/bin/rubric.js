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
            return BasicElement;
        })();        
        var LikertElement = (function (_super) {
            __extends(LikertElement, _super);
            function LikertElement(prompt, id, minLabel, maxLabel, rangeSize) {
                        _super.call(this, prompt, id);
                this.minLabel = minLabel;
                this.maxLabel = maxLabel;
                this.rangeSize = rangeSize;
            }
            LikertElement.prototype.toDOM = function () {
                var rubricItem = _super.prototype.toDOM.call(this);
                var list = document.createElement('ul');
                var min = document.createElement('li');
                list.appendChild(min);
                min.innerHTML = this.minLabel;
                for(var i = 0; i < this.rangeSize; i++) {
                    var item = document.createElement('li');
                    var input = document.createElement('input');
                    input.setAttribute('type', "radio");
                    input.setAttribute('name', this.getId());
                    input.setAttribute('value', "" + i);
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
        window.onload = function () {
            var rubric = document.getElementById('rubric');
            var basic = new BasicElement("When reviewing a file, you can leave feedback at a specific location by clicking on a line number.");
            var likert1 = new LikertElement("This code correctly implements the desired behavior.", "behavior", "Disagree", "Agree", 9);
        };
    })(CaptainTeach.Rubric || (CaptainTeach.Rubric = {}));
    var Rubric = CaptainTeach.Rubric;

})(CaptainTeach || (CaptainTeach = {}));

