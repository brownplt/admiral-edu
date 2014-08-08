var __extends = this.__extends || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
};
var AuthorItem = (function () {
    function AuthorItem(label) {
        this.label = label;
    }
    AuthorItem.prototype.getElement = function () {
        var emptyDiv = document.createElement('div');
        return emptyDiv;
    };

    AuthorItem.prototype.toDOM = function () {
        var item = document.createElement('div');
        item.className = "author-item";
        item.innerHTML = "<p>" + this.label + "</p>";
        item.appendChild(this.getElement());
        return item;
    };
    return AuthorItem;
})();

var ReviewsItem = (function (_super) {
    __extends(ReviewsItem, _super);
    function ReviewsItem(step) {
        _super.call(this, "");
        this.step = step;
    }
    ReviewsItem.prototype.toDOM = function () {
        var dom = _super.prototype.toDOM.call(this);
        dom.className = "reviews";
        return dom;
    };

    ReviewsItem.prototype.getElement = function () {
        var _this = this;

        var div = document.createElement('div');

        var reviews = document.createElement('div');

        var buttonDiv = document.createElement('div');
        buttonDiv.className = "add-step";

        var button = document.createElement('a');
        button.innerHTML = "Add Review";
        button.setAttribute('href', 'javascript:void(0);');
        button.setAttribute('title', "Add Review");

        button.onclick = function () {
            var review = _this.step.addReview();
            reviews.appendChild(review.toDOM());
        };
        div.appendChild(reviews);
        div.appendChild(buttonDiv);
        buttonDiv.appendChild(button);
        return div;
    };
    return ReviewsItem;
})(AuthorItem);

var RubricItem = (function () {
    function RubricItem() {
    }
    RubricItem.prototype.toDOM = function () {
        var parent = document.createElement('div');
        var div = document.createElement('div');
        div.className = "rubric-label";
        div.innerHTML = "<p>Rubric</p>";
        var rubric = document.createElement('div');
        rubric.className = "rubric";
        parent.appendChild(div);
        parent.appendChild(rubric);
        return parent;
    };
    return RubricItem;
})();

var ChangingItem = (function (_super) {
    __extends(ChangingItem, _super);
    function ChangingItem(label, onchange) {
        _super.call(this, label);
        this.onchange = onchange;
    }
    ChangingItem.prototype.construct = function () {
        throw "Subclass must define construct()";
    };

    ChangingItem.prototype.getElement = function () {
        var input = this.construct();
        input.onkeyup = this.onchange(input.value);
        return input;
    };
    return ChangingItem;
})(AuthorItem);

var InputItem = (function (_super) {
    __extends(InputItem, _super);
    function InputItem(label, onchange) {
        _super.call(this, label, onchange);
    }
    InputItem.prototype.construct = function () {
        var input = document.createElement('input');
        input.setAttribute('type', 'text');
        return input;
    };
    return InputItem;
})(ChangingItem);

var TextAreaItem = (function (_super) {
    __extends(TextAreaItem, _super);
    function TextAreaItem(label, onchange) {
        _super.call(this, label, onchange);
    }
    TextAreaItem.prototype.construct = function () {
        var input = document.createElement('textarea');
        return input;
    };
    return TextAreaItem;
})(ChangingItem);

var SelectItem = (function (_super) {
    __extends(SelectItem, _super);
    function SelectItem(label) {
        _super.call(this, label);
        this.options = new Array();
        this.actions = new Array();
    }
    SelectItem.prototype.addOption = function (option, onselect) {
        this.options.push(option);
        this.actions.push(onselect);
    };

    SelectItem.prototype.getElement = function () {
        var _this = this;
        var div = document.createElement('div');
        var select = document.createElement('select');

        select.onchange = function () {
            var i = select.selectedIndex;
            _this.actions[i]();
        };

        div.appendChild(select);
        for (var i = 0; i < this.options.length; i++) {
            var option = document.createElement('option');
            option.innerHTML = this.options[i];
            select.appendChild(option);
        }
        return div;
    };
    return SelectItem;
})(AuthorItem);

var AddStepButton = (function () {
    function AddStepButton(assignment, steps) {
        this.assignment = assignment;
        this.steps = steps;
    }
    AddStepButton.prototype.toDOM = function () {
        var _this = this;
        var buttonDiv = document.createElement('div');
        buttonDiv.className = "add-step";
        var button = document.createElement('a');
        button.setAttribute('href', 'javascript:void(0);');
        button.onclick = function () {
            var step = _this.assignment.addStep();
            _this.steps.appendChild(step.toDOM());
        };
        button.innerHTML = "Add Submission";
        buttonDiv.appendChild(button);
        return buttonDiv;
    };
    return AddStepButton;
})();

var AssignmentDescription = (function () {
    function AssignmentDescription() {
        this.steps = new Array();
    }
    AssignmentDescription.prototype.setName = function (name) {
        this.name = name;
    };

    AssignmentDescription.prototype.setId = function (id) {
        this.id = id;
    };

    AssignmentDescription.prototype.setDescription = function (description) {
        this.description = description;
    };

    AssignmentDescription.prototype.attach = function (parent) {
        var _this = this;
        var element = document.getElementById(parent);
        var section = document.createElement('div');
        var info = document.createElement('div');
        info.className = "section";
        var header = document.createElement('div');
        header.className = "section-header bg-1";
        header.innerHTML = "<h3>Assignment</h3>";
        info.appendChild(header);

        var name = new InputItem("Name", this.setName);
        var id = new InputItem("ID", this.setId);
        var description = new TextAreaItem("Description", this.setDescription);

        info.appendChild(name.toDOM());
        info.appendChild(id.toDOM());
        info.appendChild(description.toDOM());

        section.appendChild(info);

        var steps = document.createElement('div');
        info.appendChild(steps);

        var addStepButton = new AddStepButton(this, steps);

        info.appendChild(addStepButton.toDOM());

        element.appendChild(section);
    };

    AssignmentDescription.prototype.addStep = function () {
        var _this = this;
        var step = new Step();
        step.remove = function () {
            var i = _this.steps.indexOf(step);
            if (i > -1) {
                _this.steps.splice(i, 1);
            }
        };
        this.steps.push(step);
        return step;
    };
    return AssignmentDescription;
})();

var Step = (function () {
    function Step() {
        this.reviews = new Array();
    }
    Step.prototype.setID = function (id) {
        this.id = id;
    };

    Step.prototype.setInstructions = function (instructions) {
        this.instructions = instructions;
    };

    Step.prototype.addReview = function () {
        var _this = this;
        var review = new Review();
        review.remove = function () {
            var i = _this.reviews.indexOf(review);
            if (i > -1) {
                _this.reviews.splice(i, 1);
            }
        };
        this.reviews.push(review);
        return review;
    };

    Step.prototype.toDOM = function () {
        var _this = this;
        var section = document.createElement('div');
        section.className = "step";

        var header = document.createElement('div');
        header.className = "section-header bg-2";
        header.innerHTML = "<h3>Submission</h3>";

        var removeButton = document.createElement('a');
        removeButton.innerHTML = "&nbsp;";
        removeButton.setAttribute('href', 'javascript:void(0);');
        removeButton.setAttribute('title', 'Remove Submission');
        removeButton.onclick = function () {
            _this.remove();
            section.parentNode.removeChild(section);
        };

        header.appendChild(removeButton);
        section.appendChild(header);
        section.appendChild(new InputItem("ID", this.setID).toDOM());
        section.appendChild(new TextAreaItem("Instructions", this.setInstructions).toDOM());
        section.appendChild(new ReviewsItem(this).toDOM());

        return section;
    };
    return Step;
})();

var Review = (function () {
    function Review() {
        this.rubric = new RubricItem();
    }
    Review.prototype.toDOM = function () {
        var _this = this;
        var div = document.createElement('div');
        div.className = "review step";

        var header = document.createElement('div');
        header.className = "section-header bg-3";
        header.innerHTML = "<h3>Review</h3>";

        var remove = document.createElement('a');
        remove.innerHTML = "&nbsp;";
        remove.setAttribute('href', 'javascript:void(0);');
        remove.setAttribute('title', 'Remove Review');
        remove.onclick = function () {
            div.parentNode.removeChild(div);
            _this.remove();
        };
        header.appendChild(remove);

        div.appendChild(header);

        var type = new SelectItem("Type");

        var studentSelect = function () {
            alert("student");
        };

        var instSelect = function () {
            alert("inst");
        };

        type.addOption("Student Submission", studentSelect);
        type.addOption("Instructor Solution", instSelect);
        div.appendChild(type.toDOM());
        div.appendChild(this.rubric.toDOM());

        return div;
    };
    return Review;
})();

window.onload = function () {
    var assignment = new AssignmentDescription();
    assignment.attach("assignment");
    console.log("loaded.");
};
