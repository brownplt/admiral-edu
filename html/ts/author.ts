
class AuthorItem {
    label : string;

    constructor(label){
	this.label = label;
    }
    
    getElement() : any {
	var emptyDiv = document.createElement('div');
	return emptyDiv;
    }

    toDOM(){
	var item = document.createElement('div');
	item.className = "author-item";
	item.innerHTML = "<p>" + this.label + "</p>";
	item.appendChild(this.getElement());
	return item;
    }
}

class ReviewsItem extends AuthorItem {

    step : Step;

    constructor(step : Step){
	super("");
	this.step = step;
    }

    toDOM() {
	var dom = super.toDOM();
	dom.className = "reviews";
	return dom;
    }

    getElement() {
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
    }
}

class RubricItem {

    toDOM() {
	var parent = document.createElement('div');
	var div = document.createElement('div');
	div.className = "rubric-label";
	div.innerHTML = "<p>Rubric</p>";
	var rubric = document.createElement('div');
	rubric.className = "rubric";
	parent.appendChild(div);
	parent.appendChild(rubric);
	return parent;
    }
}


interface Changeable {
    (value : string) : void;
}

interface Changing {
    onkeyup : any;
    value : string;
}

class ChangingItem extends AuthorItem {

    onchange : Changeable;
    
    constructor(label, onchange){
	super(label);
	this.onchange = onchange;
    }

    construct() : Changing {
	throw "Subclass must define construct()";
    }

    getElement() {
	var input = this.construct();
	input.onkeyup = this.onchange(input.value);
	return input;
    }

}

class InputItem extends ChangingItem {
    
    constructor(label, onchange){
	super(label, onchange);
    }
    
    construct(){
	var input = <any>document.createElement('input');
	input.setAttribute('type', 'text');
	return <Changing>input;
    }
}

class TextAreaItem extends ChangingItem {
    constructor(label, onchange){
	super(label, onchange);
    }

    construct(){
	var input = <any>document.createElement('textarea');
	return <Changing>input;
    }
}

class SelectItem extends AuthorItem {

    options : string[];
    actions : Action[];
    
    constructor(label : string){
	super(label);
	this.options = new Array();
	this.actions = new Array();
    }

    addOption(option : string, onselect : Action){
	this.options.push(option);
	this.actions.push(onselect);
    }

    getElement(){
	var _this = this;
	var div = document.createElement('div');
	var select = document.createElement('select');

	select.onchange = function () {
	    var i = (<any>select).selectedIndex;
	    _this.actions[i]();
	}

	div.appendChild(select);
	for(var i = 0; i < this.options.length; i++){
	    var option = document.createElement('option');
	    option.innerHTML = this.options[i];
	    select.appendChild(option);
	}
	return div;
    }

}


class AddStepButton {

    assignment : AssignmentDescription;
    steps : any;

    constructor(assignment, steps){
	this.assignment = assignment;
	this.steps = steps;
    }

    toDOM(){
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
    }

}

class AssignmentDescription {

    name : string;
    id : string;
    description : string;
    steps : Step[];

    constructor() {
	this.steps = new Array();
    }

    setName(name : string){
	this.name = name;
    }

    setId(id : string){
	this.id = id;
    }

    setDescription(description : string){
	this.description = description;
    }

    attach(parent : string) {
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
	
    }

    addStep(){
	var _this = this;
	var step = new Step();
	step.remove = function () {
	    var i = _this.steps.indexOf(step);
	    if(i > -1){
		_this.steps.splice(i, 1);
	    }
	}
	this.steps.push(step);
	return step;
    }

}

class Step {

    id : string;
    instructions : string;
    reviews : Review[];
    remove : Action;
   
    constructor(){
	this.reviews = new Array();
    }

    setID (id : string){
	this.id = id;
    }

    setInstructions (instructions : string){
	this.instructions = instructions;
    }

    addReview (){
	var _this = this;
	var review = new Review();
	review.remove = function () {
	    var i = _this.reviews.indexOf(review);
	    if(i > -1){
		_this.reviews.splice(i, 1);
	    }
	}
	this.reviews.push(review);
	return review;
    }

    toDOM() {
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
	}
	
	header.appendChild(removeButton);
	section.appendChild(header);
	section.appendChild(new InputItem("ID", this.setID).toDOM());
	section.appendChild(new TextAreaItem("Instructions", this.setInstructions).toDOM());
	section.appendChild(new ReviewsItem(this).toDOM());

	return section;
    }
}

interface Action {
    () : void;
}

class Review {

    remove : Action;
    rubric : RubricItem;

    constructor(){
	this.rubric = new RubricItem();
    }

    toDOM() {
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
	}
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
    }

}



window.onload = function() {
    var assignment = new AssignmentDescription();
    assignment.attach("assignment");
    console.log("loaded.");
}