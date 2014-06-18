declare function save(json, callback) : void;
declare function load(callback) : void;

module CaptainTeach.Rubric {


    interface Element {
	getId() : string;
	getPrompt() : string;
	toDOM();
	notify();
	register(rubric) : void;
    }

    class BasicElement implements Element {

	prompt : string;
	id : string;
	rubric;
	

	constructor(prompt : string, id : string){
	    this.prompt = prompt;
	    this.id = id;
	    this.rubric = null;
	}

	getId(){
	    return this.id;
	}

	getPrompt(){
	    return this.prompt;
	}

	toDOM(){
	    var rubricItem = document.createElement('div');
	    rubricItem.className = "rubric-item instruction";
	    
	    var prompt = document.createElement('p');
	    prompt.innerHTML = this.prompt;

	    rubricItem.appendChild(prompt);
	    return rubricItem;
	}

	notify(){
	    if(this.rubric == null) return;
	    this.rubric.onchange();
	}

	register(rubric){
	    this.rubric = rubric;
	}

    }

    class LikertElement extends BasicElement {

	minLabel : string;
	maxLabel : string;
	rangeSize : number;
	selected : number;

	constructor(prompt, id, minLabel, maxLabel, rangeSize){
	    super(prompt, id);
	    this.minLabel = minLabel;
	    this.maxLabel = maxLabel;
	    this.rangeSize = rangeSize;
	    this.selected = -1;
	}

	getOnChange(_this, input){
	    return function () {
		_this.selected = input
		_this.notify();
	    }
	}

	toDOM(){
	    var rubricItem = super.toDOM();
	    rubricItem.className = "rubric-item likert question";
	    var list = document.createElement('ul');
	    rubricItem.appendChild(list);
	    var min = document.createElement('li');
	    list.appendChild(min);
	    min.innerHTML = this.minLabel;
	    for(var i = 0; i < this.rangeSize; i++){
		var item = document.createElement('li');
		var input = document.createElement('input');
		input.setAttribute('type', "radio");
		input.setAttribute('name', this.getId());
		input.setAttribute('value', ""+i);
		input.onclick = this.getOnChange(this, i);
		if(i == this.selected)
		    input.setAttribute('checked', "true");
		item.appendChild(input);
		list.appendChild(item);
	    }
	    var max = document.createElement('li');
	    list.appendChild(max);
	    max.innerHTML = this.maxLabel;
	    return rubricItem;
	}
    }

    class FreeFormElement extends BasicElement {

	content : string;
	autosave;

	constructor(prompt, id){
	    super(prompt, id);
	}

	getOnChange(_this, input){
	    return function() {
		_this.content = input;
		if(_this.autosave == null){
		    _this.autosave = function() {
			_this.notify();
			_this.autosave = null;
		    }
		    window.setTimeout(_this.autosave, 5000);
		}
	    }
	}

	toDOM(){
	    var rubricItem = super.toDOM();
	    rubricItem.className = "rubric-item free-response question";
	    var content = document.createElement('textarea');
	    content.setAttribute('name', this.getId());
	    content.onkeyup = this.getOnChange(this, (<any>content).value);
	    var _this = this;
	    content.onchange = function () { 
		_this.content = (<any>content).value;
		_this.notify();
	    }
	    rubricItem.appendChild(content);
	    return rubricItem;
	}
	
    }

    window.onload = function() {
	var rubric = document.getElementById('rubric');
	var basic = new BasicElement("When reviewing a file, you can leave feedback at a specific location by clicking on a line number.", "prompt");
	var likert1 = new LikertElement("This code correctly implements the desired behavior.", "behavior", "Disagree", "Agree", 9);
	var likert2 = new LikertElement("This code is structured well.", "structure", "Disagree", "Agree", 9);
	var freeform = new FreeFormElement("Additional Comments", "feedback");

	rubric.appendChild(basic.toDOM());
	rubric.appendChild(likert1.toDOM());
	rubric.appendChild(likert2.toDOM());
	rubric.appendChild(freeform.toDOM());

	

    }

}

