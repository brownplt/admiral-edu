declare function save(json, callback) : void;
declare function load(callback) : void;

module CaptainTeach.Rubric {


    interface Element {
	getId() : string;
	getPrompt() : string;
	toDOM();
    }

    class BasicElement implements Element {

	prompt : string;
	id : string;

	constructor(prompt : string, id : string){
	    this.prompt = prompt;
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

    }

    class LikertElement extends BasicElement {

	minLabel : string;
	maxLabel : string;
	rangeSize : number;

	constructor(prompt, id, minLabel, maxLabel, rangeSize){
	    super(prompt, id);
	    this.minLabel = minLabel;
	    this.maxLabel = maxLabel;
	    this.rangeSize = rangeSize;
	}

	toDOM(){
	    var rubricItem = super.toDOM();
	    var list = document.createElement('ul');
	    var min = document.createElement('li');
	    list.appendChild(min);
	    min.innerHTML = this.minLabel;
	    for(var i = 0; i < this.rangeSize; i++){
		var item = document.createElement('li');
		var input = document.createElement('input');
		input.setAttribute('type', "radio");
		input.setAttribute('name', this.getId());
		input.setAttribute('value', ""+i);
		item.appendChild(input);
		list.appendChild(item);
	    }
	    var max = document.createElement('li');
	    list.appendChild(max);
	    max.innerHTML = this.maxLabel;
	    return rubricItem;
	}
    }

    window.onload = function() {
	var rubric = document.getElementById('rubric');
	var basic = new BasicElement("When reviewing a file, you can leave feedback at a specific location by clicking on a line number.");
	var likert1 = new LikertElement("This code correctly implements the desired behavior.", "behavior", "Disagree", "Agree", 9);

	

    }

}

