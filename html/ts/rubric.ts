declare function save(json, callback) : void;
declare function load(callback) : void;

function escape(input : string) : string {
    var output = "";
    for(var i = 0; i < input.length; i++){
	var c = input.charAt(i);
	if(c == '"') 
	    output += "\\\"";
	else 
	    output += c;
    }
    return output;
}

function quote(toquote : string) : string {
    return "\"" + escape(toquote) + "\"";
}


function concat(map0, map1) : {[key: any]: any;} {
    var newMap = {};
    for(var key in map0){
	newMap[key] = map0[key];
    }
    for(var key in map1){
	newMap[key] = map1[key];
    }
    return newMap;
}

module CaptainTeach.Rubric {
    var elementConstructors = {};

    class Rubric {

	elements : Element[];

	constructor() {
	    this.elements = new Array();
	}

	static fromJson(json){
	    var newRubric = new Rubric();
	    var rubric = json.rubric;
	    for(var i = 0; i < rubric.length; i++){
		var el_json = rubric[i];
		if(!("class" in el_json))
		    throw("Could not create rubric from: " + rubric);
		if(!(el_json.class in elementConstructors))
		    throw("Could not create rubric from: " + rubric);
		var fromJson = elementConstructors[el_json.class];
		var element = fromJson(el_json);
		newRubric.append(element);
	    }
	    return newRubric;
	}

	append(el : Element) : Rubric {
	    this.elements.push(el);
	    el.register(this);
	    return this;
	}
	
	toJson() {
	    var array = new Array(this.elements.length);
	    for(var i = 0; i < this.elements.length; i++){
		array[i] = this.elements[i].toJson();
	    }
	    var json = { "rubric" : array };
	    return json;
	}

	onchange() : void {
	    save(this.toJson(), function () {});
	}

	attach(id : string) : void{
	    var element = document.getElementById(id);
	    for(var i = 0; i < this.elements.length; i++){
		var e = this.elements[i];
		e.toDOM(element);
	    }
	}

	

    }


    interface Element {
	getClass() : string;
	getId() : string;
	getPrompt() : string;
	toDOM(parent);
	toJson();
	notify();
	register(rubric : Rubric) : void;

    }

    class BasicElement implements Element {

	static clazz = "BasicElement";
	prompt : string;
	id : string;
	rubric : Rubric;
	

	constructor(prompt : string, id : string){
	    this.prompt = prompt;
	    this.id = id;
	    this.rubric = null;
	}

	getClass() { return BasicElement.clazz; }

	static fromJson(element) : BasicElement {
	    if(! ("id" in element &&
		  "prompt" in element))
		throw("Could not create " + BasicElement.clazz + " from" + element);
	    var el = new BasicElement(element.prompt, element.id);
	    return el;
	}

	toJson() {
	    var json = { 
		"class" : this.getClass()
	    };
	    return concat(json, this.innerJson());
	}

	innerJson() : {[key: string]: any;} {
	    var json = {
		"prompt" : this.prompt,
		"id" : this.id
	    }
	    return json;
	}

	getId(){
	    return this.id;
	}

	getPrompt(){
	    return this.prompt;
	}

	toDOM(parent){
	    var rubricItem = document.createElement('div');
	    parent.appendChild(rubricItem);
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

	static clazz = "LikertElement";

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

	static fromJson(element) : LikertElement {
	    if(! ("maxLabel" in element &&
		  "minLabel" in element &&
		  "rangeSize" in element &&
		  "selected" in element &&
		  "id" in element &&
		  "prompt" in element))
		throw("Could not create FreeFormElement from" + element);
	    var el = new LikertElement(element.prompt, element.id,
				       element.minLabel, element.maxLabel,
				       element.rangeSize);
	    
	    el.selected = element.selected;
	    return el;
	}

	getClass() { return LikertElement.clazz; }

	innerJson() {
	    var inner = super.innerJson();
	    var json =
		{ "minLabel" : this.minLabel,
		  "maxLabel" : this.maxLabel,
		  "rangeSize" : this.rangeSize,
		  "selected" : this.selected
		};
	    var merged = concat(inner, json);
	    return concat(inner, json);
	}

	getOnChange(_this, input){
	    return function () {
		_this.selected = input
		_this.notify();
	    }
	}

	toDOM(parent){
	    var rubricItem = super.toDOM(parent);
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

	static clazz = "FreeFormElement";
	content : string;
	autosave;

	constructor(prompt, id){
	    super(prompt, id);
	    this.content = "";
	}

	static fromJson(element) : FreeFormElement {
	    if(! ("content" in element &&
		  "id" in element &&
		  "prompt" in element))
		throw("Could not create FreeFormElement from" + element);
	    var el = new FreeFormElement(element.prompt, element.id);
	    el.content = element.content;
	    return el;
	}

	getClass() { return FreeFormElement.clazz; }

	innerJson() {
	    var inner = super.innerJson();
	    var val = { "content" : this.content };
	    return concat(inner, val);
	}

	getOnChange(_this, input){
	    return function() {
		_this.content = input;
		if(_this.autosave == null){
		    _this.autosave = function() {
			_this.notify();
			_this.autosave = null;
			console.log("Saved.");
		    }
		    window.setTimeout(_this.autosave, 2000);
		}
	    }
	}

	toDOM(parent){
	    var rubricItem = super.toDOM(parent);
	    rubricItem.className = "rubric-item free-response question";
	    var content = document.createElement('textarea');
	    var _this = this;
	    content.onkeyup = function (_) {
		_this.getOnChange(_this, (<any>content).value)();
		content.style.height = "";
		content.style.height = Math.min(content.scrollHeight) + "px";
	    };

	    content.onchange = function () { 
		_this.content = (<any>content).value;
		content.style.height = "";
		content.style.height = Math.min(content.scrollHeight) + "px";
		_this.notify();
	    }

	    rubricItem.appendChild(content);

	    content.setAttribute('name', this.getId());

	    (<any>content).value = this.content;
	    content.onkeyup(null);
	    return rubricItem;
	}
	
    }

    elementConstructors[BasicElement.clazz] = BasicElement.fromJson;
    elementConstructors[LikertElement.clazz] = LikertElement.fromJson;
    elementConstructors[FreeFormElement.clazz] = FreeFormElement.fromJson;


    window.onload = function() {
	load(function (json) {
	    var rubric = Rubric.fromJson(json);
	    rubric.attach("rubric");
	    console.log("Loaded.");
	});
    }

}

