module Authoring {

    export class Field {
	label : string;
	value : string;

	constructor(label : string, value : string){
	    this.label = label;
	    this.value = value;
	}

	toDOM(){
	    var dom = document.createElement('div');
	    dom.innerHTML = 
		"<h2>" + this.label + ":</h2>" +
		"<p>" + this.value + "</p>";  

	    return dom;
	}
    }

    export class Example {
	
	name : string;
	fields : Field[];
	desc: string[];
	usages : string[];
	hidden : boolean;
	
	constructor(name : string){
	    this.name = name;
	    this.fields = new Array();
	    this.usages = new Array();
	    this.desc = new Array();
	    this.hidden = true;
	}

	description(desc : string){
	    this.desc.push(desc);
	    return this;
	}

	usage(line : string){
	    this.usages.push(line);
	    return this;
	}

	field(name : string, value : string){
	    this.fields.push(new Field(name, value));
	    return this;
	}

	toDOM(){
	    var _this = this;
	    var dom = document.createElement('div');
	    dom.className = "example";

	    var description = document.createElement('div');
	    
	    description.innerHTML = '<div class="description"><p>' + _this.desc.join(" ") + '</p></div>';

	    var open = document.createElement('a');
	    open.innerHTML = this.name;
	    open.setAttribute('href', 'javascript:void(0)');
	    
	    open.onclick = function (_) { 
		description.className = _this.hidden ? "hidden" : "visible";
		_this.hidden = !_this.hidden;
	    }

	    open.onclick(null);

	    dom.appendChild(open);


	    var fields = document.createElement('h1');
	    fields.innerHTML = "Fields";
	    description.appendChild(fields);

	    for(var i = 0; i < this.fields.length; i++){
		var field = this.fields[i];
		description.appendChild(field.toDOM());
	    }

	    var textarea = <any>document.createElement('textarea');
	    textarea.value = this.usages.join("\n");
	    textarea.readOnly = true;
	    description.appendChild(textarea);

	    dom.appendChild(description);

	    return dom;
	}

    }

}
