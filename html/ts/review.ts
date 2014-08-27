/// <reference path="CodeMirrorBuilder.ts" />
/// <reference path="SyntaxMenu.ts" />
declare function save(json, callback);
declare function load(callback) : string;
declare var defaultMode : string;

module CaptainTeach {
    
    class ReviewFile {
	
	comments: {[key: number]: string; };
	// Line Number -> Line Widget
	editors: {[key: number]: any;};
	instance: any; // CodeMirror Instance
	autosave;
	
	constructor() {
	    this.comments = <any>{};
	    this.editors = <any>{};
	    this.instance = null;
	    this.autosave = null;
	}

	static fromJson(json : string){
	    var comments = JSON.parse(json).comments;
	    var rf = new ReviewFile();
	    rf.comments = comments;
	    return rf;
	}

	escape(input : string){
	    var output = "";
	    for(var i = 0; i < input.length; i++){
		var c = input.charAt(i);
		if(c == '"') output += "\\\"";
		else output += c;
	    }
	    return output;
	}

	toJSON(){
	    var json = "{\n\t\"comments\" :\n\t{\n"
	    var size = Object.keys(this.comments).length;
	    var i = 0;
	    for(var l in this.comments){
		var line : number = parseInt(l);
		var maybeComma = ++i < size ? ",\n" : "\n";
		json += "\t\t\"" + line + "\" : \"" + this.escape(this.comments[line]) + "\"" + maybeComma;
	    }

	    json += "\t}\n}";
	    return json;
	}
	
	setComment(line : number, comment : string){
	    this.comments[line] = comment;
	    this.createEditor(line);
	}				
	
	removeComment(line : number){
	    delete this.comments[line];
	    this.removeEditor(line)
	}
	
	attach(attach, cm : CodeMirrorBuilder){
	    
	    if(this.instance != null){
		throw "Cannot attach multiple CodeMirrors";
	    }
	    
	    this.instance = cm.build(attach);
	    this.instance.on("gutterClick", this.handleClick(this));
	    
	    
	    for(var l in this.comments){
		var line : number = parseInt(l);
		this.createEditor(line);
	    }
	    
	    this.instance.setSize("100%", "calc(100% - 31px)");
	    return this.instance;
	}
	
	handleClick(_this){
	    return function(instance, line, gutter, clickeEvent){
		_this.toggleEditor(line);
	    };
	}
	
	handleSave(line : number, _this, comment){
	    return function(e){
		var value = comment.value
		if(value == ""){
		    delete _this.comments[line];
		    _this.instance.setGutterMarker(line, "comments", null);
		}else{
		    _this.instance.setGutterMarker(line, "comments", _this.makeMarker());
		    _this.comments[line] = value;
		}
	    }
	}
	
	toggleEditor(line : number){
	    if(line.toString() in this.editors){
		this.removeEditor(line);
	    }else{
		this.createEditor(line);
	    }
	}
	
	removeEditor(line : number){
	    if(!(line.toString() in this.editors)) return;
	    this.editors[line].clear();
	    delete this.editors[line];
	}

	getOnChange(_this, editor, line) {
	    return function (_) {
		console.log("keyup");
		editor.style.height = "";
		editor.style.height = Math.min(editor.scrollHeight) + 5 + "px";
		if(_this.autosave == null){
		    _this.autosave = function () {
			_this.handleSave(line, _this, editor)(editor.innerHTML);
			var callback = function (a) {};
			save(_this.toJSON(), callback); 
			_this.autosave = null;
			console.log("Saved.");
		    };
		    window.setTimeout(_this.autosave, 2000);	
		}
	    };
	}
	
	createEditor(line : number){
	    if(this.instance == null) return;
	    if(line.toString() in this.editors) return this.editors[line];

	    var editorContainer = document.createElement('div');
	    editorContainer.className = "comment-container";

	    var close = document.createElement('div');
	    close.className = "comment-close";

	    var closeA = document.createElement('a');
	    closeA.setAttribute('href', "#");
	    var _this = this;
	    closeA.innerHTML = "Close Form";
	    closeA.onclick = function (e) { 
		_this.handleSave(line, _this, editor)(editor.innerHTML);
		var callback = function (a) {};
		save(_this.toJSON(), callback); 		
		_this.toggleEditor(line) 
	    };
	    close.appendChild(closeA);

	    var editor = document.createElement('textarea');
	    editor.className = "comment-box";
	    editor.innerHTML = line.toString() in this.comments ? this.comments[line] : "";
	    editor.onkeyup = this.getOnChange(this, editor, line);

	    var _this = this;

	    editorContainer.appendChild(editor);
	    editorContainer.appendChild(close);

	    this.handleSave(line, this, editor)(null);
	    this.editors[line] = this.instance.addLineWidget(line, editorContainer);
	    editor.style.height = "";
	    editor.style.height = Math.min(editor.scrollHeight) + 5 + "px";	    
	}
	
	makeMarker() {
	    var marker = document.createElement("div");
	    marker.className = "comment-marker";
	    return marker;
	}
    }

    window.onload = function() {
	
	var builder : CodeMirrorBuilder = new CodeMirrorBuilder();
	builder.mode(defaultMode).readOnly(true);

		

	var callback = function (data) {
	    var review : ReviewFile = ReviewFile.fromJson(data);
	    var file = document.getElementById('file');
	    var cm = review.attach(file, builder);
	    cm.className += " file";
	    var menu : SyntaxMenu = new SyntaxMenu(builder);
	    menu.attach(document.getElementById('syntax-menu'));
	}
	load(callback);

    }
}