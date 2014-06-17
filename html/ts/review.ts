/// <reference path="CodeMirrorBuilder.ts" />
declare function save(json, callback);
declare function load(callback) : string;

module CaptainTeach {
    
    class ReviewFile {
	
	comments: {[key: number]: string; };
	// Line Number -> Line Widget
	editors: {[key: number]: any;};
	instance: any; // CodeMirror Instance
	
	constructor() {
	    this.comments = <any>{};
	    this.editors = <any>{};
	    this.instance = null;
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
	    closeA.onclick = function (e) { _this.toggleEditor(line) };
	    close.appendChild(closeA);

	    var saveit = document.createElement('div');
	    saveit.className = "comment-save-disabled";

	    var saveitA = document.createElement('a');
	    saveitA.innerHTML = "Save";
	    saveitA.setAttribute('href', "#");

	    saveit.appendChild(saveitA);

	    var editor = document.createElement('textarea');
	    editor.className = "comment-box";
	    editor.innerHTML = line.toString() in this.comments ? this.comments[line] : "";
	    editor.onkeyup = function (_) {
		saveit.className = "comment-save"
		editor.style.height = "";
		editor.style.height = Math.min(editor.scrollHeight) + 5 + "px";
	    };

	    var _this = this;

	    saveitA.onclick = function (e) { 
		_this.handleSave(line, _this, editor)(editor.innerHTML);
		var callback = function (a) {
		    saveit.className = "comment-save-disabled";
		};
		save(_this.toJSON(), callback); 
	    };

	    editorContainer.appendChild(editor);
	    editorContainer.appendChild(close);
	    editorContainer.appendChild(saveit);

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
	builder.mode("text/x-scala").readOnly(true);

	var callback = function (data) {
	    var review : ReviewFile = ReviewFile.fromJson(data);
	    var file = document.getElementById('file');
	    var cm = review.attach(file, builder);
	    cm.className += " file";
	}
	load(callback);

    }
}