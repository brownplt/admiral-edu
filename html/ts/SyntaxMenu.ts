/// <reference path="CodeMirrorBuilder.ts" />
/// <reference path="jquery.d.ts" />

module CaptainTeach {

    export class SyntaxMenu {

	builder: CodeMirrorBuilder;
	static loadedJS : { [index: string]: boolean; } = {};
	static modeToJS : { [index: string]: string; } = null;
	static modeToMime : { [index: string]: string;} = null;
	static modes =
	    [ 
		[ "APL", "apl/apl.js", "text/apl"],
		[ "Asterisk", "asterisk/asterisk.js", "text/x-asterisk"],
		[ "C", "clike/clike.js", "text/x-csrc"],
		[ "C++", "clike/clike.js", "text/x-c++src"],
		[ "C#", "clike/clike.js", "text/x-csharp"],
		[ "Java", "clike/clike.js", "text/x-java"],
		[ "javascript", "javascript/javascript.js", "text/x-javascript" ],
		[ "json", "javascript/javascript.js", "text/x-json" ],
		[ "Markdown", "markdown/markdown.js", "text/x-markdown"],
		[ "Pyret", "pyret/pyret.js", "text/x-pyret"],
		[ "Scala", "clike/clike.js", "text/x-scala"],
		[ "Scheme", "scheme/scheme.js", "text/x-scheme"],
		[ "YAML", "yaml/yaml.js", "text/x-yaml"]
	    ];
	
	static prefix = "https://www.captain-teach.org/mode/";

	constructor(builder : CodeMirrorBuilder){
	    this.builder = builder;
	    SyntaxMenu.loadMode(builder.getMode(), builder.getCM());
	}

	static initModeMaps() {
	    if(SyntaxMenu.modeToJS == null){
		SyntaxMenu.modeToJS = {};
		SyntaxMenu.modeToMime = {};
		for(var i in SyntaxMenu.modes){
		    var mode = SyntaxMenu.modes[i];
		    SyntaxMenu.modeToJS[mode[0]] = SyntaxMenu.prefix + mode[1];
		    SyntaxMenu.modeToMime[mode[0]] = mode[2];
		}

	    }
	}

	static buildMenu(select, selected) {
	    for(var i in SyntaxMenu.modes){
		var mode = SyntaxMenu.modes[i][0];
		var option : any = document.createElement('option');
		if(mode === selected)
		    option.selected=true;
		option.innerHTML = mode;
		select.appendChild(option);
	    }
	}
	
	static loadMode(mode : string, cm : any){
	    SyntaxMenu.initModeMaps();
	    var js = SyntaxMenu.modeToJS[mode];
	    if(!(js in SyntaxMenu.loadedJS)){
		SyntaxMenu.loadedJS[js] = true;
		$.getScript(js, function (script, status, xhr){
		    SyntaxMenu.setMode(mode, cm);
		});
	    }else{
		SyntaxMenu.setMode(mode, cm);
	    }
	}

	static setMode(mode : string, cm : any){
		var mime = SyntaxMenu.modeToMime[mode];
		cm.setOption("mode", mime);
	}

	attach(divElement){
	    var label = document.createElement('p');
	    label.innerHTML = "Syntax Mode: ";
	    
	    var menu : any = document.createElement('select');
	    var selected = this.builder.getMode();
	   
	    SyntaxMenu.buildMenu(menu, selected);

	    var _this = this;
	    menu.onchange = function (e) { 
		var mode = menu.value;
		SyntaxMenu.loadMode(mode, _this.builder.getCM());
	    };

	    label.appendChild(menu);
	    divElement.appendChild(label);
	}
	
    }
}