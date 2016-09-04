//declare function CodeMirror(element, options);

declare module CodeMirror {
		function fromTextArea(element, options);
}

module CaptainTeach {

    export class CodeMirrorBuilder {
	
	_mode: string;
	_readOnly;
	cm;
	
	constructor(){
	    this._mode = "Markdown";
	    this._readOnly = false;
	    this.cm = null;
	}

	getMode(){
	    return this._mode;
	}

	getCM(){
	    return this.cm;
	}

	mode(mode : string){
	    this._mode = mode;
	    return this;
	}
	
	readOnly(readOnly){
	    this._readOnly = readOnly;
	    return this;
	}
	
	build(attach, contentStr){
	    var cm: any = CodeMirror.fromTextArea(attach, {
		lineNumbers: true,
		lineWrapping: true,
		gutters: ["comments"],
		mode: this._mode,
		readOnly: this._readOnly});
            cm.setValue(contentStr);
	    this.cm = cm;
	    return cm;
	}
	
    }
}