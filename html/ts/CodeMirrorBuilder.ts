//declare function CodeMirror(element, options);

declare module CodeMirror {
		function fromTextArea(element, options);
}

module CaptainTeach {
			 
		export	class CodeMirrorBuilder {

				_mode: string;
				_readOnly;
				
				constructor(){
						this._mode = "markdown";
						this._readOnly = false;
				}
				
				mode(mode : string){
						this._mode = mode;
						return this;
				}
				
				readOnly(readOnly){
						this._readOnly = readOnly;
						return this;
				}
				
				build(attach){
						var cm: any = CodeMirror.fromTextArea(attach, {
								lineNumbers: true,
								lineWrapping: true,
								gutters: ["comments"],
								mode: this._mode,
								readOnly: this._readOnly});
						return cm;
				}
    
		}
}