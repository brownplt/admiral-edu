declare function CodeMirror(element, options);

module CaptainTeach {
			 
		export	class CodeMirrorBuilder {

				_value: string;
				_mode: string;
				_readOnly: bool;
				
				constructor(){
						this._value = "";
						this._mode = "markdown";
						this._readOnly = false;
				}
				
				append(value : string){
						this._value += value;
						return this;
				}

				value(value : string){
						this._value = value;
						return this;
				}
				
				mode(mode : string){
						this._mode = mode;
						return this;
				}
				
				readOnly(readOnly : bool){
						this._readOnly = readOnly;
						return this;
				}
				
				build(attach){
						var cm: any = CodeMirror(attach, {
								gutters: ["Codemirror-linenumbers", "comment"],
								value: this._value,
								mode: this._mode,
								readOnly: this._readOnly,
								lineNumbers: true });
						return cm;
				}
    
		}
}