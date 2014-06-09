/// <reference path="CodeMirrorBuilder.ts" />

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

				handleChange(line : number, _this, comment){
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
				    var editor = document.createElement('textarea');
				    editor.className = "comment-box";
				    editor.innerHTML = line.toString() in this.comments ? this.comments[line] : "";
				    editor.onkeyup = this.handleChange(line, this, editor);
				    this.handleChange(line, this, editor)(null);
				    this.editors[line] = this.instance.addLineWidget(line, editor);
						
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

				var review : ReviewFile = new ReviewFile();
				var file = document.getElementById('file');
				var cm = review.attach(file, builder);
				cm.className += " file";
		}
}