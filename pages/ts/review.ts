/// <reference path="CodeMirrorBuilder.ts" />

module CaptainTeach {

		class ReviewFile {
    
				content: string;
				comments: {[key: number]: string; };
				// Line Number -> Line Widget
				editors: {[key: number]: any;};
				instance: any; // CodeMirror Instance
    
				constructor(content: string) {
            this.content = content;
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

						cm.value(this.content);
						this.instance = cm.build(attach);
						this.instance.on("gutterClick", this.handleClick(this));
						for(var l in this.comments){
								var line : number = parseInt(l);
								this.createEditor(line);
						}
				}

				handleClick(_this){
						return function(instance, line, gutter, clickeEvent){
								_this.toggleEditor(line);
						};
				}

				handleChange(line : number, _this){
						return function(instance, changeObj){
								var value = instance.getValue();
								if(value == ""){
										delete _this.comments[line];
										_this.instance.setGutterMarker(line, "comment", null);
								}else{
										_this.instance.setGutterMarker(line, "comment", _this.makeMarker());
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
						var wrapper = document.createElement('div');
						wrapper.className = "editor";
						var editor = document.createElement('div');
						editor.className = "editor-box";

						
						var editMirror = CodeMirror(editor, {value: line.toString() in this.comments ? this.comments[line] : "",
																								 mode: "markdown"});
						editMirror.setSize("100%", 50);
						editMirror.on("change", this.handleChange(line, this));
						this.handleChange(line, this)(editMirror, null);
						this.editors[line] = this.instance.addLineWidget(line, wrapper);
						wrapper.appendChild(editor);
						editMirror.refresh();
						
				}

				makeMarker() {
						var marker = document.createElement("div");
						marker.style.color = "#822";
						marker.innerHTML = "*";
						return marker;
				}
    }

		var testSource = 
				"import cmpsci220.testing._\n" +
				"import cmpsci220.support._\n" +
				"import scala.util.Random\n" +
				"// We start off defining a sealed trait called JoinList\n" +
				"sealed trait JoinList[E]\n\n" +
				"// A JoinList can be empty\n" +
				"case class EmptyJoinList[E]() extends JoinList[E]\n\n" +
				"// It can contain a single element\n" +
				"\case class One[E](elt : E) extends JoinList[E]";

		window.onload = function() {
																								
				var builder : CodeMirrorBuilder = new CodeMirrorBuilder();
				builder.mode("text/x-scala").readOnly(true);

				var review : ReviewFile = new ReviewFile(testSource);
				review.setComment(0, "Import");
				review.setComment(7, "Comment");
				var file = document.getElementById('file');
				review.attach(file, builder);
				alert("Loaded");
		}
}