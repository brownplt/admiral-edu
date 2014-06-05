var CaptainTeach;
(function (CaptainTeach) {
    var CodeMirrorBuilder = (function () {
        function CodeMirrorBuilder() {
            this._value = "";
            this._mode = "markdown";
            this._readOnly = false;
        }
        CodeMirrorBuilder.prototype.append = function (value) {
            this._value += value;
            return this;
        };
        CodeMirrorBuilder.prototype.value = function (value) {
            this._value = value;
            return this;
        };
        CodeMirrorBuilder.prototype.mode = function (mode) {
            this._mode = mode;
            return this;
        };
        CodeMirrorBuilder.prototype.readOnly = function (readOnly) {
            this._readOnly = readOnly;
            return this;
        };
        CodeMirrorBuilder.prototype.build = function (attach) {
            var cm = CodeMirror(attach, {
                gutters: [
                    "Codemirror-linenumbers", 
                    "comment"
                ],
                value: this._value,
                mode: this._mode,
                readOnly: this._readOnly,
                lineNumbers: true
            });
            return cm;
        };
        return CodeMirrorBuilder;
    })();
    CaptainTeach.CodeMirrorBuilder = CodeMirrorBuilder;    
})(CaptainTeach || (CaptainTeach = {}));

var CaptainTeach;
(function (CaptainTeach) {
    var ReviewFile = (function () {
        function ReviewFile(content) {
            this.content = content;
            this.comments = {
            };
            this.editors = {
            };
            this.instance = null;
        }
        ReviewFile.prototype.setComment = function (line, comment) {
            this.comments[line] = comment;
            this.createEditor(line);
        };
        ReviewFile.prototype.removeComment = function (line) {
            delete this.comments[line];
            this.removeEditor(line);
        };
        ReviewFile.prototype.attach = function (attach, cm) {
            if(this.instance != null) {
                throw "Cannot attach multiple CodeMirrors";
            }
            cm.value(this.content);
            this.instance = cm.build(attach);
            this.instance.on("gutterClick", this.handleClick(this));
            for(var l in this.comments) {
                var line = parseInt(l);
                this.createEditor(line);
            }
        };
        ReviewFile.prototype.handleClick = function (_this) {
            return function (instance, line, gutter, clickeEvent) {
                _this.toggleEditor(line);
            }
        };
        ReviewFile.prototype.handleChange = function (line, _this) {
            return function (instance, changeObj) {
                var value = instance.getValue();
                if(value == "") {
                    delete _this.comments[line];
                    _this.instance.setGutterMarker(line, "comment", null);
                } else {
                    _this.instance.setGutterMarker(line, "comment", _this.makeMarker());
                    _this.comments[line] = value;
                }
            }
        };
        ReviewFile.prototype.toggleEditor = function (line) {
            if(line.toString() in this.editors) {
                this.removeEditor(line);
            } else {
                this.createEditor(line);
            }
        };
        ReviewFile.prototype.removeEditor = function (line) {
            if(!(line.toString() in this.editors)) {
                return;
            }
            this.editors[line].clear();
            delete this.editors[line];
        };
        ReviewFile.prototype.createEditor = function (line) {
            if(this.instance == null) {
                return;
            }
            if(line.toString() in this.editors) {
                return this.editors[line];
            }
            var wrapper = document.createElement('div');
            wrapper.className = "editor";
            var editor = document.createElement('div');
            editor.className = "editor-box";
            var editMirror = CodeMirror(editor, {
                value: line.toString() in this.comments ? this.comments[line] : "",
                mode: "markdown"
            });
            editMirror.setSize("100%", 50);
            editMirror.on("change", this.handleChange(line, this));
            this.handleChange(line, this)(editMirror, null);
            this.editors[line] = this.instance.addLineWidget(line, wrapper);
            wrapper.appendChild(editor);
            editMirror.refresh();
        };
        ReviewFile.prototype.makeMarker = function () {
            var marker = document.createElement("div");
            marker.style.color = "#822";
            marker.innerHTML = "*";
            return marker;
        };
        return ReviewFile;
    })();    
    var testSource = "import cmpsci220.testing._\n" + "import cmpsci220.support._\n" + "import scala.util.Random\n" + "// We start off defining a sealed trait called JoinList\n" + "sealed trait JoinList[E]\n\n" + "// A JoinList can be empty\n" + "case class EmptyJoinList[E]() extends JoinList[E]\n\n" + "// It can contain a single element\n" + "\case class One[E](elt : E) extends JoinList[E]";
    window.onload = function () {
        var builder = new CaptainTeach.CodeMirrorBuilder();
        builder.mode("text/x-scala").readOnly(true);
        var review = new ReviewFile(testSource);
        review.setComment(0, "Import");
        review.setComment(7, "Comment");
        var file = document.getElementById('file');
        review.attach(file, builder);
        alert("Loaded");
    };
})(CaptainTeach || (CaptainTeach = {}));

