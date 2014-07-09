
var CaptainTeach;
(function (CaptainTeach) {
    var CodeMirrorBuilder = (function () {
        function CodeMirrorBuilder() {
            this._mode = "markdown";
            this._readOnly = false;
        }
        CodeMirrorBuilder.prototype.mode = function (mode) {
            this._mode = mode;
            return this;
        };
        CodeMirrorBuilder.prototype.readOnly = function (readOnly) {
            this._readOnly = readOnly;
            return this;
        };
        CodeMirrorBuilder.prototype.build = function (attach) {
            var cm = CodeMirror.fromTextArea(attach, {
                lineNumbers: true,
                lineWrapping: true,
                gutters: [
                    "comments"
                ],
                mode: this._mode,
                readOnly: this._readOnly
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
        function ReviewFile() {
            this.comments = {
            };
            this.editors = {
            };
            this.instance = null;
            this.autosave = null;
        }
        ReviewFile.fromJson = function fromJson(json) {
            var comments = JSON.parse(json).comments;
            var rf = new ReviewFile();
            rf.comments = comments;
            return rf;
        }
        ReviewFile.prototype.escape = function (input) {
            var output = "";
            for(var i = 0; i < input.length; i++) {
                var c = input.charAt(i);
                if(c == '"') {
                    output += "\\\"";
                } else {
                    output += c;
                }
            }
            return output;
        };
        ReviewFile.prototype.toJSON = function () {
            var json = "{\n\t\"comments\" :\n\t{\n";
            var size = Object.keys(this.comments).length;
            var i = 0;
            for(var l in this.comments) {
                var line = parseInt(l);
                var maybeComma = ++i < size ? ",\n" : "\n";
                json += "\t\t\"" + line + "\" : \"" + this.escape(this.comments[line]) + "\"" + maybeComma;
            }
            json += "\t}\n}";
            return json;
        };
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
            this.instance = cm.build(attach);
            this.instance.on("gutterClick", this.handleClick(this));
            for(var l in this.comments) {
                var line = parseInt(l);
                this.createEditor(line);
            }
            this.instance.setSize("100%", "calc(100% - 31px)");
            return this.instance;
        };
        ReviewFile.prototype.handleClick = function (_this) {
            return function (instance, line, gutter, clickeEvent) {
                _this.toggleEditor(line);
            }
        };
        ReviewFile.prototype.handleSave = function (line, _this, comment) {
            return function (e) {
                var value = comment.value;
                if(value == "") {
                    delete _this.comments[line];
                    _this.instance.setGutterMarker(line, "comments", null);
                } else {
                    _this.instance.setGutterMarker(line, "comments", _this.makeMarker());
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
        ReviewFile.prototype.getOnChange = function (_this, editor, line) {
            return function (_) {
                console.log("keyup");
                editor.style.height = "";
                editor.style.height = Math.min(editor.scrollHeight) + 5 + "px";
                if(_this.autosave == null) {
                    _this.autosave = function () {
                        _this.handleSave(line, _this, editor)(editor.innerHTML);
                        var callback = function (a) {
                        };
                        save(_this.toJSON(), callback);
                        _this.autosave = null;
                        console.log("Saved.");
                    };
                    window.setTimeout(_this.autosave, 2000);
                }
            }
        };
        ReviewFile.prototype.createEditor = function (line) {
            if(this.instance == null) {
                return;
            }
            if(line.toString() in this.editors) {
                return this.editors[line];
            }
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
                var callback = function (a) {
                };
                save(_this.toJSON(), callback);
                _this.toggleEditor(line);
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
        };
        ReviewFile.prototype.makeMarker = function () {
            var marker = document.createElement("div");
            marker.className = "comment-marker";
            return marker;
        };
        return ReviewFile;
    })();    
    window.onload = function () {
        var builder = new CaptainTeach.CodeMirrorBuilder();
        builder.mode("text/x-scala").readOnly(true);
        var callback = function (data) {
            var review = ReviewFile.fromJson(data);
            var file = document.getElementById('file');
            var cm = review.attach(file, builder);
            cm.className += " file";
        };
        load(callback);
    };
})(CaptainTeach || (CaptainTeach = {}));

