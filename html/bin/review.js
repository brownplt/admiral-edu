//declare function CodeMirror(element, options);
var CaptainTeach;
(function (CaptainTeach) {
    var CodeMirrorBuilder = (function () {
        function CodeMirrorBuilder() {
            this._mode = "Markdown";
            this._readOnly = false;
            this.cm = null;
        }
        CodeMirrorBuilder.prototype.getMode = function () {
            return this._mode;
        };
        CodeMirrorBuilder.prototype.getCM = function () {
            return this.cm;
        };
        CodeMirrorBuilder.prototype.mode = function (mode) {
            this._mode = mode;
            return this;
        };
        CodeMirrorBuilder.prototype.readOnly = function (readOnly) {
            this._readOnly = readOnly;
            return this;
        };
        CodeMirrorBuilder.prototype.build = function (attach, contentStr) {
            var cm = CodeMirror.fromTextArea(attach, {
                lineNumbers: true,
                lineWrapping: true,
                gutters: ["comments"],
                mode: this._mode,
                readOnly: this._readOnly });
            cm.setValue(contentStr);
            this.cm = cm;
            return cm;
        };
        return CodeMirrorBuilder;
    }());
    CaptainTeach.CodeMirrorBuilder = CodeMirrorBuilder;
})(CaptainTeach || (CaptainTeach = {}));
/// <reference path="CodeMirrorBuilder.ts" />
/// <reference path="jquery.d.ts" />
var CaptainTeach;
(function (CaptainTeach) {
    var SyntaxMenu = (function () {
        function SyntaxMenu(builder) {
            this.builder = builder;
            SyntaxMenu.loadMode(builder.getMode(), builder.getCM());
        }
        SyntaxMenu.initModeMaps = function () {
            if (SyntaxMenu.modeToJS == null) {
                SyntaxMenu.modeToJS = {};
                SyntaxMenu.modeToMime = {};
                for (var i in SyntaxMenu.modes) {
                    var mode = SyntaxMenu.modes[i];
                    SyntaxMenu.modeToJS[mode[0]] = SyntaxMenu.prefix + mode[1];
                    SyntaxMenu.modeToMime[mode[0]] = mode[2];
                }
            }
        };
        SyntaxMenu.buildMenu = function (select, selected) {
            for (var i in SyntaxMenu.modes) {
                var mode = SyntaxMenu.modes[i][0];
                var option = document.createElement('option');
                if (mode === selected)
                    option.selected = true;
                option.innerHTML = mode;
                select.appendChild(option);
            }
        };
        SyntaxMenu.loadMode = function (mode, cm) {
            SyntaxMenu.initModeMaps();
            var js = SyntaxMenu.modeToJS[mode];
            if (!(js in SyntaxMenu.loadedJS)) {
                SyntaxMenu.loadedJS[js] = true;
                $.getScript(js, function (script, status, xhr) {
                    SyntaxMenu.setMode(mode, cm);
                });
            }
            else {
                SyntaxMenu.setMode(mode, cm);
            }
        };
        SyntaxMenu.setMode = function (mode, cm) {
            var mime = SyntaxMenu.modeToMime[mode];
            cm.setOption("mode", mime);
        };
        SyntaxMenu.prototype.attach = function (divElement) {
            var label = document.createElement('p');
            label.innerHTML = "Syntax Mode: ";
            var menu = document.createElement('select');
            var selected = this.builder.getMode();
            SyntaxMenu.buildMenu(menu, selected);
            var _this = this;
            menu.onchange = function (e) {
                var mode = menu.value;
                SyntaxMenu.loadMode(mode, _this.builder.getCM());
            };
            label.appendChild(menu);
            divElement.appendChild(label);
        };
        SyntaxMenu.loadedJS = {};
        SyntaxMenu.modeToJS = null;
        SyntaxMenu.modeToMime = null;
        SyntaxMenu.modes = [
            ["APL", "apl/apl.js", "text/apl"],
            ["Asterisk", "asterisk/asterisk.js", "text/x-asterisk"],
            ["C", "clike/clike.js", "text/x-csrc"],
            ["C++", "clike/clike.js", "text/x-c++src"],
            ["C#", "clike/clike.js", "text/x-csharp"],
            ["Java", "clike/clike.js", "text/x-java"],
            ["javascript", "javascript/javascript.js", "text/x-javascript"],
            ["json", "javascript/javascript.js", "text/x-json"],
            ["Markdown", "markdown/markdown.js", "text/x-markdown"],
            ["Pyret", "pyret/pyret.js", "text/x-pyret"],
            ["Scala", "clike/clike.js", "text/x-scala"],
            ["Scheme", "scheme/scheme.js", "text/x-scheme"],
            ["YAML", "yaml/yaml.js", "text/x-yaml"]
        ];
        SyntaxMenu.prefix = "https://www.captain-teach.org/mode/";
        return SyntaxMenu;
    }());
    CaptainTeach.SyntaxMenu = SyntaxMenu;
})(CaptainTeach || (CaptainTeach = {}));
/// <reference path="CodeMirrorBuilder.ts" />
/// <reference path="SyntaxMenu.ts" />
var CaptainTeach;
(function (CaptainTeach) {
    var ReviewFile = (function () {
        function ReviewFile() {
            this.comments = {};
            this.editors = {};
            this.instance = null;
            this.autosave = null;
        }
        ReviewFile.fromJson = function (json) {
            var comments = JSON.parse(json).comments;
            var rf = new ReviewFile();
            rf.comments = comments;
            return rf;
        };
        ReviewFile.prototype.escape = function (input) {
            var output = "";
            for (var i = 0; i < input.length; i++) {
                var c = input.charAt(i);
                if (c == '"')
                    output += "\\\"";
                else
                    output += c;
            }
            return output;
        };
        ReviewFile.prototype.toJSON = function () {
            var json = "{\n\t\"comments\" :\n\t{\n";
            var size = Object.keys(this.comments).length;
            var i = 0;
            for (var l in this.comments) {
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
        // create a CodeMirror instance, replacing the textarea,
        // using the given content string. Attach comments to
        // corresponding lines.
        ReviewFile.prototype.attach = function (textarea, contentStr, cm) {
            if (this.instance != null) {
                throw "Cannot attach multiple CodeMirrors";
            }
            this.instance = cm.build(textarea, contentStr);
            this.instance.on("gutterClick", this.handleClick(this));
            for (var l in this.comments) {
                var line = parseInt(l);
                this.createEditor(line);
            }
            this.instance.setSize("100%", "calc(100% - 31px)");
            return this.instance;
        };
        ReviewFile.prototype.handleClick = function (_this) {
            return function (instance, line, gutter, clickeEvent) {
                _this.toggleEditor(line);
            };
        };
        ReviewFile.prototype.handleSave = function (line, _this, comment) {
            return function (e) {
                var value = comment.value;
                if (value == "") {
                    delete _this.comments[line];
                    _this.instance.setGutterMarker(line, "comments", null);
                }
                else {
                    _this.instance.setGutterMarker(line, "comments", _this.makeMarker());
                    _this.comments[line] = value;
                }
            };
        };
        ReviewFile.prototype.toggleEditor = function (line) {
            if (line.toString() in this.editors) {
                this.removeEditor(line);
            }
            else {
                this.createEditor(line);
            }
        };
        ReviewFile.prototype.removeEditor = function (line) {
            if (!(line.toString() in this.editors))
                return;
            this.editors[line].clear();
            delete this.editors[line];
        };
        ReviewFile.prototype.getOnChange = function (_this, editor, line) {
            return function (_) {
                console.log("keyup");
                editor.style.height = "";
                editor.style.height = Math.min(editor.scrollHeight) + 5 + "px";
                if (_this.autosave == null) {
                    _this.autosave = function () {
                        _this.handleSave(line, _this, editor)(editor.innerHTML);
                        var callback = function (a) { };
                        save(_this.toJSON(), callback);
                        _this.autosave = null;
                        console.log("Saved.");
                    };
                    window.setTimeout(_this.autosave, 2000);
                }
            };
        };
        // FIXME: badly structured code; knowledge about
        // line numbers lives in multiple places. Just have
        // this method create its own editors based on the comments
        // field, or accept the comments field as an argument
        // if you want to be functional.
        // given a line number, find or construct the editor for
        // editing the comment associated with that line.
        ReviewFile.prototype.createEditor = function (line) {
            if (this.instance == null)
                return;
            if (line.toString() in this.editors)
                return this.editors[line];
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
                var callback = function (a) { };
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
    }());
    window.onload = function () {
        var builder = new CaptainTeach.CodeMirrorBuilder();
        builder.mode(defaultMode).readOnly(true);
        var callback = function (commentData, fileStr) {
            var review = ReviewFile.fromJson(commentData);
            var textarea = document.getElementById('file');
            var cm = review.attach(textarea, fileStr, builder);
            cm.className += " file";
            var menu = new CaptainTeach.SyntaxMenu(builder);
            menu.attach(document.getElementById('syntax-menu'));
        };
        load(callback);
    };
})(CaptainTeach || (CaptainTeach = {}));
