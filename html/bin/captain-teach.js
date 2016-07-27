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
