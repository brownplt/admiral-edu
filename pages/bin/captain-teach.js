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
                lineNumbers: true,
                lineWrapping: true,
                gutters: [
                    "comments"
                ],
                value: this._value,
                mode: this._mode,
                readOnly: this._readOnly
            });
            return cm;
        };
        return CodeMirrorBuilder;
    })();
    CaptainTeach.CodeMirrorBuilder = CodeMirrorBuilder;    
})(CaptainTeach || (CaptainTeach = {}));

