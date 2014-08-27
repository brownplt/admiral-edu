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

        CodeMirrorBuilder.prototype.build = function (attach) {
            var cm = CodeMirror.fromTextArea(attach, {
                lineNumbers: true,
                lineWrapping: true,
                gutters: ["comments"],
                mode: this._mode,
                readOnly: this._readOnly });
            this.cm = cm;
            return cm;
        };
        return CodeMirrorBuilder;
    })();
    CaptainTeach.CodeMirrorBuilder = CodeMirrorBuilder;
})(CaptainTeach || (CaptainTeach = {}));
var Authoring;
(function (Authoring) {
    var Field = (function () {
        function Field(label, value) {
            this.label = label;
            this.value = value;
        }
        Field.prototype.toDOM = function () {
            var dom = document.createElement('div');
            dom.innerHTML = "<h2>" + this.label + ":</h2>" + "<p>" + this.value + "</p>";

            return dom;
        };
        return Field;
    })();
    Authoring.Field = Field;

    var Example = (function () {
        function Example(name) {
            this.name = name;
            this.fields = new Array();
            this.usages = new Array();
            this.desc = new Array();
            this.hidden = true;
        }
        Example.prototype.description = function (desc) {
            this.desc.push(desc);
            return this;
        };

        Example.prototype.usage = function (line) {
            this.usages.push(line);
            return this;
        };

        Example.prototype.field = function (name, value) {
            this.fields.push(new Field(name, value));
            return this;
        };

        Example.prototype.toDOM = function () {
            var _this = this;
            var dom = document.createElement('div');
            dom.className = "example";

            var description = document.createElement('div');

            description.innerHTML = '<div class="description"><p>' + _this.desc.join(" ") + '</p></div>';

            var open = document.createElement('a');
            open.innerHTML = this.name;
            open.setAttribute('href', 'javascript:void(0)');

            open.onclick = function (_) {
                description.className = _this.hidden ? "hidden" : "visible";
                _this.hidden = !_this.hidden;
            };

            open.onclick(null);

            dom.appendChild(open);

            var fields = document.createElement('h1');
            fields.innerHTML = "Fields";
            description.appendChild(fields);

            for (var i = 0; i < this.fields.length; i++) {
                var field = this.fields[i];
                description.appendChild(field.toDOM());
            }

            var textarea = document.createElement('textarea');
            textarea.value = this.usages.join("\n");
            textarea.readOnly = true;
            description.appendChild(textarea);

            dom.appendChild(description);

            return dom;
        };
        return Example;
    })();
    Authoring.Example = Example;
})(Authoring || (Authoring = {}));
/// <reference path="CodeMirrorBuilder.ts" />
/// <reference path="Examples.ts" />

var Authoring;
(function (Authoring) {
    var assignment = new Authoring.Example("Assignment Description").description("After creation, the assignment will appear on the instructor dashboard where it can be manually opened and closed for students to access.").field("name", "string").field("id", "string").field("description", "string").field("steps", "list of Step").usage('name: Clocks').usage('id: clocks').usage('description: Students develop functions representing an alarm clock.').usage('steps: ').usage('  - One or').usage('  - more Steps');

    var step = new Authoring.Example("Step");
    step.description("Students must complete each step before proceeding to the next. ").description("Optionally, each step may have any number of reviews. All of the reviews will be assigned to a student once they have submitted their solution to the step.").description("A student may not proceed to the next step until all of their assigned reviews have been completed.").field("id", "string").field("instructions", "string").field("reviews (optional)", "list of Review").usage('  - id: tests').usage('    instructions: Submit your test cases. Do not submit any implementation details.').usage('#   reviews:').usage('#     - One or').usage('#     - more Reviews');

    var instructorSolution = new Authoring.Example("Review - Instructor Provided Solution");
    instructorSolution.description("An instructor provided solution is a review where you provide the material that the students will see. After submitting an Assignment Description, you will be asked to upload an archive file for each instructor-solution in your description.").field("id", "string").field("rubric", "list of RubricElement").usage('      - instructor-solution:').usage('          id: poor-implementation').usage('          rubric:').usage('            - One or').usage('            - more RubricElements');

    var studentSubmission = new Authoring.Example("Review - Student Submission");
    studentSubmission.description("Students will be assigned the specified number of other student submissions.").field("amount", "integer").field("rubric", "list of RubricElement").usage('      - student-submission:').usage('          amount: 2').usage('          rubric:').usage('            - One or').usage('            - more RubricElements');

    var instruction = new Authoring.Example("Rubric Element - Instruction");
    instruction.description("Students will see a text box with the specified instructions.").usage('            - instruction: Add inline comments where tests are incomplete.');

    var likert = new Authoring.Example("Rubric Element - Likert");
    likert.description("Students will see a likert scale with the specified number of options between the specified min and max labels.").field("id", "string").field("text", "string").field("min-label", "string").field("max-label", "string").field("granularity", "integer").usage('            - likert:').usage('                id: correctness').usage('                text: These tests are correct.').usage('                min-label: Disagree').usage('                max-label: Agree').usage('                granularity: 9');

    var freeform = new Authoring.Example("Rubric Element - Free Form");
    freeform.description("Students will see an empty text box where they can enter a response.").field("id", "string").field("text", "string").usage('            - free-form:').usage('                id: not-covered').usage('                text: If applicable, provide inputs that are not covered by the tests. ');

    var examples = [assignment, step, instructorSolution, studentSubmission, instruction, likert, freeform];

    window.onload = function () {
        var types = document.getElementById('examples');
        for (var i = 0; i < examples.length; i++) {
            types.appendChild(examples[i].toDOM());
        }

        var textarea = document.getElementById('assignment');
        var builder = new CaptainTeach.CodeMirrorBuilder();
        builder.mode("text/x-yaml");
        instance = builder.build(textarea);
        instance.setSize("100%", "100%");
        console.log("loaded.");
    };
})(Authoring || (Authoring = {}));

var instance;

function handleResponse(response) {
    var response = response.currentTarget.response;
    if (response === "Success") {
        window.location.href = "/" + getClassName() + "/assignments/";
    } else {
        alert(response);
    }
}

function validate() {
    console.log("validating");
    var content = instance.getValue();
    save(content, handleResponse);
}
