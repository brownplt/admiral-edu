/// <reference path="CodeMirrorBuilder.ts" />
/// <reference path="Examples.ts" />

declare function save(content, callback);
declare function load(callback) : string;
declare function getClassName() : string;

module Authoring {

    var assignment = new Example("Assignment Description")
	.description("After creation, the assignment will appear on the instructor dashboard where it can be manually opened and closed for students to access.")
	.field("name", "string")
	.field("id", "string")
	.field("description", "string")
	.field("steps", "list of Step")
	.field("assignment-handler (optional)", "assignment-handler-id")
	.usage('name: Clocks')
	.usage('id: clocks')
	.usage('description: Students develop functions representing an alarm clock.')
	.usage('steps: ')
	.usage('  - One or')
	.usage('  - more Steps');
    

    var step = new Example("Step");
    step
	.description("Students must complete each step before proceeding to the next. ")
	.description("Optionally, each step may have any number of reviews. All of the reviews will be assigned to a student once they have submitted their solution to the step.")
	.description("A student may not proceed to the next step until all of their assigned reviews have been completed.")
	.field("id", "string")
	.field("instructions", "string")
	.field("reviews (optional)", "list of Review")
	.usage('  - id: tests')
	.usage('    instructions: Submit your test cases. Do not submit any implementation details.')
	.usage('#   reviews:')
	.usage('#     - One or')
	.usage('#     - more Reviews'); 

    var instructorSolution = new Example("Review - Instructor Provided Solution");
    instructorSolution
	.description("An instructor provided solution is a review where you provide the material that the students will see. After submitting an Assignment Description, you will be asked to upload an archive file for each instructor-solution in your description.")
	.field("id", "string")
	.field("rubric", "list of RubricElement")
	.usage('      - instructor-solution:')
	.usage('          id: poor-implementation')
	.usage('          rubric:')
	.usage('            - One or')
	.usage('            - more RubricElements');
    
    var studentSubmission = new Example("Review - Student Submission");
    studentSubmission
	.description("Students will be assigned the specified number of other student submissions.")
	.field("amount", "integer")
	.field("id", "string")
	.field("rubric", "list of RubricElement")
	.usage('      - student-submission:')
	.usage('          id: student-reviews')
	.usage('          amount: 2')
	.usage('          rubric:')
	.usage('            - One or')
	.usage('            - more RubricElements');

    var instruction = new Example("Rubric Element - Instruction");
    instruction
	.description("Students will see a text box with the specified instructions.")
	.usage('            - instruction: Add inline comments where tests are incomplete.');

    var likert = new Example("Rubric Element - Likert");
    likert
	.description("Students will see a likert scale with the specified number of options between the specified min and max labels.")
	.field("id", "string")
	.field("text", "string")
	.field("min-label", "string")
	.field("max-label", "string")
	.field("granularity", "integer")
	.usage('            - likert:')
	.usage('                id: correctness')
	.usage('                text: These tests are correct.')
	.usage('                min-label: Disagree')
	.usage('                max-label: Agree')
	.usage('                granularity: 9');

    var freeform = new Example("Rubric Element - Free Form");
    freeform
	.description("Students will see an empty text box where they can enter a response.")
	.field("id", "string")
	.field("text", "string")
	.usage('            - free-form:')
	.usage('                id: not-covered')
	.usage('                text: If applicable, provide inputs that are not covered by the tests. ');	

    var examples = [assignment, step, instructorSolution, studentSubmission, instruction, likert, freeform];

    window.onload = function() {
    
	var types = document.getElementById('examples');
	for(var i = 0; i < examples.length; i++){
	    types.appendChild(examples[i].toDOM());
	}
	
    
	var textarea = document.getElementById('assignment');
	var builder = new CaptainTeach.CodeMirrorBuilder();
	builder.mode("text/x-yaml");
	instance = builder.build(textarea);
	instance.setSize("100%", "100%");
	console.log("loaded.");

    }

}

var instance;

function handleResponse(response){
    var response = response.currentTarget.response;
    if(response === "Success"){
	window.location.href = "/" + getClassName() + "/assignments/";
    }else{
	alert(response);
    }
}

function validate(){
    console.log("validating");
    var content = instance.getValue();
    save(content, handleResponse)
}
