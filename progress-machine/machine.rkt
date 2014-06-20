#lang typed/racket

(struct: User ([uid : String]))
(struct: Course ([name : String]))
(struct: Assignment ([name : String]))
(struct: Step ([name : String]))

#|
S ::= MustSubmitNext(User, Course, Assignment, Step)
    | MustReviewNext(User, Course, Assignment, List[User])
    | GraderMustGradeNext(User, Course, Assignment, Step)
|#

(define-type State (U MustSubmitNext MustReviewNext GraderMustGradeNext))
(struct: MustSubmitNext ([student : User] [course : Course] [assignment : Assignment] [step : Step]))
(struct: MustReviewNext ([student : User] [course : Course] [assignment : Assignment] [reviewee : User]))
(struct: GraderMustGradeNext ([student : User] [course : Course] [assignment : Assignment] [step : Step]))

#|
I ::= SubmitStep(User, Course, Assignment, Files.tar.gz)
    | SubmitReview(User, Course, Assignment, ReviewedStudent, ReviewData.json)
    | GraderReview(Student, Course, Assignment, GraderName, Grades.json)

|#

(struct: Archive ([data : Bytes]))
(struct: Review ())

(define-type Input (U SubmitStep SubmitReview GraderReview))
(struct: SubmitStep ([student : User] [course : Course] [assignment : Assignment] [submission : Archive]))
(struct: SubmitReview ([student : User] [course : Course] [assignment : Assignment] [reviewee : User] [reviewdata : Review]))
(struct: GraderReview ([student : User] [course : Course] [assignment : Assignment] [grader : User] [reviewdata : Review]))

#|
O ::= AssignReview(Reviewer, Reviewee, Course, Assignment, Step, Files.tar.gz, ReviewFormat.json)
    | AvailableForReview(Student, Course, Assignment, Step, Files.tar.gz)
    | AcceptReview(Reviewer, Reviewee, Course, Assignment, Step, ReviewData.json)
|#

(define-type Output (U AssignReview AvailableForReview AcceptReview))
(struct: AssignReview ([reviewer : User] [reviewee : User] [course : Course] [assignment : Assignment] [step : Step] [reviewdata : Review]))
(struct: AvailableForReview ([student : User] [course : Course] [assignment : Assignment] [step : Step] [submission : Archive]))
(struct: AcceptReview ([reviewer : User] [reviewee : User] [course : Course] [assignment : Assignment] [step : Step] [reviewdata : Review]))


#|
f : S * I * Time * DB(Course, Assignment) -> Either[S' * List[O], ErrorMessage]

g : S_1 ... S_n * I * Time * DB(Course, Assignment) -> Either[S' * List[O], ErrorMessage]

Prototype this language without a database. Represent DB as a Racket list/set/map. Prototype a few assignments. Consider using TR.

I think: S, I, O are datatypes that are defined by the system. For each assignment, the assignment author writes the function f.

Also need to think of final states. Also, do prototype the g function, which takes f as an argument.
|#



