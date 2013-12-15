(load "../../vendor/newlisp-unittest/nl-unittest.lsp")
(load "nlpeg.lsp")
(context MAIN)

(setf x-options '(("start-rule" "digits")))
(setf x-parser (NLPEG-Parser x-options))
(:add-rule x-parser (list "digit"       (NLPEG-Expression {\d})))
(:add-rule x-parser (list "digits"      (NLPEG-Expression {\d+})))
(:add-rule x-parser (list "char"        (NLPEG-Expression {[a-z]})))
(:add-rule x-parser (list "chars"       (NLPEG-Expression {[a-z]+})))
(:add-rule x-parser (list "3num3chars"  (NLPEG-Sequence (list "digits" "chars"))))
(:add-rule x-parser (list "numsORchars" (NLPEG-Choice (list "digits" "chars"))))
(:add-rule x-parser (list "maybeMany"   (NLPEG-Many "digit" 0 0)))
(:add-rule x-parser (list "many"        (NLPEG-Many "digit" 1 0)))
(:add-rule x-parser (list "maybeOne"    (NLPEG-Many "digit" 0 1)))
(:add-rule x-parser (list "between3to5" (NLPEG-Many "digit" 3 5)))
(:add-rule x-parser (list "haschar"     (NLPEG-Predicate "char" "has")))
(:add-rule x-parser (list "nothaschar"  (NLPEG-Predicate "char" "not-has")))

(define-test (test_default_start_rule)
             (assert= '("123") (:value (:parse x-parser "123abc"))))

(define-test (test_given_start_rule)
             (assert= '("abc") (:value (:parse-rule x-parser "chars" "abc123"))))

(define-test (test_sequence_of_expressions)
             (assert= '(("123")("abc")) (:value (:parse-rule x-parser "3num3chars" "123abc"))))

(define-test (test_failed_sequence_of_expressions_0)
             (assert= nil (:is-matched? (:parse-rule x-parser "3num3chars" "abc123"))))

(define-test (test_failed_sequence_of_expressions_3)
             (assert= nil (:is-matched? (:parse-rule x-parser "3num3chars" "123..."))))

(define-test (test_ordered_choice_1)
             (assert= '("123") (:value (:parse-rule x-parser "numsORchars" "123abc"))))

(define-test (test_ordered_choice_2)
             (assert= '("abc") (:value (:parse-rule x-parser "numsORchars" "abc123"))))

(define-test (test_failed_ordered_choice)
             (assert= nil (:is-matched? (:parse-rule x-parser "numsORchars" "......"))))

(define-test (test_maybe_many)
             (assert= '(("1")("2")("3")) (:value (:parse-rule x-parser "maybeMany" "123abc"))))

(define-test (test_failed_maybe_many)
             (assert= '() (:value (:parse-rule x-parser "maybeMany" "abc123"))))

(define-test (test_many)
             (assert= '(("1")("2")("3")) (:value (:parse-rule x-parser "many" "123abc"))))

(define-test (test_failed_many)
             (assert= nil (:is-matched? (:parse-rule x-parser "many" "abc123"))))

(define-test (test_maybeOne)
             (assert= '(("1")) (:value (:parse-rule x-parser "maybeOne" "123abc"))))

(define-test (test_maybeOne_1)
             (assert= 1 (:pos (:parse-rule x-parser "maybeOne" "123abc"))))

(define-test (test_maybeOne_2)
             (assert= '() (:value (:parse-rule x-parser "maybeOne" "abc123"))))

(define-test (test_between_1)
             (assert= '(("1")("2")("3"))
                      (:value (:parse-rule x-parser "between3to5" "123abc"))))

(define-test (test_between_2)
             (assert= '(("1")("2")("3")("4")("5"))
                      (:value (:parse-rule x-parser "between3to5" "123456abc"))))

(define-test (test_has_1)
             (assert= '("a") (:value (:parse-rule x-parser "haschar" "abc"))))

(define-test (test_has_2)
             (assert= true (:is-matched? (:parse-rule x-parser "haschar" "abc"))))

(define-test (test_has_3)
             (assert= nil (:value (:parse-rule x-parser "haschar" "123"))))

(define-test (test_has_4)
             (assert= nil (:is-matched? (:parse-rule x-parser "haschar" "123"))))

(define-test (test_not_has_1)
             (assert= nil (:value (:parse-rule x-parser "nothaschar" "123"))))

(define-test (test_not_has_2)
             (assert= true (:is-matched? (:parse-rule x-parser "nothaschar" "123"))))

(define-test (test_not_has_3)
             (assert= '("a") (:value (:parse-rule x-parser "nothaschar" "abc"))))

(define-test (test_not_has_4)
             (assert= nil (:is-matched? (:parse-rule x-parser "nothaschar" "abc"))))

(UnitTest:run-all 'MAIN)
