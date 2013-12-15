(load "../../vendor/newlisp-unittest/nl-unittest.lsp")
(load "nlpeg.lsp")
(context MAIN)

(setf x-options '(("start-rule" "digits")))
(setf x-parser (NLPEG-Parser x-options))
(:add-rule x-parser (list "digits" (NLPEG-Expression {\d+})))
(:add-rule x-parser (list "chars" (NLPEG-Expression {[a-z]+})))

(:add-rule x-parser (list "3num3chars" (NLPEG-Sequence
                                    (list "digits" "chars"))))

(define-test (test_default_start_rule)
             (assert= (:value (:parse x-parser "123abc")) '("123")))

(define-test (test_given_start_rule)
             (assert= (:value (:parse-rule x-parser "chars" "abc123")) '("abc")))

(define-test (test_sequence_of_expressions)
             (assert= (:value (:parse-rule x-parser "3num3chars" "123abc")) '(("123") ("abc"))))
(define-test (test_failed_sequence_of_expressions_0)
             (assert= (:value (:parse-rule x-parser "3num3chars" "abc123")) nil))

(define-test (test_failed_sequence_of_expressions_3)
             (assert= (:value (:parse-rule x-parser "3num3chars" "123...")) nil))

(UnitTest:run-all 'MAIN)
