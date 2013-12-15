(load "../../vendor/newlisp-unittest/nl-unittest.lsp")
(context MAIN)

(setf x-options '(("start-rule" "digits")))
(setf x-parser (NLPEG-Parser x-options))
(:add-rule x-parser '("digits" (Expression {\d+})))
(:add-rule x-parser '("chars" (Expression {[a-z]+})))

(define-test (test_default_start_rule)
             (assert= (:value (:parse x-parser "123abc")) '("123")))
(define-test (test_given_start_rule)
             (assert= (:value (:parse-rule x-parser "chars" "abc123")) '("abc")))

(UnitTest:run-all 'MAIN)
