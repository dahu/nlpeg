(load "../../vendor/newlisp-unittest/nl-unittest.lsp")
(load "nlpeg.lsp")
(context 'TEST-NLPEG-CALC)

(define (str->num x)
  (int (first x)))

(define (calc-ncalc x)
  (x 1))

(define (calc-add x)
  (+ (first x) (last x)))

(define (calc-sub x)
  (- (first x) (last x)))

(define (calc-mul x)
  (* (first x) (last x)))

(define (calc-div x)
  (/ (first x) (last x)))

(setf c-parser (MAIN:NLPEG-Parser '(("skip-white" true) ("start-rule" "calc"))))

(:add-rule c-parser (list "plus"   (MAIN:NLPEG-Expression {\+})))
(:add-rule c-parser (list "minus"  (MAIN:NLPEG-Expression {-})))
(:add-rule c-parser (list "times"  (MAIN:NLPEG-Expression {\*})))
(:add-rule c-parser (list "divide" (MAIN:NLPEG-Expression {/})))
(:add-rule c-parser (list "oparen" (MAIN:NLPEG-Expression {\(})))
(:add-rule c-parser (list "cparen" (MAIN:NLPEG-Expression {\)})))
(:add-rule c-parser (list "calc"   (MAIN:NLPEG-Choice     '("add" "sub" "prod")) ))
(:add-rule c-parser (list "add"    (MAIN:NLPEG-Sequence   '("prod" "plus" "calc")) calc-add))
(:add-rule c-parser (list "sub"    (MAIN:NLPEG-Sequence   '("prod" "minus" "calc")) calc-sub))
(:add-rule c-parser (list "prod"   (MAIN:NLPEG-Choice     '("mul" "div" "atom")) ))
(:add-rule c-parser (list "mul"    (MAIN:NLPEG-Sequence   '("atom" "times" "prod")) calc-mul))
(:add-rule c-parser (list "div"    (MAIN:NLPEG-Sequence   '("atom" "divide" "prod")) calc-div))
(:add-rule c-parser (list "ncalc"  (MAIN:NLPEG-Sequence   '("oparen" "calc" "cparen")) calc-ncalc))
(:add-rule c-parser (list "atom"   (MAIN:NLPEG-Choice     '("num" "ncalc"))))
(:add-rule c-parser (list "num"    (MAIN:NLPEG-Expression {\d+}) str->num))

(define-test (test_add_mul)
             (assert= 7 (:value (:parse c-parser "1 + 2 * 3"))))

(define-test (test_mul_add)
             (assert= 5 (:value (:parse c-parser "1 * 2 + 3"))))

(define-test (test_nested_mul_add)
             (assert= 6 (:value (:parse c-parser "1 + (2 + 3)"))))

(define-test (test_nested_mul_add)
             (assert= 14 (:value (:parse c-parser "2 * (3 + 4)"))))

(define-test (test_nested_mul_add)
             (assert= 70 (:value (:parse c-parser "2 * ((3 + 4) * 5)"))))

(define-test (test_nested_mul_sub)
             (assert= 3 (:value (:parse c-parser "20 - ((3 * 4) + 5)"))))

(define-test (test_nested_div_add)
             (assert= 2 (:value (:parse c-parser "20 / ((2 * 3) + 4)"))))


(UnitTest:run-all 'TEST-NLPEG-CALC)
