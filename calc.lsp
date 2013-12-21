(load "../../vendor/newlisp-unittest/nl-unittest.lsp")
(load "nlpeg.lsp")
(context MAIN)

(setf x-parser (NLPEG-Parser '(("skip-white" true) ("start-rule" "calc"))))

(:add-rule x-parser (list "plus"   (NLPEG-Expression {\+})))
(:add-rule x-parser (list "minus"  (NLPEG-Expression {-})))
(:add-rule x-parser (list "times"  (NLPEG-Expression {\*})))
(:add-rule x-parser (list "divide" (NLPEG-Expression {/})))
(:add-rule x-parser (list "oparen" (NLPEG-Expression {\(})))
(:add-rule x-parser (list "cparen" (NLPEG-Expression {\)})))
(:add-rule x-parser (list "calc"   (NLPEG-Choice     '("add" "sub" "prod")) ))
(:add-rule x-parser (list "add"    (NLPEG-Sequence   '("prod" "plus" "calc")) calc-add))
(:add-rule x-parser (list "sub"    (NLPEG-Sequence   '("prod" "minus" "calc")) calc-sub))
(:add-rule x-parser (list "prod"   (NLPEG-Choice     '("mul" "div" "atom")) ))
(:add-rule x-parser (list "mul"    (NLPEG-Sequence   '("atom" "times" "prod")) calc-mul))
(:add-rule x-parser (list "div"    (NLPEG-Sequence   '("atom" "divide" "prod")) calc-div))
(:add-rule x-parser (list "ncalc"  (NLPEG-Sequence   '("oparen" "calc" "cparen")) calc-ncalc))
(:add-rule x-parser (list "atom"   (NLPEG-Choice     '("num" "ncalc"))))
(:add-rule x-parser (list "num"    (NLPEG-Expression {\d+}) str->num))

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

(define-test (test_add_mul)
             (assert= 7 (:value (:parse x-parser "1 + 2 * 3"))))

(define-test (test_mul_add)
             (assert= 5 (:value (:parse x-parser "1 * 2 + 3"))))

(define-test (test_nested_mul_add)
             (assert= 6 (:value (:parse x-parser "1 + (2 + 3)"))))

(define-test (test_nested_mul_add)
             (assert= 14 (:value (:parse x-parser "2 * (3 + 4)"))))

(define-test (test_nested_mul_add)
             (assert= 70 (:value (:parse x-parser "2 * ((3 + 4) * 5)"))))

(define-test (test_nested_mul_sub)
             (assert= 3 (:value (:parse x-parser "20 - ((3 * 4) + 5)"))))

(define-test (test_nested_div_add)
             (assert= 2 (:value (:parse x-parser "20 / ((2 * 3) + 4)"))))


(UnitTest:run-all 'MAIN)
