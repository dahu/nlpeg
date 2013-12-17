(load "nlpeg.lsp")

(setf x-parser (NLPEG-Parser '(("skip-white" true) ("start-rule" "calc"))))

(:add-rule x-parser '("plus"   (NLPEG-Expression {\+})))
(:add-rule x-parser '("minus"  (NLPEG-Expression {-})))
(:add-rule x-parser '("times"  (NLPEG-Expression {\*})))
(:add-rule x-parser '("divide" (NLPEG-Expression {/})))
(:add-rule x-parser '("oparen" (NLPEG-Expression {\(})))
(:add-rule x-parser '("cparen" (NLPEG-Expression {\)})))
(:add-rule x-parser '("calc"   (NLPEG-Choice     ("add" "sub" "prod"))))
;; (:add-rule x-parser '("add"    (NLPEG-Sequence   ("prod" (NLPEG-Expression {\+}) "calc"))))
(:add-rule x-parser '("add"    (NLPEG-Sequence   ("prod" "plus"  "calc"))))
(:add-rule x-parser '("sub"    (NLPEG-Sequence   ("prod" "minus" "calc"))))
(:add-rule x-parser '("prod"   (NLPEG-Choice     ("mul" "div" "atom"))))
(:add-rule x-parser '("mul"    (NLPEG-Sequence   ("atom" "times" "prod"))))
(:add-rule x-parser '("div"    (NLPEG-Sequence   ("atom" "divide" "prod"))))
(:add-rule x-parser '("ncalc"  (NLPEG-Sequence   ("oparen" "calc" "cparen"))))
(:add-rule x-parser '("atom"   (NLPEG-Choice     ("num" "ncalc"))))
(:add-rule x-parser '("num"    (NLPEG-Expression {\d+})))

(println (:parse x-parser "1 + 2 * 3"))