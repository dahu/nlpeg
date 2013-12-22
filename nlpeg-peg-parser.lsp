(load "./nlpeg.lsp")
(context 'NLPEG-PEG-PARSER)

(setf 'DEBUG true)
(new Tree 'MAIN:Options)

(define (dbg str)
  (if DEBUG (println str)))

(define (set-start-rule rule)
  (unless (Options "start-rule") (Options "start-rule" rule)))

;; callbacks {{{1

(define (do-line elems)
  (dbg (string "Line: " elems))
  elems)

(define (do-definition elems)
  (println (string "do-definition elems= " elems))
  (letn ((label (first elems))
         (expression (if (regex {^"} (elems 2) 0)
                       (string "(p-and (list " (elems 2) "))")
                       (elems 2)))
         (cback (if (> (length (elems 3)) 0) (first (elems 3)) "")))
        (set-start-rule label)
        (string "(:add-rule " expression " " cback ")")))

(define (do-expression elems)
  (println (string "do-expression elems= " elems))
  (if (> (length (last elems)) 0)
    (string "(p-or (list " (join (clean (fn (x) (= x "/")) (flat elems)) " ") "))")
    (first elems)))

(define (do-sequence elems)
  (println (string "do-sequence elems= " elems))
  (let ((el (clean empty? elems)))
  (if (> (length el) 1)
    (string "(p-and (list " (join el " ") "))")
    (first el))))

(define (do-prefix elems)
  (println (string "do-prefix elems= " elems))
  (if (> (length (first elems)) 0)
    (string "(p-"
            (if (= (first (first elems)) "!") "not-") "has " (last elems)
            ")")
    (last elems)))

(define (do-suffix elems)
  (println (string "do-suffix elems= " elems))
  (if (> (length (last elems)) 0)
    (string "(p-"
            (cond
              ((= (first (last elems)) "*") "maybe-many")
              ((= (first (last elems)) "+") "many")
              (true "maybe-one"))
            (first elems)
            ")")
    (first elems)))

(define (do-primary elems)
  (println (string "do-primary elems= " elems))
  (if (list? elems)
    (if (= 2 (length elems))
      (first elems)
      (last elems))
    (elems)))

(define (do-callback elems)
  (println (string "do-callback elems= " elems))
  (first elems))

(define (do-option elems)
  (Options ((elems 1) 0) (elems 3)))

(define (do-identifier elems)
  (println (string "do-identifier " elems))
  (string {"} (first elems) {"}))

(define (do-option_value elems) elems)

(define (do-regex elems)
  (println (string "do-regex " elems))
  (string "(p-e {" elems "})"))

(define (do-dquoted_string elems)
  (string (elems 0) (join (elems 1) "") (elems 2)))

(define (do-squoted_string elems)
  (string (elems 0) (join (elems 1) "") (elems 2)))

(define (do-escaped_dquote elems) (elems 0))
(define (do-double_backslash elems) (elems 0))
(define (do-backslash elems) (elems 0))
(define (do-dquote elems) (elems 0))
(define (do-double_squote elems) (elems 0))
(define (do-squote elems) (elems 0))
(define (do-right_arrow elems) (elems 0))
(define (do-mallet elems) elems)
(define (do-boolean elems) elems)
(define (do-comment elems) (string ";;; " (flat elems)))
(define (do-true elems) true)
(define (do-false elems) nil)
(define (do-or elems) (elems 0))
(define (do-dot elems) (elems 0))
(define (do-question elems) (elems 0))
(define (do-star elems) (elems 0))
(define (do-plus elems) (elems 0))
(define (do-close elems) (elems 0))
(define (do-open elems) (elems 0))

;; parser grammar {{{1

(setf p (NLPEG-Parser '(("start-rule" "line") ("skip-white" true))))

(:add-rule p (list "line" (p-or '("blank_line" "comment" "option" "definition"))
                   do-line))

      ;; calc   ::=  add | sub | prod
(:add-rule p (list "definition"
                   (p-and (list "identifier" "mallet" "expression"
                                (p-maybe-one "callback")
                                (p-maybe-one "comment")
                                "eol"))
                   do-definition))

(:add-rule p (list "expression"
                   (p-and (list "sequence"
                                (p-maybe-many (p-and '("or" "sequence")))))
                   do-expression))

(:add-rule p (list "sequence" (p-maybe-many "prefix")
                   do-sequence))

(:add-rule p (list "prefix"
                   (p-and (list (p-maybe-one (p-or '("and" "not")))
                                "suffix"))
                   do-prefix))

(:add-rule p (list "suffix"
                   (p-and (list "primary"
                                (p-maybe-one (p-or '("question" "star" "plus")))))
                   do-suffix))

(:add-rule p (list "primary"
                   (p-or (list
                           (p-and (list "identifier" (p-not-has "mallet")))
                           (p-and '("open" "expression" "close"))
                           "regex"))
                            do-primary))

(:add-rule p (list "callback" (p-and '("right_arrow" "callback_identifier"))
                   do-callback))

(:add-rule p (list "option"
                   (p-and (list "dot" "option_name" "equal" "option_value"
                                (p-maybe-one "comment")
                                "eol"))
                   do-option))

(:add-rule p (list "option_name" (p-and '("identifier"))
                   do-option-name))

(:add-rule p (list "option_value"
                   (p-or '("squoted_string" "dquoted_string" "boolean" "number"))
                   do-option-value))

(:add-rule p (list "callback_identifier" (p-e {[[:alnum:]#._:]+})
                   do-identifier))

;; TODO: confirm that \w+ here is sufficient
(:add-rule p (list "identifier" (p-e {\w+})
                   do-identifier))

(:add-rule p (list "regex" (p-or '("dquoted_string" "squoted_string"))
                   do-regex))

(:add-rule p (list "dquoted_string"
                   (p-and (list "dquote"
                                (p-maybe-many
                                  (p-or (list "double_backslash"
                                              "escaped_dquote"
                                              (p-e {[^"]}))))
                                "dquote"))
                                              do-dq-string))

(:add-rule p (list "squoted_string"
                   (p-and (list "squote"
                                (p-maybe-many
                                  (p-or (list (p-e {[^']}) "double_squote")))
                                "squote"))
                   do-sq-string))

(:add-rule p (list "escaped_dquote" (p-and '("backslash" "dquote"))
                   do-escaped-dquote))

(:add-rule p (list "double_backslash" (p-and '("backslash" "backslash"))
                   do-double-backslash))

(:add-rule p (list "backslash" (p-e {\})))

(:add-rule p (list "number" (p-e {^(0|[1-9]\d*)?(\.\d+)?})))

(:add-rule p (list "dquote" (p-e {"})))

(:add-rule p (list "double_squote" (p-e {''})))

(:add-rule p (list "squote" (p-e {'})))

(:add-rule p (list "comment" (p-e {;.*$})
                   do-comment))

(:add-rule p (list "blank_line" (p-e {^\s*$})))

;; TODO: need to include newline in $ match here?
(:add-rule p (list "eol" (p-e {$})))

(:add-rule p (list "right_arrow" (p-e {->})))

(:add-rule p (list "mallet" (p-e {::=})))

(:add-rule p (list "boolean" (p-or '("true" "false"))))

(:add-rule p (list "true" (p-e {(?i:true|on)})
                   do-true))

(:add-rule p (list "false" (p-e {(?i:false|off)})
                   do-false))

(:add-rule p (list "equal" (p-e {=})))

(:add-rule p (list "or" (p-e {\/})))

(:add-rule p (list "not" (p-e {!})))

(:add-rule p (list "and" (p-e {&})))

(:add-rule p (list "question" (p-e {\?})))

(:add-rule p (list "star" (p-e {\*})))

(:add-rule p (list "plus" (p-e {\+})))

(:add-rule p (list "open" (p-e {\(})))

(:add-rule p (list "close" (p-e {\)})))

(:add-rule p (list "dot" (p-e {\.})))

;; public interface {{{1

;; let vimpeg#peg#parser = s:p.GetSym('line')

(define (peg-parse lines)
  (let ((result '())
        (res nil))
    ;; TODO: filter blank lines
    (dolist (line (clean empty? (parse lines "\n")))
      (setf res (:parse-rule p "line" line))
      (println res)
      (if (not (:is-matched? res))
        (begin
          (println (string "Parse Failed on line " (+ 1 $idx) ": " line))
          '())
        (push (:value res) result -1)))
    result))

;; (define (write-parser path)
;;   (if (exists path)
;;     (begin
;;       (println (string "The file '" path "' already exists!"))
;;       nil)
;;     (write-file path result)))

      ;; ; Simple Calculator
      ;; ; Vimpeg Example Grammar
      ;; ; Barry Arthur, 2011 10 24
      ;; ;
      ;; .skip_white = true
      ;; .namespace = 'calculator'
      ;; .parser_name = 'calc#parser'
      ;; .root_element = 'calc'
      ;; ;
(setf the-text [text]
      calc ::= add / sub / prod
      add    ::=  prod '\+' calc      ->  #add
[/text])
      ;; sub    ::=  prod '-' calc       ->  #sub
      ;; prod   ::=  mul / div / atom
      ;; mul    ::=  atom '\*' prod      ->  #mul
      ;; div    ::=  atom '/' prod       ->  #div
      ;; ncalc  ::=  '\(' calc '\)'      ->  #nCalc
      ;; atom   ::=  num / ncalc
      ;; num    ::=  '\d+'               ->  #num

(println (peg-parse the-text))

;; vim: commentstring=;;\ %s foldmethod=marker
