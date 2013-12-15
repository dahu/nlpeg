(context 'NLPEG)

;; PCRE_ANCHORED    16  anchor at the start
;; PCRE_UTF8      2048  pattern and strings as UTF-8 characters
(set 'REGEX-MASK (+ 16 2048))
(set 'MATCHED-STR 0)
(set 'END-POS 2)
(set 'VERBOSE 1)

(define (v str) (if VERBOSE (println str)))
(define (error str) (println str))

(context MAIN)
(new Class 'NLPEG-ParseResult)
(context NLPEG-ParseResult)
(define (NLPEG-ParseResult:NLPEG-ParseResult is-matched str pos value errmsg)
  (list MAIN:NLPEG-ParseResult is-matched str pos value errmsg))
(define (NLPEG-ParseResult:is-matched?) (self 1))
(define (NLPEG-ParseResult:str) (self 2))
(define (NLPEG-ParseResult:pos) (self 3))
(define (NLPEG-ParseResult:set-pos new-pos) (setf ((self) 3) new-pos))
(define (NLPEG-ParseResult:value) (self 4))
(define (NLPEG-ParseResult:errmsg) (self 5))

(context MAIN)
(new Class 'NLPEG-Parser)
(context NLPEG-Parser)
;; '() is an assoc list of symbol:parse-object
(define (NLPEG-Parser:NLPEG-Parser options)
  (list MAIN:NLPEG-Parser options '()))
(define (NLPEG-Parser:options) (self 1))
(define (NLPEG-Parser:get-opt opt) (lookup opt (self 1)))
(define (NLPEG-Parser:grammar) (self 2))
(define (NLPEG-Parser:add-rule rule) (push rule (self 2)))
;; (define (NLPEG-Parser:add-rule rule) (push (list rule (list rule self)) (self 2)))
;; (define (NLPEG-Parser:get-rule rule) (or (lookup rule (self 2)) (append rule (list self))))
;; (define (NLPEG-Parser:get-rule rule) ((self 2) (find (list (list rule ?) ?) (self 2) unify)))
(define (NLPEG-Parser:get-rule rule) (lookup rule (self 2)))
;; TODO: merge a default set of options in with user's options
;; start-rule
;; skip-white
;; verbose
;; debug

;; TODO: add the on-match callback to the add-rule call
(define (NLPEG-Parser:parse str)
  (:parse-rule (self) (:get-opt (self) "start-rule") str))
(define (NLPEG-Parser:parse-rule rule str)
  (println (self))
  (letn ((parser (self))
         (rule (:get-rule (self) rule))
        )
  (println (string "rule-> " rule))
  (:match rule (NLPEG-ParseResult nil str 0 nil nil) parser)))

(context MAIN)
(new Class 'NLPEG-Expression)
(context NLPEG-Expression)
(define (NLPEG-Expression:NLPEG-Expression pat) (list MAIN:NLPEG-Expression pat))
(define (NLPEG-Expression:pat) (self 1))

;; matcher
;; input is a NLPEG-ParseResult with
;; :str - the input string
;; :pos - the current parse position
(define (NLPEG-Expression:m input parser)
  (letn ((pat (:pat (self)))
         (str (:str input))
         (pos (:pos input))
         (a-match (regex pat str NLPEG:REGEX-MASK pos)))
        (NLPEG:v (string "NLPEG-Expression:m /" pat "/"))
        (if (not a-match)
          (let ((errmsg (string "Failed to match /" pat "/ at char "
                                pos " in '" str "'")))
            (NLPEG:v errmsg)
            (NLPEG-ParseResult nil str pos nil errmsg))
          (NLPEG-ParseResult true str
                       (+ pos (a-match NLPEG:END-POS))
                       (list (a-match NLPEG:MATCHED-STR)) nil))))

(define (NLPEG-Expression:skip-white input)
  ;; TODO: skip-white option
  (println "skip-white 1")
  (println input)
  (if true
    (if (regex {\s+} (:str input) NLPEG:REGEX-MASK (:pos input))
      (:set-pos input ($it NLPEG:END-POS))))
  input)

;; VimPEG's pmatch()
(define (NLPEG-Expression:match input parser)
  (println input)
  (println (string "match, parser-> " parser))
  (let ((res (:m (self) (:skip-white (self) input) parser)))
        (println input)
        (println res)))

(context MAIN)
(new NLPEG-Expression 'NLPEG-Sequence)
(context NLPEG-Sequence)
(define (NLPEG-Sequence:NLPEG-Sequence seq) (list MAIN:NLPEG-Sequence seq))
(define (NLPEG-Sequence:seq) (self 1))

;; matcher
;; input is a NLPEG-ParseResult with
;; :str - the input string
;; :pos - the current parse position
(define (NLPEG-Sequence:m input parser)
  (println (string "m, parser-> " parser))
  (letn ((seq (:seq (self)))
         (str (:str input))
         (pos (:pos input))
         (is-matched true)
         (res '()))
        (NLPEG:v (string "NLPEG-Sequence:m [" seq "]"))
        (println seq)
        (println parser)
        (dolist (s seq (= is-matched nil))
          (println (string "s-> " s))
             (letn ((e (:get-rule parser s))
                    (a-match (:match e input parser))
                    )
                   (println (string "e rule-> " e))
                   (push a-match res -1)
                   (if (not (:is-matched? a-match))
                     (setf is-matched false)
                     (:set-pos input (:pos a-match)))))
        (if (not is-matched)
          (let ((errmsg (string "Failed to match [" seq "] at char "
                                pos " in '" str "'")))
            (NLPEG:v errmsg)
            (NLPEG-ParseResult nil str pos nil errmsg))
          (NLPEG-ParseResult true str
                       (:pos input)
                       (map (fn (x) (:value x)) res) nil))))


;; (apply (fn (x y) (println x y)) '(a b c) 2)

;; end of parser

(context MAIN)

(setf x-options '(("start-rule" "digits")))
(setf x-parser (NLPEG-Parser x-options))
(:add-rule x-parser '("digits" (NLPEG-Expression {\d+})))
(:add-rule x-parser '("chars" (NLPEG-Expression {[a-z]+})))
(:add-rule x-parser '("3num3chars" (NLPEG-Sequence ("digits" "chars"))))
(:parse-rule x-parser "3num3chars" "123abc")

;; ((seq (tok {\d+}) (tok {\w+})) "123hello" p f)
;; ((seq (tok {\d+}) (tok {[a-z]+}) (tok {\d+})) "123abc456" p f)
