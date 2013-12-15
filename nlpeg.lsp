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
(define (NLPEG-ParseResult:is-matched) (self 1))
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
  (:match (:get-rule (self) rule)
          (NLPEG-ParseResult nil str 0 nil nil)))

(context MAIN)
(new Class 'NLPEG-Expression)
(context NLPEG-Expression)
(define (NLPEG-Expression:NLPEG-Expression pat) (list MAIN:NLPEG-Expression pat))
(define (NLPEG-Expression:pat) (self 1))

;; matcher
;; input is a NLPEG-ParseResult with
;; :str - the input string
;; :pos - the current parse position
(define (NLPEG-Expression:m input)
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
  (if true
    (if (regex {\s+} (:str input) NLPEG:REGEX-MASK (:pos input))
      (:set-pos input ($it NLPEG:END-POS))))
  input)

;; VimPEG's pmatch()
(define (NLPEG-Expression:match input)
  (let ((res (NLPEG-Expression:m (:skip-white (self) input))))
        (println input)
        (println res)))

;; (apply (fn (x y) (println x y)) '(a b c) 2)

;; end of parser

(context MAIN)

(setf x-options '(("start-rule" "digits")))
(setf x-parser (NLPEG-Parser x-options))
(:add-rule x-parser '("digits" (NLPEG-Expression {\d+})))
(:add-rule x-parser '("chars" (NLPEG-Expression {[a-z]+})))

;; ((seq (tok {\d+}) (tok {\w+})) "123hello" p f)
;; ((seq (tok {\d+}) (tok {[a-z]+}) (tok {\d+})) "123abc456" p f)
