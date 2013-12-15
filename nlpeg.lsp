(context 'NLPEG)

;; PCRE_ANCHORED    16  anchor at the start
;; PCRE_UTF8      2048  pattern and strings as UTF-8 characters
(set 'REGEX-MASK (+ 16 2048))
(set 'MATCHED-STR 0)
(set 'END-POS 2)
(set 'DEBUG nil)
(set 'VERBOSE true)
(set 'ERROR true)

(define (d str) (if DEBUG (println str)))
(define (v str) (if VERBOSE (println str)))
(define (e str) (if ERROR (println str)))

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
  (letn ((parser (self))
         (rule (:get-rule (self) rule)))
        (NLPEG:v rule)
        (:match rule (NLPEG-ParseResult nil str 0 nil nil) parser)))

(context MAIN)
(new Class 'NLPEG-Expression)
(context NLPEG-Expression)
(define (NLPEG-Expression:NLPEG-Expression pat) (list MAIN:NLPEG-Expression pat))
(define (NLPEG-Expression:pat) (self 1))

;; expression matcher
;; input is a NLPEG-ParseResult with
;; :str - the input string
;; :pos - the current parse position
(define (NLPEG-Expression:m input parser)
  (letn ((pat (:pat (self)))
         (str (:str input))
         (pos (:pos input))
         (a-match (regex pat str NLPEG:REGEX-MASK pos)))
        (NLPEG:d (string "NLPEG-Expression:m /" pat "/"))
        (if (not a-match)
          (let ((errmsg (string "Failed to match /" pat "/ at char "
                                pos " in '" str "'")))
            (NLPEG:e errmsg)
            (NLPEG-ParseResult nil str pos nil errmsg))
          (NLPEG-ParseResult true str
                             (+ pos (a-match NLPEG:END-POS))
                             (list (a-match NLPEG:MATCHED-STR)) nil))))

(define (NLPEG-Expression:skip-white input parser)
  (if (:get-opt parser "skip-white")
    (if (regex {\s+} (:str input) NLPEG:REGEX-MASK (:pos input))
      (:set-pos input ($it NLPEG:END-POS))))
  input)

;; VimPEG's pmatch()
(define (NLPEG-Expression:match input parser)
  (NLPEG:d (string "NLPEG-Expression:match, parser=" parser ", input=" input))
  (let ((res (:m (self) (:skip-white (self) input parser) parser)))
    (NLPEG:d res) res))

(context MAIN)
(new NLPEG-Expression 'NLPEG-Sequence)
(context NLPEG-Sequence)
(define (NLPEG-Sequence:NLPEG-Sequence seq) (list MAIN:NLPEG-Sequence seq))
(define (NLPEG-Sequence:seq) (self 1))

;; sequence matcher
;; input is a NLPEG-ParseResult with
;; :str - the input string
;; :pos - the current parse position
(define (NLPEG-Sequence:m input parser)
  (letn ((seq (:seq (self)))
         (str (:str input))
         (pos (:pos input))
         (is-matched true)
         (res '()))
        (NLPEG:d (string "NLPEG-Sequence:m [" seq "]"))
        (dolist (s seq (= is-matched nil))
          (letn ((e (:get-rule parser s))
                 (a-match (:match e input parser)))
                (push a-match res -1)
                (if (not (:is-matched? a-match))
                  (setf is-matched false)
                  (:set-pos input (:pos a-match)))))
        (if (not is-matched)
          (let ((errmsg (string "Failed to match sequence [" seq "] at char "
                                pos " in '" str "'")))
            (NLPEG:e errmsg)
            (NLPEG-ParseResult nil str pos res errmsg))
          (NLPEG-ParseResult true str
                             (:pos input)
                             (map (fn (x) (:value x)) res) nil))))

(context MAIN)
(new NLPEG-Expression 'NLPEG-Choice)
(context NLPEG-Choice)
(define (NLPEG-Choice:NLPEG-Choice seq) (list MAIN:NLPEG-Choice seq))
(define (NLPEG-Choice:seq) (self 1))

;; ordered choice matcher
;; input is a NLPEG-ParseResult with
;; :str - the input string
;; :pos - the current parse position
(define (NLPEG-Choice:m input parser)
  (letn ((seq (:seq (self)))
         (str (:str input))
         (pos (:pos input))
         (is-matched nil)
         (res nil))
        (NLPEG:d (string "NLPEG-Choice:m [" seq "]"))
        (dolist (s seq (= is-matched true))
          (letn ((e (:get-rule parser s))
                 (a-match (:match e input parser)))
                (if (:is-matched? a-match)
                  (begin
                    (setf res a-match)
                    (setf is-matched true)
                    (:set-pos input (:pos a-match))))))
        (if (not is-matched)
          (let ((errmsg (string "Failed to match ordered choice [" seq "] at char "
                                pos " in '" str "'")))
            (NLPEG:e errmsg)
            (NLPEG-ParseResult nil str pos res errmsg))
          (NLPEG-ParseResult true str
                             (:pos input)
                             (:value res) nil))))

(context MAIN)
(new NLPEG-Expression 'NLPEG-Many)
(context NLPEG-Many)
(define (NLPEG-Many:NLPEG-Many expr cmin cmax) (list MAIN:NLPEG-Many expr cmin cmax))
(define (NLPEG-Many:expr) (self 1))
(define (NLPEG-Many:cmin) (self 2))
(define (NLPEG-Many:cmax) (self 3))

;; expression many matcher
;; input is a NLPEG-ParseResult with
;; :str - the input string
;; :pos - the current parse position
(define (NLPEG-Many:m input parser)
  (letn ((expr (:get-rule parser (:expr (self))))
         (cmin (:cmin (self)))
         (cmax (:cmax (self)))
         (str (:str input))
         (pos (:pos input))
         (cnt 0)
         (is-matched true)
         (res '()))
        (NLPEG:v (string "NLPEG-Many:m [" expr ":" cmin ":" cmax "]"))
        (do-while (and (or (= 0 cmax) (< cnt cmax)) (:is-matched? a-match))
                  (println $idx)
                  (setf a-match (:match expr input parser))
                  (println a-match)
                  (if (:is-matched? a-match)
                    (begin
                      (inc cnt)
                      (push a-match res -1)
                      (:set-pos input (:pos a-match)))))
        (println "end of loop")
        (if (< cnt cmin)
          (let ((errmsg (string "Failed to match enough repeated items (found " cnt ") for ["
                                expr ":" cmin ":" cmax "] at char " pos " in '" str "'" )))
            (NLPEG:e errmsg)
            (NLPEG-ParseResult nil str pos res errmsg))
          (if is-matched
            (begin
              (NLPEG-ParseResult true str (:pos input)
                             (map (fn (x) (:value x)) res) nil))))))

(context MAIN)
(new NLPEG-Expression 'NLPEG-Predicate)
(context NLPEG-Predicate)
(define (NLPEG-Predicate:NLPEG-Predicate expr type) (list MAIN:NLPEG-Predicate expr type))
(define (NLPEG-Predicate:expr) (self 1))
(define (NLPEG-Predicate:type) (self 2))

;; expression many matcher
;; input is a NLPEG-ParseResult with
;; :str - the input string
;; :pos - the current parse position
(define (NLPEG-Predicate:m input parser)
  (letn ((expr (:get-rule parser (:expr (self))))
         (type (:type (self)))
         (str (:str input))
         (pos (:pos input))
         (a-match (:match expr input parser))
         (is-matched nil))
        (NLPEG:v (string "NLPEG-Predicate:m [" expr ":" type "]"))
        (if (= type "has")
          (setf is-matched (:is-matched? a-match))
          (setf is-matched (not (:is-matched? a-match))))
        (NLPEG-ParseResult is-matched str pos (:value a-match) nil)))

;; end of parser

(context MAIN)

(setf x-options '(("start-rule" "digits")))
(setf x-parser (NLPEG-Parser x-options))

(:add-rule x-parser '("digit"        (NLPEG-Expression {\d})))
(:add-rule x-parser '("digits"       (NLPEG-Expression {\d+})))
(:add-rule x-parser '("char"         (NLPEG-Expression {[a-z]})))
(:add-rule x-parser '("chars"        (NLPEG-Expression {[a-z]+})))
(:add-rule x-parser '("numsANDchars" (NLPEG-Sequence ("digits" "chars"))))
(:add-rule x-parser '("numsORchars"  (NLPEG-Choice ("digits" "chars"))))
(:add-rule x-parser '("maybeMany"    (NLPEG-Many "digit" 0 0)))
(:add-rule x-parser '("many"         (NLPEG-Many "digit" 1 0)))
(:add-rule x-parser '("maybeOne"     (NLPEG-Many "digit" 0 1)))
(:add-rule x-parser '("between3to5"  (NLPEG-Many "digit" 3 5)))
(:add-rule x-parser '("haschar"      (NLPEG-Predicate "char" "has")))
(:add-rule x-parser '("nothaschar"   (NLPEG-Predicate "char" "not-has")))

(println (:parse-rule x-parser "haschar" "123"))
;; (println (:parse-rule x-parser "nothaschar" "abc"))
;; (println (:parse-rule x-parser "nothaschar" "123"))

;; ((seq (tok {\d+}) (tok {\w+})) "123hello" p f)
;; ((seq (tok {\d+}) (tok {[a-z]+}) (tok {\d+})) "123abc456" p f)
