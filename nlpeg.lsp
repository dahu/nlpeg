(context 'NLPEG)

;; PCRE_ANCHORED    16  anchor at the start
;; PCRE_UTF8      2048  pattern and strings as UTF-8 characters
(set 'REGEX-MASK (+ 16 2048))
(set 'MATCHED-STR 0)
(set 'END-POS 2)
(set 'ERROR true)
(set 'VERBOSE nil)
(set 'DEBUG nil)
(set 'SKIP-WHITE true)

(define (e str) (if (or ERROR VERBOSE DEBUG) (println str)))
(define (v str) (if (or VERBOSE DEBUG) (println str)))
(define (d str) (if DEBUG (println str)))

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
  (list MAIN:NLPEG-Parser options '() '()))
(define (NLPEG-Parser:options) (self 1))
(define (NLPEG-Parser:get-opt opt) (lookup opt (self 1)))
(define (NLPEG-Parser:grammar) 2)
(define (NLPEG-Parser:cache) 3)

(define (NLPEG-Parser:add-rule rule)
  (push (if (= 3 (length rule)) rule (push nil rule -1)) (self (:grammar (self)))))
(define (NLPEG-Parser:get-rule rule) (or (lookup rule (self (:grammar (self))) 1) rule))
(define (NLPEG-Parser:transform rule) (or (lookup rule (self (:grammar (self))) 2) (fn (v) v)))

(define (NLPEG-Parser:cache-dump)
  (println (string "CACHE: " (self (:cache (self))))))

;; (setf ((self) 3) new-pos)
(define (NLPEG-Parser:cache-set key res)
  (println (string "set key= " key))  (println (string "set res= " res))
  (push (list (string key) res) (self (:cache (self))))
  )

(define (NLPEG-Parser:cache-find key)
  (println (string "_______________________ find key= " key))
  (:cache-dump (self))
  (find (list (string key) X) (self (:cache (self))) unify))

(define (NLPEG-Parser:cache-get idx)
  ((self (:cache (self))) idx))

;; TODO: merge a default set of options in with user's options
;; start-rule
;; skip-white
;; verbose
;; debug

;; TODO: add the on-match callback to the add-rule call
(define (NLPEG-Parser:parse str)
  (:parse-rule (self) (:get-opt (self) "start-rule") str))

(define (NLPEG-Parser:parse-rule rule-name str)
  (NLPEG:d (string "NLPEG-Parser:parse-rule, parser=" (self)
                   ", rule= " rule-name
                   ", str= " str))
  ;; (:clear-cache (self))
  (letn ((parser (self))
         (rule (:get-rule (self) rule-name)))
        (println rule)
        (:match rule (NLPEG-ParseResult nil str 0 nil nil) parser rule-name)))

(context MAIN)
(new Class 'NLPEG-Expression)
(context NLPEG-Expression)
(define (NLPEG-Expression:NLPEG-Expression pat) (list MAIN:NLPEG-Expression pat))
(define (NLPEG-Expression:pat) (self 1))


;; expression matcher
;; input is a NLPEG-ParseResult with
;; :str - the input string
;; :pos - the current parse position
(define (NLPEG-Expression:m input parser rule-name)
  (NLPEG:d (string "NLPEG-Expression:m, parser=" parser ", rule-name=  " rule-name
                   ", input=" input
                   ", pat=" (:pat (self))))
  (letn ((spat (:pat (self)))
         (str (:str input))
         (pos (:pos input))
         (a-match (regex spat str NLPEG:REGEX-MASK pos)))
        (if (not a-match)
          (let ((errmsg (string "Failed to match /" spat "/ at char "
                                pos " in '" str "'"))
                (res (NLPEG-ParseResult nil str pos nil errmsg)))
            (NLPEG:e errmsg)
            (NLPEG:d res)
            res)
          (let ((res (NLPEG-ParseResult true str (+ pos (a-match NLPEG:END-POS))
                                        ((eval (:transform parser rule-name))
                                         (list (a-match NLPEG:MATCHED-STR))) nil)))
            (NLPEG:v res)
            res))))

(define (NLPEG-Expression:skip-white input parser)
  (NLPEG:d (string "NLPEG-Expression:skip-white, parser=" parser
                   ", input=" input
                   ", option skip-white= " (:get-opt parser "skip-white")))
  (if (and (:get-opt parser "skip-white")
           (regex {\s+} (:str input) NLPEG:REGEX-MASK (:pos input)))
    (:set-pos input (+ (:pos input) ($it NLPEG:END-POS))))
  input)

;; VimPEG's pmatch()
(define (NLPEG-Expression:match input parser rule-name)
  (NLPEG:d (string "NLPEG-Expression:match, parser=" parser
                   ", rule-name= " rule-name ", input=" input))
  (letn ((input (:skip-white (self) input parser))
         (key (list (:pos input) rule-name))
         (idx (:cache-find parser key))
         (res (if idx (:cache-get parser idx) (:m (self) input parser rule-name))))
        (if idx
          (println (string "cache index============================================== " idx))
          (:cache-set parser key res))
        (:cache-dump parser)
        res))

(context MAIN)
(new NLPEG-Expression 'NLPEG-Sequence)
(context NLPEG-Sequence)
(define (NLPEG-Sequence:NLPEG-Sequence seq) (list MAIN:NLPEG-Sequence seq))
(define (NLPEG-Sequence:seq) (self 1))

;; sequence matcher
;; input is a NLPEG-ParseResult with
;; :str - the input string
;; :pos - the current parse position
(define (NLPEG-Sequence:m input parser rule-name)
  (NLPEG:e (string "NLPEG-Sequence:m, parser=" parser ", rule-name=" rule-name
                   ", input=  " input
                   ", seq= " (:seq (self))))
  (letn ((sseq (:seq (self)))
         (str (:str input))
         (pos (:pos input))
         (is-matched true)
         (result '()))
        (dolist (s sseq (= is-matched nil))
          (letn ((e (:get-rule parser s))
                 (a-match (:match e input parser s)))
                (push a-match result -1)
                (if (not (:is-matched? a-match))
                  (setf is-matched false)
                  (:set-pos input (:pos a-match)))))
        (if (not is-matched)
          (let ((errmsg (string "Failed to match sequence [" sseq "] at char "
                                pos " in '" str "'"))
                (res (NLPEG-ParseResult nil str pos result errmsg)))
            (NLPEG:e errmsg)
            (NLPEG:d res)
            res)
          (let ((res (NLPEG-ParseResult true str
                                        (:pos input)
                                        ((eval (:transform parser rule-name))
                                         (map (fn (x) (:value x)) result)) nil)))
            (NLPEG:v res)
            res))))

(context MAIN)
(new NLPEG-Expression 'NLPEG-Choice)
(context NLPEG-Choice)
(define (NLPEG-Choice:NLPEG-Choice seq) (list MAIN:NLPEG-Choice seq))
(define (NLPEG-Choice:seq) (self 1))

;; ordered choice matcher
;; input is a NLPEG-ParseResult with
;; :str - the input string
;; :pos - the current parse position
(define (NLPEG-Choice:m input parser rule-name)
  (NLPEG:d (string "NLPEG-Choice:m, parser=" parser ", rule-name=" rule-name
                   ", input=" input
                   ", seq= " (:seq (self))))
  (letn ((sseq (:seq (self)))
         (str (:str input))
         (pos (:pos input))
         (is-matched nil)
         (result nil))
        (dolist (s sseq (= is-matched true))
          (letn ((e (:get-rule parser s))
                 (a-match (:match e input parser s)))
                (if (:is-matched? a-match)
                  (begin
                    (setf result a-match)
                    (setf is-matched true)
                    (:set-pos input (:pos a-match))))))
        (if (not is-matched)
          (let ((errmsg (string "Failed to match ordered choice [" sseq "] at char "
                                pos " in '" str "'"))
                (res (NLPEG-ParseResult nil str pos result errmsg)))
            (NLPEG:e errmsg)
            (NLPEG:d res)
            res)
          (let ((res (NLPEG-ParseResult true str
                                        (:pos input)
                                        ((eval (:transform parser rule-name))
                                         (:value result)) nil)))
            (NLPEG:v res)
            res))))

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
(define (NLPEG-Many:m input parser rule-name)
  (NLPEG:d (string "NLPEG-Many:m, parser=" parser ", rule-name=" rule-name
                   ", input=" input
                   ", expr= " (:expr (self))
                   ", cmin= " (:cmin (self))
                   ", cmax= " (:cmax (self))))
  (letn ((sexpr (:get-rule parser (:expr (self))))
         (scmin (:cmin (self)))
         (scmax (:cmax (self)))
         (str (:str input))
         (pos (:pos input))
         (cnt 0)
         (is-matched true)
         (result '()))
        (do-while (and (or (= 0 scmax) (< cnt scmax)) (:is-matched? a-match))
                  (setf a-match (:match sexpr input parser rule-name))
                  (if (:is-matched? a-match)
                    (begin
                      (inc cnt)
                      (push a-match result -1)
                      (:set-pos input (:pos a-match)))))
        (if (< cnt scmin)
          (let ((errmsg (string "Failed to match enough repeated items (found " cnt ") for ["
                                sexpr ":" scmin ":" scmax "] at char " pos " in '" str "'" ))
                (res (NLPEG-ParseResult nil str pos result errmsg)))
            (NLPEG:e errmsg)
            (NLPEG:d res)
            res)
          ;; TODO: there seems to be a hole in the logic here... what if it's not matched?
          (if is-matched
            (let ((res (NLPEG-ParseResult true str (:pos input)
                                          ((eval (:transform parser rule-name))
                                           (map (fn (x) (:value x)) result)) nil)))
              (NLPEG:v res)
              res)))))

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
(define (NLPEG-Predicate:m input parser rule-name)
  (NLPEG:d (string "NLPEG-Predicate:m, parser=" parser ", rule-name=" rule-name
                   ", input=" input
                   ", expr= " (:expr (self))
                   ", type= " (:type (self))))
  (letn ((sexpr (:get-rule parser (:expr (self))))
         (stype (:type (self)))
         (str (:str input))
         (pos (:pos input))
         (a-match (:match sexpr input parser (:expr (self))))
         (is-matched nil))
        (if (= stype "has")
          (setf is-matched (:is-matched? a-match))
          (setf is-matched (not (:is-matched? a-match))))
        (let ((res (NLPEG-ParseResult is-matched str pos
                                      ((eval (:transform parser rule-name))
                                       (:value a-match)) nil)))
          (NLPEG:v res)
          res)))

;; end of parser generator

(context MAIN)

(define (joiner lst)
  (join (map join lst)))

(setf x-options '(("start-rule" "x") ("skip-white" true)))
(setf x-parser (NLPEG-Parser x-options))

(:add-rule x-parser (list "x" (NLPEG-Choice
                                (list (NLPEG-Sequence
                                        (list (NLPEG-Expression {1})
                                              (NLPEG-Expression {a})))
                                      (NLPEG-Sequence
                                        (list (NLPEG-Expression {a})
                                              (NLPEG-Expression {1})))))
                          joiner
                          ))
(println x-parser)
(println (:parse-rule x-parser "x" " a 1"))

;; (:add-rule X-PARSER ("calc"  (NLPEG-Choice ("add" "sub"))))
;; (:add-rule x-parser ("add"   (NLPEG-Sequence ("prod" (NLPEG-Expression {\+}) "calc"))))
;; (:add-rule x-parser ("sub"   (NLPEG-Sequence ("prod" (NLPEG-Expression {-}) "calc"))))
;; (:add-rule x-parser ("prod"  (NLPEG-Choice ("mul" "num"))))
;; (:add-rule x-parser ("mul"   (NLPEG-Sequence ("num" (NLPEG-Expression {\*}) "prod"))))
;; (:add-rule x-parser ("num"   (NLPEG-Expression {\d+})))
;; (println (:parse-rule x-parser "calc" "1 + 2 - 3"))

;; (:add-rule x-parser '("num"  (NLPEG-Expression {\d+})))
;; (:add-rule x-parser '("char"  (NLPEG-Expression {[a-z]+})))
;; (:add-rule x-parser (list "numorchar"  (NLPEG-Choice ((NLPEG-Expression {\d+})(NLPEG-Expression {[a-z]+})))))
;; (:add-rule x-parser '("numandchar"  (NLPEG-Sequence ("numorchar" "numorchar"))))
;; (println (:parse-rule x-parser "numandchar" "123abc"))

;; ((seq (tok {\d+}) (tok {\w+})) "123hello" p f)
;; ((seq (tok {\d+}) (tok {[a-z]+}) (tok {\d+})) "123abc456" p f)
