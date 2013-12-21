(load "./nlpeg.lsp")
(context 'NLPEG-PEG-PARSER)

(setf p (NLPEG-Parser '(("start-rule" "line") ("skip-white" true))))

(:add-rule p (list "line" (p-and '("comment" "option" "definiton"))
                   do-line))

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

(:addd-rule p (list "primary"
                    (p-or (list (p-and (list "identifier" (p-not-has "mallet")))
                                p-and '("open" "expression" "close"))
                          "regex")
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

(:add-rule p (list "number" (p-e {(0|[1-9]\d*)(\.\d+)?})))

(:add-rule p (list "dquote" (p-e {"})))

(:add-rule p (list "double_squote" (p-e {''})))

(:add-rule p (list "squote" (p-e {'})))

(:add-rule p (list "comment" (p-e {;.*$})
                   do-commennt))

;; TODO: need to include newline in $ match here?
(:add-rule p (list "eol" (p-e {[\n]*$})))

(:add-rule p (list "right_arrow" (p-e {->})))

(:add-rule p (list "mallet" (p-e {::=})))

(:add-rule p (list "boolean" (p-or '("true" "false"))))

(:add-rule p (list "true" (p-e {(?i:true|on)})
                   do-true))

(:add-rule p (list "false" (p-e {(?i:false|off)})
                   do-false))

(:add-rule p (list "equal" (p-e {=})))

(:add-rule p (list "or" (p-e {|})))

(:add-rule p (list "not" (p-e {!})))

(:add-rule p (list "and" (p-e {&})))

(:add-rule p (list "question" (p-e {?})))

(:add-rule p (list "star" (p-e {\*})))

(:add-rule p (list "plus" (p-e {+})))

(:add-rule p (list "close" (p-e {)})))

(:add-rule p (list "open" (p-e {(})))

(:add-rule p (list "dot" (p-e {\.})))
