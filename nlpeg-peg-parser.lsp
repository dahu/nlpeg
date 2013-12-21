(load "./nlpeg.lsp")
(context 'NLPEG-PEG-PARSER)

(setf p-opt '(("start-rule" "line") ("skip-white" true)))
(setf p (NLPEG-Parser p-opt))

(:add-rule p (list "line" (NLPEG-Sequence '("comment" "option" "definiton")) do-line))

(:add-rule p (list "definition"
                   (NLPEG-Sequence (list "identifier" "mallet" "expression"
                                     (NLPEG-Maybe-one "callback")
                                     (NLPEG-Maybe-one "comment")
                                     "eol")) do-definition))

(:add-rule p (list xxx (NLPEG-Sequence '('sequence', s:p.maybe_many(s:p.and(['or', 'sequence']))],
      \{'id': 'expression', 'on_match': s:SID().'Expression'})
call s:p.maybe_many('prefix',
      \{'id': 'sequence', 'on_match': s:SID().'Sequence'})
(:add-rule p (list xxx (NLPEG-Sequence '(s:p.maybe_one(s:p.or(['and', 'not'])), 'suffix'],
      \{'id': 'prefix', 'on_match': s:SID().'Prefix'})
(:add-rule p (list xxx (NLPEG-Sequence '('primary', s:p.maybe_one(s:p.or(['question', 'star', 'plus']))],
      \{'id': 'suffix', 'on_match': s:SID().'Suffix'})
call s:p.or([s:p.and(['identifier', s:p.not_has('mallet')]), s:p.and(['open', 'expression', 'close']), 'regex'],
      \{'id': 'primary', 'on_match': s:SID().'Primary'})
(:add-rule p (list xxx (NLPEG-Sequence '('right_arrow', 'callback_identifier'],
      \{'id': 'callback', 'on_match': s:SID().'Callback'})
(:add-rule p (list xxx (NLPEG-Sequence '('dot', 'option_name', 'equal', 'option_value', s:p.maybe_one('comment'), 'eol'],
      \{'id': 'option', 'on_match': s:SID().'Option'})
(:add-rule p (list xxx (NLPEG-Sequence '('identifier'],
      \{'id': 'option_name'})
call s:p.or(['squoted_string', 'dquoted_string', 'boolean', 'number'],
      \{'id': 'option_value', 'on_match': s:SID().'Option_value'})
call s:p.e('[[:alnum:]#._:]\+',
      \{'id': 'callback_identifier', 'on_match': s:SID().'Identifier'})
call s:p.e('\h\w*',
      \{'id': 'identifier', 'on_match': s:SID().'Identifier'})
call s:p.or(['dquoted_string', 'squoted_string'],
      \{'id': 'regex', 'on_match': s:SID().'Regex'})
(:add-rule p (list xxx (NLPEG-Sequence '('dquote', s:p.maybe_many(s:p.or(['double_backslash', 'escaped_dquote', s:p.e('[^"]')])), 'dquote'],
      \{'id': 'dquoted_string', 'on_match': s:SID().'Dquoted_string'})
(:add-rule p (list xxx (NLPEG-Sequence '('squote', s:p.maybe_many(s:p.or([s:p.e("[^']"), 'double_squote'])), 'squote'],
      \{'id': 'squoted_string', 'on_match': s:SID().'Squoted_string'})
(:add-rule p (list xxx (NLPEG-Sequence '('backslash', 'dquote'],
      \{'id': 'escaped_dquote'})
(:add-rule p (list xxx (NLPEG-Sequence '('backslash', 'backslash'],
      \{'id': 'double_backslash'})
call s:p.e('\',
      \{'id': 'backslash'})
call s:p.e('\%(0\|[1-9]\d*\)\%(\.\d\+\)\?',
      \{'id': 'number'})
call s:p.e('"',
      \{'id': 'dquote'})
call s:p.e('''''',
      \{'id': 'double_squote'})
call s:p.e("'",
      \{'id': 'squote'})
call s:p.e(';.*$',
      \{'id': 'comment', 'on_match': s:SID().'Comment'})
call s:p.e('\_$',
      \{'id': 'eol'})
call s:p.e('->',
      \{'id': 'right_arrow'})
call s:p.e('::=',
      \{'id': 'mallet'})
call s:p.or(['true', 'false'],
      \{'id': 'boolean'})
call s:p.e('\ctrue\|on',
      \{'id': 'true', 'on_match': s:SID().'True'})
call s:p.e('\cfalse\|off',
      \{'id': 'false', 'on_match': s:SID().'False'})
call s:p.e('=',
      \{'id': 'equal'})
call s:p.e('|',
      \{'id': 'or'})
call s:p.e('!',
      \{'id': 'not'})
call s:p.e('&',
      \{'id': 'and'})
call s:p.e('?',
      \{'id': 'question'})
call s:p.e('\*',
      \{'id': 'star'})
call s:p.e('+',
      \{'id': 'plus'})
call s:p.e(')',
      \{'id': 'close'})
call s:p.e('(',
      \{'id': 'open'})
call s:p.e('\.',
      \{'id': 'dot'})
" }}}
