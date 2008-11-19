
:syntax clear

" Classic VIM-ist tags:
:syn keyword bracetaxTodo contained TODO FIXME XXX

" Comments from conf.vim:
:syn match bracetaxComment "\({\)\@<!#.*$" contains=bracetaxTodo

" Commands
:syn match bracetaxSpecialChar "{"
:syn match bracetaxSpecialChar "}"
:syn match bracetaxSpecialChar "|"
:syn match bracetaxSpecialChar "{begin"
:syn match bracetaxSpecialChar "{end}"

" :syn match bracetaxCommand "{\_.\{-}\(|\|}\)" contains=bracetaxSpecialChar,bracetaxKnown
:syn region bracetaxCommand keepend start="{" skip="\\|\|\\}" end="\(|\|}\)" contains=bracetaxSpecialChar,bracetaxKnown

:syn match bracetaxKnown "\({\|{begin\s\)\@<=\<\(c\|q\|i\|b\|t\|sup\|sub\|section\|link\|list\|utf\|image\|table\|header\|title\|subtitle\|authors\|note\)\>"
:syn match bracetaxSimple "{\(p\|br\|#\|{\|}\|\~\|\*\)}" contains=bracetaxSpecialChar

" Verbatim regions
" :syn region bracetaxVerbatim start="\(^{verbatim{}\_.*$\)\@<=" end="^\({endverbatim}\)\@="
:syn region bracetaxVerbatim start="^{verbatim}" end="^{endverbatim}"
:syn region bracetaxVerbatim start="^{verbatim \z(\w\+\)" end="^{\z1}"

" Setting colorization:

:hi def link bracetaxComment Comment
:hi def link bracetaxSpecialChar Keyword
:hi def link bracetaxSimple Identifier 
:hi def link bracetaxKnown Identifier
:hi def link bracetaxCommand  PreProc
:hi def link bracetaxVerbatim SpecialComment


:let b:current_syntax = "bracetax"
