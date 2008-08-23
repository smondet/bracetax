
:syntax clear

" Classic VIM-ist tags:
:syn keyword bracetaxTodo contained TODO FIXME XXX

" Comments from conf.vim:
:syn match bracetaxComment "\({\)\@<!#.*$" contains=bracetaxTodo

" Commands TODO
" :syn match bracetaxOpenBrace "{"
" :syn match bracetaxCloseBrace "}"
:syn match bracetaxCommand "{\(i\|b\|t\|sup\|sub\):"ms=s+1
" :syn match bracetaxCommand +{\w*\++ contained

" Verbatim regions
:syn region bracetaxVerbatim start="\({verbatim{}.*\)\@<=" end="\(^{endverbatim}\)\@<="
:syn region bracetaxVerbatim start="{verbatim}"rs=e+1 end="^{endverbatim}"re=s-1
:syn region bracetaxVerbatim start="\({verbatim{\z(\w\+\)}.*$\)\@<=" end="\(^{\z1}\)\@="


:hi def link bracetaxComment Comment
:hi def link bracetaxOpenBrace Keyword 
:hi def link bracetaxCloseBrace Keyword 
:hi def link bracetaxCommand  Type
:hi def link bracetaxVerbatim SpecialComment

:let b:current_syntax = "bracetax"
