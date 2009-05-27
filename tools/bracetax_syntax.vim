" Copyright (c) 2008, 2009 Sebastien MONDET                             
"                                                                  
" Permission is hereby granted, free of charge, to any person      
" obtaining a copy of this software and associated documentation   
" files (the 'Software'), to deal in the Software without          
" restriction, including without limitation the rights to use,     
" copy, modify, merge, publish, distribute, sublicense, and/or sell
" copies of the Software, and to permit persons to whom the        
" Software is furnished to do so, subject to the following         
" conditions:                                                      
"                                                                  
" The above copyright notice and this permission notice shall be   
" included in all copies or substantial portions of the Software.  
"                                                                  
" THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,  
" EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES  
" OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND         
" NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT      
" HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,     
" WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING     
" FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    
" OTHER DEALINGS IN THE SOFTWARE.                                  
"

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
:syn match bracetaxSimple "{\(p\|br\|#\|{\|}\|\~\|\*\|\.\.\.\)}" contains=bracetaxSpecialChar

" Verbatim regions
:syn region bracetaxVerbatim start="{\(code\|bypass\)}" end="{end}"
:syn region bracetaxVerbatim start="{\(code\|bypass\) \z(\w\+\)" end="{\z1}"

" Setting colorization:

:hi def link bracetaxComment Comment
:hi def link bracetaxSpecialChar Keyword
:hi def link bracetaxSimple Identifier 
:hi def link bracetaxKnown Identifier
:hi def link bracetaxCommand  PreProc
:hi def link bracetaxVerbatim SpecialComment


:let b:current_syntax = "bracetax"
