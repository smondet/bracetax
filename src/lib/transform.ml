(******************************************************************************)
(*      Copyright (c) 2009, Sebastien MONDET                                  *)
(*                                                                            *)
(*      Permission is hereby granted, free of charge, to any person           *)
(*      obtaining a copy of this software and associated documentation        *)
(*      files (the "Software"), to deal in the Software without               *)
(*      restriction, including without limitation the rights to use,          *)
(*      copy, modify, merge, publish, distribute, sublicense, and/or sell     *)
(*      copies of the Software, and to permit persons to whom the             *)
(*      Software is furnished to do so, subject to the following              *)
(*      conditions:                                                           *)
(*                                                                            *)
(*      The above copyright notice and this permission notice shall be        *)
(*      included in all copies or substantial portions of the Software.       *)
(*                                                                            *)
(*      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*      EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*      OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*      NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT           *)
(*      HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,          *)
(*      WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING          *)
(*      FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR         *)
(*      OTHER DEALINGS IN THE SOFTWARE.                                       *)
(******************************************************************************)

open Signatures

let opt_may ~f = function None -> () | Some o -> f o

let brtx_to_html ~writer ?(doc=false) ?title ?css_link ?(print_comments=false)
?(filename="<IN>") ?class_hook ?img_hook ?url_hook ~input_char
?(deny_bypass=false) () = (

    if doc then (
        writer.w_write (HtmlPrinter.header
            ~comment:"Generated with BraceTax" ?title
            ?stylesheet_link:css_link ()
        );
    );
    let printer =
        HtmlPrinter.build ?class_hook ?img_hook ?url_hook
            ~writer ~print_comments () in
    Parser.do_transformation ~deny_bypass printer input_char filename;
    
    if doc then (
        writer.w_write (HtmlPrinter.footer ());
    );

)


let brtx_to_latex ~writer ?(doc=false) ?title  ?use_package ?(deny_bypass=false)
    ?(print_comments=false) ?(href_is_footnote=false) ?img_hook ?url_hook
    ?(filename="<IN>") ~input_char () = (

    if doc then (
        writer.w_write (LatexPrinter.header
            ~comment:"Generated with BraceTax" ?title
            ?stylesheet_link:use_package ()
        );
    );
    let printer =
        LatexPrinter.build
            ~writer ~print_comments ~href_is_footnote ?img_hook ?url_hook () in
    Parser.do_transformation ~deny_bypass printer input_char filename;
    
    if doc then (
        writer.w_write (LatexPrinter.footer ());
    );

)

let get_TOC ~writer ?(filename="<IN>") ~input_char () = (
    let output_funs = TOCOutput.create () in
    let printer = GenericPrinter.build ~writer ~output_funs () in
    Parser.do_transformation printer input_char filename;
)




