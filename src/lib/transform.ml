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

(** This module is the {b main entry point} to use bracetax as a library. *)

(** The functions here are used to transform {i Bracetax} input to the
    formats currently handled by the library (XHTML, LaTeX, or a
    bracetax table of contents). *)


open Signatures

(**/**)
let opt_may ~f = function None -> () | Some o -> f o
(**/**)


(** Transformation from {i Bracetax} to HTML (actually {i almost}
    XHTML).  Most optional parameters correspond to command line
    options of [brtx -html].

    @param writer The {!type:Signatures.writer} to use for output.

    @param doc If [true], output a whole document with header and
    footer (Default: [false])).

    @param title If [doc = true], use [title] as HTML header title.

    @param css_link Add a "meta" link to a CSS URL.

    @param print_comments If [true], output Bracetax comments as HTML
    comments (Default: [false]).

    @param filename Filename used in error messages (Default: ["<IN>"]))

    @param class_hook Add a {v class="class_hook" v} to all tags in
    the HTML (it is the [-css-hook] option of [brtx]).

    @param img_hook A "hook" function to transform each image path.

    @param url_hook A "hook" function to transform each URL.

    @param input_char The "input" function.

    @param separate_header If this reference is provided, after the
    call to [brtx_to_html], this reference will contain the {i title},
    {i authors}, and {i subtitle} fields; and they won't be output to
    the [writer].

    @param deny_bypass If [true], treat [\{bypass\}] commands as
    [\{code\}] (Default: [false]).

*)
let brtx_to_html ~writer 
    ?make_section_links ?(doc=false) ?title ?css_link ?(print_comments=false)
    ?(filename="<IN>") ?class_hook ?img_hook ?url_hook ~input_char
    ?separate_header ?(deny_bypass=false) () = 

  if doc then (
    writer.w_write (HtmlPrinter.header
                      ~comment:"Generated with BraceTax" ?title
                      ?stylesheet_link:css_link ());
  );
  let printer =
    HtmlPrinter.build 
      ?make_section_links ?class_hook ?img_hook ?url_hook ?separate_header
      ~writer ~print_comments () in

  Parser.do_transformation ~deny_bypass printer input_char filename;
  
  if doc then writer.w_write (HtmlPrinter.footer ());
  ()

   

(** Transform from {i Bracetax} to LaTeX. Most parameters have the
    same meaning as for {!val:brtx_to_html}, except the following ones.

    @param title The [title] will be used for the PDF meta-data (for
    [doc = true]).

    @param use_package Add a [\\usepackage\{<package>\}] call in the
    LaTeX header (doc = true).

    @param href_is_footnote If [true], render links as footnotes
    (Default: [false]).

*)
let brtx_to_latex ~writer ?(doc=false) ?title  ?use_package ?(deny_bypass=false)
    ?(print_comments=false) ?(href_is_footnote=false) ?img_hook ?url_hook
    ?separate_header ?table_caption_after
    ?(filename="<IN>") ~input_char () = 

  if doc then (
    writer.w_write (LatexPrinter.header
                      ~comment:"Generated with BraceTax" ?title
                      ?stylesheet_link:use_package ());
  );
  let printer =
    LatexPrinter.build ~writer ~print_comments ~href_is_footnote 
      ?table_caption_after ?separate_header ?img_hook ?url_hook () in

  Parser.do_transformation ~deny_bypass printer input_char filename;

  if doc then (writer.w_write (LatexPrinter.footer ()));
  ()

(** Retrieve a table of contents from a {i Bracetax} input. The TOC is
    itself in {i Bracetax}; it is a set of lists with links. *)
let get_TOC ~writer ?(filename="<IN>") 
    ?make_links ?list_type ?numbering ~input_char () =
  let output_funs = TOCOutput.create ?list_type ?numbering ?make_links () in
  let printer = GenericPrinter.build ~writer ~output_funs () in
  Parser.do_transformation printer input_char filename;
  ()


(** Create a [writer] and a [input_char] from a [string] and two
    [Buffer.t].  With [let w, ic = (string_io input_str output_buffer
    error_buffer)], the writer [w] will write to [output_buffer],
    transform errors to strings, and output them to [error_buffer]; [ic]
    will take characters from [input_str].  [w] and [ic] can then be used
    with {!val:brtx_to_html} and {!val:brtx_to_latex}. *)
let string_io in_string out_buf err_buf =
  let error = function
    | `undefined s ->
        Buffer.add_string err_buf (s ^ "\n");
    | `message ((_, gravity, _) as msg) ->
        Buffer.add_string err_buf ((Error.to_string msg) ^ "\n"); in
  let write = Buffer.add_string out_buf in
  let read_char_opt =
    let cpt = ref (-1) in
    (fun () ->
       try Some (incr cpt; in_string.[!cpt]) with e -> None) in
  let writer = Signatures.make_writer ~write  ~error in
  (writer, read_char_opt)


(** This function is a convenience replacement for {!val:brtx_to_html},
to transform a [string]
into another [string] together with a list of {!type:Error.error}s. *)
let str_to_html
    ?make_section_links ?(doc=false) ?title ?css_link ?(print_comments=false)
    ?(filename="<IN>") ?class_hook ?img_hook ?url_hook
    ?separate_header ?(deny_bypass=false) in_str = 

  let errors = ref [] in
  let error = fun e -> errors := e :: !errors in
  let out_buf = Buffer.create 42 in
  let write = Buffer.add_string out_buf in
  let input_char =
    let cpt = ref (-1) in
    (fun () -> try Some (incr cpt; in_str.[!cpt]) with e -> None) in
  let writer = Signatures.make_writer ~write  ~error in
  
  if doc then (
    writer.w_write
      (HtmlPrinter.header ~comment:"Generated with BraceTax" ?title
         ?stylesheet_link:css_link ());
  );
  let printer =
    HtmlPrinter.build ?class_hook ?img_hook ?url_hook ?separate_header
      ?make_section_links
      ~writer ~print_comments () in
  Parser.do_transformation ~deny_bypass printer input_char filename;
  
  if doc then (
    writer.w_write (HtmlPrinter.footer ());
  );
  (Buffer.contents out_buf, List.rev !errors)


(** This function is a convenience replacement for
{!val:brtx_to_latex}, to transform a [string] into another [string]
together with a list of {!type:Error.error}s. *)
let str_to_latex  ?(doc=false) ?title  ?use_package ?(deny_bypass=false)
    ?(print_comments=false) ?(href_is_footnote=false) ?img_hook ?url_hook
    ?table_caption_after
    ?separate_header ?(filename="<IN>") in_str = 
  let errors = ref [] in
  let error = fun e -> errors := e :: !errors in
  let out_buf = Buffer.create 42 in
  let write = Buffer.add_string out_buf in
  let input_char =
    let cpt = ref (-1) in
    (fun () -> try Some (incr cpt; in_str.[!cpt]) with e -> None) in
  let writer = Signatures.make_writer ~write  ~error in
  
  if doc then
    writer.w_write
      (LatexPrinter.header
         ~comment:"Generated with BraceTax" ?title
         ?stylesheet_link:use_package ());
  
  let printer =
    LatexPrinter.build
      ~writer ~print_comments ~href_is_footnote ?table_caption_after
      ?separate_header ?img_hook ?url_hook () in
  Parser.do_transformation ~deny_bypass printer input_char filename;
  
  if doc then (
    writer.w_write (LatexPrinter.footer ());
  );
  (Buffer.contents out_buf, List.rev !errors)

(** This function is a convenience replacement for
{!val:get_TOC}, to get the table of contents from a [string]
into another [string]
together with a list of {!type:Error.error}s. *)
let str_to_TOC ?make_links ?list_type ?numbering
    ?(filename="<IN>") in_str =
  let errors = ref [] in
  let error = fun e -> errors := e :: !errors in
  let out_buf = Buffer.create 42 in
  let write = Buffer.add_string out_buf in
  let input_char =
    let cpt = ref (-1) in
    (fun () -> try Some (incr cpt; in_str.[!cpt]) with e -> None) in
  let writer = Signatures.make_writer ~write  ~error in
  let output_funs = TOCOutput.create ?list_type ?numbering ?make_links () in
  let printer = GenericPrinter.build ~writer ~output_funs () in
  Parser.do_transformation printer input_char filename;
  (Buffer.contents out_buf, List.rev !errors)




