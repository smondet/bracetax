(******************************************************************************)
(*      Copyright (c) 2008, Sebastien MONDET                                  *)
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

let (~%) = Printf.sprintf
let p = print_string

let version_string = ~% "0.1 (ocamlbracetax lib: %s)" Bracetax.Info.version

module DummyPrinter = struct
    type t = int

    let create ~writer = 0

    let strstat s =
        (~% "[%d:%d]" s.Bracetax.Signatures.s_line s.Bracetax.Signatures.s_char)
    let head = "####"
    
    let handle_comment_line t location line =
        p (~% "%s%s[comment] \"%s\"\n" head (strstat location) line)
    let handle_text t location line =
        p (~% "%s%s[text] \"%s\"\n" head (strstat location) line)

    let start_command t location name args =
        p (~% "%s%s[start %s(%s)]\n" head (strstat location)
            name (String.concat ", " args))

    let stop_command t location = 
        p (~% "%s%s[stop]\n" head (strstat location))
        
    let terminate t location = 
        p (~% "%s%s[This is the end...]\n" head (strstat location))

    let handle_verbatim_line t location line =
        p (~% "%s%s[verbatim] %s\n" head (strstat location) line)

    let enter_verbatim t location args = ()
    let exit_verbatim t location = ()

    let dummy_writer = {
        Bracetax.Signatures.w_write = (fun s -> ());
        Bracetax.Signatures.w_warn = (fun s -> ());
        Bracetax.Signatures.w_error = (fun s -> ());
    }

end

(* Used temporary to force intermodules dependency *)
module PostPro = PostProcessor

module Options = struct

    (* let arg_set ref v = Arg.Unit (fun () -> ref := v) *)

    type action_to_do = [
        | `Brtx2HTML | `Brtx2LaTeX
        | `PostPro of PostProcessor.BuiltIn.available list
    ]
    let todo = ref (`Brtx2HTML:action_to_do)
    let input_stream = ref stdin
    let output_stream = ref stdout
    let debug = ref false
    let header_footer = ref false
    let stylesheet_link = ref None
    let print_version = ref false
    let print_license = ref false
    let plugins = ref []

    let options = Arg.align [
        ("-html", Arg.Unit (fun () -> todo := `Brtx2HTML),
            ~% " Output HTML format (default)");
        ("-latex", Arg.Unit (fun () -> todo := `Brtx2LaTeX),
            ~% " Output LaTeX format");
        ("-postpro", Arg.Unit (fun () -> todo := `PostPro []),
            ~% " Do post-processing");
        ("-debug", Arg.Set debug, " Debug mode");
        ("-doc", Arg.Set header_footer, " Output a complete document");
        ("-version", Arg.Set print_version, " Print version and exit");
        ("-license", Arg.Set print_license, " Print license and exit");
        (
            "-i",
            Arg.String (fun s -> input_stream := open_in s), 
            "<file> input file (default is standard input)"
        );
        (
            "-o",
            Arg.String (fun s -> output_stream := open_out s), 
            "<file> output file (default is standard output)"
        );
        (
            "-link-stylesheet",
            Arg.String (fun s -> stylesheet_link := Some s),
            "<url> link to a stylesheet (CSS for XHTML), needs -doc"
        );
        (
            "-pp", Arg.String (fun s -> plugins := s :: !plugins),
            "<name> Add postpro plugin (try `brtx -postpro -pp help')"
        );
    ]
    let short_usage =
        ~% "usage: %s [-i file] [-o file] [-help]" Sys.argv.(0)
    let anon_fun s =
        prerr_string (~% "Warning argument %s is ignored\n" s)

    let get () = (
        Arg.parse options anon_fun short_usage;
        let sub s ofs siz  = String.sub s ofs siz in
        let lgth s = String.length s in
        let rec parse_plugins acc = function
            | [] -> acc
            | "help" :: q ->
                p "Post-processing helper:

Available plugins:\n\
-------------------------------------------------------------------------------
| -pp option        |  tag   | Help                                           |
|-----------------------------------------------------------------------------|
|-----------------------------------------------------------------------------|
| inlinelatex       | latex  | Will treat content as pure LaTeX               |
|                   |        | (input is LaTeX ouput of -latex)               |
|-----------------------------------------------------------------------------|
| inlinehtml        | html   | Will unsanitize content to rebuild             |
|                   |        | HTML (input is HTML)                           |
|-----------------------------------------------------------------------------|
| latex2html:<cmd>  | latex  | Will unsanitize HTML input and pass it         |
|                   |        | through <cmd> shell command                    |
|                   |        | example: -pp latex2html:hevea                  |
-------------------------------------------------------------------------------
Example:
    brtx -postpro -pp latex2html:hevea -i output.html -o output_and_pp.html
will catch: 
    <!--verbatimbegin:latex -->
    <pre>
    \\begin{itemize}
      \\item {\\em some emph} \\&amp; \\textbf{bold}
      \\item dfkljg $I_x (\\theta) = \\theta^2$
    \\end{itemize}
    </pre>
    <!--verbatimend:latex -->
and give (note the unsanitization: &amp; -> &):
    \\begin{itemize}
      \\item {\\em some emph} \\& \\textbf{bold}
      \\item dfkljg $I_x (\\theta) = \\theta^2$
    \\end{itemize}
as standard input to hevea (http://hevea.inria.fr),
and replace the catched text with its standard output:
    <UL CLASS=\"itemize\">
    <LI CLASS=\"li-itemize\"><EM>jkfg</EM> &amp; <B>bold</B></LI>
    <LI CLASS=\"li-itemize\">
        dfkljg <I>I</I><SUB><I>x</I></SUB> (&#X3B8;) = &#X3B8;<SUP>2</SUP>
    </LI>
    </UL>

";
                parse_plugins acc q
            | "inlinelatex" :: q ->
                parse_plugins (`inline_latex :: acc) q
            | "inlinehtml" :: q ->
                parse_plugins (`inline_html :: acc) q
            | t :: q when sub t 0 11 = "latex2html:" ->
                let filter = sub t 11 (lgth t - 11) in
                let plugin =
                    `shell ( 
                        (`HTML, `LaTeX),
                        "latex", ~% "echo '<!-- begin %s -->'" filter,
                        filter, ~% "echo '<!-- end %s -->'" filter)
                in
                parse_plugins (plugin :: acc) q
            | t :: q ->
                p (~% "WARNING: Unknown plugin name: \"%s\"\n" t);
                parse_plugins acc q
        in
        if !todo = `PostPro [] then (
            todo := `PostPro (parse_plugins [] !plugins);
        );
        if !print_version then
            `print_version
        else
            if !print_license then
                `print_license
            else
                (`process
                    (!todo, !debug, !input_stream, !output_stream))
    )

end

let read_line_opt i () = try Some (input_line i) with e -> None


let () = (
    let to_do, dbg, i, o =
        match Options.get () with 
        | `process something -> something
        | `print_version ->
            p (~% "brtx %s\n" version_string);
            exit 0;
        | `print_license ->
            p Bracetax.Info.license;
            exit 0;
    in
    Sys.catch_break true;
    at_exit (fun () -> 
        close_in i;
        flush o;
        close_out o;
    );
    if dbg then (
        let module DummyTransformer = Bracetax.Transformer.Make(DummyPrinter) in
        let t =
            DummyTransformer.create
                ~writer:DummyPrinter.dummy_writer ~read:(read_line_opt i)
        in
        DummyTransformer.do_transformation t;
        p (~% "DEBUG Done;\n");
    ) else (
        let module HtmlTransformer =
            Bracetax.Transformer.Make(Bracetax.HtmlPrinter) in
        let write = output_string o in
        let writer =
            let warn = prerr_string in
            let error = prerr_string in
            Bracetax.Signatures.make_writer ~write ~warn ~error in
        let read = read_line_opt i in
        begin match to_do with
        | `Brtx2HTML ->
                let t = HtmlTransformer.create ~writer ~read in
                if !Options.header_footer then
                    write (Bracetax.HtmlPrinter.header
                        ~comment:"Generated with BraceTax" ()
                        ?stylesheet_link:!Options.stylesheet_link
                    );
                HtmlTransformer.do_transformation t;
                if !Options.header_footer then
                    write (Bracetax.HtmlPrinter.footer ());
        | `Brtx2LaTeX ->
                let module LatexTransformer =
                    Bracetax.Transformer.Make(Bracetax.LatexPrinter) in
                let t = LatexTransformer.create ~writer ~read in
                if !Options.header_footer then
                    write (Bracetax.LatexPrinter.header
                        ~comment:"Generated with BraceTax" ()
                        ?stylesheet_link:!Options.stylesheet_link
                    );
                LatexTransformer.do_transformation t;
                if !Options.header_footer then
                    write (Bracetax.LatexPrinter.footer ());
        | `PostPro (t :: q as l) ->
            PostProcessor.process (PostProcessor.BuiltIn.make_list l)
                read (Printf.fprintf o "%s\n")
        | `PostPro [] -> ()
        end;
    );
)

