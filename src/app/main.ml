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

let version_string = ~% "0.2 (ocamlbracetax lib: %s)" Bracetax.Info.version


(* Used temporary to force intermodules dependency *)
module PostPro = PostProcessor

module Options = struct

    (* let arg_set ref v = Arg.Unit (fun () -> ref := v) *)

    type action_to_do = [
        | `Brtx2HTML | `Brtx2LaTeX
        | `PostPro of PostProcessor.BuiltIn.available list
        | `GetTOC
    ]
    let todo = ref (`Brtx2HTML:action_to_do)
    let input_stream = ref stdin
    let output_stream = ref stdout
    let debug = ref false
    let header_footer = ref None
    let stylesheet_link = ref None
    let print_version = ref false
    let print_license = ref false
    let plugins = ref []

    let options = Arg.align [
        ("-html", Arg.Unit (fun () -> todo := `Brtx2HTML),
            ~% " Output HTML format (default)");
        ("-latex", Arg.Unit (fun () -> todo := `Brtx2LaTeX),
            ~% " Output LaTeX format");
        ("-toc", Arg.Unit (fun () -> todo := `GetTOC),
            ~% " Get the table of contents");
        ("-postpro", Arg.Unit (fun () -> todo := `PostPro []),
            ~% " Do post-processing");
        ("-debug", Arg.Set debug, " Debug mode");
        ("-doc",
            Arg.String (fun s -> header_footer := Some s),
            "<title> Output a complete document and use <title>");
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

type in_out = {
    file_in: string option;
    file_out: string option;
}
let meta_open_inout io = (
    let i, fi =
        match io.file_in with
        | None -> (stdin, "<STDIN>")
        | Some "-" -> (stdin, "<STDIN>")
        | Some f -> (open_in f, f) in
    let o = 
        match io.file_out with
        | None -> stdout
        | Some "-" -> stdout
        | Some f -> open_out f in
    Sys.catch_break true;
    at_exit (fun () -> 
        close_in i;
        flush o;
        close_out o;
    );
    let write = output_string o in
    let read_char_opt i () = try Some (input_char i) with e -> None in
    (read_char_opt i, fi, write)
)


module CommandLine = struct


    type todo = [
        | `PrintVersion
        | `PrintLicense
        | `Help of [ `main | `html | `latex | `toc ]
        | `Brtx2HTML of
            in_out * bool * string option * string option * bool
                (* inout, doc, title, css, print_comments *)
        | `Brtx2Latex of
            in_out * bool * string option * string option * bool
                (* inout, doc, title, package, print_comments *)
        | `GetTOC of in_out
    ]
    let parse () = (
        let f_in = ref None in
        let f_out = ref None in
        let is_doc = ref false in
        let title = ref None in
        let print_comments = ref false in
        let link_css = ref None in 
        let ltx_package = ref None in 
        let todo = ref `html in

        let options = Arg.align [
            ("-version", Arg.Unit (fun () -> todo := `version),
                " Print version and exit");
            ("-license", Arg.Unit (fun () -> todo := `license), 
                " Print license and exit");
            ("-html", Arg.Unit (fun () -> todo := `html),
                ~% " Output HTML format (default)");
            ("-latex", Arg.Unit (fun () -> todo := `latex),
                ~% " Output LaTeX format");
            ("-toc", Arg.Unit (fun () -> todo := `toc),
                ~% " Get the table of contents");
            ("-i",
            Arg.String (fun s -> f_in := Some s), 
                "<file> input file (default or \"-\" is standard input)");
            ("-o",
                Arg.String (fun s -> f_out := Some s), 
                "<file> output file (default or \"-\" is standard output)");
            ("-doc",
                Arg.Unit (fun () -> is_doc := true),
                " Output a complete document");
            ("-title",
                Arg.String (fun s -> title := Some s),
                "<url> Set the title of the document (<head><title> for XHTML,\
                PDF meta-data for LaTeX), requires -doc");
            ("-link-css",
                Arg.String (fun s -> link_css := Some s),
                "<url> link to a CSS (for XHTML), requires -doc");
            ("-use-package",
                Arg.String (fun s -> ltx_package := Some s),
                "<url> use a given package (for LaTeX), requires -doc");
            ("-print-comments",
                Arg.Unit (fun () -> print_comments := true),
                " activate the transmission of brtx comments to the output's \
                comments (-html or -latex)");
        ] in
        let short_usage =
            ~% "usage: %s [-i file] [-o file] [-help]" Sys.argv.(0) in
        let anon_fun s =
            prerr_string (~% "Warning argument %s is ignored\n" s) in
        Arg.parse options anon_fun short_usage;
        match !todo with
        | `version -> `PrintVersion
        | `license -> `PrintLicense
        | `html ->
            `Brtx2HTML ({file_in = !f_in; file_out = !f_out}, !is_doc, !title,
                !link_css, !print_comments)
        | `latex ->
            `Brtx2Latex ({file_in = !f_in; file_out = !f_out}, !is_doc, !title,
                !ltx_package, !print_comments)
        | `toc ->
            `GetTOC ({file_in = !f_in; file_out = !f_out})
    )
end

let () = (
    let return_value_to_shell = ref 0 in
    let to_do = CommandLine.parse () in
        let error = function
            | `undefined s ->
                return_value_to_shell := 2;
                prerr_string (s ^ "\n")
            | `message msg ->
                return_value_to_shell := 2;
                prerr_string ((Bracetax.Error.to_string msg) ^ "\n") in
    begin match to_do with
    | `PrintVersion ->
        p (~% "brtx %s\n" version_string);
        exit 0;
    | `PrintLicense ->
        p Bracetax.Info.license;
        exit 0;
    | `Brtx2HTML (io, doc, title, css_link, print_comments) ->
        let input_char, filename, write = meta_open_inout io in
        let writer = Bracetax.Signatures.make_writer ~write  ~error in
        Bracetax.Transform.brtx_to_html ~writer ~doc ?title ?css_link
            ~print_comments ~input_char ~filename ();
    | `Brtx2Latex (io, doc, title, use_package, print_comments) ->
        let input_char, filename, write = meta_open_inout io in
        let writer = Bracetax.Signatures.make_writer ~write  ~error in
        Bracetax.Transform.brtx_to_latex ~writer ~doc ?title ?use_package 
            ~print_comments ~input_char ~filename ();
    | `GetTOC io ->
        let input_char, filename, write = meta_open_inout io in
        let writer = Bracetax.Signatures.make_writer ~write  ~error in
        Bracetax.Transform.get_TOC ~writer ~input_char ();
    end;
    exit !return_value_to_shell;
)

