(******************************************************************************)
(*      Copyright (c) 2008, 2009, Sebastien MONDET                            *)
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

    type html_latex_common = {
        doc: bool; 
        title: string option;
        print_comments: bool;
        deny_bypass: bool;
    }

    type todo = [
        | `PrintVersion
        | `PrintLicense
        (* | `Help of [ `main | `html | `latex | `toc ] *)
        | `Brtx2HTML of
            in_out * html_latex_common * string option * string option
                (* inout, com, css, csshook *)
        | `Brtx2Latex of
            in_out * html_latex_common * string option
                (* inout, com, package *)
        | `GetTOC of in_out
    ]
    let parse () : todo = (
        let f_in = ref None in
        let f_out = ref None in
        let is_doc = ref false in
        let title = ref None in
        let print_comments = ref false in
        let deny_bypass = ref false in
        let link_css = ref None in 
        let css_hook = ref None in
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
                "<text> Set the title of the document ('head.title' for XHTML, \
                PDF meta-data for LaTeX), requires -doc");
            ("-link-css",
                Arg.String (fun s -> link_css := Some s),
                "<url> link to a CSS, requires -html,-doc");
            ("-css-hook",
                Arg.String (fun s -> css_hook := Some s),
                "<text> add a class=\"text...\" to all tags, requires -html");
            ("-use-package",
                Arg.String (fun s -> ltx_package := Some s),
                "<name> use a given package, requires -latex,-doc");
            ("-print-comments",
                Arg.Unit (fun () -> print_comments := true),
                " activate the transmission of brtx comments to the output's \
                comments (-html or -latex)");
            ("-deny-bypass",
                Arg.Unit (fun () -> deny_bypass := true),
                " treat all {bypass} as {code} \
                (security of interpreted webapps)");
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
            `Brtx2HTML (
                {file_in = !f_in; file_out = !f_out},
                {doc = !is_doc; title = !title; 
                    print_comments = !print_comments;
                    deny_bypass = !deny_bypass},
                !link_css, !css_hook)
        | `latex ->
            `Brtx2Latex (
                {file_in = !f_in; file_out = !f_out},
                {doc = !is_doc; title = !title; 
                    print_comments = !print_comments;
                    deny_bypass = !deny_bypass},
                !ltx_package)
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
    | `Brtx2HTML (io, common, css_link, class_hook) ->
        let input_char, filename, write = meta_open_inout io in
        let {CommandLine.doc = doc; title = title; deny_bypass = deny_bypass;
            print_comments = print_comments;} = common in
        let writer = Bracetax.Signatures.make_writer ~write  ~error in
        Bracetax.Transform.brtx_to_html ~writer ~doc ?title ?css_link ~deny_bypass
            ~print_comments ~input_char ~filename ?class_hook ();
    | `Brtx2Latex (io, common, use_package) ->
        let input_char, filename, write = meta_open_inout io in
        let {CommandLine.doc = doc; title = title; deny_bypass = deny_bypass;
            print_comments = print_comments;} = common in
        let writer = Bracetax.Signatures.make_writer ~write  ~error in
        Bracetax.Transform.brtx_to_latex ~writer ~doc ?title ?use_package 
            ~print_comments ~input_char ~filename ~deny_bypass ();
    | `GetTOC io ->
        let input_char, filename, write = meta_open_inout io in
        let writer = Bracetax.Signatures.make_writer ~write  ~error in
        Bracetax.Transform.get_TOC ~writer ~input_char ();
    end;
    exit !return_value_to_shell;
)

