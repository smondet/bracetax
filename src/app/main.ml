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

    type io_common = {
        in_out: in_out;
        warn_error: bool; (* default to true *)
    }
    type html_latex_common = {
        doc: bool; 
        title: string option;
        print_comments: bool;
        deny_bypass: bool;
        ignore_header: bool;
    }

    type todo = [
        | `PrintVersion
        | `PrintLicense
        (* | `Help of [ `main | `html | `latex | `toc ] *)
        | `Brtx2HTML of
            io_common * html_latex_common * string option * string option
                (* inout, com, css, csshook *)
        | `Brtx2Latex of
            io_common * html_latex_common * string option * bool * bool
                (* inout, com, package, href_is_footnote, table_caption_after *)
        | `GetTOC of io_common
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
        let href_is_footnote = ref false in
        let todo = ref `html in
        let warn_error = ref true in
        let ignore_header = ref false in
        let table_caption_after = ref true in

        let options = Arg.align [
            ("-version", Arg.Unit (fun () -> todo := `version),
                " \n\tPrint version and exit");
            ("-license", Arg.Unit (fun () -> todo := `license), 
                " \n\tPrint license and exit");
            ("-html", Arg.Unit (fun () -> todo := `html),
                ~% " \n\tOutput HTML format (default)");
            ("-latex", Arg.Unit (fun () -> todo := `latex),
                ~% " \n\tOutput LaTeX format");
            ("-toc", Arg.Unit (fun () -> todo := `toc),
                ~% " \n\tGet the table of contents");
            ("-i",
            Arg.String (fun s -> f_in := Some s), 
                "<file> \n\tinput file (default or \"-\" is standard input)");
            ("-o",
                Arg.String (fun s -> f_out := Some s), 
                "<file> \n\toutput file (default or \"-\" is standard output)");
            ("-doc",
                Arg.Unit (fun () -> is_doc := true),
                " \n\tOutput a complete document");
            ("-title",
                Arg.String (fun s -> title := Some s),
                "<text> \n\tSet the title of the document ('head.title' for \
                 XHTML,\n\tPDF meta-data for LaTeX), requires -doc");
            ("-link-css",
                Arg.String (fun s -> link_css := Some s),
                "<url> \n\tlink to a CSS, requires -html,-doc");
            ("-css-hook",
                Arg.String (fun s -> css_hook := Some s),
                "<text> \n\tadd a class=\"text...\" to all tags, requires -html");
            ("-use-package",
                Arg.String (fun s -> ltx_package := Some s),
                "<name> \n\tuse a given package, requires -latex,-doc");
            ("-href-footnote",
                Arg.Unit (fun s -> href_is_footnote := true),
                " \n\ttreat links as LaTeX footnotes with URLs, requires -latex");
            ("-print-comments",
                Arg.Unit (fun () -> print_comments := true),
                " \n\tactivate the transmission of brtx comments to the output's \
                comments\n\t(-html or -latex)");
            ("-deny-bypass",
                Arg.Unit (fun () -> deny_bypass := true),
                " \n\ttreat all {bypass} as {code} \
                (security of interpreted webapps)");
            (   "-ignore-header",
                Arg.Unit (fun () -> ignore_header := true),
                " \n\tDo not process the {header| ... } part");
            ("-no-warn-error",
                Arg.Unit (fun () -> warn_error := false),
                " \n\tDo not treat warnings as errors (return 0 to shell/make/...)");
            ("-warn-error",
                Arg.Unit (fun () -> warn_error := true),
                " \n\tTreat warnings as errors (default, return 2 to shell)");
            ("-table-caption-before",
             Arg.Unit (fun () -> table_caption_after := false),
             " \n\tIn LaTeX output, put the captions before the tables\n\t\
             (like LNCS, IEEE, ...)");
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
                {in_out = {file_in = !f_in; file_out = !f_out};
                    warn_error = !warn_error},
                {doc = !is_doc; title = !title; 
                    print_comments = !print_comments;
                    deny_bypass = !deny_bypass;
                    ignore_header = !ignore_header},
                !link_css, !css_hook)
        | `latex ->
            `Brtx2Latex (
                {in_out = {file_in = !f_in; file_out = !f_out};
                    warn_error = !warn_error},
                {doc = !is_doc; title = !title; 
                    print_comments = !print_comments;
                    deny_bypass = !deny_bypass;
                    ignore_header = !ignore_header},
                !ltx_package, !href_is_footnote, !table_caption_after)
        | `toc ->
            `GetTOC (
                {in_out = {file_in = !f_in; file_out = !f_out};
                    warn_error = !warn_error})
    )
end

let () = (
    let module CL = CommandLine in
    let return_value_to_shell = ref 0 in
    let warn_error = ref true in
    let to_do = CommandLine.parse () in
    let error = function
        | `undefined s ->
            return_value_to_shell := 2;
            prerr_string (s ^ "\n")
        | `message ((_, gravity, _) as msg) ->
            if gravity <> `warning || (!warn_error && gravity = `warning)
            then (
                return_value_to_shell := 2;
            );
            prerr_string ((Bracetax.Error.to_string msg) ^ "\n") in
    begin match to_do with
    | `PrintVersion ->
        p (~% "brtx %s\n" version_string);
        exit 0;
    | `PrintLicense ->
        p Bracetax.Info.license;
        exit 0;
    | `Brtx2HTML (io, common, css_link, class_hook) ->
        let input_char, filename, write = meta_open_inout io.CL.in_out in
        warn_error := io.CL.warn_error;
        let {CommandLine.doc = doc; title = title;
             deny_bypass = deny_bypass;
             print_comments = print_comments;
             ignore_header = ignore_header} = common in
        let writer = Bracetax.Signatures.make_writer ~write  ~error in
        let separate_header = 
            if ignore_header then Some (ref ("", "", "")) else None in
        Bracetax.Transform.brtx_to_html ~writer ~doc ?title ?css_link
            ?separate_header
            ~deny_bypass ~print_comments ~input_char ~filename ?class_hook ();
    | `Brtx2Latex (io, common, use_package, href_is_footnote, table_caption_after) ->
        let input_char, filename, write = meta_open_inout io.CL.in_out in
        warn_error := io.CL.warn_error;
        let {CommandLine.doc = doc; title = title;
             deny_bypass = deny_bypass;
             print_comments = print_comments;
             ignore_header = ignore_header} = common in
        let writer = Bracetax.Signatures.make_writer ~write  ~error in
        let separate_header = 
            if ignore_header then Some (ref ("", "", "")) else None in
        Bracetax.Transform.brtx_to_latex ~writer ~doc ?title ?use_package 
            ?separate_header ~table_caption_after
            ~href_is_footnote
            ~print_comments ~input_char ~filename ~deny_bypass ();
    | `GetTOC io ->
        let input_char, filename, write = meta_open_inout io.CL.in_out in
        warn_error := io.CL.warn_error;
        let writer = Bracetax.Signatures.make_writer ~write  ~error in
        Bracetax.Transform.get_TOC ~writer ~input_char ();
    end;
    exit !return_value_to_shell;
)

