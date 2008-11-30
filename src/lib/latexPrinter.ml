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
type t = {
    stack: Commands.Stack.t;
    mutable write: string -> unit;
    write_mem: (string -> unit) Stack.t;
    mutable inside_header:bool;
    mutable current_table: Commands.Table.table option;
    error: Error.error_fun;
    mutable loc: Error.location;
}
type aux = unit
module CS = Commands.Stack

let (~%) = Printf.sprintf
let p = print_string

let create ~writer () =  (
    let module S = Signatures in
    let write = writer.S.w_write in
    {
        stack = CS.empty ();
        write = write;
        write_mem = Stack.create ();
        inside_header = false;
        current_table = None;
        error = writer.S.w_error;
        loc = {Error.l_line = -1; Error.l_char = -1;};
    }
)

let strstat s = (~% "[%d:%d]" s.Error.l_line s.Error.l_char)
let debugstr t s msg = 
    if false then
        (~% "%%%%DEBUG:[%s] Loc:[%d;%d]\n"
            msg s.Error.l_line s.Error.l_char)
    else
        ""

let sanitize_comments line = line

let sanitize_text line = (
    let patterns = [
        ('$' , "\\$" );
        ('&' , "\\&" );
        ('%' , "\\%" );
        ('#' , "\\#" );
        ('{' , "\\{" );
        ('}' , "\\}" );
        ('_' , "\\_" );
        ('\\', "\\textbackslash{}" );
        ('^' , "\\textasciicircum{}" );
        ('~' , "\\textasciitilde{}" );
    ] in
    Escape.replace_chars ~src:line ~patterns
)
let sanitize_nontext line = (
    let patterns = [
        ('$' , "" );
        ('&' , "" );
        ('%' , "" );
        ('#' , "" );
        ('{' , "" );
        ('}' , "" );
        ('_' , "" );
        ('\\', "" );
        ('^' , "" );
        ('~' , "" );
    ] in
    Escape.replace_chars ~src:line ~patterns
)
let sanitize_url line = (
    let patterns = [
        ('$' , "" );
        ('{' , "" );
        ('}' , "" );
        ('\\', "" );
        ('^' , "" );
        ('~' , "" );
    ] in
    Escape.replace_chars ~src:line ~patterns
)



(* Sections *)
let section_start n l =
    let section = 
        match n with
        | 4 -> "paragraph"
        | 3 -> "subsubsection"
        | 2 -> "subsection"
        | 1 | _ -> "section"
    in
    ~% "\n\\%s{" section

let section_stop n l = (
    let lsan =
        match l with | "" -> "" | s -> ~% "\\label{%s}" (sanitize_nontext s) in
    ~% "}\n%s\n" lsan
)

(* Links *)
let link_start t args = (
    let link, new_write = Commands.Link.start args in
    Stack.push t.write t.write_mem;
    t.write <- new_write;
    link
)
let link_stop t l = (
    t.write <- Stack.pop t.write_mem;
    let kind, target, text = Commands.Link.stop l in
    let target_str = 
        (match target with Some s -> s | None -> "notarget") in
    t.write (
        match kind with
        | `local ->
            let target = sanitize_nontext target_str in
            (match text with
            | Some s ->  ~% "%s (\\ref{%s})" (sanitize_nontext s) target
            | None ->  ~% "\\ref{%s}" target)
        | _ ->
            ~% "\\href{%s}{%s}" 
                (sanitize_url target_str)
                (sanitize_nontext 
                    (match text with Some s -> s | None -> target_str))
    );
)


(* Quotations *)
let quotation_open_close a = (
    let default = ("``", "''") in
    try
        match List.hd a with
        | "'"  -> ("`", "'")
        | "en" -> ("``", "''")
        | "fr" -> ("«~", "~»")
        | "de" -> ("\\unichar{8222}", "\\unichar{8220}")
        | "es" -> ("\\unichar{171}", "\\unichar{187}")
        | s    ->  default
    with
    | e -> default
)


(* Header: *)
let header_start t = (
    t.inside_header <- true; 
    ~% "%% HEADER:\n\\date{}"
)
let header_stop t = (
    t.inside_header <- false;
    "\\maketitle\n\n\n"
)

let title_start = "\\title{"
let title_stop = "}\n"
let authors_start = "\\author{"
let authors_stop = "}\n"
let subtitle_start = "\\date{"
let subtitle_stop = "}\n"


(* Images *)
let image_start t args = (
    let error_msg m = t.error (Error.mk t.loc `error m) in
    let src, opts, lbl = Commands.Names.image_params error_msg args in
    let opts_str =
        match opts with
        | `wpx w -> (~% "[width=%dpt]"  w)
        | `wpercent w -> (~% "[width=%f\\columnwidth]" ((float w) /. 100.))
        | `none -> ""
    in
    let sansrc = match src with "" -> "IMAGEWITHNOSOURCE" | s -> s in
    let sanlbl =
        match sanitize_nontext lbl with 
        | "" -> "" | s -> ~% "\\label{%s}" s in
    t.write (~% "\n\
        \\begin{figure}[htcb]\n\
        \  \\centering\n\
        \  \\includegraphics%s{%s}\n\
        \  \\caption{%s"
        opts_str sansrc sanlbl
    );
    `image (src, opts, lbl)
)
let image_stop = "}\n\\end{figure}\n"

(* Tables *)
let table_start t args = (
    let table, to_stack, new_write = Commands.Table.start args in
    t.current_table <- Some table;
    Stack.push t.write t.write_mem;
    t.write <- new_write;
    to_stack
)
let print_table write table = (
    let module CT = Commands.Table in
    let lbl_str =
        match table.CT.label with
        | None -> ""
        | Some s -> (~% "\\label{%s}" (sanitize_nontext s))
    in
    let table_format = 
        let rec make acc = function
            | 0 -> acc
            | n -> make ("c" :: acc) (n - 1)
        in
        "|" ^ (String.concat "|" (make [] table.CT.col_nb)) ^ "|"
    in
    write (~% "\n\
        \\begin{table}[htcb]\n\
        \  \\begin{center}\n\
        \    \\begin{tabular}{%s}\n\
        \    \\hline " table_format
    );
    let rec write_cells cells count =
        match cells with
        | [] -> (* fill the gap + warning *)
            ()
        | c :: t ->
            if count <> 0 then (
                if count mod table.CT.col_nb = 0 then (
                    write "\\\\\n      \\hline "
                ) else (
                    write " & "
                );
            );
            let text = (Buffer.contents c.CT.cell_text) in
            let text_with_type =
                if c.CT.is_head then ~% "\\textbf{%s}" text else text in
            let alignment =
                match c.CT.align with
                | `right -> "r"
                | `center -> "c"
                | `left -> "l"
                | `default -> "c"
            in
            let multicol = 
                (~% "\\multicolumn{%d}{|%s|}{%s}"
                    c.CT.cols_used alignment text_with_type)
            in
            (*write (~% "%s %s" ""[>TODO alignement<] multicol);*)
            write multicol;
            write_cells t (count + c.CT.cols_used);
    in
    write_cells (List.rev table.CT.cells) 0;
    write (~% "\\\\\n    \\hline\n\
        \  \\end{tabular}\n\
        \  \\end{center}\n\
        \  \\caption{%s%s}\n\
        \\end{table}\n" (Buffer.contents table.CT.caption) lbl_str);

)

let table_stop t = (
    match t.current_table with
    | None -> failwith "Why am I here ??? no table to end."
    | Some tab ->
        (* p (~% "End of table: %s\n" (Buffer.contents tab.caption)); *)
        t.write <- Stack.pop t.write_mem;
        t.current_table <- None;
        print_table t.write tab;
)
let cell_start t args = (
    let head, cnb, align = Commands.Table.cell_args args in
    let def_cell = `cell (head, cnb, align) in
    match t.current_table with
    | None ->
        t.error (Error.mk t.loc `error `cell_out_of_table);
        def_cell
    | Some tab -> Commands.Table.cell_start ~error:t.error tab args
)
let cell_stop t env = (
    match t.current_table with
    | None ->
        (* Already warned: *)
        (* t.error (Error.mk t.loc `warning `cell_out_of_table); *)
        ()
    | Some tab -> Commands.Table.cell_stop ~error:t.error tab
)


(* Lists *)
let list_start = function
    | `itemize -> "\n\\begin{itemize}\n"
    | `numbered -> "\n\\begin{enumerate}\n"
let list_item = "\\item "
let list_stop = function
    | `itemize -> "\n\\end{itemize}\n"
    | `numbered -> "\n\\end{enumerate}\n"



(* ==== PRINTER module type's functions ==== *)

let handle_text t location line = (
    t.loc <- location;

    if ((not t.inside_header)) ||
        (t.inside_header && (CS.head t.stack <> Some `header)) then (
        let data = sanitize_text line in
        t.write (~% "%s" data);
    ) else (
        if
            CS.head t.stack = Some `header
            && (not (Escape.is_white_space line))
        then (
            t.write (~% "%%%%IGNORED TEXT: %s\n" (sanitize_comments line));
        );

    )
)

let handle_comment_line t location line = (
    t.loc <- location;
    t.write (~% "%%%s %s\n"
        (debugstr t location "Comment") (sanitize_comments line));
)

let enter_verbatim t location args = (
    t.loc <- location;
    CS.push t.stack (`verbatim args);
    begin match args with
    | q :: _ -> t.write (~% "%%\n%%verbatimbegin:%s\n\\begin{verbatim}\n" q)
    | _ -> t.write "%\n\\begin{verbatim}\n";
    end;
)
let exit_verbatim t location = (
    t.loc <- location;
    let env =  (CS.pop t.stack) in
    match env with
    | Some (`verbatim args) ->
        t.write "\\end{verbatim}\n";
        begin match args with
        | q :: _ -> t.write (~% "%%verbatimend:%s\n" q)
        | _ -> ()
        end;
    | _ ->
        (* warning ? error ? anyway, *)
        failwith "Shouldn't be there, Parser's fault ?";
)
let handle_verbatim_line t location line = (
    t.loc <- location;
    t.write (~% "%s\n" line);
)

let terminate t location = (
    t.loc <- location;
    if (CS.to_list t.stack) <> [] then (
        let l = List.map Commands.env_to_string (CS.to_list t.stack) in
        t.error (Error.mk t.loc `error (`terminating_with_open_environments l));
    );  
)

let start_environment ?(is_begin=false) t location name args = (
    let module C = Commands.Names in
    t.loc <- location;
    let cmd name args =
        match name with
        | s when C.is_quotation s        ->
            let op, clo = quotation_open_close args in
            t.write op;
            `quotation (op, clo)
        | s when C.is_italic s      -> t.write "{\\it{}"  ; `italic
        | s when C.is_bold s        -> t.write "{\\bf{}"  ; `bold
        | s when C.is_mono_space s  -> t.write "\\texttt{" ; `mono_space
        | s when C.is_superscript s -> t.write "$^{\\textrm{"; `superscript
        | s when C.is_subscript s   -> t.write "$_{\\textrm{"; `subscript
        | s when (C.is_end s)           -> `cmd_end
        | s when C.is_list s             ->
            let style, other_args, waiting =
                let error_msg m = t.error (Error.mk t.loc `error m) in
                match args with
                | [] -> (`itemize, [], ref true)
                | h :: t -> (C.list_style error_msg h, t, ref true) in
            t.write (list_start style);
            `list (style, other_args, waiting)
        | s when C.is_item s -> `item
        | s when C.is_section s -> 
            let level, label = C.section_params args in
            t.write (section_start level label);
            `section (level, label)
        | s when C.is_link s -> (link_start t args)
        | s when C.is_image s -> image_start t args
        | s when C.is_header s -> t.write (header_start t); `header
        | s when C.is_title s -> t.write title_start; `title
        | s when C.is_subtitle s -> t.write subtitle_start; `subtitle
        | s when C.is_authors s -> t.write authors_start; `authors
        | s when C.is_table s -> table_start t args
        | s when C.is_cell s -> cell_start t args
        | s when C.is_note s -> t.write "\\footnote{" ; `note
        | s ->
            t.error (Error.mk t.loc `error (`unknown_command  s));
            `unknown (s, args)
    in
    let the_cmd =
        if C.is_begin name then (
            match args with
            | [] ->
                t.error (Error.mk t.loc `error `begin_without_arg);
                (`cmd_begin ("", []))
            | h :: t -> (`cmd_begin (h, t))
        ) else (
            cmd name args
        )
    in
    if is_begin then (
        CS.push t.stack (`cmd_inside the_cmd);
    ) else (
        CS.push t.stack the_cmd;
    );

)

let start_command t location name args = (
    t.loc <- location;
    (* p (~% "%%%s[start %s(%s)]\n" (strstat location) *)
    (* name (String.concat ", " args)); *)
    match Commands.non_env_cmd_of_name name args with
    | `unknown (name, args) -> start_environment t location name args
    | cmd -> CS.push t.stack cmd
)


let stop_command t location = (
    t.loc <- location;
    (* p (~% "%%%s[stop]\n" (strstat location)); *)
    let rec out_of_env env =
        match env with
        | `cmd_end ->
            begin match CS.pop t.stack with
            | Some (`cmd_inside benv) ->
                (* p (~% "{end} %s\n" (Commands.env_to_string benv)); *)
                out_of_env benv
            | Some c ->
                t.error (Error.mk t.loc `error `non_matching_end);
                CS.push t.stack c;
            | None ->
                t.error (Error.mk t.loc `error `non_matching_end);
            end
        | `cmd_begin (nam, args) ->
            (* p (~% "cmd begin %s(%s)\n" nam (String.concat ", " args)); *)
            start_environment ~is_begin:true t location nam args;
        | `paragraph -> t.write "\\par\n"
        | `new_line -> t.write "\\\n"
        | `non_break_space -> t.write "~"
        | `horizontal_ellipsis -> t.write "\\ldots{}"
        | `open_brace -> t.write "\\{"
        | `close_brace -> t.write "\\}"
        | `sharp -> t.write "\\#"
        | (`utf8_char i) -> t.write (~% "\\unichar{%d}" i)
        | (`quotation (op, clo)) -> t.write clo
        | `italic       ->  t.write "}"  
        | `bold         ->  t.write "}"  
        | `mono_space   ->  t.write "}" 
        | `superscript  ->  t.write "}}$"
        | `subscript    ->  t.write "}}$"
        | `list (style, _, r) -> t.write (list_stop style)
        | `item ->
            begin match CS.head t.stack with
            | Some (`list (style, _, _))
            | Some (`cmd_inside (`list (style, _, _))) ->
                t.write list_item;
            | Some c ->
                t.error (Error.mk t.loc `error `item_out_of_list);
                CS.push t.stack c;
            | None ->
                t.error (Error.mk t.loc `error `item_out_of_list)
            end
        | `section (level, label) -> t.write (section_stop level label);
        | `link l -> link_stop t l;
        | `image _ -> t.write image_stop;
        | `header ->  t.write (header_stop t);
        | `title -> t.write title_stop;
        | `subtitle -> t.write subtitle_stop;
        | `authors -> t.write authors_stop;
        | `table _ -> table_stop t
        | `cell _ as c -> cell_stop t c
        | `note -> t.write "}"
        | `cmd_inside c ->
            t.error (Error.mk t.loc `error `closing_brace_matching_begin);
        | `unknown c -> () (* Already "t.error-ed" in start_environment *)
        | c -> (* shouldn't be there !! *)
            t.error (Error.mk t.loc `fatal_error 
                (`transformer_lost (Commands.env_to_string c)));

    in
    match CS.pop t.stack with
    | Some env -> out_of_env env
    | None ->
        t.error (Error.mk t.loc `error `nothing_to_end_with_brace);
)


(* ==== Directly exported functions ==== *)

let header ?(title="") ?(comment="") ?stylesheet_link () = (
    let package_str =
        match stylesheet_link with
        | None -> ""
        | Some f -> ~% "\\usepackage{%s}" f
    in
    ~% "\
    \\documentclass[a4paper,10pt]{article}\n\
    \n\
    \\clubpenalty=10000\n\
    \\widowpenalty=10000\n\
    \n\
    \\usepackage[T1]{fontenc}\n\
    \\usepackage[english]{babel}\n\
    \\usepackage{ucs}\n\
    \\usepackage[utf8x,utf8]{inputenc}\n\
    \\usepackage[                         \n\
        bookmarks         = true,         \n\
        bookmarksnumbered = true,         \n\
        colorlinks        = true,         \n\
    ]{hyperref}                           \n\
    \\usepackage{color}\n\
    \\definecolor{webred}{rgb}{0.3,0,0}\n\
    \\definecolor{blurl}{rgb}{0,0,0.3}\n\
    \\hypersetup{\n\
    linkcolor         = webred, %%black\n\
    citecolor         = webred, %%black\n\
    urlcolor          = blurl , %%black\n\
    linkbordercolor   = {1 1 1},\n\
    citebordercolor   = {1 1 1},\n\
    urlbordercolor    = {1 1 1},\n\
    pdfauthor   = {},\n\
    pdftitle    = {%s},\n\
    pdfsubject  = {},\n\
    pdfkeywords = {},\n\
    pdfcreator  = {Bracetax and PDFLaTeX},\n\
    pdfproducer = {Bracetax and PDFLaTeX}}\n\
    \n\
    \\usepackage[pdftex]{graphicx}\n\
    \\DeclareGraphicsExtensions{.jpg,.mps,.pdf,.png}\n\
    \n\
    %%%s\n\
    %s\n\
    \n\
    \\begin{document}\n\
    \n\
    " title (sanitize_comments comment) package_str
)
let footer () = "\n\\end{document}\n"

