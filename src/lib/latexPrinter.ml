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
type t = {
    stack: Commands.Stack.t;
    mutable write: string -> unit;
    write_mem: (string -> unit) Stack.t;
    mutable inside_header:bool;
    mutable current_table: Commands.Table.table option;
    error: Error.error_fun;
    mutable loc: Error.location;
    opt_href_footnote: bool;
    url_hook: string -> string;
    img_hook: string -> string;
    separate_header: (string * string * string) ref option;
}

module CS = Commands.Stack

let (~%) = Printf.sprintf
let p = print_string

let create
~writer ?(href_is_footnote=false) ?separate_header ?(img_hook=fun s -> s)
?(url_hook=fun s -> s) () =  (
    let module S = Signatures in
    let write = writer.S.w_write in
    {
        stack = CS.empty ();
        write = write;
        write_mem = Stack.create ();
        inside_header = false;
        current_table = None;
        error = writer.S.w_error;
        loc = {Error.l_line = -1; l_char = -1;l_file = "NO FILE";};
        opt_href_footnote = href_is_footnote;
        url_hook = url_hook;
        img_hook = img_hook;
        separate_header = separate_header;
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
        ('-' , "-{}" );
        ('&' , "\\&\\linebreak[0]" );
        ('%' , "\\%" );
        ('#' , "\\#" );
        ('{' , "\\{" );
        ('}' , "\\}" );
        ('/' , "\\slash{}\\linebreak[0]" );
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
        ('%' , "\\%" );
        ('#' , "\\#" );
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
    let link, new_write = Commands.Link.start ~url_hook:t.url_hook args in
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
            | Some s ->  ~% "%s (\\ref{%s})" s target
            | None ->  ~% "\\ref{%s}" target)
        | _ ->
            let san_urltxt url =
                let patterns = [
                    ('?',"?\\linebreak[0]");
                    ('.',".\\linebreak[0]");
                    ('=',"=\\linebreak[0]"); ] in
                let src = sanitize_text url in
                Escape.replace_chars ~src ~patterns in
            if not t.opt_href_footnote then
                (~% "\\href{%s}{%s}" 
                    (sanitize_url target_str)
                    (match text with
                    Some s -> s | None -> san_urltxt target_str))
            else
                let san_text =
                    (match text with
                    Some s -> s | None -> san_urltxt target_str) in
                (~% "%s\\footnote{ \\href{%s}{%s}}" 
                    san_text (sanitize_url target_str)
                    (san_urltxt target_str))
    );
)


(* Quotations *)
let quotation_open_close t a = (
    let default = ("``", "''") in
    try
        match List.hd a with
        | "'"  -> ("`", "'")
        | "en" -> ("``", "''")
        | "fr" -> ("«~", "~»")
        | "de" -> ("\\unichar{8222}", "\\unichar{8220}")
        | "es" -> ("\\unichar{171}", "\\unichar{187}")
        | s    ->
            t.error (Error.mk t.loc `warning (`unknown_quotation_style s));
            default
    with
    | e -> default
)


(* Header: *)
let header_start t = (
    t.inside_header <- true; 
    begin match t.separate_header with
    | None ->
        t.write (~% "%% HEADER:\n\\date{}");
    | Some r ->
        Stack.push t.write t.write_mem;
        t.write <- (fun str -> 
            begin match CS.head t.stack with
            | Some `title    -> let t,a,s = !r in r := (t ^ str, a, s);
            | Some `authors  -> let t,a,s = !r in r := (t, a ^ str, s);
            | Some `subtitle -> let t,a,s = !r in r := (t, a, s ^ str);
            | _ -> ()
            end;
        );
    end;
)
let header_stop t = (
    t.inside_header <- false;
    begin match t.separate_header with
    | None ->
        t.write "\\maketitle\n\n\n";
    | Some r ->
        t.write <- Stack.pop t.write_mem;
    end;
)

let title_start t = t.write "\\title{"
let title_stop t = t.write "}\n"
let authors_start t = t.write "\\author{"
let authors_stop t = t.write "}\n"
let subtitle_start t = t.write "\\date{"
let subtitle_stop t = t.write "}\n"


(* Images *)
let image_start t args = (
    let error_msg m = t.error (Error.mk t.loc `error m) in
    let src, opts, lbl =
        Commands.Names.image_params ~img_hook:t.img_hook error_msg args in
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
    let nb_rows, nb_cols, matrix = 
        CT.Util.cells_to_matrix table in
    let string_of_cell c =
        let text = (Buffer.contents c.CT.cell_text) in
        let text_with_type =
            if c.CT.is_head then ~% "\\textbf{%s}" text else text in
        let text_with_type_and_rows =
            if c.CT.rows_used <> 1 then 
                ~% "\\multirow{%d}{*}{%s}" c.CT.rows_used text_with_type
            else
                text_with_type in
        let alignment =
            match c.CT.align with
            | `right -> "r"
            | `center -> "c"
            | `left -> "l"
        in
        let multicol = 
            (~% "\\multicolumn{%d}{|%s|}{%s}"
                c.CT.cols_used alignment text_with_type_and_rows)
        in
        multicol
    in 
    let empty_multicol cols = (~% "\\multicolumn{%d}{|c|}{}" cols) in
    let separator cur_col cols_used nb_cols =
        if cur_col + cols_used - 1 <> nb_cols - 1 then " & " else " \\\\\n" in
    let str_matrix = Array.make_matrix nb_rows nb_cols "" in
    let lines_matrix = Array.make_matrix nb_rows nb_cols false in
    for row = 0 to nb_rows - 1 do
        for col = 0 to nb_cols - 1 do
            begin match matrix.(row).(col) with
            | `none -> ()
            | `cell c ->
                let the_cell =
                    (string_of_cell c) ^ (separator col c.CT.cols_used nb_cols) in
                str_matrix.(row).(col) <- the_cell;
                for i = row + 1 to row + c.CT.rows_used - 1 do
                    str_matrix.(i).(col) <- 
                        (empty_multicol c.CT.cols_used) 
                        ^ (separator col c.CT.cols_used nb_cols);
                done;
                for i = col to col + c.CT.cols_used - 1 do
                    lines_matrix.(row + c.CT.rows_used - 1).(i) <- true;
                done;
            | `filled (r, c) -> ()
            end;
        done;
    done;
    for row = 0 to nb_rows - 1 do
        let buf_separ = Buffer.create 42 in
        let buf_row = Buffer.create 42 in
        for col = 0 to nb_cols - 1 do
            Buffer.add_string buf_row str_matrix.(row).(col);
            if (lines_matrix.(row).(col)) then (
                Buffer.add_string buf_separ
                    (~% " \\cline{%d-%d}" (col + 1) (col + 1));
            );
        done;
        write (Buffer.contents buf_row);
        write (Buffer.contents buf_separ);
        write "\n";
    done;
    write (~% "\n\
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
    match t.current_table with
    | None ->
        t.error (Error.mk t.loc `error `cell_out_of_table);
        `cell (false, 1, `center)
    | Some tab -> Commands.Table.cell_start ~loc:t.loc ~error:t.error tab args
)
let cell_stop t env = (
    match t.current_table with
    | None ->
        (* Already warned: *)
        (* t.error (Error.mk t.loc `warning `cell_out_of_table); *)
        ()
    | Some tab -> Commands.Table.cell_stop ~loc:t.loc ~error:t.error tab
)


(* Lists *)
let list_start = function
    | `itemize -> "\n\\begin{itemize}\n"
    | `numbered -> "\n\\begin{enumerate}\n"
let list_item = "\n    \\item "
let list_stop = function
    | `itemize -> "\n\\end{itemize}\n"
    | `numbered -> "\n\\end{enumerate}\n"



let start_subsup t supsub = (
    let discriminating_char =
        match supsub with `superscript -> '^' | `subscript -> '_' | _ -> '%' in
    let inside_italic, inside_bold =
        let it, bf = ref false, ref false in
        List.iter (function
            | `italic -> it := true;
            | `bold -> bf := true;
            | _ -> ()) (CS.to_list t.stack);
        !it, !bf
    in
    t.write (~% "$%c{\\textnormal{\\footnotesize{}%s%s"
        discriminating_char
        (if inside_italic then "\\it{}" else "")
        (if inside_bold then "\\bf{}" else "")
    );
    supsub
)
let stop_subsup t = (
    t.write "}}$"
)

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
            let op, clo = quotation_open_close t args in
            t.write op;
            `quotation (op, clo)
        | s when C.is_italic s      -> t.write "{\\it{}"  ; `italic
        | s when C.is_bold s        -> t.write "{\\bf{}"  ; `bold
        | s when C.is_mono_space s  -> t.write "\\texttt{" ; `mono_space
        | s when C.is_superscript s -> start_subsup t `superscript
        | s when C.is_subscript s   -> start_subsup t `subscript
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
        | s when C.is_header s -> header_start t; `header
        | s when C.is_title s -> title_start t; `title
        | s when C.is_subtitle s -> subtitle_start t; `subtitle
        | s when C.is_authors s -> authors_start t; `authors
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
        | `new_line -> t.write "\\\\\n"
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
        | `superscript  -> stop_subsup t
        | `subscript    ->  stop_subsup t
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
        | `header ->  header_stop t;
        | `title -> title_stop t;
        | `subtitle -> subtitle_stop t;
        | `authors -> authors_stop t;
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

let start_raw_mode t location kind args = (
    t.loc <- location;
    match kind with
    | `code ->
        CS.push t.stack (`code args);
        begin match args with
        | q :: _ -> t.write (~% "%%\n%%verbatimbegin:%s\n\\begin{verbatim}" q)
        | _ -> t.write "%\n\\begin{verbatim}";
        end;
    | `bypass ->
        CS.push t.stack (`bypass);
)
let handle_raw_text t location text = (
    t.loc <- location;
    t.write text;
)
let stop_raw_mode t location = (
    t.loc <- location;
    match CS.pop t.stack with
    | Some (`code args) ->
        t.write "\\end{verbatim}";
        begin match args with
        | q :: _ -> t.write (~% "\n%%verbatimend:%s\n" q)
        | _ -> ()
        end;
    | Some `bypass -> ()
    | _ ->
        (* warning ? error ? anyway, *)
        failwith "Shouldn't be there, Parser's fault ?";

)

(* ==== Directly exported functions ==== *)

let build
?(print_comments=false) ?separate_header 
?img_hook ?url_hook ?href_is_footnote ~writer () = (
    let t =
        create ~writer ?href_is_footnote
            ?separate_header ?img_hook ?url_hook () in
    let printer = {
        Signatures.
        print_comment =
            if print_comments then 
                (handle_comment_line t)
            else 
                (fun a b -> ());
        print_text =    handle_text t;
        enter_cmd =     start_command t;
        leave_cmd =     stop_command t;
        terminate =     terminate t;
        enter_raw =     start_raw_mode t;
        print_raw =     handle_raw_text t;
        leave_raw =     stop_raw_mode t;
        error = writer.Signatures.w_error; } in
    printer
)
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
    \\usepackage{multirow}\n\
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
    \\frenchspacing\n\
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

