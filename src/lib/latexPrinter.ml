type t = {
    stack: Commands.Stack.t;
    mutable write: string -> unit;
    write_mem: string -> unit;
    mutable inside_header:bool;
    mutable current_table: Commands.Table.table option;
    warn: string -> unit;
    error: string -> unit;
}
module CS = Commands.Stack

let (~%) = Printf.sprintf
let p = print_string

let create ~writer =  (
    let module S = Signatures in
    let write = writer.S.w_write in
    {
        stack = CS.empty ();
        write = write;
        write_mem = write;
        inside_header = false;
        current_table = None;
        warn = writer.S.w_warn;
        error = writer.S.w_error;
    }
)

let strstat s = (~% "[%d:%d]" s.Signatures.s_line s.Signatures.s_char)
let debugstr t s msg = 
    if false then
        (~% "%%%%DEBUG:[%s] Loc:[%d;%d]\n"
            msg s.Signatures.s_line s.Signatures.s_char)
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
        ('\\', "\\textbackslash" );
        ('^' , "\\textasciicircum" );
        ('~' , "\\textasciitilde" );
    ] in
    Escape.replace_chars ~src:line ~patterns
)

(* ==== PRINTER module type's functions ==== *)

let handle_text t location line = (

    if ((not t.inside_header)) ||
        (t.inside_header && (CS.head t.stack <> Some `header)) then (
        let data = sanitize_text line in
        t.write (~% "%s" data);
    ) else (
        if
            CS.head t.stack = Some `header
            && (not (Escape.is_white_space line))
        then (
            t.write (~% "%%%%IGNORED TEXT: %s" (sanitize_comments line));
        );

    )
)

let handle_comment_line t location line = (
    t.write (~% "%%%s %s-->\n"
        (debugstr t location "Comment") (sanitize_comments line));
)

let enter_verbatim t location args = (
    CS.push t.stack (`verbatim args);
    t.write "\n\\begin{verbatim}\n";
)
let exit_verbatim t location = (
    let env =  (CS.pop t.stack) in
    match env with
    | Some (`verbatim _) ->
        t.write "\\end{verbatim}\n";
    | _ ->
        (* warning ? error ? anyway, *)
        failwith "Shouldn't be there, Parser's fault ?";
)
let handle_verbatim_line t location line = (
    t.write (~% "%s\n" line);
)

let terminate t location = ()

let start_environment ?(is_begin=false) t location name args = (
    let module C = Commands.Names in
    let cmd name args =
        match name with
        (*| s when C.is_quotation s        ->*)
            (*let op, clo = quotation_open_close args in*)
            (*t.write op;*)
            (*`quotation (op, clo)*)
        | s when C.is_italic s      -> t.write "{\\it{}"  ; `italic
        | s when C.is_bold s        -> t.write "{\\bf{}"  ; `bold
        | s when C.is_mono_space s  -> t.write "\\texttt{" ; `mono_space
        | s when C.is_superscript s -> t.write ""; `superscript
        | s when C.is_subscript s   -> t.write ""; `subscript
        | s when (C.is_end s)           -> `cmd_end
    (*    | s when C.is_list s             ->
            let style, other_args, waiting =
                match args with
                | [] -> (`itemize, [], ref true)
                | h :: t -> (C.list_style h, t, ref true) in
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
        *)
        | s -> t.warn (~% "unknown: %s\n" s); `unknown (s, args)
    in
    let the_cmd =
        if C.is_begin name then (
            match args with
            | [] -> t.warn "Lonely begin ??!!"; (`cmd_begin ("", []))
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
    p (~% "%%%s[start %s(%s)]\n" (strstat location)
    name (String.concat ", " args));
    match Commands.non_env_cmd_of_name name args with
    | `unknown (name, args) -> start_environment t location name args
    | cmd -> CS.push t.stack cmd
)


let stop_command t location = (
    p (~% "%%%s[stop]\n" (strstat location));
    let rec out_of_env env =
        match env with
        | `cmd_end ->
            begin match CS.pop t.stack with
            | Some (`cmd_inside benv) ->
                (* p (~% "{end} %s\n" (Commands.env_to_string benv)); *)
                out_of_env benv
            | Some c ->
                t.warn (~% "Warning {end} does not end a {begin...} but %s\n"
                    (Commands.env_to_string c));
                CS.push t.stack c;
            | None -> t.warn (~% "Nothing to {end} there !!\n")
            end
        | `cmd_begin (nam, args) ->
            (* p (~% "cmd begin %s(%s)\n" nam (String.concat ", " args)); *)
            start_environment ~is_begin:true t location nam args;
        | `paragraph -> t.write "\n\\par\n"
        | `new_line -> t.write "\\\n"
        | `non_break_space -> t.write "~"
        | `open_brace -> t.write "\\{"
        | `close_brace -> t.write "\\}"
        | `sharp -> t.write "\\#"
        | (`utf8_char i) -> t.write (~% "%% (TODO) UTF:0x%x\n" i)
        | (`quotation (op, clo)) -> t.write clo
        | `italic       ->  t.write "}"  
        | `bold         ->  t.write "}"  
        | `mono_space   ->  t.write "}" 
        | `superscript  ->  t.write ""
        | `subscript    ->  t.write ""
        | `list (style, _, r) -> t.write "" (*(list_stop style)*)
        | `item ->
            begin match CS.head t.stack with
            | Some (`list (style, _, r))
            | Some (`cmd_inside (`list (style, _, r))) ->
                if !r then (
                    t.write "" (*(list_firstitem style)*);
                    r := false;
                ) else (
                    t.write ""(*(list_item style)*);
                );
            | Some c ->
                t.warn (~% "Warning {item} is not just under list but %s\n"
                    (Commands.env_to_string c));
                CS.push t.stack c;
            | None -> t.warn (~% "Warning {item}... nothing to itemize !\n")
            end
        | `section (level, label) ->
            t.write ""(*(section_stop level label)*);
        | `link l -> ()(*link_stop t l*);
        | `image _ -> t.write ""(*image_stop*);
        | `header ->  t.write ""(*(header_stop t)*);
        | `title -> t.write ""(*title_stop*);
        | `subtitle -> t.write ""(*subtitle_stop*);
        | `authors -> t.write ""(*authors_stop*);
        | `table _ -> ()(*table_stop t*)
        | `cell _ -> ()(*cell_stop t c*)
        | `cmd_inside c ->
            t.warn (~% "Warning: a '}' is trying to terminate a {begin %s\n"
                (Commands.env_to_string c));
        | s -> t.warn (~% "Unknown command... %s\n" (Commands.env_to_string s));
    in
    match CS.pop t.stack with
    | Some env -> out_of_env env
    | None -> ()
)


(* ==== Directly exported functions ==== *)

let header ?(title="") ?(comment="") ?stylesheet_link () = (
    let package_str =
        match stylesheet_link with
        | None -> ""
        | Some f -> ~% "\\usepackage{%s}" f
    in
    ~% "\
    \\documentclass[a4,10pt]{article}\n\
    \\usepackage[T1]{fontenc}\n\
    \\usepackage[english]{babel}\n\
    \\usepackage{ucs}\n\
    \\usepackage[utf8]{inputenc}\n\
    %%%s\n\
    %s\n\
    \n\
    \\begin{document}\n\
    \n\
    " (sanitize_comments comment) package_str
)
let footer () = "\n\\end{document}\n"

