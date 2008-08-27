
type t = {
    stack: Commands.Stack.t;
    mutable write: string -> unit;
    write_mem: string -> unit;
    mutable current_line: int;
    mutable started_text: bool;
    mutable inside_header:bool;
    mutable current_table: Commands.Table.table option;
    warn: string -> unit;
    error: string -> unit;
}
module CS = Commands.Stack

let (~%) = Printf.sprintf

let create ~writer =  (
    let module S = Signatures in
    let write = writer.S.w_write in
    {
        stack = CS.empty ();
        write = write;
        write_mem = write;
        current_line = 1;
        started_text = false;
        inside_header = false;
        current_table = None;
        warn = writer.S.w_warn;
        error = writer.S.w_error;
    }
)

let strstat s = (~% "[%d:%d]" s.Signatures.s_line s.Signatures.s_char)
let debugstr t s msg = 
    if false then
        (~% "<!--DEBUG:[%s] Loc:[%d;%d] CurLine:%d-->"
            msg s.Signatures.s_line s.Signatures.s_char t.current_line)
    else
        ""

let sanitize_comments line =
    let patterns = [('<', "LT"); ('>', "GT"); ('&', "AMP"); ('-', "DASH")] in
    Escape.replace_chars ~src:line ~patterns
    (* let src = Escape.replace_string ~src:line ~find:"-->" ~replace_with:"XXX" in *)
    (* Escape.replace_string ~src ~find:"<!--" ~replace_with:"XXXX" *)

let sanitize_pcdata line =
    let patterns = [('<', "&lt;"); ('>', "&gt;"); ('&', "&amp;")] in
    Escape.replace_chars ~src:line ~patterns

let sanitize_xml_attribute src =
    let patterns =
        [('<', "&lt;"); ('>', "&gt;"); ('&', "&amp;"); ('"', "&quot;")] in
    Escape.replace_chars ~src ~patterns


let quotation_open_close a = (
    let default = ("&ldquo;", "&rdquo;") in
    try
        match List.hd a with
        | "'"  -> ("&lsquo;", "&rsquo;")
        | "en" -> ("&ldquo;", "&rdquo;")
        | "fr" -> ("&laquo;&nbsp;", "&nbsp;&raquo;")
        | "de" -> ("&bdquo;", "&rdquo;")
        | "es" -> ("&laquo;", "&raquo;")
        | s    ->  default
    with
    | e -> default
)

let list_start =
    function `itemize -> "\n<ul>\n" | `numbered -> "\n<ol>\n"
let list_item = 
    function `itemize -> "</li>\n<li>" | `numbered -> "</li>\n<li>"
let list_firstitem = 
    function `itemize -> "<li>" | `numbered -> "<li>"
let list_stop = 
    function `itemize -> "</li>\n</ul>\n" | `numbered -> "</li>\n</ol>\n"

let section_start n l =
    let lsan =
        match sanitize_xml_attribute l with
        | "" -> "" | s -> ~% "name=\"%s\" id=\"%s\"" s s
    in
    ~% "</p>\n<h%d><a %s>" (n + 1) lsan

let section_stop n l =
    ~% "</a></h%d>\n<p>" (n + 1)

let link_start printer args = (
    match args with
    | linkto :: t ->
        let is_local, link = Commands.Names.is_local linkto in
        let href =
            if is_local then ~% "#%s" (link) else linkto in
        printer.write (~% "<a href=\"%s\">" (sanitize_xml_attribute href));
        `link (href, t)
    | [] -> 
        printer.write (~% "<a>");
        `link ("", [])
)

let image_start t args = (
    (* http://www.w3.org/Style/Examples/007/figures *)
    let src, opts, lbl = Commands.Names.image_params args in
    let opts_str =
        let strs =List.map (function
        | `w w -> (~% "width=\"%d\""  w)
        | `h h -> (~% "height=\"%d\"" h)) opts in
        String.concat " " strs
    in
    let sansrc =
        match sanitize_xml_attribute src with
        "" -> "http://IMAGEWITHNOSOURCE" | s -> s in
    let sanlbl =
        match sanitize_xml_attribute lbl with 
        | "" -> "" | s -> ~% "id=\"%s\" " s in
    t.write (~%
        "</p>\n<div class=\"figure\">\n  <a href=\"%s\">\
        \n    <img src=\"%s\" %s %s alt=\"%s\"/>\n  </a><br/>\n"
        sansrc sansrc opts_str sanlbl sansrc
    );
    `image (src, opts, lbl)
)
let image_stop = "</div><p>"

let header_start t = (
    t.inside_header <- true; 
    ~% "%s\n<div class=\"header\">\n" (if t.started_text then "</p>" else "")
)
let header_stop t = (
    t.inside_header <- false;
    t.started_text <- true; (* we put the <p> *)
    "</div> <!-- END HEADER -->\n<p>\n"
)

let title_start = "\n  <h1>"
let title_stop = "</h1>\n"
let authors_start = "  <div class=\"authors\">"
let authors_stop = "</div>\n"
let subtitle_start = "  <div class=\"subtitle\">"
let subtitle_stop = "</div>\n"

let table_start t args = (
    (* http://www.topxml.com/xhtml/articles/xhtml_tables/ *)
    let table, to_stack, new_write = Commands.Table.start args in
    t.current_table <- Some table;
    t.write <- new_write;
    to_stack
)
let print_table write table = (
    let module CT = Commands.Table in
    let lbl_str =
        match table.CT.label with
        | None -> ""
        | Some s -> (~% "id=\"%s\"" (sanitize_xml_attribute s))
    in
    write (~% "</p><table border=\"1\" %s >\n" lbl_str);
    write (~% "<caption>%s</caption>\n<tr>" (Buffer.contents table.CT.caption));
    let rec write_cells cells count =
        match cells with
        | [] -> (* fill the gap + warning *)
            ()
        | c :: t ->
            if count <> 0 && count mod table.CT.col_nb = 0 then (
                write "</tr>\n<tr>"
            );
            let typ_of_cell = if c.CT.is_head then "h" else "d" in
            let alignement =
                match c.CT.align with
                | `right -> "class=\"rightalign\" style=\"text-align:right;\""
                | `center -> "class=\"centeralign\" style=\"text-align:center;\""
                | `left -> "class=\"leftalign\" style=\"text-align:left;\""
                | `default -> ""
            in
            write (~% "<t%s colspan=\"%d\" %s >%s</t%s>"
                typ_of_cell c.CT.cols_used alignement
                (Buffer.contents c.CT.cell_text)
                typ_of_cell);
            write_cells t (count + c.CT.cols_used)
    in
    write_cells (List.rev table.CT.cells) 0;
    write "</tr></table><p>\n"
)

let table_stop t = (
    match t.current_table with
    | None -> failwith "Why am I here ??? no table to end."
    | Some tab ->
        (* p (~% "End of table: %s\n" (Buffer.contents tab.caption)); *)
        t.write <- t.write_mem;
        t.current_table <- None;
        print_table t.write tab;
)
let cell_start t args = (
    let head, cnb, align = Commands.Names.cell_args args in
    let def_cell = `cell (head, cnb, align) in
    match t.current_table with
    | None ->
        t.warn (~% "Warning: no use for a cell here !\n");
        def_cell
    | Some tab -> Commands.Table.cell_start tab args
)
let cell_stop t env = (
    match t.current_table with
    | None -> t.warn (~% "Warning: still no use for a cell here !\n");
    | Some tab -> Commands.Table.cell_stop tab
)


let start_environment ?(is_begin=false) t location name args = (
    let module C = Commands.Names in
    let cmd name args =
        match name with
        | s when C.is_quotation s        ->
            let op, clo = quotation_open_close args in
            t.write op;
            `quotation (op, clo)
        | s when C.is_italic s           -> t.write "<i>"  ; `italic
        | s when C.is_bold s             -> t.write "<b>"  ; `bold
        | s when C.is_mono_space s       -> t.write "<tt>" ; `mono_space
        | s when C.is_superscript s      -> t.write "<sup>"; `superscript
        | s when C.is_subscript s        -> t.write "<sub>"; `subscript
        | s when (C.is_end s)           -> `cmd_end
        | s when C.is_list s             ->
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

(* ==== PRINTER module type's functions ==== *)

let start_command t location name args = (
    (* p (~% "Command: \"%s\"(%s)\n" name (String.concat ", " args)); *)
    match Commands.non_env_cmd_of_name name args with
    | `unknown (name, args) -> start_environment t location name args
    | cmd -> CS.push t.stack cmd
)
let stop_command t location = (
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
        | `paragraph -> t.write "</p>\n<p>" (* TODO: unstack and restack ? *)
        | `new_line -> t.write "<br/>\n"
        | `non_break_space -> t.write "&nbsp;"
        | `open_brace -> t.write "{"
        | `close_brace -> t.write "}"
        | `sharp -> t.write "#"
        | (`utf8_char i) -> t.write (~% "&#%d;" i)
        | (`quotation (op, clo)) -> t.write clo
        | `italic       ->  t.write "</i>"  
        | `bold         ->  t.write "</b>"  
        | `mono_space   ->  t.write "</tt>" 
        | `superscript  ->  t.write "</sup>"
        | `subscript    ->  t.write "</sub>"
        | `list (style, _, r) -> t.write (list_stop style)
        | `item ->
            begin match CS.head t.stack with
            | Some (`list (style, _, r))
            | Some (`cmd_inside (`list (style, _, r))) ->
                if !r then (
                    t.write (list_firstitem style);
                    r := false;
                ) else (
                    t.write (list_item style);
                );
            | Some c ->
                t.warn (~% "Warning {item} is not just under list but %s\n"
                    (Commands.env_to_string c));
                CS.push t.stack c;
            | None -> t.warn (~% "Warning {item}... nothing to itemize !\n")
            end
        | `section (level, label) ->
            t.write (section_stop level label);
        | `link _ -> t.write "</a>";
        | `image _ -> t.write image_stop;
        | `header ->  t.write (header_stop t);
        | `title -> t.write title_stop;
        | `subtitle -> t.write subtitle_stop;
        | `authors -> t.write authors_stop;
        | `table _ -> table_stop t
        | `cell _ as c -> cell_stop t c
        | s -> t.warn (~% "Unknown command... %s\n" (Commands.env_to_string s));
    in
    match CS.pop t.stack with
    | Some env -> out_of_env env
    | None -> ()
) 

let handle_comment_line t location line = (
    t.write (~% "%s<!--%s-->\n" (debugstr t location "Comment")
        (sanitize_comments line));
    t.current_line <- t.current_line + 1;
)

let handle_text t location line = (
    if
        not t.started_text &&
        not t.inside_header &&
        not (Escape.is_white_space line)
    then (
        t.started_text <- true;
        t.write "<p>";
    );
        
    if 
        (t.started_text && (not t.inside_header)) ||
        (t.inside_header && (CS.head t.stack <> Some `header)) then (

        let debug = debugstr t location "Text" in
        let pcdata = sanitize_pcdata line in
        if location.Signatures.s_line > t.current_line then (
            t.write (~% "%s%s\n" debug pcdata);
            t.current_line <- location.Signatures.s_line;
        ) else (
            t.write (~% "%s%s" debug pcdata);
        )
    ) else (
        if
            CS.head t.stack = Some `header
            && (not (Escape.is_white_space line))
        then (
            t.write (~% "<!-- IGNORED TEXT: %s -->" (sanitize_comments line));
        );

    )
)

let terminate t location = (
    t.write "</p>\n";
) 

let enter_verbatim t location args = (
    CS.push t.stack (`verbatim args);
    t.write "</p><pre>\n";
    t.current_line <- location.Signatures.s_line;
)
let exit_verbatim t location = (
    let env =  (CS.pop t.stack) in
    match env with
    | Some (`verbatim _) ->
        t.write "</pre><p>\n";
        t.current_line <- location.Signatures.s_line;
    | _ ->
        (* warning ? error ? anyway, *)
        failwith "Shouldn't be there, Parser's fault ?";
)

let handle_verbatim_line t location line = (
    let pcdata = sanitize_pcdata line in
    t.write (~% "%s\n" pcdata);
    t.current_line <- location.Signatures.s_line;
)

(* ==== Directly exported functions ==== *)

let header ?(title="") ?(comment="") () = (
    ~% "<!DOCTYPE html
    PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
    <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
    <!-- %s -->
    <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
    <title>%s</title>
    </head>
    <body>" (sanitize_comments comment) (sanitize_pcdata title)
)
let footer () = "</body>\n</html>\n"

