
type t = {
    stack: Commands.Stack.t;
    write: string -> unit;
    mutable current_line: int;
}
module CS = Commands.Stack

let (~%) = Printf.sprintf
let p = print_string

let create ~write = {
    stack = CS.empty ();
    write = write;
    current_line = 0;
}

let strstat s = (~% "[%d:%d]" s.Signatures.s_line s.Signatures.s_char)
let debugstr t s msg = 
    if true then
        (~% "<!--DEBUG:[%s] Loc:[%d;%d] CurLine:%d-->"
            msg s.Signatures.s_line s.Signatures.s_char t.current_line)
    else
        ""

let sanitize_comments line =
    Escape.replace_string ~src:line ~find:"-->" ~replace_with:"XXX"

let sanitize_pcdata line =
    let patterns = [('<', "&lt;"); ('>', "&gt;"); ('&', "&amp;")] in
    Escape.replace_chars ~src:line ~patterns

let sanitize_xml_attribute src =
    let patterns =
        [('<', "&lt;"); ('>', "&gt;"); ('&', "&amp;"); ('"', "&quot;")] in
    Escape.replace_chars ~src ~patterns


let handle_comment_line t location line = (
    t.write (~% "%s<!--%s-->\n" (debugstr t location "Comment")
        (sanitize_comments line));
    t.current_line <- t.current_line + 1;
)

let handle_text t location line = (
    if t.current_line = 0 then (
        t.write "<p>";
        t.current_line <- location.Signatures.s_line;
    );

    let debug = debugstr t location "Text" in
    let pcdata = sanitize_pcdata line in
    if location.Signatures.s_line > t.current_line then (
        t.write (~% "%s%s\n" debug pcdata);
        t.current_line <- location.Signatures.s_line;
    ) else (
        t.write (~% "%s%s" debug pcdata);
    )
)

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
    function `itemize -> "<ul>\n" | `numbered -> "<ol>\n"
let list_item = 
    function `itemize -> "</li>\n<li>" | `numbered -> "</li>\n<li>"
let list_firstitem = 
    function `itemize -> "<li>" | `numbered -> "<li>"
let list_stop = 
    function `itemize -> "</li>\n</ul>\n" | `numbered -> "</li>\n</ol>\n"

let section_start n l =
    let lsan = sanitize_xml_attribute l in
    ~% "<h%d><a name=\"%s\" id=\"%s\">" (n + 1) lsan lsan

let section_stop n l =
    ~% "</a></h%d>\n" (n + 1)

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
    let src, opts, lbl = Commands.Names.image_params args in
    let opts_str =
        let strs =List.map (function
        | `w w -> (~% "width=\"%d\""  w)
        | `h h -> (~% "height=\"%d\"" h)) opts in
        String.concat " " strs
    in
    let sansrc = sanitize_xml_attribute src in
    t.write (~%
        "<div class=\"figure\"><a href=\"%s\">\
        <img src=\"%s\" %s id=\"%s\" /></a><br/>"
        sansrc sansrc opts_str (sanitize_xml_attribute lbl)
    );
    `image (src, opts, lbl)
)
let image_stop = "</div>"

let header_start = "</p><div class=\"header\">"
let header_stop = "</div><p>"
let title_start = "<h1>"
let title_stop = "</h1>"
let authors_start = "<div class=\"authors\">"
let authors_stop = "</div>"
let subtitle_start = "<div class=\"subtitle\">"
let subtitle_stop = "</div>"

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
        | s when (C.is_end s)           -> p "push end\n"; `cmd_end
        | s when C.is_list s             ->
            let style, other_args, waiting =
                match args with
                | [] -> (`itemize, [], ref true)
                | h :: t -> (C.list_style h, t, ref true) in
            t.write (list_start style);
            `list (style, other_args, waiting)
        | s when C.is_item s -> p "push item"; `item
        | s when C.is_section s -> 
            let level, label = C.section_params args in
            t.write (section_start level label);
            `section (level, label)
        | s when C.is_link s -> (link_start t args)
        | s when C.is_image s -> image_start t args
        | s when C.is_header s -> t.write header_start; `header
        | s when C.is_title s -> t.write title_start; `title
        | s when C.is_subtitle s -> t.write subtitle_start; `subtitle
        | s when C.is_authors s -> t.write authors_start; `authors
        | s -> p (~% "unknown: %s\n" s); `unknown (s, args)
    in
    let the_cmd =
        if C.is_begin name then (
            match args with
            | [] -> p "Lonely begin !!"; (`cmd_begin ("", []))
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
    p (~% "Command: \"%s\"(%s)\n" name (String.concat ", " args));
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
                p (~% "{end} %s\n" (Commands.env_to_string benv));
                out_of_env benv
            | Some c ->
                p (~% "Warning {end} does not end a {begin...} but %s\n"
                    (Commands.env_to_string c));
                CS.push t.stack c;
            | None -> p (~% "Nothing to {end} there !!\n")
            end
        | `cmd_begin (nam, args) ->
            p (~% "cmd begin %s(%s)\n" nam (String.concat ", " args));
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
                p (~% "Warning {item} is not just under list but %s\n"
                    (Commands.env_to_string c));
                CS.push t.stack c;
            | None -> p (~% "Warning {item}... nothing to itemize !\n")
            end
        | `section (level, label) ->
            t.write (section_stop level label);
        | `link _ -> t.write "</a>";
        | `image _ -> t.write image_stop;
        | `header -> t.write header_stop;
        | `title -> t.write title_stop;
        | `subtitle -> t.write subtitle_stop;
        | `authors -> t.write authors_stop;
        | s -> p (~% "Unknown command... %s\n" (Commands.env_to_string s)); ()
    in
    match CS.pop t.stack with
    | Some env -> out_of_env env
    | None -> ()
) 

let terminate t location = (
    t.write "</p>\n";
) 

let enter_verbatim t location args = (
    CS.push t.stack (`verbatim args);
    t.write "<pre>\n";
    t.current_line <- location.Signatures.s_line;
)
let exit_verbatim t location = (
    let env =  (CS.pop t.stack) in
    match env with
    | Some (`verbatim _) ->
        t.write "</pre>\n";
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


