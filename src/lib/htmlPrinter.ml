
type t = {
    stack: Commands.Stack.t;
    write: string -> unit;
    mutable current_line: int;
}
module CS = Commands.Stack

let (~%) = Printf.sprintf
let p = print_string

let create ~write = {stack = CS.empty (); write = write; current_line = 0;}

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

let start_environment ?(is_begin=false) t location name args = (
    let module C = Commands.Names in
    let cmd name args =
        match name with
        | s when s = C.quotation        ->
            let op, clo = quotation_open_close args in
            t.write op;
            `quotation (op, clo)
        | s when s = C.italic           -> t.write "<i>"  ; `italic
        | s when s = C.bold             -> t.write "<b>"  ; `bold
        | s when s = C.mono_space       -> t.write "<tt>" ; `mono_space
        | s when s = C.superscript      -> t.write "<sup>"; `superscript
        | s when s = C.subscript        -> t.write "<sub>"; `subscript
        | s when (C.is_end s)           -> p "push end\n"; `cmd_end
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
let rec stop_command t location = (
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
                ()
            | None -> p (~% "Nothing to {end} there !!\n")
            end
        | `cmd_begin (nam, args) ->
            p (~% "cmd begin %s(%s)\n" nam (String.concat ", " args));
            start_environment ~is_begin:true t location nam args;
        | `paragraph -> t.write "</p><p>" (* TODO: unstack and restack ? *)
        | `new_line -> t.write "<br/>"
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


