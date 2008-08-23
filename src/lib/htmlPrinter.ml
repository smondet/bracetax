
type t = {
    stack: Commands.Stack.t;
    write: string -> unit;
    mutable current_line: int;
}
module CS = Commands.Stack

let (~%) = Printf.sprintf
let p = print_string

let create ~write = {stack = CS.empty (); write = write; current_line = 1;}

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

let start_environment t location name args = (
    let module C = Commands.Names in
    let cmd =
        match name with
        | s when s = C.quotation        ->
            let op, clo = quotation_open_close args in
            t.write op;
            `quotation (op, clo)
        | s when s = C.italic           ->  `italic
        | s when s = C.bold             ->  `bold           
        | s when s = C.mono_space       ->  `mono_space          
        | s when s = C.under_line       ->  `under_line          
        | s when s = C.superscript      ->  `superscript          
        | s when s = C.subscript        ->  `subscript          
        | s -> `unknown (s, args)
    in
    CS.push t.stack cmd;
)


let start_command t location name args = (
    p (~% "Command: \"%s\"\n" name);
    match Commands.non_env_cmd_of_name name args with
    | `unknown (name, args) -> start_environment t location name args
    | cmd -> CS.push t.stack cmd
)
let stop_command t location = (
    match CS.pop t.stack with
    | Some `paragraph -> t.write "<p/>"
    | Some `new_line -> t.write "<br/>"
    | Some `non_break_space -> t.write "&nbsp;"
    | Some `open_brace -> t.write "{"
    | Some `close_brace -> t.write "}"
    | Some `sharp -> t.write "#"
    | Some (`utf8_char i) -> t.write (~% "&#%d;" i)
    | Some (`quotation (op, clo)) -> t.write clo
    | _ -> ()
) 

let terminate t location = (
    t.write "\n";
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


