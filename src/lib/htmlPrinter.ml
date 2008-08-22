
type t = {
    stack: GrammarStack.t;
    write: string -> unit;
    mutable current_line: int;
}
module GS = GrammarStack

let (~%) = Printf.sprintf
let p = print_string

let create ~write = {stack = GS.empty (); write = write; current_line = 1;}

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

let start_command t location name args = ()
let stop_command t location = () 
let terminate t location = (
    t.write "\n";
) 

let enter_verbatim t location args = (
    t.write "<pre>\n";
    t.current_line <- location.Signatures.s_line;
)
let exit_verbatim t location = (
    t.write "</pre>\n";
    t.current_line <- location.Signatures.s_line;
)
let handle_verbatim_line t location line = (
    let pcdata = sanitize_pcdata line in
    t.write (~% "%s\n" pcdata);
    t.current_line <- location.Signatures.s_line;
)


