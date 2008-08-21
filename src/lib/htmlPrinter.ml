
type t = {
    stack: GrammarStack.t;
    write: string -> unit;
}
module GS = GrammarStack

let (~%) = Printf.sprintf
let p = print_string

let create ~write = {stack = GS.empty (); write = write;}

let strstat s = (~% "[%d:%d]" s.Signatures.s_line s.Signatures.s_char)

let sanitize_comments line = Escape.replace_string ~src:line ~find:"-->" ~replace_with:"XXX"

let handle_comment_line t location line = (
    t.write (~% "<!--%s-->\n" (sanitize_comments line)) (* TODO sanitize *)
)

let handle_text t location line = ()
let start_command t location name args = ()
let stop_command t location = () 
let terminate t location = () 
let handle_verbatim_line t location line args = ()


