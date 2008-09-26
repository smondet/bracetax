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
    (* let patterns = [('<', "LT"); ('>', "GT"); ('&', "AMP"); ('-', "DASH")] in *)
    (* Escape.replace_chars ~src:line ~patterns *)

let sanitize_text line =
    let patterns = [
        ('$' , "\\$" );
        ('&' , "\\&" );
        ('%' , "\\%" );
        ('#' , "\\#" );
        ('{' , "\\{" );
        ('}' , "\\}" );
        ('_' , "\\_" );
        ('\\', "\textbackslash" );
        ('^' , "\textasciicircum" );
        ('~' , "\textasciitilde" );
    ] in
    Escape.replace_chars ~src:line ~patterns

    
(* TO help with compilation, the DummyPrinter: *)
let head = "####"

let handle_comment_line t location line =
    p (~% "%s%s[comment] \"%s\"\n" head (strstat location) line)
let handle_text t location line =
    p (~% "%s%s[text] \"%s\"\n" head (strstat location) line)

let start_command t location name args =
    p (~% "%s%s[start %s(%s)]\n" head (strstat location)
    name (String.concat ", " args))

let stop_command t location = 
    p (~% "%s%s[stop]\n" head (strstat location))

let terminate t location = 
    p (~% "%s%s[This is the end...]\n" head (strstat location))

let handle_verbatim_line t location line =
    p (~% "%s%s[verbatim] %s\n" head (strstat location) line)

let enter_verbatim t location args = ()
let exit_verbatim t location = ()

let dummy_writer = {
    Signatures.w_write = (fun s -> ());
    Signatures.w_warn = (fun s -> ());
    Signatures.w_error = (fun s -> ());
}


