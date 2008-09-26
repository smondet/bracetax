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

(* TO help with compilation, the DummyPrinter: *)

let start_command t location name args =
    p (~% "%%%s[start %s(%s)]\n" (strstat location)
    name (String.concat ", " args))

let stop_command t location = 
    p (~% "%%%s[stop]\n" (strstat location))




