
type environnement =
    | Comment
    | Bold
    | Italic
    | Type
    | Section of (int * string)

type state = {
    s_line: int;
    s_char: int;
    s_stack: environnement list;
}

module type PRINTER = sig
    type t
    val create: write:(string -> unit) -> t
    (* val handle_comment_line: t -> state -> string -> unit *)
end


module type TRANSFORMER = sig
    type t
    val create: read:(unit -> string option) -> write:(string -> unit) -> t
    val do_transformation: t -> unit
end



