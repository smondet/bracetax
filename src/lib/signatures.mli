
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
    val create: unit -> t
    val print: t -> out_channel -> string -> unit
end


module type TRANSFORMER = sig
    type t
    val create: unit -> t
    val transform: t -> in_channel -> out_channel -> unit
end



