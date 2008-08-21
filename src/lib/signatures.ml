

type location = {
    s_line: int;
    s_char: int;
}

module type PRINTER = sig
    type t
    val create: write:(string -> unit) -> t
    val handle_comment_line: t -> location -> string -> unit
    val handle_text: t -> location -> string -> unit
    val handle_verbatim_line: t -> location -> string -> string list -> unit
    val start_command: t -> location -> string -> string list -> unit
    val stop_command: t -> location -> unit
    val terminate: t -> location -> unit
end


module type TRANSFORMER = sig
    type t
    val create: read:(unit -> string option) -> write:(string -> unit) -> t
    val do_transformation: t -> unit
end



