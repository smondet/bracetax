

type location = {
    s_line: int;
    s_char: int;
}
type write_fun = string -> unit
type writer = {
    w_write: write_fun;
    w_warn: write_fun;
    w_error: write_fun;
}
let make_writer ~write ~warn ~error = (
    {
        w_write = write;
        w_warn = warn;
        w_error = error;
    }
)

module type PRINTER = sig
    type t
    val create: writer:writer -> t
    val handle_comment_line: t -> location -> string -> unit
    val handle_text: t -> location -> string -> unit
    val start_command: t -> location -> string -> string list -> unit
    val stop_command: t -> location -> unit
    val terminate: t -> location -> unit

    val enter_verbatim: t -> location -> string list -> unit
    val handle_verbatim_line: t -> location -> string -> unit
    val exit_verbatim: t -> location -> unit
end


module type TRANSFORMER = sig
    type t
    val create: read:(unit -> string option) -> writer:writer -> t
    val do_transformation: t -> unit
end



