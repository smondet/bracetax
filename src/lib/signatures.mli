
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



