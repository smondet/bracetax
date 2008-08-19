

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


module FunctorMake =
functor (Printer: PRINTER) -> struct
    type t = {
        t_printer: Printer.t;
    }
    let create () = {t_printer = Printer.create ()}
    let transform t fin fout = (
        let s = input_line fin in
        Printer.print t.t_printer fout s;
    )
end

module Make =
    (FunctorMake: functor (Printer: PRINTER) -> (TRANSFORMER))
    (* with type t = FunctorMake.t)) *)
