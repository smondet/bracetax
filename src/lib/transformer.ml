
module FunctorMake =
functor (Printer: Signatures.PRINTER) -> struct
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
    (FunctorMake: functor (Printer: Signatures.PRINTER) -> (Signatures.TRANSFORMER))
    (* with type t = FunctorMake.t)) *)
