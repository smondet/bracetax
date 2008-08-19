let (~%) = Printf.sprintf
let p = print_string

module DummyPrinter = struct
    type t = int

    let create ~write = 0
    let print t f s = output_string f (~% "[%d]%s\n" t s)

end

let () = (
    let module DummyTransformer = Transformer.Make(DummyPrinter) in
    let t = DummyTransformer.create ~write:(fun s -> ()) ~read:(fun () -> None) in
    DummyTransformer.do_transformation t;
    p (~% "Done;\n")
)
