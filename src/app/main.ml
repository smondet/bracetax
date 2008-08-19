let (~%) = Printf.sprintf

module DummyPrinter = struct
    type t = int

    let create () = 0
    let print t f s = output_string f (~% "[%d]%s\n" t s)

end

let () = (
    let module DummyTransformer = Transformer.Make(DummyPrinter) in
    let t = DummyTransformer.create () in
    DummyTransformer.transform t stdin stdout;
)
