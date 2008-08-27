let (~%) = Printf.sprintf
let p = print_string

module DummyPrinter = struct
    type t = int

    let create ~write = 0

    let strstat s = (~% "[%d:%d]" s.Signatures.s_line s.Signatures.s_char)
    let head = "####"
    
    let handle_comment_line t location line =
        p (~% "%s%s[comment] \"%s\"\n" head (strstat location) line)
    let handle_text t location line =
        p (~% "%s%s[text] \"%s\"\n" head (strstat location) line)

    let start_command t location name args =
        p (~% "%s%s[start %s(%s)]\n" head (strstat location)
            name (String.concat ", " args))

    let stop_command t location = 
        p (~% "%s%s[stop]\n" head (strstat location))
        
    let terminate t location = 
        p (~% "%s%s[This is the end...]\n" head (strstat location))

    let handle_verbatim_line t location line =
        p (~% "%s%s[verbatim] %s\n" head (strstat location) line)

    let enter_verbatim t location args = ()
    let exit_verbatim t location = ()
end
module Options = struct

    (* let arg_set ref v = Arg.Unit (fun () -> ref := v) *)

    let output_html = ref true
    let input_stream = ref stdin
    let output_stream = ref stdout
    let debug = ref false

    let options = Arg.align [
        ("-html", Arg.Set output_html, ~% " The output format (now only HTML)");
        ("-debug", Arg.Set debug, " Debug mode");
        (
            "-i",
            Arg.String (fun s -> input_stream := open_in s), 
            "<file> input file (default is standard input)"
        );
        (
            "-o",
            Arg.String (fun s -> output_stream := open_out s), 
            "<file> output file (default is standard output)"
        );

    ]
    let short_usage =
        ~% "usage: %s [-i file] [-o file] [-help]" Sys.argv.(0)
    let anon_fun s =
        prerr_string (~% "Warning argument %s is ignored\n" s)

    let get () =
        Arg.parse options anon_fun short_usage;
        (!output_html, !debug, !input_stream, !output_stream)


end

let read_line_opt i () = try Some (input_line i) with e -> None


let () = (
    let _, dbg, i, o = Options.get () in
    if dbg then (
        let module DummyTransformer = Transformer.Make(DummyPrinter) in
        let t =
            DummyTransformer.create
                ~write:(fun s -> ())
                ~read:(read_line_opt i) in
        DummyTransformer.do_transformation t;
        p (~% "DEBUG Done;\n");
    ) else (
        let module HtmlTransformer = Transformer.Make(HtmlPrinter) in
        let t =
            HtmlTransformer.create
                ~write:(output_string o )
                ~read:(read_line_opt i)
        in
        HtmlTransformer.do_transformation t;
    );
    close_in i;
    close_out o;
)

