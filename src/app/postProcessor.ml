let pr = Printf.printf

type plugout = {
    tag: string;
    begin_handler: unit -> string option;
    line_handler: string -> string option;
    end_handler: unit -> string option;
}

let line_matches_begin tag line = (
    let latex = "%verbatimbegin:" ^ tag in
    let html = "<!--verbatimbegin:" ^ tag ^ " -->" in
    (line = latex) || (line = html)
)
let line_matches_end tag line = (
    let latex = "%verbatimend:" ^ tag in
    let html = "<!--verbatimend:" ^ tag ^ " -->" in
    (line = latex) || (line = html)
)
let line_is_verbatim_begin line = (
    let latex = "\\begin{verbatim}" in
    let html = "<pre>" in
    (line = latex) || (line = html)
)
let line_is_verbatim_end line = (
    let latex = "\\end{verbatim}" in
    let html = "</pre>" in
    (line = latex) || (line = html)
)

let begins tags line1 line2 = (
    let tag_matches tag =
        (* pr "Trying: %s Vs \"%s\" and \"%s\"\n" tag line1 line2; *)
        (line_matches_begin tag line1) && (line_is_verbatim_begin line2) in
    let rec test_tags =
        function
        | [] -> None
        | a :: b when tag_matches a.tag -> Some a
        | a :: b -> test_tags b
    in
    test_tags tags
)
let ends tag line1 line2 = (
    (line_matches_end tag.tag line2) && (line_is_verbatim_end line1)
)

type currrent_state = [ `writing | `read_begin | `read_verbend ]

let option_do ~opt ~some ~none = (
    match opt with
    | Some s -> some s
    | None -> none ()
)

let read_two_lines_or_write read write = (
    begin match read () with
    | None -> None
    | Some s ->
        begin match read () with
        | None -> write s; None
        | Some t -> Some (s,t)
        end
    end
)
let opt_unit_map ~f = function None -> () | Some s -> f s

let process plugouts readline writeline = (

    let f = writeline in (* f for opt_unit_map *)
    let rec look_for_begin line1 line2 = 
        match begins plugouts line1 line2 with
        | Some po ->
            opt_unit_map ~f (po.begin_handler ());
            begin match read_two_lines_or_write
                readline (fun s -> opt_unit_map ~f (po.line_handler s)) with
            | Some (line3, line4) ->
                look_for_end po line3 line4
            | None -> ()
            end;
        | None ->
            writeline line1;
            option_do ~opt:(readline ())
                ~some:(look_for_begin line2) ~none:(fun () -> writeline line2);
    and look_for_end po line1 line2 = 
        match ends po line1 line2 with
        | true ->
            opt_unit_map ~f (po.end_handler ());
            begin match read_two_lines_or_write readline writeline with
            | Some (line3, line4) ->
                look_for_begin line3 line4
            | None -> ()
            end;
        | false ->
            opt_unit_map ~f (po.line_handler line1);
            option_do ~opt:(readline ()) ~some:(look_for_end po line2)
                ~none:(fun () -> opt_unit_map ~f (po.line_handler line2));
    in
    begin match read_two_lines_or_write readline writeline with
    | Some (line1, line2) ->
        look_for_begin line1 line2
    | None -> ()
    end;
)

module BuiltIn = struct

    type available = [
        | `debug
        | `inline_latex
    ]

    let debug_postpro = 
        let pr = Printf.printf in
        {
            tag = "debug";
            begin_handler =
                (fun () -> pr "------------ BEGIN DEBUG SECTION --------\n"; None);
            line_handler = (fun s -> pr "----- %s\n" s ; None);
            end_handler = 
                (fun () -> pr "------------ END DEBUG SECTION --------\n"; None);
        }
    let inlinelatex_postpro =
        {
        tag = "latex";
        begin_handler = (fun () -> Some "% < inline latex >");
        line_handler = (fun s -> Some s);
        end_handler = (fun () -> Some "% </ inline latex >");
    }

    let postpro_of_variant (v:available) = (
        match v with
        | `debug -> debug_postpro
        | `inline_latex -> inlinelatex_postpro
    )

    let make_list = List.map postpro_of_variant

end

