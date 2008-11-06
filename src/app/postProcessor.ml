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

module Shell = struct

    let string_of_command cmd = (
        let o = Unix.open_process_in cmd in
        let b = Buffer.create 1024 in
        begin try 
            while true do Buffer.add_char b (input_char o) done
        with End_of_file -> () end;
        Buffer.contents b;
    )

    let pipe_process input_str cmd = (
        let ich, och = Unix.open_process cmd in
        let dic, doc, cid, cod =
            Unix.descr_of_in_channel, Unix.descr_of_out_channel,
            Unix.in_channel_of_descr, Unix.out_channel_of_descr in
        let i, o = dic ich, doc och in
        let b = Buffer.create 1024 in
        let s = String.make 4 'B' in
        let written = ref 0 in
        let o_closed = ref false in
        let i_closed = ref false in
        while not !i_closed do
            let to_write =
                if !written >= (String.length input_str) then (
                    if not !o_closed then (
                        Unix.close o;
                        o_closed := true;
                        (* pr "Closed output\n%!"; *)
                    );
                    []
                ) else (
                    [o]
                )
            in
            begin match Unix.select [i] to_write [] 10.0 with
            | [ii],_,_ -> 
                if Unix.read ii s 0 1 = 1 then (
                    Buffer.add_char b s.[0];
                    (* pr "read: %s\n" (Buffer.contents b); *)
                ) else (
                    Unix.close i;
                    i_closed := true;
                );
            | [], [oo], _ ->
                if Unix.write oo input_str !written 1 = 1 then (
                    incr written;
                    (* pr "wrote: %s\n" (String.sub input_str 0 !written); *)
                ) else (
                    pr "Didn't write at all\n";
                );
            | _, _, _ ->
                failwith "Don't understand Unix.select behaviour";
            end;
        done;
        (Buffer.contents b)

    )

end
module BuiltIn = struct

    type format_type = [ `LaTeX | `HTML | `Unknown ]
    type io_format = format_type * format_type
    type available = [
        | `debug
        | `inline_latex
        | `inline_html
        | `shell of io_format * string * string * string * string
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

    let inlinelatex_postpro = {
        tag = "latex";
        begin_handler = (fun () -> Some "% < inline latex >");
        line_handler = (fun s -> Some s);
        end_handler = (fun () -> Some "% </ inline latex >");
    }

    let unsanitize_html_pre src = (
        let t = ref src in
        let module Esc = Bracetax.Escape in
        t := Esc.replace_string ~src:!t ~find:"&lt;" ~replace_with:"<";
        t := Esc.replace_string ~src:!t ~find:"&gt;" ~replace_with:">";
        t := Esc.replace_string ~src:!t ~find:"&amp;" ~replace_with:"&";
        !t
    )

    let inlinehtml_postpro = {
        tag = "html";
        begin_handler = (fun () -> Some "<-- inline html -->");
        line_handler = (fun s -> Some (unsanitize_html_pre s));
        end_handler = (fun () -> Some "<!-- inline html -->");
    }

    let unsanitize kind str = (
        match kind with
        | `HTML, `LaTeX -> 
            (* HTML is in a <pre>, and translation will be LaTeX *)
            unsanitize_html_pre str
        | _ -> (* TODO *) str
    )

    let shell_postpro io_form t b l e =
        let rm_last_backslash_n s =
            let l = String.length s in
            if s.[l - 1] = '\n' then String.sub s 0 (l - 1) else s
        in
        let mem = ref [] in
        {
            tag = t;
            begin_handler = (fun () ->
                mem := [];
                Some (rm_last_backslash_n (Shell.string_of_command b))
            );
            (* XXX must unsanitize HTML -> LaTeX *)
            line_handler = (fun s ->
                mem := (unsanitize io_form s) :: !mem;
                None
            );
            end_handler = (fun () ->
                let filtered = 
                    (String.concat "\n" (List.rev !mem)) ^ "\n" in
                Some (
                    (Shell.pipe_process filtered l)
                    ^ (rm_last_backslash_n (Shell.string_of_command e))
                )
            );
        }

    let postpro_of_variant (v:available) = (
        match v with
        | `debug -> debug_postpro
        | `inline_latex -> inlinelatex_postpro
        | `inline_html -> inlinehtml_postpro
        | `shell (io_form, tag, b, l, e) ->
            shell_postpro io_form tag b l e
    )

    let make_list = List.map postpro_of_variant
    let all = 
        make_list [
            `debug; `inline_latex; `inline_html;
            `shell ( 
                (`HTML, `LaTeX),
                "hevea", "echo '<!-- begin HEVEA -->'",
                "hevea", "echo '<!-- end HEVEA -->'")
        ]

end

