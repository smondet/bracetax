(******************************************************************************)
(*      Copyright (c) 2008, 2009, Sebastien MONDET                            *)
(*                                                                            *)
(*      Permission is hereby granted, free of charge, to any person           *)
(*      obtaining a copy of this software and associated documentation        *)
(*      files (the "Software"), to deal in the Software without               *)
(*      restriction, including without limitation the rights to use,          *)
(*      copy, modify, merge, publish, distribute, sublicense, and/or sell     *)
(*      copies of the Software, and to permit persons to whom the             *)
(*      Software is furnished to do so, subject to the following              *)
(*      conditions:                                                           *)
(*                                                                            *)
(*      The above copyright notice and this permission notice shall be        *)
(*      included in all copies or substantial portions of the Software.       *)
(*                                                                            *)
(*      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*      EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*      OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*      NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT           *)
(*      HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,          *)
(*      WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING          *)
(*      FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR         *)
(*      OTHER DEALINGS IN THE SOFTWARE.                                       *)
(******************************************************************************)

open Signatures 

let (~%) = Printf.sprintf

let str_of_raw_cmd = function
    | `bypass -> "bypass"
    | `code -> "code"
let raw_cmd_of_str = function
    | "bypass"   -> `bypass
    | "code" -> `code
    | s -> failwith (~% "Bad usage of raw_cmd_of_str: %S" s)

let default_raw_end = "end"
let check_end_pattern pattern = (
    try
        String.iter (function
            | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' | '-' | ':'  -> ()
            | _ -> raise Not_found) pattern;
        true
    with Not_found ->
        false
)

let err pr loc typ = pr.error (Error.mk loc `error typ)
let loc line file = { Error.l_line = line; l_char = -1; l_file = file }
let incr_loc location = loc (location.Error.l_line + 1) location.Error.l_file

let rec parse_text printer read_fun location = (
    let buf = Buffer.create 42 in
    let rec read_loop location = 
        match read_fun () with
        | None ->
            printer.print_text location (Buffer.contents buf);
            printer.terminate location;
        | Some '\n' ->
            Buffer.add_char buf ' ';
            read_loop (incr_loc location)
        | Some '#' ->
            printer.print_text location (Buffer.contents buf);
            parse_comment printer read_fun location
        | Some '{' ->
            printer.print_text location (Buffer.contents buf);
            parse_command printer read_fun location
        | Some '}' ->
            printer.print_text location (Buffer.contents buf);
            Buffer.reset buf;
            printer.leave_cmd location;
            read_loop location
        | Some given_char ->
            Buffer.add_char buf given_char;
            read_loop location
    in
    read_loop location
)
and parse_comment printer read_fun location = (
    let buf = Buffer.create 42 in
    let rec read_loop location = 
        match read_fun () with
        | None ->
            printer.print_comment location (Buffer.contents buf);
            printer.terminate location;
        | Some '\n' ->
            printer.print_comment location (Buffer.contents buf);
            parse_text printer read_fun (incr_loc location)
        | Some given_char ->
            Buffer.add_char buf given_char;
            read_loop location
    in
    read_loop location
)
and parse_command printer read_fun location = (
    let buf = Buffer.create 42 in
    let cmd = ref [] in
    let rec read_loop location escaping = 
        match read_fun () with
        | None ->
            err printer location (`end_of_input_not_in_text "Reading Command");
            printer.terminate location;
        | Some '\\' ->
            if escaping then (
                Buffer.add_char buf '\\';
                read_loop location false
            ) else
                read_loop location true
        | Some c when c = ' ' || c = '\t' || c = '\n' ->
            let loc = if c = '\n' then (incr_loc location) else location in
            if escaping then (
                Buffer.add_char buf c;
                read_loop loc false
            ) else (
                cmd := (Buffer.contents buf) :: !cmd;
                Buffer.reset buf;
                read_loop loc false
            )
        | Some '}' ->
            if escaping || (Buffer.length buf = 0 && !cmd = []) then (
                Buffer.add_char buf '}';
                read_loop location false
            ) else (
                cmd := (Buffer.contents buf) :: !cmd;
                match List.rev !cmd with
                | [] ->
                    failwith "Shouldn't be here..."
                | c :: t when c = "{" || c = "}" || c = "#" ->
                    printer.print_text location c;
                    if t <> [] then (
                        printer.error (Error.mk location `warning
                            (`command_shouldnot_have_args c));
                    );
                    parse_text printer read_fun location
                | c :: t when
                    c = (str_of_raw_cmd `code) ||
                    c = (str_of_raw_cmd `bypass) ->
                    let endpat,args =
                        match t with [] -> default_raw_end,[]
                        | h :: q ->
                            if check_end_pattern h then
                                h,q
                            else (
                                err printer location (`invalid_end_pattern h);
                                (default_raw_end, [])
                            ) in
                    printer.enter_raw location (raw_cmd_of_str c) args;
                    parse_raw printer read_fun location endpat;
                | q :: t ->
                    printer.enter_cmd location q t;
                    printer.leave_cmd location;
                    parse_text printer read_fun location
            )
        | Some '|' ->
            if escaping then (
                Buffer.add_char buf '|';
                read_loop location false
            ) else (
                cmd := (Buffer.contents buf) :: !cmd;
                match List.rev !cmd with
                | [] ->
                    failwith "Shouldn't be here..."
                | "" :: [] ->
                    err printer location (`unknown_command "EMPTY CMD!!!");
                    parse_text printer read_fun location
                | q :: t ->
                    printer.enter_cmd location q t;
                    parse_text printer read_fun location
            )
        | Some given_char ->
            Buffer.add_char buf given_char;
            read_loop location false
    in
    read_loop location false
)
and parse_raw printer read_fun location end_pattern = (
    let buf = Buffer.create 42 in
    let last_chars str nb =
        let ls = String.length str in
        let i = ls - nb in
        String.sub str i nb in
    let without_last_chars str nb =
        let ls = String.length str in
        let l = ls - nb in
        String.sub str 0 l
    in

    let try_pattern buf patt =
        let lb = Buffer.length buf and lp = String.length patt in
        (lb >= lp + 2) &&
        ((last_chars (Buffer.contents buf) (lp+2)) = ("{"^patt^"}"))
    in

    let rec read_loop location escaping = 
        match read_fun () with
        | None ->
            err printer location
                (`end_of_input_not_in_text "Reading Code/Bypass");
            printer.terminate location;
        | Some '\n' ->
            Buffer.add_char buf '\n';
            printer.print_raw location (Buffer.contents buf);
            Buffer.reset buf;
            read_loop (incr_loc location) false
        | Some given_char ->
            Buffer.add_char buf given_char;
            if try_pattern buf end_pattern then (
                let to_write =
                    let len = String.length end_pattern + 2 in
                    without_last_chars (Buffer.contents buf) len in
                printer.print_raw location to_write;
                printer.leave_raw location;
                parse_text printer read_fun location
            ) else
                read_loop location false
    in
    read_loop location false
)


let do_transformation printer read_fun filename = (
    parse_text printer read_fun (loc 1 filename)
)
