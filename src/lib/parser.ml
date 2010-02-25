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

let str_of_raw_cmd : raw_t -> string = function
  | `bypass -> "bypass"
  | `code -> "code"
  | `text -> "text"
  | `ignore -> "ignore"

let raw_cmd_of_str: string -> raw_t = function
  | "bypass"   -> `bypass
  | "code" -> `code
  | "ignore" -> `ignore
  | "text" -> `text
  | s -> failwith (~% "Bad usage of raw_cmd_of_str: %S" s)

let is_raw_cmd c =
  (c = "bypass") || (c = "code") || (c = "ignore") || (c = "text")


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
let mv_loc location line = loc line location.Error.l_file

type t = {
    printer: Signatures.printer;
    read_fun: unit -> char option;
    deny_bypass: bool;
}

let rec parse_text t location = (
    let buf = Buffer.create 42 in
    let rec read_loop location = 
        match t.read_fun () with
        | None ->
            t.printer.print_text location (Buffer.contents buf);
            t.printer.terminate location;
        | Some '\n' | Some '\r' ->
            Buffer.add_char buf ' ';
            read_loop (incr_loc location)
        | Some '#' ->
            t.printer.print_text location (Buffer.contents buf);
            parse_comment t location
        | Some '{' ->
            t.printer.print_text location (Buffer.contents buf);
            parse_command t location
        | Some '}' ->
            t.printer.print_text location (Buffer.contents buf);
            Buffer.reset buf;
            t.printer.leave_cmd location;
            read_loop location
        | Some given_char ->
            Buffer.add_char buf given_char;
            read_loop location
    in
    read_loop location
)
and parse_comment t location = (
    let buf = Buffer.create 42 in
    let rec read_loop location = 
        match t.read_fun () with
        | None ->
            t.printer.print_comment location (Buffer.contents buf);
            t.printer.terminate location;
        | Some '\n' ->
            let comment_line = (Buffer.contents buf) in
            t.printer.print_comment location comment_line;
            let new_loc =
                try 
                    Scanf.sscanf comment_line "line %d %S"
                        (fun i s ->
                            Printf.eprintf "%d %s...\n" i s; (loc i s))
                with
                _ -> (try
                    Scanf.sscanf comment_line "line %d"
                        (fun i -> Printf.eprintf "%d same file...\n" i;
                        mv_loc location i)
                    with _ -> (incr_loc location)) in
            parse_text t new_loc
        | Some given_char ->
            Buffer.add_char buf given_char;
            read_loop location
    in
    read_loop location
)
and parse_command t location = (
    let buf = Buffer.create 42 in
    let cmd = ref [] in
    let rec read_loop location escaping = 
        match t.read_fun () with
        | None ->
            err t.printer location
              (`end_of_input_not_in_text "Reading Command");
            t.printer.terminate location;
        | Some '\\' ->
            if escaping then (
                Buffer.add_char buf '\\';
                read_loop location false
            ) else
                read_loop location true
        | Some c when c = ' ' || c = '\t' || c = '\n' || c = '\r' ->
            let loc = if c = '\n' then (incr_loc location) else location in
            if escaping then (
                Buffer.add_char buf c;
                read_loop loc false
            ) else (
                let str = Buffer.contents buf in
                if str <> "" then (
                    cmd := (Buffer.contents buf) :: !cmd;
                );
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
                | c :: tl when is_raw_cmd c ->
                    let endpat,args =
                        match tl with [] -> default_raw_end,[]
                        | h :: q ->
                            if check_end_pattern h then
                                h,q
                            else (
                                err t.printer location (`invalid_end_pattern h);
                                (default_raw_end, [])
                            ) in
                    let kind = 
                        if t.deny_bypass then `code else raw_cmd_of_str c in
                    t.printer.enter_raw location kind args;
                    parse_raw t location endpat;
                | q :: tl ->
                    t.printer.enter_cmd location q tl;
                    t.printer.leave_cmd location;
                    parse_text t location
            )
        | Some '|' ->
            if escaping then (
                Buffer.add_char buf '|';
                read_loop location false
            ) else (
                cmd := (Buffer.contents buf) :: !cmd;
                match List.rev (List.filter ((<>) "") !cmd) with
                | [] | "" :: [] ->
                    err t.printer location (`unknown_command "EMPTY CMD!!!");
                    parse_text t location
                | q :: tl ->
                    t.printer.enter_cmd location q tl;
                    parse_text t location
            )
        | Some given_char ->
            Buffer.add_char buf given_char;
            read_loop location false
    in
    read_loop location false
)
and parse_raw t location end_pattern = (
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
        match t.read_fun () with
        | None ->
            err t.printer location
                (`end_of_input_not_in_text "Reading Code/Bypass");
            t.printer.terminate location;
        | Some '\n' ->
            Buffer.add_char buf '\n';
            t.printer.print_raw location (Buffer.contents buf);
            Buffer.reset buf;
            read_loop (incr_loc location) false
        | Some given_char ->
            Buffer.add_char buf given_char;
            if try_pattern buf end_pattern then (
                let to_write =
                    let len = String.length end_pattern + 2 in
                    without_last_chars (Buffer.contents buf) len in
                t.printer.print_raw location to_write;
                t.printer.leave_raw location;
                parse_text t location
            ) else
                read_loop location false
    in
    read_loop location false
)


let do_transformation ?(deny_bypass=false) printer read_fun filename = (
    let parserator =
        {printer = printer; read_fun = read_fun; deny_bypass = deny_bypass} in
    parse_text parserator (loc 1 filename)
)
