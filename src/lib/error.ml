(******************************************************************************)
(*      Copyright (c) 2008, Sebastien MONDET                                  *)
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

type gravity = [
    | `fatal_error
    | `error
    | `warning
]

type message = [
    | `transformer_lost of string
    | `ignored_text_after_verbatim_begin of string (* the ignored text *)
    | `malformed_verbatim_begin
    | `cell_out_of_table
    | `cell_inside_cell
    | `unknown_command of string
    | `begin_without_arg
    | `non_matching_end
    | `closing_brace_matching_begin
    | `nothing_to_end_with_brace
    | `item_out_of_list
    | `terminating_with_open_environments of string list
    | `bad_size_specification_in_image of string
    | `unknown_list_style of string
    | `end_of_input_not_in_text of string
    | `invalid_end_pattern of string
    | `command_shouldnot_have_args of string
    | `unknown_quotation_style of string
]

type location = {
    l_line: int;
    l_char: int;
    l_file: string;
}

type error = [
    | `message of location * gravity * message
    | `undefined of string (* Temporary, will be removed progressively *)
]

type error_fun = error -> unit

let mk location gravity message =
    (`message (location, gravity, message): error)

(* An built-in error to string function *)
let to_string (location, gravity, message) = (
    let spr = Printf.sprintf in
    let gravity_str = 
        match gravity with
        | `fatal_error -> "FATAL ERROR"
        | `error -> "ERROR"
        | `warning -> "Warning"
    in
    let message_str =
        match (message:message) with
        | `transformer_lost s -> 
            spr "Shouldn't be there (%s)" s
        | `ignored_text_after_verbatim_begin s ->
            spr "Text after {verbatim ...} will be ignored: %s" s
        | `malformed_verbatim_begin -> "Malformed begin of verbatim environment"
        | `cell_out_of_table -> "Cell (\"{c ...}\") command not in table"
        | `cell_inside_cell -> "Cell (\"{c ...}\") command inside another cell"
        | `unknown_command s -> spr "Unknown command: %s" s
        | `begin_without_arg -> "{begin} without argument"
        | `non_matching_end -> "{end} does not match a {begin}"
        | `closing_brace_matching_begin ->
            "Closing brace ('}') matches a {begin ...}"
        | `nothing_to_end_with_brace -> "Closing brace doesn't match"
        | `item_out_of_list -> "Item ({*}) not in a {list} environment}"
        | `terminating_with_open_environments l -> 
            spr "Reached end of input with opened environments: %s"
                (String.concat ", " l)
        | `bad_size_specification_in_image s ->
            spr "Bad size specification in image: \"%s\"" s
        | `unknown_list_style s ->
            spr "Unknown list syle: \"%s\"" s
        | `end_of_input_not_in_text s ->
            spr "Reached end of input in wrong state... \"%s\"" s
        | `invalid_end_pattern s ->
            spr "Invalid end pattern: \"%S\"" s
        | `command_shouldnot_have_args s ->
            spr "This command should not have arguments: %s" s
        | `unknown_quotation_style s ->
            spr "Unknown quotation style \"%s\" (using default)" s
    in
    Printf.sprintf "[%s:%d%s][%s] %s"
        location.l_file location.l_line 
        (if location.l_char = -1 then "" else (spr ",%d" location.l_char))
        gravity_str message_str
)



