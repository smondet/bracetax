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

(** Common types used for printers. *)

(**/**)
module Error = Bracetax_error
(**/**)

type writer = {
  w_write: string -> unit;
  w_error: Error.error -> unit;
}


let make_writer ~write  ~error = {
  w_write = write;
  w_error = error;
}

type printer = {
  print_comment: Error.location -> string -> unit;
  print_text:    Error.location -> string -> unit;
  enter_cmd:     Error.location -> string -> string list -> unit;
  leave_cmd:     Error.location -> unit;
  terminate:     Error.location -> unit;

  is_raw: string -> bool;
  default_raw_end: string -> string;
  enter_raw:     Error.location -> string -> string list -> unit;
  print_raw:     Error.location -> string -> unit;
  leave_raw:     Error.location -> unit;
  error: Error.error -> unit;
}
    (** The functions called by {!val:Parser.do_transformation}. For
        example, {!val:HtmlPrinter.build} outputs a {!type:printer}. *)


