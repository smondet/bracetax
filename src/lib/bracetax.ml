(******************************************************************************)
(*      Copyright (c) 2009, Sebastien MONDET                                  *)
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

(** This module is the {i legacy} top-level module of the bracetax library. *)

(**
This is the legacy interface of the bracetax library.
The modules are all modules are "packed" inside the
Bracetax module:
*)

module Error           = Bracetax_error
module Commands        = Bracetax_commands       
module Escape          = Bracetax_escape         
module HtmlPrinter     = Bracetax_HTML_printer    
module LatexPrinter    = Bracetax_latex_printer   
module GenericPrinter  = Bracetax_generic_printer 
module TOCOutput       = Bracetax_TOC_output      
module Parser          = Bracetax_parser         
module Transform       = Bracetax_transform      
module Signatures      = Bracetax_signatures     
module Info            = Bracetax_info
           
