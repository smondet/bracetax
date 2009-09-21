#! /usr/bin/env ocamlrun ocaml
open Printf
#use "topfind";;

#directory "../_build/src/lib/";;

#load "Bracetax.cmo";;

let b2h = Bracetax.Transform.brtx_to_html;;
let b2l = Bracetax.Transform.brtx_to_latex;;

let writer =
    let error = function
        | `undefined s ->
            prerr_string (s ^ "\n")
        | `message ((_, gravity, _) as msg) ->
            prerr_string ((Bracetax.Error.to_string msg) ^ "\n") in
    Bracetax.Signatures.make_writer ~write:print_string ~error
;;

let input_char str =
    let b = ref 0 in
    let e = String.length str - 1 in
    fun () -> if !b > e then None else let c = str.[!b] in incr b; Some c

;;

let () = (
    printf "==== URL HOOKS ====\n"
    let url_hook = String.uppercase in
    b2h ~writer ~input_char:(input_char "{i|Italic {link link/to/|Thing}}") ~url_hook ();
    b2l ~writer ~input_char:(input_char "{i|Italic {link link/to/|Thing}}") ~url_hook ();
    printf "\n";
    let url_hook = (^) "local:" in
    b2h ~writer ~input_char:(input_char "{i|Italic {link link/to/|Thing}}") ~url_hook ();
    b2l ~writer ~input_char:(input_char "{i|Italic {link link/to/|Thing}}") ~url_hook ();
    printf "\n";
);;
