(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of okotoba: A little word translation keeper                             *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* okotoba is free software: you can redistribute it and/or modify it under the terms         *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* okotoba is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with okotoba.      *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)


type _ input_kind = 
| NoEmpty : string -> string input_kind
| AllowEmpty : string option input_kind

module CreateLession = struct
  let rec ask_line : type a. a input_kind  -> string -> unit -> a = 
    
    fun kind prompt () -> 
      let () = print_string prompt in
      let s = String.trim @@ read_line () in
      let is_empty = String.length s = 0 in
      match kind with
      | NoEmpty error_message -> 
        if is_empty then
          let () = print_endline error_message in
          ask_line kind prompt ()
        else s
      | AllowEmpty -> if not is_empty then Some s else None  
       

  let ask_main_word = 
    ask_line (NoEmpty "\"word\" can not be empty") "Enter the word to learn : " 
  let ask_single_alternative ~word () = 
    let prompt = Printf.sprintf "Enter an alternative to %s (empty mean skip) : " word in
    ask_line AllowEmpty prompt ()

  let rec ask_alternatives ~word () = 
    match ask_single_alternative ~word () with
    | None -> []
    | Some read -> 
      let nexts = ask_alternatives ~word () in
      read::nexts

  let ask_meaning ~word () = 
    let prompt = Printf.sprintf "Enter the definition of \"%s\" : " word in
    ask_line (NoEmpty "a meaning must be provided") prompt ()

  let ask_sample ~word () = 
    let () = ignore word in
    let prompt = Printf.sprintf "Path to the sample (default: empty) : " in
    ask_line AllowEmpty prompt ()

  let ask_definition () =
    let open Lesson in
    let word = ask_main_word () in
    let alternatives = ask_alternatives ~word () in
    let meaning = ask_meaning ~word () in
    let sample = ask_sample ~word () in
    {
      main_word = word;
      alternatives;
      translate = meaning;
      sample
    }

  let is_confirmed string = 
    let yes = ["y"; "Y"] in
    let no = ["n"; "N"] in
    if List.mem string yes then Some true
    else if List.mem string no then Some false
    else None
  
  let rec ask_while_confirmed validate_function ~prompt ~on_error () = 
    let content = ask_line (NoEmpty "") prompt () in
    match validate_function content with
    | Some b -> b
    | None -> 
      let () = print_endline on_error in
    ask_while_confirmed validate_function ~prompt ~on_error ()

  let rec ask_lesson () = 
    let content = ask_while_confirmed 
      is_confirmed
      ~prompt:"Add an word [y/N] : " 
      ~on_error:"Wrong response"
      ()
    in
    match content with
    | false -> []
    | true -> 
      let lword = ask_definition () in
      lword::(ask_lesson ())

    let ask_lesson () = 
      let definition = ask_definition () in
      let others = ask_lesson () in
      definition::others

end