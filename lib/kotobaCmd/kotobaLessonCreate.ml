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

open Cmdliner

let command_name = "create"

type command = {
  numero: int option;
  name: string;
}

let numero_term =
  Arg.(
    value & opt (some int) None & info ["n"; "numero"] ~doc:"numero of the lesson" ~docv:"no_lesson"
  )
let name_term =
  Arg.(
    required & Arg.pos 0 (Arg.some string) None & info [] ~docv:"lesson name"
  )

let combine fcommand = 
  let combine numero name = 
    fcommand {numero; name}
  in
  Term.(
    const combine
    $ numero_term
    $ name_term
  )

let create_doc = "Create a lesson"

let create_man = [
  `S Manpage.s_description;
  `P "Create a new lesson by giving its name and an optional number";
  `P "After that, a wizard will help you create your lesson";
  `P "A lesson is a list of definition";
  `Noblank;
  `P "A definition is:";
  `I ("- $(b,a main word)", "which represent the word that you want to learn");
  `Noblank;
  `I ("- $(b,alternatives)", "which represent the others writting for the main word");
  `Noblank;
  `I ("- $(b,meaning)", "which represent the meaning of the main word");
  `Noblank;
  `I ("- $(b,sample)", "an optional audio")
]

let create_lesson command = 
  let () = ignore command in
  ()

let command = 
  let info = 
    Cmd.info command_name
    ~doc:create_doc
    ~man:create_man
  in
  Cmd.v info @@ combine create_lesson