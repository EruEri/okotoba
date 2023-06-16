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
let version = "0.1.0"

let name = "okotoba"

let doc = "kotoba is a little program to store word along with their translation"

let man = [
  `S Cmdliner.Manpage.s_description;
  `P "kotoba is a little program to store word along with their translation"
]

let cmd = 
  let info = 
    Cmd.info name
    ~doc
    ~man 
    ~version
  in
  let () = KotobaCore.Error.register_kotota_exn () in
  Cmd.group info [KotobaInit.cmd; KotobaAdd.cmd]

  let eval () = cmd |> Cmdliner.Cmd.eval