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

module KError = KotobaCore.Error
module KApp = KotobaCore.App

let command = "init"

type cmd = {
  force: bool
}

let force_term = 
  Arg.( 
    value & flag & info ~doc:"force the initialisation of $(b,kotoba) by deleting older lessons even if it exists" ["f"; "force"]
  
  )

let init_term init_func = 
  let combine force = 
    init_func @@ {force}
  in

  Term.(
    const combine
    $ force_term
  )

  let cmd_init_doc = "Initialise kotoba for lesson"

  let cmd_man = [
    `S Manpage.s_description;
    `P "Initialise kotoba for lesson";
  ]

  let init_function cmd = 
    let {force} = cmd in
    let () = if KApp.is_kotoba_lesson_folder_exists () && not force then
      raise @@ KError.KotobaErrror KError.KotobaLessonFolderAlreadyExist
    in
    let () = if KApp.is_kotoba_lesson_folder_exists () && force then 
        Util.File.rmrf KApp.kotoba_lesson_dir ()
    in 
    let () = match KApp.is_kotoba_lesson_folder_exists () with
    | true -> ()
    | false -> begin
      let ( >== ) = Result.bind in
      let base = 
         if KApp.is_kotoba_folder_exist () then Result.ok () else 
          Result.map (fun _ -> ()) @@ Util.File.create_folder ~on_error:(KError.UnableToCreateFolder KApp.kotoba_dir) KApp.kotoba_dir 
      in
      let res = base >== fun () -> 
        Util.File.create_folder ~on_error:(KError.UnableToCreateFolder KApp.kotoba_lesson_dir) KApp.kotoba_lesson_dir
     in
     match res with
     | Ok _ -> ()
     | Error e -> raise @@ KError.KotobaErrror e
    end
  in
    (* let () = if KApp.is_kotoba_word_file_exist () && force then 
      Util.File.rmrf KApp.kotoba_word_file ()
    in *)

    (* let () = if not @@ KApp.is_kotoba_word_file_exist () (* Should always be true*) then
      let words = KotobaCore.Word.empty in
      KApp.write_json words ()
    in *)
    let () = Printf.printf "Kotoba Lesson initialized\n%!" in
    ()
    
    let command = 
      let info = 
        Cmd.info command
        ~doc:cmd_init_doc
        ~man:cmd_man
      in
      Cmd.v info (init_term init_function)