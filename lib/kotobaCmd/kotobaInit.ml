(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of okotoba: A little word translation keeper                             *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* Hisoka is free software: you can redistribute it and/or modify it under the terms          *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* Hisoka is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;        *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with Hisoka.       *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

open Cmdliner

module KError = KotobaCore.Error
module KApp = KotobaCore.App

let command = "init"

type cmd_init = {
  force: bool
}

let force_term = 
  Arg.( 
    value & flag & info ~doc:"force the initialisation of $(b,kotoba) by deleting older files even if it exists" ["f"; "force"]
  
  )

let init_term init_func = 
  let combine force = 
    init_func @@ {force}
  in

  Term.(
    const combine
    $ force_term
  )

  let cmd_init_doc = "Initialise kotoba"

  let cmd_man = [
    `S Manpage.s_description;
    `P "Initialise kotoba";
  ]

  let init_function cmd = 
    let {force} = cmd in
    let () = if KApp.is_kotoba_folder_exist () && not force || KApp.is_kotoba_word_file_exist () && not force then
      raise @@ KError.KotobaErrror KError.KotobaFolderAlreadyExist
    in
    let () = match KApp.is_kotoba_folder_exist () with
    | true -> ()
    | false -> begin
      match Util.File.create_folder ~on_error:(KError.UnableToCreateFolder KApp.kotoba_data_dir) KApp.kotoba_data_dir with
      | Ok _ -> ()
      | Error e -> raise @@ KError.KotobaErrror e
    end in

    let () = if KApp.is_kotoba_word_file_exist () && force then 
      Util.File.rmrf KApp.kotoba_word_file ()
    in

    let () = if not @@ KApp.is_kotoba_word_file_exist () (* Should always be true*) then
      let words = KotobaCore.Word.empty in
      KApp.write_json words ()
    in
    let () = Printf.printf "Kotoba initialized\n%!" in
    ()
    
    let cmd = 
      let info = 
        Cmd.info command
        ~doc:cmd_init_doc
        ~man:cmd_man
      in
      Cmd.v info (init_term init_function)