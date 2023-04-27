
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

module KotobaInit = struct
  open Cmdliner
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
      let config_folder = Pathbuf.to_string App.config_path in
      let () = match Pathbuf.is_file_exists App.config_path with
      | true -> ()
      | false -> begin 
        match Pathbuf.File.create_folder ~on_error:(App.Error.UnableToCreateFolder config_folder) App.config_path with
        | Ok _ -> ()
        | Error e -> raise @@ App.Error.KotobaErrror e
      end in
      let () = if App.is_kotoba_folder_exist && not force || App.is_kotoba_word_file_exist && not force then
        raise @@ App.Error.KotobaErrror App.Error.KotobaFolderAlreadyExist
      in
      let () = if App.is_kotoba_word_file_exist && force then 
        Pathbuf.File.rmrf (Pathbuf.to_string App.kotoba_words_file) ()
      in

      let () = if not App.is_kotoba_folder_exist then 
        match Pathbuf.File.create_folder ~on_error:(App.Error.UnableToCreateFolder (Pathbuf.to_string App.kotoba_path)) App.kotoba_path with
        | Ok _ -> ()
        | Error e -> raise @@ App.Error.KotobaErrror e
      in

      let () =  if App.is_kotoba_word_file_exist (* Should always be true*) then
        match Pathbuf.File.create_file ~on_error:(App.Error.UnableToCreateFile (Pathbuf.to_string App.kotoba_words_file)) App.kotoba_words_file with
        | Ok _ -> ()
        | Error e -> raise @@ App.Error.KotobaErrror e
      in
      let () = Printf.printf "Hisoka initialized\n%!" in
      ()
      
      let cmd = 
        let info = 
          Cmd.info command
          ~doc:cmd_init_doc
          ~man:cmd_man
        in
        Cmd.v info (init_term init_function)

end


module KotobaAdd = struct 
  let command = "add"

end

module Main = struct
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
    Cmd.group info [KotobaInit.cmd]

    let eval () = cmd |> Cmdliner.Cmd.eval

end