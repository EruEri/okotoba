
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


module Error = struct
  type kotoba_error = 
  | UnableToCreateFolder of string
  | UnableToCreateFile of string
  | UnableToOpenFile of string
  | UnexistingFile of string
  | UnableToParseJson of string
  | KotobaFolderAlreadyExist

  exception KotobaErrror of kotoba_error

  let string_of_error = let open Printf in function
  | UnableToCreateFolder path -> sprintf "Unable to create the folder \"%s\"" path
  | UnableToCreateFile path -> sprintf "Unable to create the file \"%s\"" path
  | UnableToOpenFile path -> sprintf "Unable to open the file \"%s\"" path
  | UnexistingFile path -> sprintf "Unexisting file \"%s\"" path
  | UnableToParseJson path -> sprintf "Error while parsing the json file: %s" path
  | KotobaFolderAlreadyExist -> "Le ficher kotoba existe deja"

  let register_kotota_exn () = 
    Printexc.register_printer (function
    | KotobaErrror ke -> Option.some @@ string_of_error ke
    | _ -> None 
    )
end

let kotoba_default_input = "KOTOBA_DEFAULT_INPUT_LANG"

let config_dir = ".config"
let kotoba_folder_name = "kotoba"

let word_file = "words.json"

let kotoba_data_dir = 
  let xdg = Xdg.create ~env:Sys.getenv_opt () in
  let dir = Xdg.data_dir xdg in
  Printf.sprintf "%s%s%s" dir Filename.dir_sep kotoba_folder_name

let kotoba_word_file = 
  Printf.sprintf "%s%s%s" kotoba_data_dir Filename.dir_sep word_file
let is_kotoba_folder_exist () = 
  Sys.file_exists kotoba_data_dir && Sys.is_directory kotoba_data_dir

let is_kotoba_word_file_exist () = 
  Sys.file_exists kotoba_word_file && not @@ Sys.is_directory kotoba_word_file

let kotoba_word_json () = 
  let () = if not @@ Sys.file_exists kotoba_word_file then 
    raise @@ Error.KotobaErrror (Error.UnexistingFile kotoba_word_file)
  in 
  match Word.words_of_yojson @@ Yojson.Safe.from_file kotoba_word_file with
  | Error _ -> raise @@ Error.(KotobaErrror (UnableToParseJson kotoba_word_file) )
  | Ok json -> json

let write_json words () = 
  Out_channel.with_open_bin kotoba_word_file (fun oc ->
    words |> Word.words_to_yojson |> Yojson.Safe.to_channel oc
  )