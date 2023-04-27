
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
end


let home_dir = Sys.getenv "HOME"

let kotoba_default_input = "KOTOBA_DEFAULT_INPUT_LANG"

let config_dir = ".config"
let kotoba_folder_name = "kotoba"

let word_file = "words.json"

let config_path = Pathbuf.from_list [home_dir; config_dir]
let kotoba_path = Pathbuf.push kotoba_folder_name config_path

let kotoba_words_file = Pathbuf.push word_file kotoba_path



let is_kotoba_folder_exist = 
  let path = Pathbuf.to_string kotoba_path in 
  Sys.file_exists path && Sys.is_directory path

let is_kotoba_word_file_exist = 
  let path = Pathbuf.to_string kotoba_words_file in
  Sys.file_exists kotoba_folder_name && not @@ Sys.is_directory path

let kotoba_word_json () = 
  let path = Pathbuf.to_string kotoba_words_file in
  let () = if not @@ Sys.file_exists path then 
    raise @@ Error.KotobaErrror (Error.UnexistingFile path)
  in 
  match Word.words_of_yojson @@ Yojson.Safe.from_file path with
  | Error _ -> raise @@ Error.(KotobaErrror (UnableToParseJson path) )
  | Ok json -> json

let write_json words () = 
  let path = Pathbuf.to_string kotoba_words_file in
  let () = if not @@ Sys.file_exists path then 
    raise @@ Error.KotobaErrror (Error.UnexistingFile path)
  in
  Out_channel.with_open_bin path (fun oc ->
    words |> Word.words_to_yojson |> Yojson.Safe.to_channel oc
  )