
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

let home_dir = Sys.getenv "HOME"

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


module Error = struct
  type kotoba_error = 
  | UnableToCreateFolder of string
  | UnableToCreateFile of string
  | KotobaFolderAlreadyExist

  exception KotobaErrror of kotoba_error
end
