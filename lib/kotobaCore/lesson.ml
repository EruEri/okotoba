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

type lword = {
  main_word: string;
  alternatives: string list;
  translate: string;
  sample: string option;
}
[@@deriving yojson]

type difficulty = 
| Hard
| NotEasy
| KindOf
| Easy
[@@deriving yojson]

type score = {
  good: int;
  total_attemps: int
}
[@@deriving yojson]

type stat = {
  difficulty: difficulty;
  score: score;
  last_play: float;
  creation_instant: float;
}
[@@deriving yojson]

type definition = {
  word: lword;
  stat: stat
}
[@@deriving yojson]

type lesson = {
  name: string;
  numero: int option;
  definitions : definition list
}
[@@deriving yojson]

type lessons = lesson list
[@@deriving yojson]


let default_score = {
  good = 0;
  total_attemps = 0;
}

let default_stat = 
  let time = Unix.time () in 
  {
    difficulty = Hard;
    score = default_score;
    last_play = time;
    creation_instant = time
  }

let lesson_format_name ~numero lesson_name = 
  let lesson_name = lesson_name |> String.map (fun c -> if c = ' ' then '-' else c ) in
  let numero = numero |>  Option.value ~default:0 in
  Printf.sprintf "%02u-%s" numero lesson_name

let does_lesson_exist lesson_formatted = 
  let files = Sys.readdir (App.kotoba_lesson_dir) in
  Array.mem lesson_formatted files

let create_lesson ~numero lesson_name lwords =
  let name = lesson_format_name ~numero lesson_name in
  let () = if does_lesson_exist name then
    raise @@  App.Error.(kotoba_error @@ LessonAlreadyExist name) 
  in
  let full_path = Printf.sprintf "%s%s%s" App.kotoba_lesson_dir Filename.dir_sep name in

  let definitions = lwords |> List.map (fun lword -> 
    {
      word = lword;
      stat = default_stat
    }
  )
  in

  let lesson = 
    {
      name;
      numero;
      definitions
    }
  in

  let () = lesson |> lesson_to_yojson |> Yojson.Safe.to_file full_path in
  ()