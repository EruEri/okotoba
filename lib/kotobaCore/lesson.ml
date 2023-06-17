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