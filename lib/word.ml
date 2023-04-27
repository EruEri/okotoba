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

type translation = {
  output_lang: string;
  translation: string
}
[@@deriving yojson]

type word = {
  input_lang: string;
  word: string;
  translations: translation list;
}
[@@deriving yojson]

type words = word list
[@@deriving yojson]

module TranslationSet = Set.Make(struct type t = translation let compare = Stdlib.compare end)

let empty : words = []

let replace_translation translations word = 
  {
    word with translations = translations
  }

let find_word_opt (il, word) words = 
  words |> List.find_opt (fun w -> w.word = word && w.input_lang = il)

let create_translation (output_lang, translation) =
  {
    output_lang = String.trim output_lang;
    translation = String.trim translation
  }

let add (il, word, trans) words = 
  match find_word_opt (il, word) words with
  | None -> 
    let word = {input_lang = il; translations = trans; word} in
    `Added, word::words
  | Some word -> 
    let comming_translation_set = TranslationSet.of_list trans in
    let existing_translation_set = TranslationSet.of_list word.translations in
    let translations_set = TranslationSet.union comming_translation_set existing_translation_set in
    let extended_word = replace_translation (TranslationSet.elements translations_set) word in
    `Extended(TranslationSet.cardinal translations_set - List.length trans), extended_word::words
