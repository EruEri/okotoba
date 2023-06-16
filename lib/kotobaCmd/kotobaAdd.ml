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
let name = "add"

type cmd_add = {
  input_lang: string;
  word: string;
  translations: (string * string) list
}

let intput_lang_term = 
  Arg.( 
    required & opt (some & string) None
    & info 
      ~docv:("lang") 
      ~doc:("lang of the word")
      ~env:(Cmd.Env.info ~doc:"If this environment variable is present, \
      the $(b, --input-lang) is set to the value of $(env), See $(b, okotoba-add)"
      KotobaCore.App.kotoba_default_input)
      ["i"; "input-lang"]
  )

  let word_term = 
    Arg.( 
      required & pos 0 (some string) None & info ~docv:"WORD" ~doc:"word to translate" []
    )

  let translations_term =
    Arg.( 
      non_empty
      & pos_right 0 (Arg.pair string string) [] & info [] ~docv:"TRANSLATIONS" ~doc:"translations of the word, the first part is the lang of the \
      translation and the other part is the translation, splitted by a \",\" without space
      "
    )

  let add_term add_func = 
    let combine input_lang word translations = 
      add_func @@ {input_lang; word; translations} 
    in
    Term.( 
      const combine
      $ intput_lang_term
      $ word_term
      $ translations_term
    )

    let doc = "Add a word with its translations"

    let man = [
      `S Manpage.s_description;
      `P "Add a word with its translations";
    ]

    let add_function cmd = 
      let {input_lang; word; translations} = cmd in
      let input_lang = String.trim input_lang in
      let word = String.trim word in
      let words = KotobaCore.App.kotoba_word_json () in
      let translations = translations |> List.map KotobaCore.Word.create_translation in
      let added_kind, extented_words = KotobaCore.Word.add (input_lang, word, translations) words in
      let () = KotobaCore.App.write_json extented_words () in
      let () = match added_kind with
        | `Added -> Printf.printf "The word \"%s\" has been added\n%!" word
        | `Extended n -> Printf.printf "The word \"%s\"'s translations has been extended by %u\n%!" word n
      in
      ()

  let cmd = 
    let info = 
      Cmd.info name 
      ~doc ~man 
    in
    Cmd.v info (add_term add_function)