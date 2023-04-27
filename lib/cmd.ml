
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

      let () = if not App.is_kotoba_word_file_exist (* Should always be true*) then
        let words = Word.empty in
        App.write_json words ()
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

end


module KotobaAdd = struct 
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
        App.kotoba_default_input)
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
        let words = App.kotoba_word_json () in
        let translations = translations |> List.map Word.create_translation in
        let added_kind, extented_words = Word.add (input_lang, word, translations) words in
        let () = App.write_json extented_words () in
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
    Cmd.group info [KotobaInit.cmd; KotobaAdd.cmd]

    let eval () = cmd |> Cmdliner.Cmd.eval

end