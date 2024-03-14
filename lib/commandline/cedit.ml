(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of shoshi: a little bibtex manager                                       *)
(* Copyright (C) 2024 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* shoshi is free software: you can redistribute it and/or modify it under the terms          *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* shoshi is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;        *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with shoshi.       *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

open Cmdliner

let name = "edit"

type t = unit

let term_cmd run =
  let combine () = run () in
  Term.(const combine $ const ())

let doc = "Edit the bibliography"

let man =
  [
    `S Manpage.s_environment;
    `I
      ( Printf.sprintf "$(b,%s)" Libshoshi.Config.ShoshiEnv.shoshi_editor,
        {|If set, $(mname) tries to open the bibtex database with program set by the variable |}
      );
    `I
      ( Printf.sprintf "$(b,%s)" Libshoshi.Config.ShoshiEnv.shoshi_editor_options,
        Printf.sprintf
          {|If set and %s is set, give a spaced-separated list args to %s|}
          Libshoshi.Config.ShoshiEnv.shoshi_editor
          Libshoshi.Config.ShoshiEnv.shoshi_editor );
  ]

let cmd run =
  let info = Cmd.info ~doc ~man name in
  Cmd.v info @@ term_cmd run

let try_shoshi_editor () =
  let options =
    Sys.getenv_opt Libshoshi.Config.ShoshiEnv.shoshi_editor_options
    |> Option.map (String.split_on_char ' ')
    |> Option.fold ~none:[] ~some:Fun.id
  in
  let editor = Sys.getenv_opt Libshoshi.Config.ShoshiEnv.shoshi_editor in
  match editor with
  | Some editor -> (
      let args = (editor :: options) @ [ Libshoshi.Config.shoshi_bibtex ] in
      let args = Array.of_list args in
      try Unix.execvp editor args with _ -> ())
  | None -> ()

let run cmd =
  let () = Libshoshi.Config.check_shoshi_initialiazed () in
  let () = cmd in

  let pid = try Unix.fork () with _ -> failwith "Unable to fork" in
  match pid with
  | 0 ->
      let () = try_shoshi_editor () in
      let () =
        Array.iter
          (fun editor ->
            try Unix.execvp editor [| editor; Libshoshi.Config.shoshi_bibtex |]
            with _ -> ())
          Libshoshi.Config.shoshi_knwon_editors
      in
      let () = prerr_endline "No editor found" in
      Unix._exit 1
  | _ ->
      let _, _ = Unix.wait () in
      ()

let command = cmd run
