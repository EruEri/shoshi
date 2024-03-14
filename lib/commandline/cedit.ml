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
let man = []

let cmd run =
  let info = Cmd.info ~doc ~man name in
  Cmd.v info @@ term_cmd run

let run cmd =
  let () = Libshoshi.Config.check_shoshi_initialiazed () in
  let () = cmd in

  let pid = try Unix.fork () with _ -> failwith "Unable to fork" in
  match pid with
  | 0 ->
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
