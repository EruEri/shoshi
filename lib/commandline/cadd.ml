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

let name = "add"

type t = { rstdin : bool }

let term_rstdin =
  Arg.(
    value & flag & info ~doc:"Read the bibtex content from stdin" [ "stdin" ])

let term_cmd run =
  let combine rstdin = run { rstdin } in
  Term.(const combine $ term_rstdin)

let doc = "Add bibtex entry to the bibliography"
let man = []

let cmd run =
  let info = Cmd.info ~doc ~man name in
  Cmd.v info @@ term_cmd run

let run cmd =
  let () = Libshoshi.Config.check_shoshi_initialiazed () in
  let { rstdin } = cmd in
  match rstdin with
  | false -> ()
  | true ->
      let shoshi_database =
        match ShoshiBibtex.Database.of_file Libshoshi.Config.shoshi_bibtex with
        | Ok database -> database
        | Error _ -> failwith "Parsing error 1"
      in
      let database =
        match ShoshiBibtex.Database.of_lexing @@ Lexing.from_channel stdin with
        | Ok database -> database
        | Error _ -> failwith "Parsing error 2"
      in

      let shoshi_database = shoshi_database @ database in
      let () = Libshoshi.Config.save shoshi_database in
      ()

let command = cmd run
