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

let name = "merge"

type t = { bibtex : string; paper : string option }

let term_bibtex =
  Arg.(
    required
    & pos 0 (some string) None
    & info ~doc:"Bibtex file" ~docv:"<BIBTEX>" [])

let term_paper =
  Arg.(
    value & pos 1 (some string) None & info ~doc:"Paper file" ~docv:"<PAPER>" [])

let term_cmd run =
  let combine bibtex paper = run { bibtex; paper } in
  Term.(const combine $ term_bibtex $ term_paper)

let doc = "Merge bibtex with $(mname)"
let man = []

let cmd run =
  let info = Cmd.info ~doc ~man name in
  Cmd.v info @@ term_cmd run

let run cmd =
  let () = Libshoshi.Config.check_shoshi_initialiazed () in
  let { bibtex; paper = _ } = cmd in

  let database = match ShoshiBibtex.Database.of_file bibtex with
    | Ok databse -> databse
    | Error (Right position) -> Util.Position.print_position (Fun.id) position; failwith ""
    | Error e -> failwith @@ Printf.sprintf "Error while parsing: %u" @@ Obj.tag (Obj.repr e)
  in
  let () = Printf.printf "%s\n%!" @@ ShoshiBibtex.Database.to_string database in
  ()

let command = cmd run
