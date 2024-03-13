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

let name = "init"

type t = { force : bool }

let term_force =
  Arg.(value & flag & info [ "f"; "force" ] ~doc:"Force the initialisation")

let term_cmd run =
  let combine force = run { force } in
  Term.(const combine $ term_force)

let doc = "Initizalize $(mname)"
let man = []

let cmd run =
  let info = Cmd.info ~doc ~man name in
  Cmd.v info @@ term_cmd run

let run t =
  let { force = delete } = t in

  let () =
    match Libshoshi.Config.initialize_shoshi ~delete () with
    | Ok () -> ()
    | Error message -> failwith message
  in

  let () = Printf.printf "%s initialized\n%!" Libshoshi.Config.shoshi_name in
  ()

let command = cmd run
