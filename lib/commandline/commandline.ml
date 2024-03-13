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

let term_cmd run =
  let combine () = run () in
  Term.(const combine $ const ())

let name = Libshoshi.Config.shoshi_name
let version = Libshoshi.Config.version
let doc = "A command-line bibtex manager"

let man =
  [
    `S Manpage.s_description;
    `P "$(mname) is a command-line bibtex manager";
    `P
      "To use $(mname), you need to initialize it. Use the $(mname) init \
       subcommand";
  ]

let info = Cmd.info ~doc ~version ~man name
let subcommands = Cmd.group info [ Cinit.command; Cmerge.command ]

let eval () =
  (* let () = Libcithare.Error.register_cithare_error () in *)
  Cmd.eval ~catch:false subcommands
