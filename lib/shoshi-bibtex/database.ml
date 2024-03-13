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

type t = BibBaseType.database

let empty : t = []
let add entry database = database @ [ entry ]

let of_lexing lexbuf =
  BibParsing.parse lexbuf
  @@ BibParser.Incremental.bibtex_database lexbuf.lex_curr_p

let of_string content =
  let lexbuf = Lexing.from_string content in
  of_lexing lexbuf

let of_file file =
  In_channel.with_open_bin file (fun ic ->
      let lexbuf = Lexing.from_channel ic in
      of_lexing lexbuf)

let to_string database = 
  String.concat "\n\n" @@ List.map BibEntry.to_string database
