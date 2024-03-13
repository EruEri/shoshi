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

open Lexing

let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  (line_number, column)

let line_column_of_position p =
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  (line_number, column)

let line_of_position p =
  let line, _ = line_column_of_position p in
  line

let column_of_position p =
  let _, column = line_column_of_position p in
  column

type position = {
  start_position : Lexing.position;
  end_position : Lexing.position;
}

type 'a location = { value : 'a; position : position }

let end_to_start position =
  { position with end_position = position.start_position }

let map f location = { location with value = f location.value }
let map_use f location = { value = f location; position = location.position }
let value { value; _ } = value
let position { position; _ } = position
let assocs_value ({ value = v1; _ }, { value = v2; _ }) = (v1, v2)
let assoc_value_left ({ value = v1; _ }, v) = (v1, v)
let assoc_value_right (v, { value = v1; _ }) = (v, v1)

let print_position f { value; position } =
  let line, column = line_column_of_position position.start_position in
  Printf.printf "Line %d, Characher %d, %s" line column (f value)

let located_value start_position end_position value =
  { value; position = { start_position; end_position } }

let current_position lexbuf =
  { start_position = lexbuf.lex_start_p; end_position = lexbuf.lex_curr_p }

let current_located value lexbuf =
  let position = current_position lexbuf in
  { value; position }
