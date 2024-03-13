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
module I = BibParser.MenhirInterpreter

let rec parse lexbuf (checkpoint : BibBaseType.database I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env -> (
      try
        let token = BibLexer.token lexbuf in
        let startp = lexbuf.lex_start_p and endp = lexbuf.lex_curr_p in
        let checkpoint = I.offer checkpoint (token, startp, endp) in
        parse lexbuf checkpoint
      with
      | Error.BibtexParserError e -> Result.Error (Either.Left e)
      | e -> raise e)
  | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse lexbuf checkpoint
  | I.HandlingError _ | I.Rejected ->
      let position = Util.Position.current_position lexbuf in
      let current_lexeme = Lexing.lexeme lexbuf in
      let location : string Util.Position.location =
        { value = current_lexeme; position }
      in
      Result.error @@ Either.right location
  | I.Accepted v -> Ok v
