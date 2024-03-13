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

type t = 
  | ShoshiAlreadyInitialized
  | ShoshiNotInitialized
  | BibtexParsingError

type exn += ShoshiError of t

let to_string = function
  | ShoshiAlreadyInitialized -> "shoshi is already initialized"
  | ShoshiNotInitialized -> "shoshi is not initialized"
  | BibtexParsingError -> "Error while parsing the bibtex file"

let shoshi_already_init () = raise @@ ShoshiError ShoshiAlreadyInitialized

let shoshi_not_init () = raise @@ ShoshiError ShoshiNotInitialized

let bibtex_parsing_error () = raise @@ ShoshiError BibtexParsingError

