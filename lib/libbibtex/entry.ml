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

type entry_type =
  | Article
  | Book
  | Booklet
  | Conference
  | Inbook
  | Incollection
  | Inproceedings
  | Manual
  | Masterthesis
  | Misc
  | Phdthesis
  | Proceedings
  | Techreport
  | Unpublished

let of_string s =
  let s = String.trim @@ String.lowercase_ascii s in
  match s with
  | "article" -> Article
  | "book" -> Book
  | "booklet" -> Booklet
  | "conference" -> Conference
  | "inbook" -> Inbook
  | "incollection" -> Incollection
  | "inproceedings" -> Inproceedings
  | "manual" -> Manual
  | "masterthesis" -> Masterthesis
  | "misc" -> Misc
  | "phdthesis" -> Phdthesis
  | "proceedings" -> Proceedings
  | "techreport" -> Techreport
  | "unpublished" -> Unpublished
  | _ as s -> invalid_arg @@ Printf.sprintf "Unknown entry type : %s" s

module FieldMap = Map.Make (struct
  type t = String.t

  let compare lhs rhs =
    String.compare (String.lowercase_ascii lhs) (String.lowercase_ascii rhs)
end)

type t = {
  entry_type : entry_type;
  citekey : string;
  fields : string FieldMap.t;
}

let t = FieldMap.of_seq
