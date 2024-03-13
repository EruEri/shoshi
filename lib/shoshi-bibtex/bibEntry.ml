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

module Type = struct
  type t =
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

    let to_string entry_type = 
      let s = function
      | Article -> "article"
      | Book -> "book"
      | Booklet -> "booklet"
      | Conference -> "Conference"
      | Inbook -> "inbook"
      | Incollection -> "incollection"
      | Inproceedings -> "onproceedings"
      | Manual -> "manual"
      | Masterthesis -> "masterthesis"
      | Misc -> "misc"
      | Phdthesis -> "phsthesis"
      | Proceedings -> "proceedings"
      | Techreport -> "techreport"
      | Unpublished -> "unpublished" in
    Printf.sprintf "@%s" @@ s entry_type
end

module FieldMap = Map.Make (struct
  type t = String.t

  let compare lhs rhs =
    String.compare (String.lowercase_ascii lhs) (String.lowercase_ascii rhs)
end)

type t = { entry_type : Type.t; citekey : string; fields : string FieldMap.t }


let to_string entry = 
  let string_fields = String.concat "\n" 
    @@ FieldMap.fold (fun key value acc -> 
      (Printf.sprintf "%s = \"%s\"" key value) :: acc
    ) entry.fields [] in
  Printf.sprintf "%s{%s,\n%s\n}" 
  (Type.to_string entry.entry_type)
  entry.citekey
  string_fields

let create entry_type citekey fields = { entry_type; citekey; fields }
let raw_field field entry = FieldMap.find_opt field entry.fields
let address = raw_field "address"
let annote = raw_field "annote"
let author = raw_field "author"
let booktitle = raw_field "booktitle"
let chapter = raw_field "chapter"
let edition = raw_field "edition"
let editor = raw_field "editor"
let howpublished = raw_field "howpublished"
let institution = raw_field "institution"
let journal = raw_field "journal"
let month = raw_field "month"
let note = raw_field "note"
let number = raw_field "number"
let organization = raw_field "organization"
let pages = raw_field "pages"
let publisher = raw_field "publisher"
let school = raw_field "school"
let series = raw_field "series"
let title = raw_field "title"
let type_ = raw_field "type"
let volume = raw_field "volume"
let year entry = Option.bind (raw_field "year" entry) int_of_string_opt
