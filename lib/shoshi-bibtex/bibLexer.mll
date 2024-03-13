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

{
    open BibParser

    (*
        article: any article published in a periodical like a journal article or magazine article
        book: a book
        booklet: like a book but without a designated publisher
        conference: a conference paper
        inbook: a section or chapter in a book
        incollection: an article in a collection
        inproceedings: a conference paper (same as the conference entry type)
        manual: a technical manual
        masterthesis: a Masters thesis
        misc: used if nothing else fits
        phdthesis: a PhD thesis
        proceedings: the whole conference proceedings
        techreport: a technical report, government report or white paper
        unpublished: 
    *)
}

let digit = ['0'-'9']
let loLetter = ['a'-'z']
let upLetter = ['A'-'Z']
let identifiant = (loLetter | upLetter | digit)+

let citekey = (loLetter | upLetter | digit | "_"| ":" | "-")+

let number = digit+

let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']


rule token = parse
| newline {  
    (* let _ = if ( String.contains s '\n') then (line := !line + 1) else () in  *)
    let () = Lexing.new_line lexbuf in
    token lexbuf 
}
| blank+ { token lexbuf }
| "{" { LBRACE }
| "}" { RBRACE }
| "=" { EQUAL }
| "," { COMMA }
| "\"" { 
    let buffer = Buffer.create 17 in
    let content = read_string buffer lexbuf in
    Content (content)
}
| "@" (identifiant as entryname) {
    let entrykind = try 
        BibEntry.Type.of_string entryname 
    with _ -> 
        Error.unknonw_entry_type @@ Util.Position.current_located entryname lexbuf
    in
    EntryType (entrykind)
}
| number as s {
    Number (int_of_string s)
}
| identifiant as s { Identifier s }
| eof { EOF }
| eof { EOF }
and read_string buffer = parse
| '"' { Buffer.contents buffer }
| '\\' (_ as c) { 
    let () = if c = '\\' then () else Buffer.add_char buffer '\\' in
    let () = Buffer.add_char buffer c in
    read_string buffer lexbuf 
}
| _ as c { 
    if c = '\n' then 
        let () = Lexing.new_line lexbuf in
        read_string buffer lexbuf
    else
        let () = Buffer.add_char buffer c in
        read_string buffer lexbuf 
}
| eof { 
    let e = Util.Position.current_position lexbuf in
    Error.unclosed_string e
}