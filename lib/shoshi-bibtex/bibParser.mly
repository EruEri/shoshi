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


%{
    
%}

%token <BibEntry.Type.t> EntryType
%token <string> Identifier
%token <int> Number
%token <string> Content
%token LBRACE RBRACE
%token EQUAL COMMA
%token EOF


%start <BibBaseType.database> bibtex_database

%%

%inline bracketed(X):
    | delimited(LBRACE, X, RBRACE) { $1 } 

%inline splitted(lhs, sep, rhs):
    | lhs=lhs sep rhs=rhs { lhs, rhs }

bibtex_database:
    | l=list(bibtex_entry) EOF { l }

bibtex_entry:
    | entry_type=EntryType citekeys_fields=bracketed(splitted(Identifier, COMMA, separated_list(COMMA, bibtex_field))) {
        let (citekey, fields) = citekeys_fields in
        let fields = BibEntry.FieldMap.of_seq (List.to_seq fields) in
        BibEntry.{
            entry_type;
            citekey;
            fields
        }
    }  

bibtex_field:
    | Identifier EQUAL bibtex_field_value {
        ($1, $3)
    }
    | Identifier EQUAL bracketed(list(bibtex_field_value)) {
        ($1, String.concat " " $3)
    }

bibtex_field_value:
    | Identifier { $1 }
    | Content { $1 }
    | Number { string_of_int $1}
    | COMMA { ","}


