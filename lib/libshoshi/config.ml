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

module ShoshiXdg = struct
  let xdg = Xdg.create ~env:Sys.getenv_opt ()
  let xdg_data = Xdg.data_dir xdg
  let xdg_config = Xdg.config_dir xdg
  let xdg_state = Xdg.state_dir xdg
end

module ShoshiEnv = struct
  let shoshi_editor = "SHOSHI_EDITOR"
  let shoshi_editor_options = "SHOSHI_EDITOR_OPTS"
end

let ( / ) = Filename.concat

let version =
  match Build_info.V1.version () with
  | Some s -> Build_info.V1.Version.to_string s
  | None -> "[n/a]"

let shoshi_name = "shoshi"
let shoshi_bibtex_name = Printf.sprintf "%s-db.bib" shoshi_name

(**
    [$XDG_DATA_HOME/shoshi] probably [$HOME/.local/share/shoshi]
*)
let shoshi_share_dir = ShoshiXdg.xdg_data / shoshi_name

let shoshi_state_dir = ShoshiXdg.xdg_state / shoshi_name
let shoshi_config_dir = ShoshiXdg.xdg_config / shoshi_name
let shoshi_bibtex = shoshi_share_dir / shoshi_bibtex_name
let is_shoshi_initialized () = Util.Filesys.file_exists @@ shoshi_bibtex

let check_shoshi_initialiazed () =
  if not @@ is_shoshi_initialized () then Error.shoshi_not_init ()

let initialize_shoshi ?(delete = false) () =
  let root = "/" in
  let is_initialized = is_shoshi_initialized () in
  match is_initialized with
  | true ->
      if delete then
        let () = Util.Filesys.rmrf shoshi_share_dir in
        Util.Filesys.mkfilep root
          (String.split_on_char '/' shoshi_share_dir)
          shoshi_bibtex_name
      else Ok ()
  | false ->
      Util.Filesys.mkfilep root
        (String.split_on_char '/' shoshi_share_dir)
        shoshi_bibtex_name

let shoshi_knwon_editors = [| "ee"; "emacs"; "nano"; "hx"; "vi"; "vim" |]

let save ?(where = shoshi_bibtex) database =
  Out_channel.with_open_bin where (fun oc ->
      Printf.fprintf oc "%s%!" @@ ShoshiBibtex.Database.to_string database)
