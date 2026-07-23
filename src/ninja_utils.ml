(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020 Inria,
   contributor: Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Ninja variable names *)
module rec Var : sig
  type 'a t = { name : string; pp : Format.formatter -> 'a -> unit }
  val make_atom : string -> Expr.atom t
  val make_expr : string -> Expr.t t
  val name : 'a t -> string
  val ref : Expr.atom t -> string
end = struct
  type 'a t =  { name : string; pp : Format.formatter -> 'a -> unit }

  let make_atom name = { name; pp = Format.pp_print_string }
  let make_expr name = { name; pp = Expr.format }
  let name v = v.name

  let ref v = Printf.sprintf "${%s}" v.name
end

and Expr : sig
  type atom = string
  type elt =
    | Atom of atom
    | List of t
    | Var of t Var.t
    | Raw of string
  and t = elt list
  val format : Format.formatter -> t -> unit
end = struct
  type atom = string
  type elt =
    | Atom of atom
    | List of t
    | Var of t Var.t
    | Raw of string
  and t = elt list

  let ninja_escaping =
    let esc_re =
      Re.(compile (alt [space; char ':']))
    in
    Re.replace esc_re ~f:(fun g -> "$" ^ Re.Group.get g 0)

  let quote s = "\"" ^ s ^ "\"" (* TODO *)

  let rec ninja_format_elt ppf = function
    | Atom s -> Format.pp_print_string ppf (quote (ninja_escaping s))
    | List t -> format ppf t
    | Var v -> Format.fprintf ppf "${%s}" (Var.name v)
    | Raw op -> Format.pp_print_string ppf (ninja_escaping op)
  and format ppf t =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ' ')
      ninja_format_elt ppf t
end

module Binding = struct
  type 'a t = 'a Var.t * 'a
  type any = Any : 'a t -> any

  let make v x = v, x
  let make_any v x = Any (make v x)

  let format ~global ppf b =
  match b with
  | Any (v, x) ->
    if not global then Format.pp_print_string ppf "  ";
    Format.fprintf ppf "%s = %a" (Var.name v) v.Var.pp x

  let format_list ~global ppf l =
    Format.pp_print_list ~pp_sep:Format.pp_print_newline
      (format ~global) ppf l
end

module Rule = struct
  type t = {
    name : string;
    command : Expr.t;
    description : Expr.t option;
    vars : Binding.any list;
  }

  let make ?(vars = []) name ~command ~description =
    { name; command; description = Option.some description; vars }

  let format fmt rule =
    let bindings =
      Binding.make_any (Var.make_expr "command") rule.command
      :: Option.(
           to_list
             (map
                (fun d -> Binding.make_any (Var.make_expr "description") d)
                rule.description))
      @ rule.vars
    in
    Format.fprintf fmt "rule %s\n%a" rule.name
      (Binding.format_list ~global:false)
      bindings
end

module Build = struct
  type t = {
    rule : string;
    inputs : Expr.t option;
    implicit_in : Expr.t;
    outputs : Expr.t;
    implicit_out : Expr.t option;
    vars : Binding.any list;
  }

  let make ?inputs ?(implicit_in = []) ~outputs ?implicit_out ?(vars = []) rule
      =
    { rule; inputs; implicit_in; outputs; implicit_out; vars }

  let empty = make ~outputs:[Atom "empty"] "phony"

  let unpath ?(sep = "-") path =
    Re.replace_string Re.(compile (str Filename.dir_sep)) ~by:sep path

  let format fmt t =
    Format.fprintf fmt "build %a%a: %s%a%a%a%a" Expr.format t.outputs
      (Format.pp_print_option (fun fmt i ->
           Format.pp_print_string fmt " | ";
           Expr.format fmt i))
      t.implicit_out t.rule
      (Format.pp_print_option (fun ppf e ->
           Format.pp_print_char ppf ' ';
           Expr.format ppf e))
      t.inputs
      (fun ppf -> function
        | [] -> ()
        | e ->
          Format.pp_print_string ppf " | ";
          Expr.format ppf e)
      t.implicit_in
      (if t.vars = [] then fun _ () -> () else Format.pp_print_newline)
      ()
      (Binding.format_list ~global:false)
      t.vars
end

module Default = struct
  type t = Expr.t

  let make rules = rules
  let format ppf t = Format.fprintf ppf "default %a" Expr.format t
end

type def =
  | Comment of string
  | Binding of Binding.any
  | Rule of Rule.t
  | Build of Build.t
  | Default of Default.t

let comment s = Comment s
let binding v e = Binding (Binding.make_any v e)

let rule ?vars name ~command ~description =
  Rule (Rule.make ?vars name ~command ~description)

let build ?inputs ?implicit_in ~outputs ?implicit_out ?vars rule =
  Build (Build.make ?inputs ?implicit_in ~outputs ?implicit_out ?vars rule)

let default rules = Default (Default.make rules)

let format_def ppf def =
  let () =
    match def with
    | Comment s ->
      Format.pp_print_list ~pp_sep:Format.pp_print_newline
        (fun ppf s ->
          if s <> "" then Format.pp_print_string ppf "# ";
          Format.pp_print_string ppf s)
        ppf
        (String.split_on_char '\n' s)
    | Binding b -> Binding.format ~global:true ppf b
    | Rule r ->
      Rule.format ppf r;
      Format.pp_print_newline ppf ()
    | Build b -> Build.format ppf b
    | Default d -> Default.format ppf d
  in
  Format.pp_print_flush ppf ()

type ninja = def Seq.t

let format ppf t =
  Format.pp_print_seq ~pp_sep:Format.pp_print_newline format_def ppf t;
  Format.pp_print_newline ppf ()
