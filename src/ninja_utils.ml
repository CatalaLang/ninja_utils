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
  type 'a t = Scalar : string -> string t | Vector : string -> Expr.t t
  type scalar = string t
  type vector = Expr.t t
  val make_scalar : string -> scalar
  val make_vector : string -> vector
  val name : 'a t -> string
  val pp : 'a t -> Format.formatter -> 'a -> unit
  val ref : string t -> string
end = struct
  type 'a t = Scalar : string -> string t | Vector : string -> Expr.t t
  type scalar = string t
  type vector = Expr.t t

  let make_scalar name = Scalar name
  let make_vector name = Vector name
  let name (type a) (v: a t) = match v with
    | Scalar s -> s
    | Vector s -> s

  let pp (type a) (v : a t) : Format.formatter -> a -> unit =
    match v with
    | Scalar _ -> Format.pp_print_string
    | Vector _ -> Expr.format_command

  let ref (Scalar s) = Printf.sprintf "${%s}" s
end

and Expr : sig
  type elt =
    | Word of string
    | Splice of Var.vector
    | Raw of string
  and t = elt list
  val format_command : Format.formatter -> t -> unit
  val format_display : Format.formatter -> t -> unit
  val format_path : Format.formatter -> t -> unit
end = struct
  type elt =
    | Word of string
    | Splice of t Var.t
    | Raw of string
  and t = elt list

  let ninja_escaping =
    let esc_re =
      Re.(compile (alt [space; char ':']))
    in
    Re.replace esc_re ~f:(fun g -> "$" ^ Re.Group.get g 0)

  let quote =
  let trailing_backslashes = Re.(compile (seq [group (rep1 (char '\\')) ; eos])) in
  fun s ->
    let s = Re.replace trailing_backslashes 
    ~f:(fun g -> let backslash = Re.Group.get g 1 in backslash ^ backslash ) s in
    "\"" ^ s ^ "\""

  let check_raw op =
  if String.contains op ' ' then invalid_arg ("Raw with space: " ^ op)
  else op

  let rec ninja_format_elt ppf = function
    | Word s -> Format.pp_print_string ppf (quote (ninja_escaping s))
    | Splice v -> Format.fprintf ppf "${%s}" (Var.name v)
    | Raw op -> Format.pp_print_string ppf (ninja_escaping (check_raw op))
  and format_command ppf t =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ' ')
      ninja_format_elt ppf t

  let rec format_display_elt ppf = function
    | Word s -> Format.pp_print_string ppf s
    | Splice v -> Format.fprintf ppf "${%s}" (Var.name v)
    | Raw op -> Format.pp_print_string ppf (check_raw op)

  and format_display ppf t =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ' ')
      format_display_elt ppf t

  let rec format_path_elt ppf = function
    | Word s -> Format.pp_print_string ppf (ninja_escaping s)
    | Splice v -> invalid_arg ("vector ${" ^ Var.name v ^ "} in path position")
    | Raw op -> Format.pp_print_string ppf (ninja_escaping (check_raw op))

  and format_path ppf t =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ' ')
      format_path_elt ppf t
end

module Binding = struct
  type 'a t = 'a Var.t * 'a
  type any = Any : 'a t -> any

  let make v x = Any (v, x)

  let format ~global ppf b =
  match b with
  | Any (v, x) ->
    if not global then Format.pp_print_string ppf "  ";
    Format.fprintf ppf "%s = %a" (Var.name v) (Var.pp v) x

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

  let make ?(vars = []) ?description name ~command =
    { name; command; description; vars }

  let format fmt rule =
    Format.fprintf fmt "rule %s\n%a" rule.name
      (Binding.format ~global:false)
      (Binding.make (Var.make_vector "command") rule.command);
    Option.iter
      (fun d ->
        Format.fprintf fmt "\n  description = %a" Expr.format_display d)
      rule.description;
    List.iter
      (fun b -> Format.fprintf fmt "\n%a" (Binding.format ~global:false) b)
      rule.vars
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

  let format fmt t =
    Format.fprintf fmt "build %a%a: %s%a%a%a%a" Expr.format_path t.outputs
      (Format.pp_print_option (fun fmt i ->
           Format.pp_print_string fmt " | ";
           Expr.format_path fmt i))
      t.implicit_out t.rule
      (Format.pp_print_option (fun ppf e ->
           Format.pp_print_char ppf ' ';
           Expr.format_path ppf e))
      t.inputs
      (fun ppf -> function
        | [] -> ()
        | e ->
          Format.pp_print_string ppf " | ";
          Expr.format_path ppf e)
      t.implicit_in
      (if t.vars = [] then fun _ () -> () else Format.pp_print_newline)
      ()
      (Binding.format_list ~global:false)
      t.vars
end

module Default = struct
  type t = Expr.t

  let make rules = rules
  let format ppf t = Format.fprintf ppf "default %a" Expr.format_path t
end

type def =
  | Comment of string
  | Binding of Binding.any
  | Rule of Rule.t
  | Build of Build.t
  | Default of Default.t

let comment s = Comment s
let binding b = Binding b

let rule ?vars ?description name ~command =
  Rule (Rule.make ?vars ?description name ~command)

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
