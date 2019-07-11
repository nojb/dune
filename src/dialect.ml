open! Stdune

module Filter = struct
  type t =
    | No_filter
    | Action of Loc.t * Action_dune_lang.t

  open Dyn.Encoder

  let to_dyn = function
    | No_filter ->
        constr "no_filter" []
    | Action (loc, action) ->
        constr "action"
          [ Loc.to_dyn loc
          ; Dune_lang.to_dyn (Action_dune_lang.encode action)
          ]
end

module File_kind = struct
  type t =
    { kind       : Ml_kind.t
    ; extension  : string
    ; preprocess : Filter.t
    ; format     : Filter.t
    }

  let to_dyn { kind ; extension ; preprocess ; format } =
    let open Dyn.Encoder in
    record
      [ "kind"      , Ml_kind.to_dyn kind
      ; "extension" , string extension
      ; "preprocess", Filter.to_dyn preprocess
      ; "format"    , Filter.to_dyn format
      ]
end

type t =
  { name       : string
  ; file_kinds : File_kind.t Ml_kind.Dict.t
  }

let name t = t.name

let to_dyn { name ; file_kinds } =
  let open Dyn.Encoder in
  record
    [ "name"      , string name
    ; "file_kinds", Ml_kind.Dict.to_dyn File_kind.to_dyn file_kinds
    ]

let extension { file_kinds = { Ml_kind.Dict.intf ; impl } ; _ } = function
  | Ml_kind.Intf -> intf.extension
  | Impl         -> impl.extension

let preprocess { file_kinds = { Ml_kind.Dict.intf ; impl } ; _ } = function
  | Ml_kind.Intf -> intf.preprocess
  | Impl         -> impl.preprocess

let format { file_kinds = { Ml_kind.Dict.intf ; impl } ; _ } = function
  | Ml_kind.Intf -> intf.format
  | Impl         -> impl.format

let ocaml =
  let file_kind kind extension =
    { File_kind.
      kind
    ; extension
    ; preprocess = Filter.No_filter
    ; format     = Filter.No_filter
    }
  in
  let intf = file_kind Ml_kind.Intf ".mli" in
  let impl = file_kind Ml_kind.Impl ".ml"  in
  { name       = "ocaml"
  ; file_kinds = Ml_kind.Dict.make ~intf ~impl
  }

let reason =
  let file_kind kind extension =
    let module S = String_with_vars in
    let preprocess =
      Action_dune_lang.run (S.virt __POS__ "refmt")
        [ S.virt     __POS__ "--print"
        ; S.virt     __POS__ "binary"
        ; S.virt_var __POS__ "input-file"
        ]
    in
    let format =
      Action_dune_lang.run (S.virt __POS__ "refmt")
        [ S.virt_var __POS__ "input-file"
        ]
    in
    { File_kind.
      kind
    ; extension
    ; preprocess = Filter.Action (Loc.none, preprocess)
    ; format     = Filter.Action (Loc.none, format)
    }
  in
  let intf = file_kind Ml_kind.Intf ".rei" in
  let impl = file_kind Ml_kind.Impl ".re"  in
  { name       = "reason"
  ; file_kinds = Ml_kind.Dict.make ~intf ~impl
  }

let ml_suffix dialect ml_kind =
  match dialect.name with
  | "ocaml" -> None
  | _ -> Some (extension ocaml ml_kind)
