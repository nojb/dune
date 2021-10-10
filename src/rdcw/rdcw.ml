module Event = struct
  type t =
    | Buffer_overflow
    | Added of string
    | Removed of string
    | Modified of string
    | Renamed_old_name of string
    | Renamed_new_name of string

  let to_dyn t =
    let open Dyn.Encoder in
    match t with
    | Buffer_overflow -> string "Buffer_overflow"
    | Added s -> constr "Added" [string s]
    | Removed s -> constr "Removed" [string s]
    | Modified s -> constr "Modified" [string s]
    | Renamed_old_name s -> constr "Renamed_old_name" [string s]
    | Renamed_new_name s -> constr "Renamed_new_name" [string s]
end

type t

external create : path:string -> f:(t -> Event.t -> unit) -> t = "dune_rdcw_create"

external loop : t -> unit = "dune_rdcw_loop"

external destroy : t -> unit = "dune_rdcw_destroy"
