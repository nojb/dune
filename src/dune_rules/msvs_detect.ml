open Stdune
module Log = Dune_util.Log
module Process = Dune_engine.Process
module Predicate_lang = Dune_engine.Predicate_lang
module Re = Dune_re

module Arch = struct
  type t =
    | X86
    | X64

  let to_dyn t =
    let open Dyn.Encoder in
    match t with
    | X86 -> constr "X86" []
    | X64 -> constr "X64" []

  let equal t1 t2 =
    match (t1, t2) with
    | X86, X86
    | X64, X64 ->
      true
    | _ -> false

  let to_string = function
    | X86 -> "x86"
    | X64 -> "x64"

  let assembler_fn = function
    | X86 -> "ml.exe"
    | X64 -> "ml64.exe"

  let hash t = Hashtbl.hash t
end

let find_in path fn =
  List.exists path ~f:(fun dir -> Path.exists (Path.relative dir fn))

let check_environment ~_INCLUDE ~_LIB ~_PATH arch =
  let _PATH = Bin.parse_path _PATH in
  let _INCLUDE = Bin.parse_path _INCLUDE in
  let _LIB = Bin.parse_path _LIB in
  find_in _PATH "cl.exe" &&
  find_in _PATH "rc.exe" &&
  find_in _PATH "link.exe" &&
  find_in _INCLUDE "windows.h" &&
  find_in _LIB "kernel32.lib" &&
  find_in _INCLUDE "stdlib.h" &&
  find_in _LIB "msvcrt.lib" &&
  find_in _LIB "oldnames.lib" &&
  find_in _PATH (Arch.assembler_fn arch) &&
  find_in _PATH "mt.exe"

let detect_env arch =
  match Bin.which ~path:(Env.path Env.initial) "cl" with
  | None -> Memo.Build.return false
  | Some cl ->
let open Memo.Build.O in
Log.info [Pp.textf "Environment contains CL compiler at %s" (Path.to_string cl)];
let fn = Temp.create File ~prefix:"cl" ~suffix:".out" in
let+ () = Memo.Build.of_reproducible_fiber (Process.run
~stdout_to:(Process.Io.null Out)
~stderr_to:(Process.Io.file fn Out)
   Strict cl []) in
  match Io.lines_of_file fn with
  | s :: _ ->
    let re = Re.seq Re.[str " for "; group (rep1 alnum); stop] in
    begin match Re.exec_opt (Re.compile re) s with
    | Some g ->
       let s = Re.Group.get g 1 in
      begin match s, arch with
      | ("x64" | "AMD64"), Arch.X64
      | ("x86" | "80x86"), X86 ->
    begin match Env.get Env.initial "INCLUDE", Env.get Env.initial "LIB", Env.get Env.initial "PATH" with
    | Some _INCLUDE, Some _LIB, Some _PATH ->
      let ok = check_environment ~_INCLUDE ~_LIB ~_PATH arch in
      if not ok then Log.info [Pp.textf "Incomplete compiler installation, environment CL %s excluded" s];
      ok
    | _ ->
Log.info [Pp.textf "INCLUDE and/or LIB not set, environment CL %s excluded" s];
false
    end
      | _ ->
Log.info [Pp.textf "CL architecture does not match required value (%s != %s)" s (Arch.to_string arch)];
false
end
      | None -> false
end
  | [] ->
    false

let exists fn =
  match Unix.stat fn with
  | { st_kind = S_DIR; _ } -> false
  | exception Unix.Unix_error _ -> false
  | _ -> true

module Vswhere : sig
  type t =
    { installation_path : string
    ; installation_version : string
    ; display_name : string
    ; script : string
    }

  val query : unit -> t list Memo.Build.t
end = struct
  type t =
    { installation_path : string
    ; installation_version : string
    ; display_name : string
    ; script : string
    }

  let parse_output l =
    let rec loop accu ~instance_id ~installation_path ~installation_version =
      function
      | s :: l -> (
        match String.lsplit2 s ~on:':' with
        | Some ("instanceId", s) ->
          let instance_id = Some (String.trim s) in
          loop accu ~instance_id ~installation_path ~installation_version l
        | Some ("installationPath", s) ->
          let installation_path = Some (String.trim s) in
          loop accu ~instance_id ~installation_path ~installation_version l
        | Some ("installationVersion", s) ->
          let installation_version = Some (String.trim s) in
          loop accu ~instance_id ~installation_path ~installation_version l
        | Some ("displayName", s) ->
          let display_name = String.trim s in
          let accu =
            match (instance_id, installation_path, installation_version) with
            | ( Some instance_id
              , Some installation_path
              , Some installation_version ) ->
              let script =
                  (Printf.sprintf "%s\\VC\\Auxiliary\\Build\\vcvarsall.bat"
                     installation_path) in
              if
                exists script
              then (
                Log.info
                  [ Pp.textf "Found instance %s at %s (%s %s)" instance_id
                      installation_path installation_version display_name
                  ];
                { installation_path; installation_version; display_name; script }
                :: accu
              ) else
                accu
            | _ -> accu
          in
          loop accu ~instance_id:None ~installation_path:None
            ~installation_version:None l
        | None
        | Some _ ->
          loop accu ~instance_id ~installation_path ~installation_version l)
      | [] -> List.rev accu
    in
    loop [] ~instance_id:None ~installation_path:None ~installation_version:None
      l

  let query () =
    match Env.get Env.initial "ProgramFiles(x86)" with
    | None -> Log.info [ Pp.text "0" ]; Memo.Build.return []
    | Some path ->
      let path =
        Printf.sprintf "%s\\Microsoft Visual Studio\\Installer\\vswhere.exe"
          path
      in
      if not (exists path) then begin
        Log.info [ Pp.text "vswhere.exe not found." ];
        Memo.Build.return []
      end else (
        let open Memo.Build.O in
        let+ l =
          Memo.Build.of_reproducible_fiber
            (Process.run_capture_lines Strict (Path.of_string path)
               [ "-all"; "-products"; "*"; "-nologo" ])
        in
        parse_output l)
end

type t =
  { extend_PATH : string
  ; var_LIB : string
  ; var_INCLUDE : string
  }

let to_dyn { extend_PATH; var_LIB; var_INCLUDE } =
  let open Dyn.Encoder in
  record
    [ ("extend_PATH", string extend_PATH)
    ; ("var_LIB", string var_LIB)
    ; ("var_INCLUDE", string var_INCLUDE)
    ]

let equal { extend_PATH; var_LIB; var_INCLUDE } t =
  String.equal extend_PATH t.extend_PATH
  && String.equal var_LIB t.var_LIB
  && String.equal var_INCLUDE t.var_INCLUDE

let hash t = Hashtbl.hash t

let magic = "?msvs-detect?"

let run_test_command arch {Vswhere.script; _} =
  let env = Env.initial in
   match Env.get env "COMSPEC" with
| None -> Memo.Build.return None
| Some cmd ->
  let env = Env.remove env ~var:"LIB" in
  let env = Env.remove env ~var:"INCLUDE" in
  let env = Env.update env ~var:"PATH" ~f:(fun oldpath ->
    let oldpath = match oldpath with None -> "" | Some oldpath -> ";" ^ oldpath in
    Some (Printf.sprintf "%s;%s" magic oldpath)
   )
  in
  let env = Env.remove env ~var:"ORIGINALPATH" in
  let open Memo.Build.O in
  let+ l =
          Memo.Build.of_reproducible_fiber
(
  let fn = Temp.create File ~prefix:"msvs-detect" ~suffix:".bat" in
  Stdune.Io.with_file_out fn ~f:(fun oc ->
    Printf.fprintf oc "%s %s && echo XMARKER && echo !PATH! && echo !LIB! && echo !INCLUDE!\n"
      (Filename.basename script) (Arch.to_string arch)
);
    Process.run_capture_lines (Accept Predicate_lang.any) (Path.of_string cmd)
             ~dir:(Path.of_string (Filename.dirname script))
           ~stderr_to:(Process.Io.null Out)
~env
             [ "/v:on"; "/c"; Path.to_string fn])
  in
  match l with
  | Error n ->
Log.info [Pp.textf "error %d" n];
None
  | Ok l ->
let l = List.map l ~f:String.trim in
    let rec loop = function
| "XMARKER" :: _PATH :: _LIB :: _INCLUDE :: _ ->
        let re = Re.str magic in
        begin match Re.exec_opt (Re.compile re) _PATH with
        | Some g ->
             let _PATH = String.sub _PATH 0 (Re.Group.start g 0) in
   if check_environment ~_INCLUDE ~_LIB ~_PATH arch then
    Some {extend_PATH = _PATH; var_LIB = _LIB; var_INCLUDE = _INCLUDE}
else
None
| None ->
              None
end
| _ :: l -> loop l
| [] -> None
  in
 loop l

let detect arch =
  let open Memo.Build.O in
  let* in_env = detect_env arch in
  if in_env then Memo.Build.return None else
 let* candidates = Vswhere.query () in
 let+ ts = Memo.Build.List.filter_map candidates ~f:(run_test_command arch) in
let open Pp.O in Log.info [ Pp.text "Found: " ++ Dyn.pp (Dyn.Encoder.list to_dyn ts) ];
match ts with
| t :: _ -> Some t
| [] -> None
