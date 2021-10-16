open Stdune
module Log = Dune_util.Log
module Process = Dune_engine.Process

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

  let hash t = Hashtbl.hash t
end

(* let find_in path fn = *)
(*   List.find path ~f:(fun dir -> Path.exists (Path.relative dir fn)) *)

(* let check_environment ~_INCLUDE ~_LIB ~_PATH arch = *)
(*   find_in _PATH "cl.exe" && *)
(*   find_in _PATH "rc.exe" && *)
(*   find_in _PATH "link.exe" && *)
(*   find_in _INCLUDE "windows.h" && *)
(*   find_in _LIB "kernel32.lib" && *)
(*   find_in _INCLUDE "stdlib.h" && *)
(*   find_in _LIB "msvcrt.lib" && *)
(*   find_in _LIB "oldnames.lib" && *)
(*   find_in _PATH (function X86 -> "ml.exe" | X64 -> "ml64.exe") && *)
(*   find_in _PATH "mt.exe" *)

(* let detect_env ~env = *)
(*   let* cl = Bin.which ~path:(Env.path env) "cl" in *)
(* let* lines = Memo.Build.of_reproducible_fiber (Process.run_capture_lines ~env
   Strict cl []) in *)
(*   match lines with *)
(*   | first :: _ -> *)
(*     let arch = Re... in *)
(*     match match Env.get env "INCLUDE", Env.get env "LIB" with *)
(*     | None, _ | _, None -> None *)
(*     | Some _INCLUDE, Some _LIB -> *)
(*       let _INCLUDE = Bin.parse_path _INCLUDE *)
(*       and _LIB = Bin.parse_path _LIB in *)
(*       if check_environment ~_INCLUDE ~_LIB ~_PATH then *)
(*         Some cl *)
(*       else *)
(*         None *)
(*     end *)
(*   | [] -> *)
(*     None *)

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
    }

  val query : unit -> t list Memo.Build.t
end = struct
  type t =
    { installation_path : string
    ; installation_version : string
    ; display_name : string
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
              if
                exists
                  (Printf.sprintf "%s\\VC\\Auxiliary\\Build\\vcvarsall.bat"
                     installation_path)
              then (
                Log.info
                  [ Pp.textf "Found instance %s at %s (%s %s)" instance_id
                      installation_path installation_version display_name
                  ];
                { installation_path; installation_version; display_name }
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
    | None -> Memo.Build.return []
    | Some path ->
      let path =
        Printf.sprintf "%s\\Microsoft Visual Studio\\Installer\\vswhere.exe"
          path
      in
      if not (exists path) then
        Memo.Build.return []
      else
        let open Memo.Build.O in
        let+ l =
          Memo.Build.of_reproducible_fiber
            (Process.run_capture_lines Strict (Path.of_string path)
               [ "-all"; "-products"; "*"; "-nologo" ])
        in
        parse_output l
end

module Sdk = struct
  let root = "HKLM\\SOFTWARE\\Microsoft\\Microsoft SDKs\\Windows"

  let which prog = Bin.which ~path:(Env.path Env.initial) prog

  (* Retrieves a REG_SZ value from the registry (redirected on WOW64) *)
  let reg_string key value =
    match which "reg" with
    | None -> Memo.Build.return None
    | Some reg ->
      let open Memo.Build.O in
      let+ l =
        Memo.Build.of_reproducible_fiber
          (Process.run_capture_lines
             ~stderr_to:Process.Io.(null Out)
             Strict reg
             [ "query"; key; "/v"; value ])
      in
      Some l

  type t =
    { display_name : string
    ; setenv_script : string
    }

  (* Enumerate installed SDKs for v6.0+ *)
  let query () =
    match which "reg" with
    | None -> Memo.Build.return []
    | Some reg ->
      let open Memo.Build.O in
      let* l =
        Memo.Build.of_reproducible_fiber
          (Process.run_capture_lines
             ~stderr_to:Process.Io.(null Out)
             Strict reg [ "query"; root ])
      in
      let f s =
        match String.drop_prefix s ~prefix:"Windows\\" with
        | None -> Memo.Build.return None
        | Some name -> (
          let+ install_dir =
            reg_string (Printf.sprintf "%s\\%s" root name) "InstallationFolder"
          in
          match install_dir with
          | None ->
            Log.info
              [ Pp.textf
                  "Registry key for Windows SDK %s doesn't contain expected \
                   IntallationFolder value"
                  name
              ];
            None
          | Some [ install_dir ] ->
            let setenv_script =
              Printf.sprintf "%s\\Bin\\SetEnv.cmd" install_dir
            in
            if exists setenv_script then
              let display_name = Printf.sprintf "Windows SDK %s" name in
              Some { display_name; setenv_script }
            else (
              Log.info
                [ Pp.textf
                    "Registry set for Windows SDK %s, but SetEnv.cmd not found"
                    name
                ];
              None
            )
          | Some _ -> assert false)
      in
      Memo.Build.List.filter_map l ~f
end

(* module Vs : sig *)
(*   type t *)

(* val all : t list *)

(* val name : t -> string *)

(* val version : t -> int * int *)

(*   type result = *)
(*     | Express *)
(*     | Normal *)

(*   val find : t -> Env.t -> result option *)
(* end = struct *)
(*   type t = *)
(*     | Vs_7_0 *)
(*     | Vs_7_1 *)
(*     | Vs_8_0 *)
(*     | Vs_9_0 *)
(*     | Vs_10_0 *)
(*     | Vs_11_0 *)
(*     | Vs_12_0 *)
(*     | Vs_14_0 *)

(* let all = [Vs_7_0; Vs_7_1; Vs_8_0; Vs_9_0; Vs_10_0; Vs_11_0; Vs_12_0;
   Vs_14_0] *)

(*   let name = function *)
(*     | Vs_7_0 -> "Visual Studio .NET 2002" *)
(*     | Vs_7_1 -> "Visual Studio .NET 2003" *)
(*     | Vs_8_0 -> "Visual Studio 2005" *)
(*     | Vs_9_0 -> "Visual Studio 2008" *)
(*     | Vs_10_0 -> "Visual Studio 2010" *)
(*     | Vs_11_0 -> "Visual Studio 2012" *)
(*     | Vs_12_0 -> "Visual Studio 2013" *)
(*     | Vs_14_0 -> "Visual Studio 2015" *)

(*   let version = function *)
(*     | Vs_7_0 -> (7, 0) *)
(*     | Vs_7_1 -> (7, 1) *)

(*   type result = *)
(*     | Express *)
(*     | Normal *)

(*   let ms_root env = *)
(*     match Env.get env "PROCESSOR_ARCHITEW6432" with *)
(*     | Some _ -> *)
(*       "HKLM\\SOFTWARE\\Microsoft" *)
(*     | None -> *)
(*       "HKLM\\SOFTWARE\\Wow6432Node\\Microsoft" *)

(*   let reg64 env = *)
(*     match Env.get env "PROCESSOR_ARCHITEW6432" with *)
(*     | Some _ -> *)
(*       let windir = Option.get (Env.get env "WINDIR") in *)
(*       "%s\\sysnative\\reg.exe" *)
(*     | None -> *)
(*       "reg.exe" *)

(*   let find t env = *)
(*     let major, minor = version t in *)
(*     let var = Printf.sprintf "VS%d%dCOMNTOOLS" major minor in *)
(*     match Env.get env var with *)
(*     | None -> None *)
(*     | Some v -> *)
(* if not (Sys.file_exists (Filename.concat v "vsvars32.bat")) then begin *)
(* Log.info [ Pp.textf "%s: %s set, but vsvars32.bat not found" (name t) var
   ]; *)
(*         None *)
(*       end else begin *)
(*         let install_dir = *)
(* match reg_string (Printf.sprintf "%s\\VisualStudio\\%d.%d" ms_root major
   minor) "InstallDir" with *)
(*           | "" -> *)
(*             ... *)
(*           | s -> Some s *)
(*         in *)
(*         match install_dir with *)
(*         | "" -> *)
(* Log.info [ Pp.textf "%s: vsvars32.bat found, but registry settings not found"
   (name t) ]; *)
(*           None *)
(*         | install_dir -> *)
(*           if ... then *)
(*             Ok () *)
(*           else begin *)
(* Log.info [ Pp.textf "%s: %s doesn't agree with registry" (name t) var ]; *)
(*             None *)
(*           end *)
(*       end *)

(*   let find_vswhere = *)
(*     let* program_files = Env.get "ProgramFiles(x86)" in *)
(* let vswhere = Filename.concat program_files ("Microsoft Visual
   Studio\\Installer\\vswhere.exe") in *)
(* if Sys.file_exists vswhere then *)
(* let* lines = Memo.Build.of_reproducible_fiber (Process.run_capture_lines ~env
   Strict vswhere ["-all"; "products '*'"; "-nologo"]) *)
(*       in *)
(*       let rec loop = *)
(*         match x with *)
(*         | "instanceId" -> *)
(*           ... *)
(*         | "installationPath" -> *)
(*           ... *)
(*         | "installationVersion" -> *)
(*           ... *)
(*         | "displayName" -> *)
(*           .. *)
(*         | _ -> *)
(*           () *)
(*       in *)
(*       loop 0 *)
(*     else *)
(*       None *)

(* end *)

(* module Found : sig *)
(*   type t = *)
(*     { *)
(*       script: string; *)
(*       flags: string list; *)
(*     } *)
(* end *)

(* module Sdk : sig *)
(*   type t *)

(* val all : t list *)

(*   module Found : sig *)
(*     type nonrec t = *)
(*       { *)
(*         sdk: t; *)
(*         setenv: string; *)
(*       } *)
(*   end *)

(*   val find : t -> Env.t -> Found.t option *)
(* end = struct *)

(*   let name = function *)
(*     | Sdk_5_2 -> "Windows Server 2003 SP1 SDK" *)
(*     | Sdk_6_1 -> "Windows Server 2008 with .NET 3.5 SDK" *)
(*     | Sdk_7_0 -> "Windows 7 with .NET 3.5 SP1 SDK" *)
(*     | Sdk_7_1 -> "Windows 7 with .NET 4 SDK" *)
(*     | Sdk -> "Generalised Windows SDK" *)

(*   module Found = struct *)
(*     type nonrec t = { *)
(*       sdk: t; *)
(*       setenv: string; *)
(*       } *)
(*   end *)

(* let find_5_2 env = *)
(* let install_dir = reg64_string
   "HKLM\\SOFTWARE\\Microsoft\\MicrosoftSDK\\InstalledSDKs\\8F9E5EF3-A9A5-491B-A889-C58EFFECE8B3"
   "Install Dir" in *)
(*     if install_dir <> "" then begin *)
(*       let setenv = Filename.concat install_dir "SetEnv.cmd" in *)
(*       if Sys.file_exists setenv then *)
(*         Some {Found.sdk = t; setenv} *)
(*       else begin *)
(* Log.info [ Pp.textf "%s: registry set for Windows Server 2003 SDK, but
   SetEnv.cmd not found" (name t) ]; *)
(*         None *)
(*       end *)
(*     end else *)
(*       None *)

(*   let find_all env = *)
(*     let root = "HKLM\\SOFTWARE\\Microsoft\\Microsoft SDKs\\Windows" in *)
(*     let lines = "reg" [ "query" ; root ] in *)
(*     List.filter_map (fun i -> *)
(* let install_dir = reg_string (Printf.sprintf "%s\\%s" sdk_root i)
   "InstallationFolder" in *)
(* if install_dir <> "" then *)
(* if Sys.file_exists (Printf.sprintf "%s\\Bin\\SetEnv.cmd" install_dir) then *)
(*           else *)
(*             None *)
(*         else begin *)
(* Log.info [ "Registry key for Windows SDK $i doesn't contain expected
   InstallationFolder value" ]; *)
(*           None *)
(*         end *)
(*       ) ... *)

(* end *)

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

(* let run_test_command () = *)
(*   let path = extend_path "?msvs-detect?;%s;%s" dir path in *)
(*   let comspec = Env.get env "COMSPEC" in *)
(*   let lines = spawn "%s /v:on /c %s 2>NUL" in *)
(*   ... *)

let detect _arch = Memo.Build.return None
