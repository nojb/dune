module Arch = struct
type t =
    X86 | X64

let to_dyn t =
  let open Dyn.Encoder in
  match t with
  | X86 -> constr "X86" []
  | X64 -> constr "X64" []

let equal t1 t2 =
  match t1, t2 with
  | X86, X86 | X64, X64 -> true
  | _ -> false

let hash t =
  Hashtbl.hash t
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
(*   let* lines = Memo.Build.of_reproducible_fiber (Process.run_capture_lines ~env Strict cl []) in *)
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

(* module Vs : sig *)
(*   type t *)

(*   val all : t list *)

(*   val name : t -> string *)

(*   val version : t -> int * int *)

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

(*   let all = [Vs_7_0; Vs_7_1; Vs_8_0; Vs_9_0; Vs_10_0; Vs_11_0; Vs_12_0; Vs_14_0] *)

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
(*       if not (Sys.file_exists (Filename.concat v "vsvars32.bat")) then begin *)
(*         Log.info [ Pp.textf "%s: %s set, but vsvars32.bat not found" (name t) var ]; *)
(*         None *)
(*       end else begin *)
(*         let install_dir = *)
(*           match reg_string (Printf.sprintf "%s\\VisualStudio\\%d.%d" ms_root major minor) "InstallDir" with *)
(*           | "" -> *)
(*             ... *)
(*           | s -> Some s *)
(*         in *)
(*         match install_dir with *)
(*         | "" -> *)
(*           Log.info [ Pp.textf "%s: vsvars32.bat found, but registry settings not found" (name t) ]; *)
(*           None *)
(*         | install_dir -> *)
(*           if ... then *)
(*             Ok () *)
(*           else begin *)
(*             Log.info [ Pp.textf "%s: %s doesn't agree with registry" (name t) var ]; *)
(*             None *)
(*           end *)
(*       end *)

(*   let find_vswhere = *)
(*     let* program_files = Env.get "ProgramFiles(x86)" in *)
(*     let vswhere = Filename.concat program_files ("Microsoft Visual Studio\\Installer\\vswhere.exe") in *)
(*     if Sys.file_exists vswhere then *)
(*       let* lines = Memo.Build.of_reproducible_fiber (Process.run_capture_lines ~env Strict vswhere ["-all"; "products '*'"; "-nologo"]) *)
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

(*   val all : t list *)

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

(*   let find_5_2 env = *)
(*     let install_dir = reg64_string "HKLM\\SOFTWARE\\Microsoft\\MicrosoftSDK\\InstalledSDKs\\8F9E5EF3-A9A5-491B-A889-C58EFFECE8B3" "Install Dir" in *)
(*     if install_dir <> "" then begin *)
(*       let setenv = Filename.concat install_dir "SetEnv.cmd" in *)
(*       if Sys.file_exists setenv then *)
(*         Some {Found.sdk = t; setenv} *)
(*       else begin *)
(*         Log.info [ Pp.textf "%s: registry set for Windows Server 2003 SDK, but SetEnv.cmd not found" (name t) ]; *)
(*         None *)
(*       end *)
(*     end else *)
(*       None *)

(*   let find_all env = *)
(*     let root = "HKLM\\SOFTWARE\\Microsoft\\Microsoft SDKs\\Windows" in *)
(*     let lines = "reg" [ "query" ; root ] in *)
(*     List.filter_map (fun i -> *)
(*         let install_dir = reg_string (Printf.sprintf "%s\\%s" sdk_root i) "InstallationFolder" in *)
(*         if install_dir <> "" then *)
(*           if Sys.file_exists (Printf.sprintf "%s\\Bin\\SetEnv.cmd" install_dir) then *)
(*           else *)
(*             None *)
(*         else begin *)
(*           Log.info [ "Registry key for Windows SDK $i doesn't contain expected InstallationFolder value" ]; *)
(*           None *)
(*         end *)
(*       ) ... *)

(* end *)


type t =
  {
    extend_PATH: string;
    var_LIB: string;
    var_INCLUDE: string;
  }

let to_dyn {extend_PATH; var_LIB; var_INCLUDE} =
  let open Dyn.Encoder in
  record
    [ "extend_PATH", string extend_PATH ;
      "var_LIB", string var_LIB ;
      "var_INCLUDE", string var_INCLUDE ]

let equal {extend_PATH; var_LIB; var_INCLUDE} t =
  String.equal extend_PATH t.extend_PATH &&
  String.equal var_LIB t.var_LIB &&
  String.equal var_INCLUDE t.var_INCLUDE

let hash t =
  Hashtbl.hash t

(* let run_test_command () = *)
(*   let path = extend_path "?msvs-detect?;%s;%s" dir path in *)
(*   let comspec = Env.get env "COMSPEC" in *)
(*   let lines = spawn "%s /v:on /c %s 2>NUL" in *)
(*   ... *)

let detect arch = Memo.Build.return None
