open Core_kernel

open Bap_report_types

type location_id = string
type machine_id = string
type addr = string

module Machine_id = String
module Location_id = String
module Addr = String

type event =
  | Incident_location of location_id * addr list
  | Incident of string * location_id list
  | Fork   of machine_id * machine_id
  | Switch of machine_id * machine_id
  | Call of string
  | Call_return of string
  | Symbol of string * addr
  | Pc_changed of addr

module Parse = struct

  open Sexp

  let addr_of_string str =
    match String.split str ~on:':' with
    | x :: _ ->
       let prefix = "0x" in
       let x =
         if String.is_prefix x ~prefix:"0x" then
           String.chop_prefix_exn x ~prefix
         else x in
       prefix ^ String.lowercase x
    | _ -> str

  let point_of_sexp x = match x with
    | List _ -> None
    | Atom x ->
      match String.split ~on:':' x  with
      | [_; addr] -> Some (addr_of_string addr)
      | _ -> None

  let trace_of_sexps xs =
    List.filter_map ~f:point_of_sexp xs

  let locs_of_sexps xs =
    List.filter_map xs ~f:(function
        | Atom s -> Some s
        | _ -> None)

  let some = Option.some

  let of_sexp s = match s with
    | List (Atom "incident-location" :: List [Atom loc_id; List points] :: _)  ->
      Incident_location (loc_id, trace_of_sexps points) |> some
    | List (Atom "incident" :: List (Atom name :: locs) :: _) ->
      Incident (name, locs_of_sexps locs) |> some
    | List (Atom "machine-switch" :: List [Atom from; Atom to_ ] :: _ ) ->
      Switch (from,to_) |> some
    | List (Atom "machine-fork" :: List [Atom from; Atom to_ ] :: _ ) ->
      Fork (from,to_) |> some
    | List (Atom "call" :: List (Atom name :: _) :: _ ) ->
      Call name |> some
    | List (Atom "call-return" :: List (Atom name :: _) :: _ ) ->
      Call_return name |> some
    | List (Atom "symbol" :: List [Atom name; Atom addr] :: _) ->
      Symbol (name, addr_of_string addr) |> some
    | List (Atom "pc-changed" :: Atom addr :: _) ->
      Pc_changed (addr_of_string addr) |> some
    | _ -> None

  let of_sexp s =
    try
      of_sexp s
    with _ -> None

  let rec read ch =
    let read_sexp ch =
      try
        Sexp.input_sexp ch |> some
      with _ -> None in
    match read_sexp ch with
    | None -> None
    | Some s ->
      match of_sexp s with
      | None -> read ch
      | Some _ as r -> r

end

type machine = {
  pid : string;
  pc  : addr;
  stack : (addr * string) list;
  prev_pc : addr option;
}


type incident = {
  check : check;
  mach : string;
  locs  : addr list;
  last_calls : string list;
}


type state = {
  cur   : machine_id;
  machs : machine Machine_id.Map.t;
  hist  : addr list Location_id.Map.t;
  syms  : addr String.Map.t;
  calls : string list Addr.Map.t;
  incs  : incident list
}

let check_of_string name =
  try
    let name = String.map ~f:(fun c -> if c = '-' then '_' else c) name in
    let name = String.capitalize name in
    Some (check_of_sexp (Sexp.of_string name))
  with _ -> None

let empty = Map.empty (module String)

let unresolved = "__primus_linker_unresolved_call"

let new_machine pid = {pid;stack=[];pc=""; prev_pc=None;}

let stack_top_name = function
  | [] -> None
  | (_,name) :: _ -> Some name

let get s = Map.find_exn s.machs s.cur
let put s m = {s with machs = Map.set s.machs m.pid m; cur = m.pid}

let on_call state name =
  let m = get state  in
  let calls,stack =
    match m.stack, m.prev_pc with
    | (pc,_) :: _,_ when pc = m.pc ->
       (* to prevent adding lisp calls on the stack  *)
       state.calls, m.stack
    | stack, Some prev_pc ->
       let last_few = name :: (List.take stack 5 |> List.map ~f:snd) in
       Map.set state.calls prev_pc last_few,
       (m.pc, name) :: stack
    | _ -> state.calls, m.stack in
  let cur = {m with stack} in
  let state = put state cur in
  {state with calls;}

let on_call_return state name =
  let m = get state in
  let stack,calls =
    match m.stack with
    | (pc,name') :: stack' when name = name' ->
       let calls =
         if m.pc = pc then
           (* for lisp calls, want be sure it is in calls:
                 i.e. there aren't pc-change event between
                 call  and call-return, therefore the
                 correct address of the can be infered like that *)
           let last_few = name :: (List.take stack' 5 |> List.map ~f:snd) in
           Map.set state.calls pc last_few
         else state.calls in
       stack', calls
    | _ -> m.stack, state.calls in
  let cur = {m with stack;} in
  let state = put state cur in
  {state with calls}

let on_incident s name locs =
  let location_addr hist id = Option.(Map.find hist id >>= List.hd) in
  match check_of_string name with
  | None -> s
  | Some check ->
     let m = get s in
     let locs = List.filter_map locs ~f:(location_addr s.hist) in
     let last_calls = List.take m.stack 5 |> List.map ~f:snd in
     {s with incs={check; locs; last_calls;mach=m.pid} :: s.incs}

let on_switch s (_, id) =
  let mach = match Map.find s.machs id with
    | None -> new_machine id
    | Some m -> m in
  put s mach

let on_fork s (from, to_) =
  let mach =
    match Map.find s.machs from with
    | None -> new_machine to_
    | Some m -> {m with pid=to_} in
  put s mach

let on_pc_change s addr =
  let m = get s in
  let m = {m with pc = addr; prev_pc=Some m.pc} in
  put s m

let parse file =
  let ch = In_channel.create file in
  let rec loop s =
    match Parse.read ch with
    | None -> s
    | Some ev -> match ev with
      | Switch (a,b) -> loop (on_switch s (a,b))
      | Fork   (a,b) -> loop (on_fork s (a,b))
      | Call name | Call_return name when name = unresolved -> loop s
      | Call name -> loop (on_call s name)
      | Call_return name -> loop (on_call_return s name)
      | Incident_location (id,addrs) -> loop {s with hist = Map.set s.hist id addrs}
      | Pc_changed addr -> loop (on_pc_change s addr)
      | Symbol (name,addr) -> loop {s with syms = Map.set s.syms addr name}
      | Incident (name, locs) -> loop (on_incident s name locs) in
  let r = loop {cur = ""; hist=empty;
                calls=empty;syms=empty;
                machs=empty;incs=[]} in
  In_channel.close ch;
  r

let make_data state =
  let update checks check data =
    Map.add_multi checks check data in
  let with_symbol inc acc =
    match inc.locs with
    | [] -> acc
    | a :: _ ->
      let r = match Map.find state.syms a with
        | Some name -> [name]
        | _ ->  [a] in
      update acc inc.check r in
  let with_symbol_call inc acc =
    match inc.locs with
    | [] -> acc
    | a :: _ ->
      match Map.find state.calls a with
      | Some (name :: prev :: _ )->
         update acc inc.check [prev; name; a]
      | Some (name :: [])->
         update acc inc.check ["-----"; name; a]
      | _ -> acc in
  let null_ptr_deref inc acc =
    match inc.locs with
    | event::_ ->
      let last = Option.value ~default:"" (List.hd inc.last_calls) in
      update acc inc.check [last;event]
    | _ -> acc in
  let with_three_addresses inc acc =
    match inc.locs with
    | use :: free :: alloc :: _ -> update acc inc.check [use; free; alloc]
    | _ -> acc in
  List.fold state.incs ~init:(Map.empty (module Check))
    ~f:(fun acc inc ->
        match inc.check with
        | Forbidden_function | Complex_function
        | Non_structural_cfg | Recursive_function -> with_symbol inc acc
        | Unused_return_value -> with_symbol_call inc acc
        | Null_ptr_deref -> null_ptr_deref inc acc
        | Memcheck_use_after_release -> with_three_addresses inc acc
        | _ -> acc)

let form_data (check, data) =
  let total = List.length data in
  let stat =
    {false_pos=0; false_neg=0;confirmed=0;undecided=total;
     took_time="-"} in
  check,stat,List.map data ~f:(fun x -> x, Undecided)

let run file =
  let state = parse file in
  let data = make_data state  in
  List.map (Map.to_alist data) ~f:form_data
