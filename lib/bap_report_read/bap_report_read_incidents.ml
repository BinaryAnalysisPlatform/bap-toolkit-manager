open Core_kernel
open Monads.Std
open Bap_report_types
open Bap_report_read_types

module Parse = Bap_report_parse_incidents


type machine = {
  pid : string;
  pc  : addr;
  stack : (addr * string) list;
  prev_pc : addr option;
}

type t = {
  cur   : machine_id option;
  machs : machine Machine_id.Map.t;
  hist  : addr list Location_id.Map.t;
  syms  : addr String.Map.t;
  calls : string list Addr.Map.t;
  incs  : incident list
}

module Make(M : Monad.S) = struct
  module S =
    Monad.State.Make(struct
        type nonrec t = t
        end)(M)

  open S.Syntax

  let check_of_string name =
    try
      let name = String.map ~f:(fun c -> if c = '-' then '_' else c) name in
      let name = String.capitalize name in
      Some (check_of_sexp (Sexp.of_string name))
    with _ -> None

  let unresolved = "__primus_linker_unresolved_call"

  let new_machine pid = {pid;stack=[];pc=""; prev_pc=None;}

  let stack_top_name = function
    | [] -> None
    | (_,name) :: _ -> Some name

  let machine =
    S.gets (fun s ->
        match s.cur with
        | None -> None
        | Some id -> Map.find s.machs id)

  let update_machine m =
    S.update (fun s ->
        {s with machs = Map.set s.machs ~key:m.pid ~data:m; cur = Some m.pid})


  let opt x f =
    x >>= fun x ->
    match x with
    | None -> !! ()
    | Some x -> f x

  let (>>~) = opt

  let call name =
    S.get () >>= fun state ->
    machine >>~ fun m ->
    let calls,stack =
      match m.stack, m.prev_pc with
      | (pc,_) :: _,_ when pc = m.pc ->
         (* to prevent adding lisp calls on the stack  *)
         state.calls, m.stack
      | stack, Some prev_pc ->
         let last_few = name :: (List.take stack 5 |> List.map ~f:snd) in
         Map.set state.calls ~key:prev_pc ~data:last_few,
         (m.pc, name) :: stack
      | _ -> state.calls, m.stack in
    update_machine {m with stack} >>= fun () ->
    S.update (fun s -> {s with calls})

  let call_return name =
    S.get () >>= fun s ->
    machine >>~ fun m ->
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
             Map.set s.calls ~key:pc ~data:last_few
           else s.calls in
         stack', calls
      | _ -> m.stack, s.calls in
    update_machine {m with stack} >>= fun () ->
    S.update (fun s -> {s with calls})

  let incident name locs =
    let location_addr hist id = Option.(Map.find hist id >>= List.hd) in
    match check_of_string name with
    | None -> !! ()
    | Some check ->
       S.get () >>= fun s ->
       machine >>= fun m ->
       let trace,machine = match m with
         | None -> [],None
         | Some m ->
            List.take m.stack 5 |> List.map ~f:snd,
            Some m.pid in
       let locs = List.filter_map locs ~f:(location_addr s.hist) in
       let inc = Incident.create ~trace ?machine check locs in
       S.update (fun s -> {s with incs = inc :: s.incs})

  let incident_location (id,addrs) =
    S.update (fun s -> {s with hist = Map.set s.hist ~key:id ~data:addrs})

  let switch (_, id) =
    S.get () >>= fun s ->
    let mach = match Map.find s.machs id with
      | None -> new_machine id
      | Some m -> m in
    update_machine mach

  let fork (from, to_) =
    S.get () >>= fun s ->
    let mach =
      match Map.find s.machs from with
      | None -> new_machine to_
      | Some m -> {m with pid=to_} in
    update_machine mach

  let pc_change addr =
    machine >>~ fun m ->
    update_machine {m with pc = addr; prev_pc=Some m.pc}

  let symbol (name,addr) =
    S.update (fun s -> {s with syms = Map.set s.syms ~key:addr ~data:name})

  let event = function
    | Switch (a,b) -> switch (a,b)
    | Fork   (a,b) -> fork (a,b)
    | Call name | Call_return name when name = unresolved -> !! ()
    | Call name -> call name
    | Call_return name -> call_return name
    | Incident_location (id,addrs) -> incident_location (id,addrs)
    | Pc_changed addr -> pc_change addr
    | Symbol (name,addr) -> symbol (name,addr)
    | Incident (name, locs) -> incident name locs

  let run ch =
    let rec loop () =
      match Parse.read ch with
      | None -> !! ()
      | Some ev -> event ev >>= loop in
    loop ()
end

module Main = Make(Monad.Ident)


module Data_format = struct

  let symbol_name s inc =
    match Incident.locations inc with
    | [] -> None
    | a :: _ ->
       match Map.find s.syms a with
       | Some name -> Some [name]
       | _ ->  Some [a]

  let null_deref _ inc =
    let trace = Incident.trace inc in
    match Incident.locations inc with
    | event :: _ ->
       let last = Option.value ~default:"" (List.hd trace) in
       Some [last;event]
    | _ -> None

  let unused_return s inc =
    match Incident.locations inc with
    | [] -> None
    | a :: _ ->
      match Map.find s.calls a with
      | Some (name :: prev :: _ )->
         Some  [prev; name; a]
      | Some (name :: [])->
         Some ["-----"; name; a]
      | _ -> None

  let use_after_free _ inc =
    match Incident.locations inc with
    | use :: free :: alloc :: _ -> Some [use; free; alloc]
    | _ -> None

end

let collect results =
  let update acc inc f =
    match f results inc with
    | None -> acc
    | Some data ->
       let inc = Incident.add_data inc data in
       inc :: acc in
  List.fold results.incs ~init:[]
    ~f:(fun arti inc ->
      let f =
        match Incident.check inc with
        | Forbidden_function | Complex_function
        | Non_structural_cfg | Recursive_function -> Data_format.symbol_name
        | Unused_return_value -> Data_format.unused_return
        | Null_ptr_deref -> Data_format.null_deref
        | Memcheck_use_after_release -> Data_format.use_after_free
        | _ -> (fun _ _ -> None) in
      update arti inc f)

let read ch =
  let empty = Map.empty (module String) in
  let fresh = {cur = None; hist=empty;
               calls=empty;syms=empty;
               machs=empty;incs=[]} in
  let results = Monad.State.exec (Main.run ch) fresh in
  collect results
