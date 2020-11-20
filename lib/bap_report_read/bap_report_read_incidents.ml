open Core_kernel
open Monads.Std
open Bap_report_types
open Bap_report_read_types

module Parse = Bap_report_parse_incidents

type machine = {
  pid : machine_id;
  pc  : addr;
  stack : (addr * string) list;
  prev_pc : addr option;
}

type t = {
  cur   : machine_id option;
  machs : machine Machine_id.Map.t;
  hist  : addr list Location_id.Map.t;
  syms  : string Addr.Map.t;
  calls : string list Addr.Map.t;
  incs  : incident list
}

type incident_data =
  | Name of string
  | Locations of addr * addr list
  | Path of string list


module Make(M : Monad.S) = struct
  module S =
    Monad.State.Make(struct
      type nonrec t = t
    end)(M)

  open S.Syntax

  let unresolved = "__primus_linker_unresolved_call"

  let return = S.return

  let new_machine pid =
    let pc = Addr.of_string "0" in
    {pid;stack=[];pc; prev_pc=None;}

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
    | None -> return ()
    | Some x -> f x

  let (>>~) = opt

  let last_n = 5

  let string_of_addrs addrs =
    String.concat ~sep:" " (List.map ~f:Addr.to_string addrs)

  let call name =
    S.get () >>= fun state ->
    machine >>~ fun m ->
    let calls,stack =
      match m.stack, m.prev_pc with
      | (pc,_) :: _,_ when Poly.(pc = m.pc) ->
        (* to prevent adding lisp calls on the stack  *)
        state.calls, m.stack
      | stack, Some prev_pc ->
        (* to prevent data rewriting when we have a tail call, e.g.
           call memset
           call sub_deadbeaf with no return *)
        let pc =
          match Map.find state.calls prev_pc with
          | Some _ -> m.pc
          | None  -> prev_pc in
        let last_few = name :: (List.take stack last_n |> List.map ~f:snd) in
        Map.set state.calls ~key:pc ~data:last_few,
        (m.pc, name) :: stack
      | _ -> state.calls, m.stack in
    update_machine {m with stack} >>= fun () ->
    S.update (fun s -> {s with calls})

  let call_return name =
    S.get () >>= fun s ->
    machine >>~ fun m ->
    let stack,calls =
      match m.stack with
      | (pc,name') :: stack' when String.(name = name') ->
        let calls =
          if Poly.(m.pc = pc) then
            (*  for lisp calls, want be sure it is in calls:
                i.e. there wasn't pc-change event between
                call  and call-return, therefore the
                correct address of the can be infered like that *)
            let last_few = name :: (List.take stack' last_n |> List.map ~f:snd) in
            Map.set s.calls ~key:pc ~data:last_few
          else s.calls in
        stack', calls
      | _ -> m.stack, s.calls in
    update_machine {m with stack} >>= fun () ->
    S.update (fun s -> {s with calls})

  (* path is a little bit complex thing to define
     since there are different incidents types, and it's not so
     easy to make a uniform solution for all of them. so there will be
     the next approach:
     - check if there is a stored stack state in the {calls} map
         for incident address
         (e.g. for incidents whose subject is a function call, like cwe-252)
     - otherwise just take last few from the current stack
     - otherwise try to find a symbol by address of the incidents  *)
  let path addr =
    let from_syms s =
      match Map.find s.syms addr with
      | None -> return []
      | Some s -> return [s] in
    let from_calls s =
      return (Option.value ~default:[] (Map.find s.calls addr)) in
    let from_stack =
      machine >>= fun m ->
      match m with
      | None -> return []
      | Some m ->
        return (List.take m.stack last_n |> List.map ~f:snd) in
    S.get () >>= fun s ->
    let order = [from_calls s; from_stack; from_syms s] in
    S.List.find_map order ~f:(fun f ->
        f >>= function
        | [] -> return None
        | x -> return (Some x)) >>= function
    | None -> return []
    | Some x -> return x

  let locs_to_str locs =
    String.concat ~sep:" " @@
    List.map locs  ~f:Addr.to_string

  let incident kind locs =
    let location_addr hist id = Option.(Map.find hist id >>= List.hd) in
    S.get () >>= fun s ->
    machine >>= fun m ->
    match List.filter_map locs ~f:(location_addr s.hist) with
    | [] ->
      eprintf "WARNING! missed incident with locations %s\n" @@
      List.fold locs ~init:"" ~f:(fun s loc ->
          sprintf "%s%s " s @@ Location_id.to_string loc);
      return ()
    | addr :: prev ->
      path addr >>= fun path ->
      let locs = Locations.create ~prev addr in
      let inc = Incident.create ~path locs kind in
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
    | Call name | Call_return name when String.(name = unresolved) -> return ()
    | Call name -> call name
    | Call_return name -> call_return name
    | Incident_location (id,addrs) -> incident_location (id,addrs)
    | Pc_changed addr -> pc_change addr
    | Symbol (name,addr) -> symbol (name,addr)
    | Incident (name, locs) -> incident name locs

  let run ch =
    let rec loop () =
      match Parse.read ch with
      | None -> return ()
      | Some ev -> event ev >>= loop in
    loop ()
end

module Main = Make(Monad.Ident)

let read ch =
  let fresh = {cur = None;
               hist =Map.empty (module Location_id);
               calls=Map.empty (module Addr);
               syms =Map.empty (module Addr);
               machs=Map.empty (module Machine_id);
               incs=[]} in
  let results = Monad.State.exec (Main.run ch) fresh in
  results.incs
