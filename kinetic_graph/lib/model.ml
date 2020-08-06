open Base
open Graph_builds

module Edge = struct
  type t = { name : string
           ; from : string
           ; dest : string
           ; rate : float
           ; sens : float option
           }

  (* Delta along edge as a function of state, time, and maybe agoinst. *)
  let get_flow { from = a; rate = r; sens = sens; _ } =
    match sens with
    | None -> fun state dt _ ->
      (Map.find_exn state a) *. r *. dt
    | Some s -> fun state dt agonist ->
      (Map.find_exn state a) *. s *. agonist *. r *. dt

  let build (name, rate, sens) =
    let (from, dest) =
      String.split name ~on:'>'
      |> function
      | [ f; d ] -> (f, d)
      | _ -> raise (Failure "Invalid edge.") in
    { name = name
    ; from = from
    ; dest = dest
    ; rate = rate
    ; sens = if sens > 0  then Some (Float.of_int sens) else None
    }
end

module Graph = struct
  type t = { nodes : string list; edges : Edge.t list }

  let build { names = nodes; wiring = specs } =
    { nodes = nodes ; edges = List.map ~f:Edge.build specs }

  (* First node is assumed to begin with all of the state value. *)
  let init_state { nodes; _ } =
    nodes
    |> List.map ~f:(fun n -> (n, 0.))
    |> Map.of_alist_exn (module String)
    |> Map.set ~key:(List.hd_exn nodes) ~data:1.

  let get_flows { edges; _ } =
    edges |> List.map ~f:Edge.get_flow

  (* Calculate delta along each edge for current time step. *)
  let commit dt agonist flows state =
    List.map ~f:(fun f -> f state dt agonist) flows

  (* Move value along edges, sub from node A, add to node B. *)
  let apply edges state deltas =
    let updater d = function
      | None -> 0.
      | Some s -> s +. d |> Float.clamp_exn ~min:0. ~max:1. in
    let do_shift m (e : Edge.t) d =
      Map.update m e.dest ~f:(updater d)
      |> fun m -> Map.update m e.from ~f:(updater (-.d)) in
    List.zip_exn edges deltas
    |> List.fold_left ~init:state ~f:(fun m (e, d) -> do_shift m e d)

  let step dt edges flows agonist state =
    commit dt agonist flows state
    |> apply edges state
end

module Rig = struct
  type t = { tstop : float
           ; dt : float
           ; time : float list
           ; graph : Graph.t
           ; agonist : float list
           }

  let time_wave tstop dt =
    Sequence.range 1 (tstop /. dt |> Int.of_float)
    |> Sequence.map ~f:(Float.of_int |> Fn.compose (( *. ) dt))
    |> Sequence.to_list

  let build (module B : BuildSpec) space =
    let time = time_wave B.tstop B.dt in
    { tstop = B.tstop
    ; dt = B.dt
    ; time = time
    ; graph = Graph.build B.spec
    ; agonist = Diffusion.get_profile space time
    }

  let set_agonist rig space =
    { rig with agonist = Diffusion.get_profile space rig.time }

  (* Convert list of state snapshot Maps into a Map of recording lists.
   * List is reversed to correct chronological order in process. *)
  let collect_recs l =
    let init =
      List.hd_exn l
      |> Map.keys
      |> List.map ~f:(fun k -> (k, []))
      |> Map.of_alist_exn (module String) in
    let prepend_point p = function
      | None -> raise (Failure "Incomplete recording set.")
      | Some data -> p :: data in
    let collector collection snapshot =
      let f ~key:k ~data:v c = Map.update c k ~f:(prepend_point v) in
      Map.fold ~init:collection ~f snapshot in
    List.fold_left ~init ~f:collector l

  let run rig =
    let flows = Graph.get_flows rig.graph in
    let init = [ Graph.init_state rig.graph ] in
    let step' = Graph.step rig.dt rig.graph.edges flows in
    let f recs agon = List.hd_exn recs |> step' agon |> fun s -> s :: recs in
    List.fold_left ~init ~f rig.agonist
    |> collect_recs
end
