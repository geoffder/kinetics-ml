open Base

module Edge = struct
  type t = { name : string
           ; from : string
           ; dest : string
           ; rate : float
           ; sens : float option
           }

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

  let build nodes specs =
    { nodes = nodes ; edges = List.map ~f:Edge.build specs }

  (* First node is assumed to begin with all of the state value. *)
  let init_state { nodes = nodes; _ } =
    nodes
    |> List.map ~f:(fun n -> (n, 0.))
    |> Map.of_alist_exn (module String)
    |> Map.set ~key:(List.hd_exn nodes) ~data:1.

  let get_flows { edges = edges; _ } =
    edges |> List.map ~f:Edge.get_flow

  let shifts dt agonist flows state =
    List.map ~f:(fun f -> f state dt agonist) flows

  let apply state edges deltas =
    let updater d = function
      | None -> 0.
      | Some s -> s +. d |> Float.clamp_exn ~min:0. ~max:1. in
    let do_shift m (e : Edge.t) d =
      Map.update m e.dest ~f:(updater d)
      |> fun m -> Map.update m e.from ~f:(updater ((-1.) *. d)) in
    List.zip_exn edges deltas
    |> List.fold_left ~init:state ~f:(fun m (e, d) -> do_shift m e d)
end

module Rig = struct
  type t = { mutable dt : float }

  let config = { dt = 0.1 }
end

class rig step = object
  val dt = step
end