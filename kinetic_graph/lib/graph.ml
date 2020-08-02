open Base
open Graph_builds

module FloatUtil = struct
  let  ( *^. ) a b = a *. 10. **. b
end

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
  let init_state { nodes = nodes; _ } =
    nodes
    |> List.map ~f:(fun n -> (n, 0.))
    |> Map.of_alist_exn (module String)
    |> Map.set ~key:(List.hd_exn nodes) ~data:1.

  let get_flows { edges = edges; _ } =
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

module Diffusion = struct
  open FloatUtil

  type t = float list

  type spec2D = { n_mols : float
                ; coef : float
                ; height : float
                ; radius : float
                }

  type spec3D = { n_mols : float
                ; coef : float
                ; radius : float
                ; alpha : float
                ; lambda : float
                }

  type space = TwoD of spec2D | ThreeD of spec3D

  let glut_2D r = TwoD { n_mols = 4700.
                       ; coef   = 7.6 *^. -10.
                       ; height = 20. *^. -9.
                       ; radius = r
                       }

  let ach_2D r = TwoD { n_mols = 10000.
                      ; coef   = 4. *^. -10.
                      ; height = 20. *^. -9.
                      ; radius = r
                      }

  let glut_3D  r = ThreeD { n_mols = 4700.
                          ; coef   = 7.6 *^. -10.
                          ; alpha = 0.12
                          ; lambda = 1.55
                          ; radius = r
                          }

  let ach_3D r = ThreeD { n_mols = 10000.
                        ; coef   = 4. *^. -10.
                        ; alpha = 0.12
                        ; lambda = 1.55
                        ; radius = r
                        }

  let calc2D { n_mols = m; coef = d; height = h; radius = r } = fun t ->
    let moles = m /. (6.02 *^. 23.) in
    let d' = d *. t in
    let x = moles /. (4. *. h *. Float.pi *. d') in
    let y = Float.exp ((-.r) **. 2. /. (4. *. d')) in
    x *. y

  let calc3D { n_mols = m; coef = d; radius = r; alpha = a; lambda = l } = fun t ->
    let moles = m /. (6.02 *^. 23.) in
    let d' = t *. (d /. l **. 2.) in
    let x = moles /. (8. *. a *. (Float.pi *. d') **. 1.5 ) in
    let y = Float.exp ((-.r) **. 2. /. (4. *. d)) in
    x *. y

  (* Temporal profile of agonist concentration. *)
  let get_profile space_spec time =
    let calc = match space_spec with
      | TwoD spec -> calc2D spec
      | ThreeD spec -> calc3D spec in
    List.map ~f:calc time
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

  let build tstop dt graph_specs space =
    let time = time_wave tstop dt in
    { tstop = tstop
    ; dt = dt
    ; time = time
    ; graph = Graph.build graph_specs
    ; agonist = Diffusion.get_profile space time
    }

  let build' (module B : BuildSpec) space =
    let time = time_wave B.tstop B.dt in
    { tstop = B.tstop
    ; dt = B.dt
    ; time = time
    ; graph = Graph.build B.spec
    ; agonist = Diffusion.get_profile space time
    }

  let set_agonist rig space =
    { rig with agonist = Diffusion.get_profile space rig.time }

  let run rig =
    let flows = Graph.get_flows rig.graph in
    let init = [ Graph.init_state rig.graph ] in
    let step' = Graph.step rig.dt rig.graph.edges flows in
    let f recs agon = List.hd_exn recs |> step' agon |> fun s -> s :: recs in
    List.fold_left ~init ~f rig.agonist
end

module Test = struct
  (* Testing out a run. Not settled on exact architecture that I'd like to
   * follow here. *)
  let test () =
    Diffusion.ach_2D 0.
    (* |> Rig.build' @@ Gaba.make () *)
    |> Rig.build' @@ Alpha7.make ~on_multi:1. ~desens_div:1. ()
    |> Rig.run
end
