open Base
open Matplotlib

(* Locals *)
open Graph_builds
open Model

(* Consider making PR to add this in. Pretty basic convenience function.
 * Think about whether other basic conveniences are missing that would be
 * easy to contribute. *)
let subplots ?figsize nrows ncols =
  let fig = Fig.create ?figsize () in
  let add index = Fig.add_subplot fig ~nrows ~ncols ~index in
  let axes = List.range 1 (nrows * ncols) |> List.map ~f:add in
  fig, axes

(* Add together all open states (any node with an '*'). *)
let open_prob m =
  Map.keys m
  |> List.filter ~f:(fun s -> String.contains s '*')
  |> List.map ~f:(Map.find_exn m)
  |> List.reduce_exn ~f:(List.map2_exn ~f:( +. ))

let prox_vs_distal title diff builder =
  let rig_prox : Rig.t = 0. |> diff |> builder in
  let rig_dist = Diffusion.(1.1 *^. -.6. |> diff) |> builder in
  let time = rig_prox.time |> List.cons 0. |> List.to_array in
  let prox = rig_prox |> Rig.run |> open_prob |> List.to_array in
  let dist = rig_dist |> Rig.run |> open_prob |> List.to_array in
  let fig, ax = Fig.create_with_ax () in
  Ax.plot ax ~label:"Proximal" ~xs:time prox;
  Ax.plot ax ~label:"Distal" ~xs:time dist;
  Ax.set_xlabel ax "Time (ms)";
  Ax.set_ylabel ax "Probability";
  Ax.legend ax;
  Fig.suptitle fig title;
  Mpl.show ()

let alpha6_pvd () =
  Rig.build @@ Alpha7.make ~on_multi:1. ~desens_div:4. ()
  |> prox_vs_distal "Alpha6" Diffusion.ach_2D

let alpha7_pvd () =
  Rig.build @@ Alpha7.make ()
  |> prox_vs_distal "Alpha7" Diffusion.ach_2D

let alpha3_pvd () =
  Rig.build @@ Alpha3.make ()
  |> prox_vs_distal "Alpha3" Diffusion.ach_2D

let gaba_pvd () =
  Rig.build @@ Gaba.make ()
  |> prox_vs_distal "Gaba" Diffusion.glut_2D
