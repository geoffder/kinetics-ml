open Base

type spec_2D = { n_mols : float  (* molecules released *)
               ; coef   : float  (* diffusion coefficient *)
               ; height : float  (* distance across cleft [m] *)
               ; radius : float  (* distance to site [m] *)
               }

type spec_3D = { n_mols : float  (* molecules released *)
               ; coef   : float  (* diffusion coefficient *)
               ; radius : float  (* distance across cleft [m] *)
               ; alpha  : float  (* volume fraction *)
               ; lambda : float  (* tortuosity *)
               }

type pulse = { onset         : float  (* agonist pulse onset *)
             ; duration      : float  (* square pulse duration *)
             ; concentration : float  (* concentration [mM] *)
             }

type space = TwoD of spec_2D | ThreeD of spec_3D | Pulse of pulse list

(* Scientific notation convenience operator. *)
let  ( *^. ) a b = a *. 10. **. b

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

let calc_2D { n_mols = m; coef = d; height = h; radius = r } = fun t ->
  let moles = m /. (6.02 *^. 23.) in
  let d' = d *. t /. 1000. in
  let x = moles /. (4. *. h *. Float.pi *. d') in
  let y = Float.exp (-.(r **. 2.) /. (4. *. d')) in
  x *. y

let calc_3D { n_mols = m; coef = d; radius = r; alpha = a; lambda = l } = fun t ->
  let moles = m /. (6.02 *^. 23.) in
  let d' = (d /. l **. 2.) *. t /. 1000. in
  let x = moles /. (8. *. a *. (Float.pi *. d') **. 1.5 ) in
  let y = Float.exp (-.(r **. 2.) /. (4. *. d')) in
  x *. y

let calc_pulse ps =
  let check t acc { onset = on; duration = dur; concentration = conc } =
    if Float.(t > on && t < on +. dur) then conc +. acc else acc in
  fun t -> List.fold ~init:0. ~f:(check t) ps

(* Temporal profile of agonist concentration. Time [s] -> concentration [mM] *)
let get_profile space_spec time =
  let calc = match space_spec with
    | TwoD spec -> calc_2D spec
    | ThreeD spec -> calc_3D spec
    | Pulse pulses -> calc_pulse pulses in
  List.map ~f:calc time
