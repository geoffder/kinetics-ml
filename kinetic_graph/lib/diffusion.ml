open Base

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

let calc2D { n_mols = m; coef = d; height = h; radius = r } = fun t ->
  let moles = m /. (6.02 *^. 23.) in
  let d' = d *. t /. 1000. in
  let x = moles /. (4. *. h *. Float.pi *. d') in
  let y = Float.exp ((-.r) **. 2. /. (4. *. d')) in
  x *. y

let calc3D { n_mols = m; coef = d; radius = r; alpha = a; lambda = l } = fun t ->
  let moles = m /. (6.02 *^. 23.) in
  let d' = (d /. l **. 2.) *. t /. 1000. in
  let x = moles /. (8. *. a *. (Float.pi *. d') **. 1.5 ) in
  let y = Float.exp ((-.r) **. 2. /. (4. *. d)) in
  x *. y

(* Temporal profile of agonist concentration. Time [s] -> concentration [mM] *)
let get_profile space_spec time =
  let calc = match space_spec with
    | TwoD spec -> calc2D spec
    | ThreeD spec -> calc3D spec in
  List.map ~f:calc time
