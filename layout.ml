let bbox pos =
    let x0, y0 = pos.(0) in
    let min_x, max_x = ref x0, ref x0 in
    let min_y, max_y = ref y0, ref y0 in
    for i = 1 to Array.length pos - 1 do
        let x, y = pos.(i) in
        if !min_x > x
        then min_x := x;
        if !max_x < x
        then max_x := x;
        if !min_y > y
        then min_y := y;
        if !max_y < y
        then max_y := y
    done;
    ( (!min_x, !min_y), (!max_x, !max_y) )

let init g radius =
    let n = Graph.nvertices g in 
    Array.init n (fun _ -> Random.float radius -. radius /. 2., 
        Random.float radius -. radius /. 2.)

let iterate g pos config radius =
    let c1, c2, c3, c4 = config in
    let (x1,y1), (x2,y2) = bbox pos in
    let eades_r = float_of_int (Graph.nvertices g) *. 0.5 in
    let calibrate (x, y) =
        ( eades_r *. (x -. x1) /. radius, eades_r *. (y -. y1) /. radius )
    in
    let restore (x, y) =
        ( radius *. (x /. eades_r), radius *. (y /. eades_r) )
    in
    let n = Graph.nvertices g in
    Array.init n (fun i ->
        let x, y = calibrate pos.(i) in
        let fx, fy = ref 0., ref 0. in
        for j = 0 to n-1 do
            let x', y' = calibrate pos.(j) in
            let dx = x' -. x in
            let dy = y' -. y in
            let thres = 1e-2 in
            let d = sqrt(dx *. dx +. dy *. dy ) in
            if d > thres then begin
                (* EADES  *)
                if Graph.connected g i j || Graph.connected g j i
                then begin
                    fx := !fx +. c1 *. log (d /. c2) *. dx /. d;
                    fy := !fy +. c1 *. log (d /. c2) *. dy /. d
                end;
                fx := !fx -. c3 *. dx /. (d ** 3.0);
                fy := !fy -. c3 *. dy /. (d ** 3.0)
                (* Frucheterman & Reingold *)
                (*
                if g.Graph.mat.(i).(j)  || g.Graph.mat.(j).(i)
                then begin
                    fx := !fx +. d *. d /. c2 *. dx /. d;
                    fy := !fy +. d *. d /. c2 *. dy /. d
                end;
                fx := !fx -. c2 *. c2 *. dx /. (d ** 2.0);
                fy := !fy -. c2 *. c2 *. dy /. (d ** 2.0)
                *)
            end
        done;
        restore (x +. c4 *. !fx, y +. c4 *. !fy)
    )

let optimal_parameters g =
    let n = Graph.nvertices g in
    let p = Graph.nedges g in
    let d = Graph.diameter g in
    let d_avg = Graph.average_degree g in
    (2., 2., 1., 0.2)

let eades g graph_radius =
    let n = Graph.nvertices g in
    let p = Graph.nedges g in
    let layout = ref (optimal_parameters g) in
    let pos = ref (init g graph_radius) in
    for _ = 0 to 1000 do
        pos := iterate g !pos !layout graph_radius;
        let a, b, c, d = !layout in
        layout := (a,b,c,0.999 *. d)
    done;
    !pos

