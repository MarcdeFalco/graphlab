
let init g r =
    let n = Graph.nvertices g in 
    Array.init n (fun _ -> Random.float r -. r /. 2., 
        Random.float r -. r /. 2.)

let iterate g pos config =
    let c1, c2, c3, c4 = config in
    let n = Graph.nvertices g in
    Array.init n (fun i ->
        let x, y = pos.(i) in
        let fx, fy = ref 0., ref 0. in
        for j = 0 to n-1 do
            let x', y' = pos.(j) in
            let dx = x' -. x in
            let dy = y' -. y in
            let d = sqrt(dx *. dx +. dy *. dy ) in
            if i <> j then begin
                (* EADES  *)
                if g.Graph.mat.(i).(j)  || g.Graph.mat.(j).(i)
                then begin
                    fx := !fx +. c1 *. log (d /. c2) *. dx /. d;
                    fy := !fy +. c1 *. log (d /. c2) *. dy /. d
                end else begin
                    fx := !fx -. c3 *. dx /. (d ** 3.0);
                    fy := !fy -. c3 *. dy /. (d ** 3.0)
                end
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
        x +. c4 *. !fx, y +. c4 *. !fy
    )

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

