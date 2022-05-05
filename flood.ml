
let () = 
    let open Raylib in
    let n = 200 in

    Printexc.record_backtrace true;
    let p = Array.make_matrix n n (Color.white) in
    let invalid = Array.make_matrix n n false in

    let current_click = ref 0 in

    let plot i j c =
        p.(i).(j) <- c;
        invalid.(i).(j) <- true
    in

    for i = 0 to n - 1 do
        plot (n / 3) i Color.black;
        plot ((2 *n) / 3) i Color.black
    done;
    
    let zoom = 3 in
    set_config_flags [ConfigFlags.Msaa_4x_hint];
    init_window (n*zoom) (n*zoom) "Graphlab";
    set_target_fps 60;
    begin_drawing ();
    clear_background Color.raywhite;
    end_drawing ();

    let hue = ref 0.0 in
    let i_hue = ref 0.01 in

    let paint_stack = Stack.create () in
    let paint_queue = Queue.create () in
    let paint_random = ref [] in
    let rec suppr l x = match l with
        | [] -> []
        | t::q -> if x = t then q else t :: suppr q x
    in
    let take_random p =
        let k = Random.int (List.length !p) in
        let x = List.nth !p k in
        p := suppr !p x;
        x
    in

    let rec melange l =
        match l with
        | [] -> []
        | _ -> 
            let k = Random.int (List.length l) in
            let x = List.nth l k in
            x :: melange (suppr l x)
    in

    let voisins l = 
        (*
        List.rev l
        *)
        melange l
    in

    while not (window_should_close ()) do
        for _ = 0 to 50 do
            if !paint_random <> []
            then begin
                let (i, j) = take_random paint_random in
                if p.(i).(j) = Color.white
                then begin
                    hue := !hue +. !i_hue;
                    if !hue >= 360.
                    then hue := 0.;

                    let c = color_from_hsv !hue 1.0 1.0 in

                    plot i j c;
                    let lvois = ref [] in
                    for di = -1 to 1 do
                        for dj = -1 to 1 do
                            if ((di = 0 && dj <> 0) || (dj = 0 && di <> 0))
                                && 0 <= i+di && i+di < n
                                && 0 <= j+dj && j+dj < n
                                && p.(i+di).(j+dj) = Color.white
                            then begin
                                lvois := (i+di,j+dj) :: !lvois
                            end
                        done
                    done;
                    List.iter
                        (fun c -> paint_random := c :: !paint_random)
                        (voisins !lvois)
                end
            end;

            if not (Queue.is_empty paint_queue)
            then begin
                let (i, j) = Queue.take paint_queue in
                if p.(i).(j) = Color.white
                then begin
                    hue := !hue +. !i_hue;
                    if !hue >= 360.
                    then hue := 0.;

                    let c = color_from_hsv !hue 1.0 1.0 in

                    plot i j c;
                    let lvois = ref [] in
                    for di = -1 to 1 do
                        for dj = -1 to 1 do
                            if ((di = 0 && dj <> 0) || (dj = 0 && di <> 0))
                                && 0 <= i+di && i+di < n
                                && 0 <= j+dj && j+dj < n
                                && p.(i+di).(j+dj) = Color.white
                            then begin
                                lvois := (i+di,j+dj) :: !lvois
                            end
                        done
                    done;
                    List.iter
                        (fun c -> Queue.add c paint_queue)
                        (voisins !lvois)
                end
            end;

            if not (Stack.is_empty paint_stack)
            then begin
                let (i, j) = Stack.pop paint_stack in
                if p.(i).(j) = Color.white
                then begin
                    hue := !hue +. !i_hue;
                    if !hue >= 360.
                    then hue := 0.;

                    let c = color_from_hsv !hue 1.0 1.0 in

                    plot i j c;
                    let lvois = ref [] in
                    for di = -1 to 1 do
                        for dj = -1 to 1 do
                            if ((di = 0 && dj <> 0) || (dj = 0 && di <> 0))
                                && 0 <= i+di && i+di < n
                                && 0 <= j+dj && j+dj < n
                                && p.(i+di).(j+dj) = Color.white
                            then begin
                                lvois := (i+di,j+dj) :: !lvois
                            end
                        done
                    done;
                    List.iter
                        (fun c -> Stack.push c paint_stack)
                        (voisins !lvois)
                end
            end
        done;

        begin_drawing ();

        for i = 0 to n-1 do
            for j = 0 to n-1 do
                if invalid.(i).(j)
                then begin
                    for k = 0 to zoom-1 do
                    for l = 0 to zoom-1 do
                        draw_pixel (zoom*i+k) (zoom*j+l) p.(i).(j);
                    done
                    done;
                    invalid.(i).(j) <- false
                end
            done
        done;


        if is_mouse_button_pressed MouseButton.Left
        then begin
            let pos = get_mouse_position () in
            let i = int_of_float (Vector2.x pos) in
            let j = int_of_float (Vector2.y pos) in
            Stack.push (i/zoom,j/zoom) paint_stack;
            Queue.add (n/3 + i/zoom,j/zoom) paint_queue;
            paint_random := [ ((2*n)/3 + i/zoom,j/zoom) ]
        end;

        end_drawing ()
    done;
    close_window ()
