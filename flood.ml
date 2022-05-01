let w, h = 800., 800.

type tile = {
    mutable flipped : bool;
    mutable flipping : bool;
    mutable flip_timer : int;
    mutable flip_timer_start : int
}

let tile_grid n p =
    Array.init n (fun i ->
        Array.init p (fun j -> {
            flipped = false;
            flipping = false;
            flip_timer = 0;
            flip_timer_start = 0
        }) )

type flip_type =
    Single | FloodQueue | FloodStack

type status = {
    cam : Raylib.Camera3D.t;
    time : int;
    tiles : tile array array;
    to_flip_stack : (int * int) Stack.t;
    to_flip_queue : (int * int) Queue.t;
    mutable flip_target : bool;
    flip_length : int;
    mutable flip_type : flip_type
}

let rec loop st =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      let n = Array.length st.tiles in
      let p = Array.length st.tiles.(0) in

      let flip i j = 
          let tile = st.tiles.(i).(j) in
          tile.flipping <- true;
          let length =
              let t = 1. -. (float_of_int st.flip_length
                    /. float_of_int (n * p)) in
              if st.flip_length == n * p
              then 20
              else if t < 0.1
              then 15
              else if t < 0.5
              then 10
              else if t < 0.75
              then 5
              else 2
          in
          tile.flip_timer <- length;
          tile.flip_timer_start <- length;
          if st.flip_type <> Single
          then begin
          for di = -1 to 1 do
              for dj = -1 to 1 do
                  let i' = i + di in
                  let j' = j + dj in
                  if di <> dj && (di = 0 || dj = 0) && 0 <= i' && i' < n
                    && 0 <= j' && j' < p 
                    && st.tiles.(i').(j').flipped <> st.flip_target
                  then (
                   if st.flip_type = FloodQueue
                   then Queue.add (i',j') st.to_flip_queue
                   else Stack.push (i',j') st.to_flip_stack)
              done
          done
          end
      in
      let process_flip tile = 
          tile.flipping <- false;
          tile.flipped <- st.flip_target;
          let rec aux () =
              let should_loop = match st.flip_type with
                | Single -> false
                | FloodQueue -> not (Queue.is_empty st.to_flip_queue)
                | FloodStack -> not (Stack.is_empty st.to_flip_stack)
             in
            if should_loop
            then begin
              let (i,j) = 
                  match st.flip_type with
                  | FloodQueue -> Queue.take st.to_flip_queue
                  | FloodStack -> Stack.pop st.to_flip_stack
                  | _ -> failwith "unreachable" in
              if st.tiles.(i).(j).flipped <> st.flip_target
              then (flip i j; true)
              else aux ()
            end else false
         in aux () 
      in

      let tw = 100. /. (1. +. float_of_int n) in
      let th = 100. /. (1. +. float_of_int p) in

      let mouse_position = get_mouse_position () in
      let mouse_ray = get_mouse_ray mouse_position st.cam in

      let new_flip_length = ref st.flip_length in

      begin_drawing ();
      clear_background Color.black;
      begin_mode_3d st.cam;

      let waiting =
        (List.of_seq (Stack.to_seq st.to_flip_stack))
        @ (List.of_seq (Queue.to_seq st.to_flip_queue)) in

      (*
      if waiting <> []
      then 
          Printf.printf "%s\n%!"
            (String.concat ","
                (List.map (fun (i,j) -> Printf.sprintf "(%d,%d)" i j) waiting));
                *)

      Rlgl.push_matrix ();
      Rlgl.translatef (tw /. 2.) (th/. 2.) 0.;

      for i = 0 to n-1 do
      for j = 0 to p-1 do


          let tile = st.tiles.(i).(j) in
          let is_waiting = tile.flipped <> st.flip_target && List.mem (i,j) waiting in

      Rlgl.push_matrix ();

      let x = tw *. float_of_int (i - n/2) in
      let y = th *. float_of_int (j - p/2) in

      let collision = 
          get_ray_collision_box mouse_ray
          (BoundingBox.create
            (Vector3.create (x) (y) (-0.5))
            (Vector3.create (x+.tw) (y+.th) (0.5)))
      in
      let over = RayCollision.hit collision in
      let p_col = RayCollision.point collision in

      Rlgl.translatef x y 0.;


      (if tile.flipping
      then begin
        let t = float_of_int tile.flip_timer 
            /. float_of_int tile.flip_timer_start in
        let theta = 
            if tile.flipped
            then 180. *. t
            else 180. *. (1. -. t)
        in
        Rlgl.rotatef theta 1. 1. 0.;
        tile.flip_timer <- tile.flip_timer - 1;
        if tile.flip_timer = 0
        then if process_flip tile
             then new_flip_length := !new_flip_length - 1
      end
      else if over then begin
          let px = (Vector3.x p_col -. x) /. tw -. 0.5 in
          let py = (Vector3.y p_col -. y) /. th -. 0.5 in

          let d_pcol = Vector2.create px py in
          let dir_center =  Vector2.normalize d_pcol in
          let rd = Vector2.create 
            (-. (Vector2.y dir_center))
            (Vector2.x dir_center) in

          let tilt = 60. in

          Rlgl.scalef 1.3 1.3 1.3;
          Rlgl.translatef 0. 0. (-3.);
          Rlgl.rotatef (-. tilt *. (Vector2.length d_pcol)) (Vector2.x rd) (Vector2.y rd) 0.;
          if tile.flipped then Rlgl.rotatef 180. 1. 1. 0.;

          if is_mouse_button_pressed MouseButton.Left
            || is_mouse_button_pressed MouseButton.Middle
            || is_mouse_button_pressed MouseButton.Right
          then begin
              st.flip_type <- (
                  if is_mouse_button_pressed MouseButton.Left
                  then FloodQueue
                  else if is_mouse_button_pressed MouseButton.Middle
                  then Single
                  else FloodStack
              );
              st.flip_target <- not (tile.flipped);
              flip i j;
              new_flip_length := n * p
          end
      end else
        Rlgl.rotatef (if tile.flipped then 180. else 0.) 1. 1. 0.);


       draw_cube
        (Vector3.create 0. 0. 0.05) tw th 0.1 
            (if is_waiting
            then Color.create (255-10*(i+j)) 128 128 255
            else Color.create (255-10*(i+j)) 0 0 255);
       draw_cube_wires
        (Vector3.create 0. 0. 0.05) tw th 0.1
            (if is_waiting
            then Color.create 255 0 0 255
            else Color.create 128 0 0 255);
       draw_cube
        (Vector3.create 0. 0. (-0.05)) tw th 0.1 
            (if is_waiting
            then Color.create 128 (255-10*(i+j)) 128 255
            else Color.create 0 (255-10*(i+j)) 0 255);
       draw_cube_wires
        (Vector3.create 0. 0. (-0.05)) tw th 0.1 
            (if is_waiting
            then Color.create 255 0 0 255
            else Color.create 0 128 0 255);

      Rlgl.pop_matrix ();

      done
      done;
      Rlgl.pop_matrix ();

      end_mode_3d ();

      if not (Stack.is_empty st.to_flip_stack)
      then draw_text "Flood fill avec un parcours en profondeur" 10 0 30 Color.white;
      if not (Queue.is_empty st.to_flip_queue)
      then draw_text "Flood fill avec un parcours en largeur" 10 0 30 Color.white;

      end_drawing ();

      loop { 
          st with 
            time = st.time + 1;
            flip_length = !new_flip_length
      }

let setup () =
  let iw = int_of_float w in
  let ih = int_of_float h in
  let n = 10 in
  let open Raylib in
  set_config_flags [ConfigFlags.Msaa_4x_hint];
  init_window iw ih "Graphlab";
  set_target_fps 60;
  {
    cam = Camera3D.create
        (Vector3.create 0. 0. (-100.))
        (Vector3.create 0. 0. 0.)
        (Vector3.create 0. 1. 0.)
        90.
        CameraProjection.Orthographic;
    time = 0;
    tiles = tile_grid n n;
    flip_length = n*n;
    flip_target = false;
    to_flip_stack = Stack.create ();
    to_flip_queue = Queue.create ();
    flip_type = Single
  }

let () = 
    Printexc.record_backtrace true;
    setup () |> loop
