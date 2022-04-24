let w, h, ch = 800., 800., 20.

type button_state = Down | Up | Clicked

type zone = {
    camera : Raylib.Camera2D.t;
    rect : Raylib.Rectangle.t;
    draw : state -> unit;
    process_input : state -> input -> zone -> state * bool;
    mutable panning : bool
} and state = {
    zones : zone array;
    graph : int Graph.t;
    (* TODO move to Vector2 *)
    pos : (float * float) array; 
    dragged_vertex : (int * zone) option;
    input : input;
    gui : panel array;
    layout : float * float * float * float;
    iterate : bool;
    vertex_radius : float;
    edge_thickness : float;
    search_trace : Graph.search_trace option
} and input = {
    wheel : float;
    mouse : Raylib.Vector2.t;
    delta : Raylib.Vector2.t;
    left : button_state;
    middle : button_state;
    right : button_state
} and panel = {
    name : string;
    items : gui_item array;
    mutable displayed : bool
} and gui_item =
    Button of {
        text : string;
        click : state -> state
    }
    | Toggle of {
        text : string;
        value : state -> bool;
        set_value : state -> bool -> state
    }
    | Slider of {
        text : string;
        min : float;
        max : float;
        value : state -> float;
        set_value : state -> float -> state
    }

let dragging s =
    s.dragged_vertex <> None 
        || Array.exists (fun z -> z.panning) s.zones

let zone_equals z1 z2 = z1.camera = z2.camera && z1.rect = z2.rect

let draw_graph s =
    let open Raylib in 
    let open Graph in
    let n = Array.length s.pos in
    for i = 0 to n-1 do
    for j = 0 to n-1 do
        if s.graph.mat.(i).(j) && (i < j || not s.graph.mat.(j).(i))
        then begin
            let vi = let x, y = s.pos.(i) in Vector2.create x y in
            let vj = let x, y = s.pos.(j) in Vector2.create x y in
            draw_line_ex vi vj s.edge_thickness Color.black
        end
    done
    done;
    Array.iteri 
        (fun i v ->
            let x, y = s.pos.(i) in
            draw_circle_v (Vector2.create x y) (s.edge_thickness +. s.vertex_radius)
               (Color.create 0 0 0 255);
            draw_circle_v (Vector2.create x y) s.vertex_radius
               (Color.create 255 200 200 255);

            )
        s.graph.vtx

let input_graph s input zone =
    let open Raylib in
    let prevent = ref false in
    let mouse_in_zone = check_collision_point_rec input.mouse zone.rect in
    (match s.dragged_vertex with
    | Some (i, z) when zone_equals z zone ->
            let zoom = Camera2D.zoom zone.camera in
            let x, y = s.pos.(i) in
            s.pos.(i) <-
                (x +. Vector2.x input.delta /. zoom,
                y +. Vector2.y input.delta /. zoom)
    | _ -> ());

    let s = ref s in
    if mouse_in_zone
    then begin
        let mpos = get_screen_to_world_2d input.mouse zone.camera in
        for i = 0 to Array.length (!s).pos - 1 do
            let x, y = (!s).pos.(i) in
            if check_collision_point_circle
                mpos
                (Vector2.create x y) (!s).vertex_radius
            then begin
                if input.right = Clicked
                then begin
                    let pred = Graph.search (!s).graph i Graph.stack_search in
                    for j = 0 to Array.length (!s).pos - 1 do
                        match pred.(j) with
                        | Some k -> Printf.printf "%d->%d " k j
                        | None -> ()
                    done;
                    Printf.printf "\n%!";
                    prevent := true
                end;
                if input.left = Down 
                then begin
                    if not (dragging !s)
                    then (s := { !s with
                        dragged_vertex = Some (i, zone) });
                    prevent := true
                end
            end
        done
    end;

    if input.left = Clicked
    then s := { !s with dragged_vertex = None };

    !s, !prevent

let fit_zone_to_graph s zone =
    let (x1, y1), (x2, y2) = Layout.bbox s.pos in
    let r = zone.rect in
    let cam = zone.camera in
    let open Raylib in
  let x, y, w, h = Rectangle.x r, Rectangle.y r,
    Rectangle.width r, Rectangle.height r in
  let zoom = min
    ( 0.8 *. h /. (y2 -. y1) )
    ( 0.8 *. w /. (x2 -. x1) ) in
  Camera2D.set_zoom cam zoom;
  Camera2D.set_offset cam 
    (Vector2.create (x +. w *. 0.5) (y +. h *. 0.5));
  Camera2D.set_target cam
    (Vector2.create (0.5 *. (x1 +. x2)) (0.5 *. (y1 +. y2)))

let scalable_rect x y width height =
  Raylib.Rectangle.create (w *. x) (h *. y) (w *. width) (h *. height)

let setup () =
  let iw = int_of_float w in
  let ih = int_of_float h in
  let open Raylib in
  init_window iw ih "Graphlab";
  set_target_fps 60;
  let g = Graph.hypercube 5 in
  let pos = Layout.init g 10. in
  let s = {
    zones = [|
        {
            camera = Camera2D.create 
                (Vector2.create 0. 0.)
                (Vector2.create 0. 0.) 0. 1.;
            rect = scalable_rect 0.3 0.01 0.69 0.98;
            draw = draw_graph;
            process_input = input_graph;
            panning = false
        };
        {
            camera = Camera2D.create 
                (Vector2.create 0. 0.)
                (Vector2.create 0. 0.) 0. 0.05;
            rect = scalable_rect 0.9 0.9 0.095 0.095;
            draw = draw_graph;
            process_input = input_graph;
            panning = false
        }
    |];
    input = {
        wheel = 0.;
        mouse = Vector2.create 0. 0.;
        delta = Vector2.create 0. 0.;
        left = Up;
        middle = Up;
        right = Up
    };
    dragged_vertex = None;
    graph = g;
    pos = pos;
    gui = [|
        { 
            name = "Layout";
            items = [|
                Button {
                    text = "Randomize";
                    click = fun s -> { s with pos = Layout.init s.graph 10. }
                };
                Toggle {
                    text = "Iterate";
                    value = (fun s -> s.iterate);
                    set_value = fun s b -> { s with iterate = b }
                };
                Slider {
                    text = "c1";
                    min = 1.;
                    max = 10.0;
                    value = (fun s -> match s.layout with
                                (c1, c2, c3, c4) -> c1);
                    set_value = (fun s v -> match s.layout with
                        (c1, c2, c3, c4) -> { s with layout = (v, c2, c3, c4) })
                };
                Slider {
                    text = "c2";
                    min = 1.;
                    max = 10.0;
                    value = (fun s -> match s.layout with
                                (c1, c2, c3, c4) -> c2);
                    set_value = (fun s v -> match s.layout with
                        (c1, c2, c3, c4) -> { s with layout = (c1, v, c3, c4) })
                };
                Slider {
                    text = "c3";
                    min = 1.;
                    max = 10.0;
                    value = (fun s -> match s.layout with
                                (c1, c2, c3, c4) -> c3);
                    set_value = (fun s v -> match s.layout with
                        (c1, c2, c3, c4) -> { s with layout = (c1, c2, v, c4) })
                };
                Slider {
                    text = "c4";
                    min = 0.01;
                    max = 1.0;
                    value = (fun s -> match s.layout with
                                (c1, c2, c3, c4) -> c4);
                    set_value = (fun s v -> match s.layout with
                        (c1, c2, c3, c4) -> { s with layout = (c1, c2, c3, v) })
                }
            |];
            displayed = true
        }
    |];
    layout = (2., 1., 1., 0.1);
    iterate = true;
    edge_thickness = 0.02;
    vertex_radius = 0.1;
    search_trace = None
  } in
  for i = 0 to Array.length s.zones - 1 do
      fit_zone_to_graph s s.zones.(i)
  done;
  s

let process_zone s input zone =
    let open Raylib in
    let x = int_of_float (Rectangle.x zone.rect) in
    let y = int_of_float (Rectangle.y zone.rect) in
    let width = int_of_float (Rectangle.width zone.rect) in
    let height = int_of_float (Rectangle.height zone.rect) in
    begin_scissor_mode x y width height;
    clear_background Color.raywhite;
    begin_mode_2d zone.camera;
    zone.draw s;
    end_mode_2d ();
    end_scissor_mode ();

    let s, prevent = zone.process_input s input zone in
    let mouse_in_zone = check_collision_point_rec input.mouse zone.rect in
    if not prevent && mouse_in_zone
    then begin
        if input.wheel <> 0.
        then (
            let m = (1. +. input.wheel *. 0.1) in
            (*
            let pos = get_screen_to_world_2d input.mouse zone.camera in
*)
            Camera2D.set_zoom zone.camera (m *. (Camera2D.zoom zone.camera))
            (*
            let pos' = get_screen_to_world_2d input.mouse zone.camera in
            let delta = Vector2.subtract pos' pos in
            Camera2D.set_offset zone.camera
                (Vector2.add delta (Camera2D.offset zone.camera))
        *)
        );
        if not (dragging s) && input.left = Down
        then zone.panning <- true
    end;

    if zone.panning && input.left = Clicked
    then zone.panning <- false;

    if zone.panning
    then begin
        Camera2D.set_offset
            zone.camera
            (Vector2.add (Camera2D.offset zone.camera) input.delta)
    end;

    s

let get_input input = 
    let open Raylib in
    let mouse_state old b =
        if is_mouse_button_down b
        then Down
        else if is_mouse_button_released b && old = Down
             then Clicked
             else Up
    in
    {
        wheel = get_mouse_wheel_move ();
        mouse = get_mouse_position ();
        delta = get_mouse_delta ();
        left = mouse_state input.left MouseButton.Left;
        middle = mouse_state input.middle MouseButton.Middle;
        right = mouse_state input.right MouseButton.Right
    }

let rec loop s =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      begin_drawing ();
      clear_background Color.gray;

      let input = get_input s.input in
      let s' = ref { s with input = input } in

    let rgui = scalable_rect 0.01 0.01 0.275 0.98 in
    Raygui.panel rgui;
    
    let x = 5. +. Rectangle.x rgui in
    let ctrl_width = Rectangle.width rgui -. 10. in
    let ctrl_height = 20. in
    let dec = ref (5. +. Rectangle.y rgui) in
    for i = 0 to Array.length s.gui - 1 do
        let p = s.gui.(i) in

        let r = Rectangle.create x !dec 20. ctrl_height in

        p.displayed <- Raygui.check_box r p.name p.displayed;

        if p.displayed
        then begin
            let n_child = Array.length p.items in
            let r_items = Rectangle.create
                x (!dec +. ctrl_height +. 5.)
                ctrl_width
                ( 5. +. (ctrl_height +. 5.) *. float_of_int n_child )
            in 
            Raygui.panel r_items;
            for j = 0 to n_child - 1 do
                match p.items.(j) with
                | Toggle t ->
                    let v = Raygui.toggle (Rectangle.create
                        (x +. 5.) (!dec +. 5. +. 
                            (1. +. float_of_int j) *. (ctrl_height +. 5.))
                        (ctrl_width -. 10.) (ctrl_height)
                    ) t.text (t.value s) in
                    s' := t.set_value (!s') v
                | Button b ->
                    if Raygui.button (Rectangle.create
                        (x +. 5.) (!dec +. 5. +. 
                            (1. +. float_of_int j) *. (ctrl_height +. 5.))
                        (ctrl_width -. 10.) (ctrl_height)
                    ) b.text
                    then s' := b.click (!s')
                | Slider t ->
                    let v = Raygui.slider (Rectangle.create
                        (x +. 30.) (!dec +. 5. +. 
                            (1. +. float_of_int j) *. (ctrl_height +. 5.))
                        (ctrl_width -. 70.) (ctrl_height))
                        t.text (Printf.sprintf "%0.3f" @@ t.value s)
                        (t.value s) ~min:t.min ~max:t.max
                    in s' := t.set_value (!s') v
            done
        end
    done;

      (* Last zone (on top) is fit to size *)
      fit_zone_to_graph s s.zones.(Array.length s.zones - 1);

      let extend r =
          let b = 5. in
          Rectangle.create
            (Rectangle.x r -. b)
            (Rectangle.y r -. b)
            (Rectangle.width r +. 2. *. b)
            (Rectangle.height r +. 2. *. b)
     in
      
      for i = 0 to Array.length s.zones - 1 do
          let zone = s.zones.(i) in
          draw_rectangle_gradient_ex 
            (extend zone.rect)
            Color.white Color.darkblue Color.black Color.blue;
          s' := process_zone !s' input zone
      done;

      if (!s').iterate
      then begin
          let npos = Layout.iterate (!s').graph (!s').pos (!s').layout in
          for i = 0 to Array.length (!s').pos - 1 do
              let ign = match (!s').dragged_vertex with
              | None -> false
              | Some (j, _) when j = i -> true
              | _ -> false in
              if not ign
              then (!s').pos.(i) <- npos.(i)
          done 
      end;

      end_drawing ();
      loop !s'

let () = 
    Printexc.record_backtrace true;
    setup () |> loop
