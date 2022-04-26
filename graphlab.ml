let w, h, ch = 1400., 880., 20.

type button_state = Down | Up | Clicked

type zone = {
    camera : Raylib.Camera2D.t;
    rect : Raylib.Rectangle.t;
    draw : state -> unit;
    process_input : state -> input -> zone -> state * bool;
    mutable panning : bool
} and state = {
    zones : zone array;
    graph : string Graph.t;
    (* TODO move to Vector2 *)
    pos : (float * float) array; 
    dragged_vertex : (int * zone) option;
    input : input;
    gui : panel array;
    layout : float * float * float * float;
    iterate : bool;
    vertex_radius : float;
    edge_thickness : float;
    search_stack : bool;
    search_trace : Graph.search_trace option;
    current_search_trace_steps : int;
    graph_radius : float;
    show_arrows : bool
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
        text : state -> string;
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
    | Spinner of {
        text : string;
        max : state -> int;
        value : state -> int;
        set_value : state -> int -> state
    }
    | Textbox of {
        text : state -> string;
        height : float
    }
    | Dropdown of {
        choices : (string * (state -> state)) array;
        mutable active : bool;
        mutable selected : int
    }

let dragging s =
    s.dragged_vertex <> None 
        || Array.exists (fun z -> z.panning) s.zones

let zone_equals z1 z2 = z1.camera = z2.camera && z1.rect = z2.rect

let draw_graph s =
    let open Raylib in 
    let open Graph in


    let draw_edge i j thickness color show_arrows =
        let vi = let x, y = s.pos.(i) in Vector2.create x y in
        let vj = let x, y = s.pos.(j) in Vector2.create x y in
        draw_line_ex vi vj thickness color;
        if show_arrows
        then begin
            let vij = Vector2.subtract vj vi in
            let l = Vector2.length vij in
            let dij = Vector2.normalize vij in
            let perp = Vector2.rotate dij (3.14 /. 2.) in
            let arrow_height = thickness *. 3. in
            let v = Vector2.add vj 
                (Vector2.scale dij (-. s.vertex_radius)) in
            let v1 = Vector2.add 
                (Vector2.add vj 
                (Vector2.scale dij (-. s.vertex_radius *. 2. )))
                (Vector2.scale perp arrow_height) in
            let v2 = Vector2.add 
                (Vector2.add vj
                (Vector2.scale dij (-. s.vertex_radius *. 2. )))
                (Vector2.scale perp (-. arrow_height)) in
            draw_triangle v v2 v1 color
        end
    in


    let n = Array.length s.pos in
    for i = 0 to n-1 do
    for j = 0 to n-1 do
        if s.graph.mat.(i).(j) && (s.show_arrows || i < j || not s.graph.mat.(j).(i))
        then begin
            draw_edge i j s.edge_thickness (Color.create 0 0 0 255)
            s.show_arrows
        end
    done
    done;

    (match s.search_trace with
    | None -> ()
    | Some st -> begin
            let step = List.nth st.steps s.current_search_trace_steps in
            List.iter (fun (i,j) ->
                draw_edge i j (1.5 *. s.edge_thickness)
                    (Color.create 255 0 0 255) true) step.pred
        end)
    ;

    Array.iteri 
        (fun i v ->
            let x, y = s.pos.(i) in
            draw_circle_v (Vector2.create x y) (s.edge_thickness +. s.vertex_radius)
               (Color.create 0 0 0 255);
            draw_circle_v (Vector2.create x y) s.vertex_radius
               (Color.create 128 128 128 255)            )
        s.graph.vtx;

    (match s.search_trace with
    | None -> ()
    | Some st -> begin
            let x, y = s.pos.(st.source) in
            draw_circle_v (Vector2.create x y) s.vertex_radius
               (Color.create 255 0 0 255);

            let step = List.nth st.steps s.current_search_trace_steps in

            List.iter (fun i ->
                let x, y = s.pos.(i) in
                draw_circle_v 
                   (Vector2.create x y) s.vertex_radius
                   (Color.create 0 0 0 255)) step.visited;
            
            List.iter (fun i ->
                let x, y = s.pos.(i) in
                draw_circle_v 
                   (Vector2.create x y) s.vertex_radius
                   (Color.create 0 0 255 255)) step.to_visit;
            
            let x, y = s.pos.(step.current) in
            draw_circle_v 
               (Vector2.create x y) s.vertex_radius
               (Color.create 0 255 0 255);

    end);

    Array.iteri 
        (fun i v ->
            let x, y = s.pos.(i) in
            let fs = (int_of_float (s.vertex_radius *. 1.0)) in
            let lbl = s.graph.Graph.vtx.(i) in
            let tw = measure_text lbl fs in
            draw_text lbl (int_of_float x-tw/2) (int_of_float y-fs/2) 
                fs (Color.white)) s.graph.Graph.vtx


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
                    s := { !s with
                       search_trace = Some (
                            (if (!s).search_stack
                            then Graph.search (!s).graph i Graph.stack_search
                            else Graph.search (!s).graph i  Graph.queue_search)
                       );
                       current_search_trace_steps = 0
                    };
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
  set_config_flags [ConfigFlags.Msaa_4x_hint];
  let g = Graph.grid 5 in
  let n = Graph.nvertices g in
  let p = Graph.nedges g in
  let graph_radius = float_of_int (Array.length g.Graph.vtx) *. 50. in
  let pos = Layout.eades g graph_radius in
  let set_graph g s =
    let graph_radius = float_of_int (Array.length g.Graph.vtx) *. 50. in
    let pos = Layout.eades g graph_radius in
    let s = { s with graph = g; pos = pos } in
    for i = 0 to Array.length s.zones - 1 do
        fit_zone_to_graph s s.zones.(i)
    done;
    s
  in

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
            name = "Style";
            items = [|
                Toggle {
                    text = "Show arrows";
                    value = (fun s -> s.show_arrows);
                    set_value = fun s b -> { s with show_arrows = b }
                };
                Slider {
                    text = "vtx";
                    min = 1.;
                    max = 100.0;
                    value = (fun s -> s.vertex_radius);
                    set_value = (fun s v -> { s with vertex_radius = v })
                };
                Slider {
                    text = "edge";
                    min = 0.1;
                    max = 10.0;
                    value = (fun s -> s.edge_thickness);
                    set_value = (fun s v -> { s with edge_thickness = v })
                }
            |];
            displayed = true
        };
                {
            name = "Search";
            items = [|
                Button {
                    text = (fun _ -> "Reset");
                    click = fun s -> 
                        { s with search_trace = None }
                };
                Button {
                    text = (fun s -> if s.search_stack then "Stack (DFS)" else
                        "Queue (BFS)");
                    click = fun s -> { s with search_stack = not s.search_stack }
                };
                Spinner {
                    text = "step";
                    max = (fun s -> match s.search_trace with
                        | None -> 0
                        | Some l -> List.length l.steps - 1);
                    value = (fun s -> s.current_search_trace_steps);
                    set_value = (fun s v -> { s with current_search_trace_steps
                    =   (match s.search_trace with
                        | None -> 0
                        | Some l -> min (max 0 v) (List.length l.steps - 1)
                    )})
                    }
            |];
            displayed = true
        };
        {
            name = "Repr.";
            items = [|
                Textbox {
                    text = (fun s -> Graph.text_matrix s.graph);
                    height = 200.
                };
                Textbox {
                    text = (fun s -> Graph.text_ladj s.graph);
                    height = 200.
                }

            |];
            displayed = false
        };
        { 
            name = "Layout";
            items = [|
                Button {
                    text = (fun _ -> "Randomize");
                    click = fun s -> { s with pos = Layout.init s.graph s.graph_radius }
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
                    min = 0.0;
                    max = 0.2;
                    value = (fun s -> match s.layout with
                                (c1, c2, c3, c4) -> c4);
                    set_value = (fun s v -> match s.layout with
                        (c1, c2, c3, c4) -> { s with layout = (c1, c2, c3, v) })
                }
            |];
            displayed = true
        };
        {
            name = "Data";
            items = [|
                Dropdown {
                    choices = [|
                        ("Set graph", fun s -> s);
                        ("Hcube 3", set_graph (Graph.hypercube 3));
                        ("Hcube 4", set_graph (Graph.hypercube 4));
                        ("Cycle 5", set_graph (Graph.cycle 5));
                        ("Mobius 5", set_graph (Graph.mobius 5));
                        ("Grid 3", set_graph (Graph.grid 3));
                        ("Grid 7", set_graph (Graph.grid 7))
                    |];
                    active = false;
                    selected = 0
                }
            |];
            displayed = true
        }
    |];
    layout = Layout.optimal_parameters g;
    iterate = true;
    edge_thickness = 1.;
    vertex_radius = 10.;
    graph_radius = graph_radius;
    search_stack = true;
    search_trace = None;
    current_search_trace_steps = 0;
    show_arrows = true
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

  set_config_flags [ConfigFlags.Msaa_4x_hint];
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

        dec := !dec +. ctrl_height +. 5.;

        if p.displayed
        then begin
            let n_child = Array.length p.items in

            let total_size = ref 0. in
            for j = 0 to n_child - 1 do
                match p.items.(j) with
                | Textbox t -> total_size := !total_size +. t.height
                | _ -> total_size := !total_size  +. ctrl_height +. 5.
            done;

            let r_items = Rectangle.create
                x !dec 
                ctrl_width
                !total_size
            in 
            Raygui.panel r_items;
            for j = 0 to n_child - 1 do
                match p.items.(j) with
                | Toggle t ->
                    let v = Raygui.toggle (Rectangle.create
                        (x +. 5.) !dec
                        (ctrl_width -. 10.) (ctrl_height)
                    ) t.text (t.value s) in
                    dec := !dec +. ctrl_height +. 5.;
                    s' := t.set_value (!s') v
                | Button b ->
                    (if Raygui.button (Rectangle.create
                        (x +. 5.) !dec
                        (ctrl_width -. 10.) (ctrl_height)
                    ) (b.text s)
                    then s' := b.click (!s') );
                    dec := !dec +. ctrl_height +. 5.;
                | Spinner t ->
                    let v, b = Raygui.spinner (Rectangle.create
                        (x +. 30.) !dec
                        (ctrl_width -. 70.) (ctrl_height))
                        t.text (t.value s)
                        ~min:0 ~max:(t.max s)
                        false
                    in 
                    dec := !dec +. ctrl_height +. 5.;
                    s' := t.set_value (!s') v
                | Slider t ->
                    let v = Raygui.slider (Rectangle.create
                        (x +. 30.) !dec
                        (ctrl_width -. 70.) (ctrl_height))
                        t.text (Printf.sprintf "%0.3f" @@ t.value s)
                        (t.value s) ~min:t.min ~max:t.max
                    in 
                    dec := !dec +. ctrl_height +. 5.;
                    s' := t.set_value (!s') v
                | Dropdown t ->
                    let v, b = Raygui.dropdown_box 
                        (Rectangle.create
                        (x +. 30.) !dec
                        (ctrl_width -. 70.) (ctrl_height))
                        (String.concat ";"
                            (Array.to_list (Array.map fst t.choices)))
                        t.selected t.active
                    in 
                    t.selected <- v;
                    if b
                    then begin
                        t.active <- not t.active;
                        t.selected <- 0;
                        dec := !dec +. ctrl_height +. 5.;
                        s' := (snd t.choices.(v)) (!s')
                    end
                | Textbox t ->
                        let text = t.text s in
                        let r = Rectangle.create 
                            (x +. 5.) !dec
                            (ctrl_width -. 10.) t.height in
                        dec := !dec +. t.height;
                        let _ = Raygui.text_box_multi r text false in
                        ()
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
          let npos = Layout.iterate (!s').graph (!s').pos 
            (!s').layout (!s').graph_radius in
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
