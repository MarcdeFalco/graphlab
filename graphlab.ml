

let rgui_w = 0.3
let w, h =
    Raylib.init_window 10 10 "Initializing";
    let monitor = Raylib.get_current_monitor () in
    let w = float_of_int (Raylib.get_monitor_width monitor) in
    let h = float_of_int (Raylib.get_monitor_height monitor) in
    Raylib.close_window ();
    w, h


type button_state = Down | Up | Clicked

let margin_rect r hmargin vmargin =
    let open Raylib in
    Rectangle.create
        (Rectangle.x r -. hmargin)
        (Rectangle.y r -. vmargin)
        (Rectangle.width r +. 2. *. hmargin)
        (Rectangle.height r +. 2. *. vmargin)

type zone = {
    camera : Raylib.Camera2D.t;
    rect : Raylib.Rectangle.t;
    draw : state -> unit;
    process_input : state -> input -> zone -> state * bool;
    mutable panning : bool
} and state = {
    zones : zone array;
    graph : (string, int) Graph.t;
    (* TODO move to Vector2 *)
    pos : (float * float) array; 
    dragged_vertex : (int * zone) option;
    input : input;
    gui : panel array;
    layout : float * float * float * float;
    iterate : bool;
    vertex_radius : float;
    edge_thickness : float;
    search_type : Graph.search_type;
    search_trace : Graph.search_trace option;
    current_search_trace_steps : int;
    graph_radius : float;
    directed : bool
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


    let draw_arrow vi vj thickness color show_arrows =
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

    let draw_edge i j thickness color directed show_arrows =
        let vi = let x, y = s.pos.(i) in Vector2.create x y in
        let vj = let x, y = s.pos.(j) in Vector2.create x y in
        let vij = Vector2.subtract vj vi in
        let l = Vector2.length vij in
        let dij = Vector2.normalize vij in
        let perp = Vector2.rotate dij (3.14 /. 2.) in
        let vm = Vector2.add vi
            (Vector2.add (Vector2.scale dij (l /. 2.))
                (Vector2.scale perp (s.vertex_radius))) in
        if directed
        then begin
            draw_arrow vi vm thickness color false;
            draw_arrow vm vj thickness color show_arrows
        end else
            draw_arrow vi vj thickness color show_arrows
    in

    let n = Array.length s.pos in
    for i = 0 to n-1 do
    for j = 0 to n-1 do
        if Graph.connected s.graph i j 
        then begin
            if s.directed || i < j || not (Graph.connected s.graph j i)
            then draw_edge i j s.edge_thickness (Color.create 0 0 0 255) 
                s.directed s.directed;

            match s.search_trace with
            | None -> ()
            | Some st -> 
                let step = List.nth st.steps s.current_search_trace_steps in
                if step.edge_status.(i).(j) <> Graph.NoStatus
                then let col = match step.edge_status.(i).(j) with
                        | Tree -> Color.green
                        | Back -> Color.red
                        | Forward -> Color.blue
                        | _ -> Color.create 128 128 128 255 in
                    draw_edge i j s.edge_thickness
                                col s.directed true
        end
    done
    done;

    Array.iteri 
        (fun i v ->
            let x, y = s.pos.(i) in
            let color_out, color_fill = match s.search_trace with
                        | None -> Color.black, Color.gray
                        | Some st ->
                            let step = List.nth st.steps s.current_search_trace_steps in
                            let c_ext = if step.current = i then Color.green else Color.black in
                            let c_fill = match step.status.(i) with
                                | Unknown -> Color.gray
                                | Discovered -> Color.create 128 0 0 255
                                | Processed -> Color.create 0 128 0 255
                            in c_ext, c_fill
            in

            draw_circle_v (Vector2.create x y) (s.edge_thickness +.
                s.vertex_radius) color_out;
            draw_circle_v (Vector2.create x y) s.vertex_radius
              color_fill
        )
        s.graph.vtx;

    
    Array.iteri 
        (fun i v ->
            let x, y = s.pos.(i) in
            let fs = (int_of_float (s.vertex_radius *. 1.0)) in
            let lbl = s.graph.Graph.vtx.(i) in
            let tw = measure_text lbl fs in
            draw_text lbl (int_of_float x-tw/2) (int_of_float y-fs/2) 
                fs (Color.white);

            (match s.search_trace with
            | None -> ()
            | Some st -> 
                let step = List.nth st.steps s.current_search_trace_steps in
                (match step.entry_time.(i) with
                | None -> ()
                | Some t -> 
            let st = string_of_int t in
            let tw = measure_text st (fs/2) in
            draw_text st (int_of_float (x -. s.vertex_radius *. 1.2) - tw) (int_of_float y)
                (fs / 2) (Color.red));
                (match step.exit_time.(i) with
                | None -> ()
                | Some t -> 
            draw_text (string_of_int t) (int_of_float (x +. s.vertex_radius *.
            1.2)) (int_of_float y)
                (fs / 2) (Color.green)))


        ) s.graph.Graph.vtx


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
                       search_trace = Some (Graph.search (!s).directed (!s).search_type (!s).graph i);
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
    ( 0.7 *. h /. (y2 -. y1) )
    ( 0.7 *. w /. (x2 -. x1) ) in
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
  set_config_flags [ConfigFlags.Msaa_4x_hint];
  init_window iw ih "Graphlab";
  toggle_fullscreen ();
  set_target_fps 60;
  let g = Graph.complet 5 in
  let n = Graph.nvertices g in
  let p = Graph.nedges g in
  let graph_radius = float_of_int (Graph.diameter g) *. 50. in
  let pos = Layout.eades g graph_radius in

  let set_graph g s =
    let graph_radius = float_of_int (Graph.diameter g) *. 20. *. s.vertex_radius in
    let pos = Layout.eades g graph_radius in
    let s = { s with graph = g; 
        pos = pos; 
        graph_radius = graph_radius;
        layout = Layout.optimal_parameters g;
        directed = g.Graph.directed } in
    for i = 0 to Array.length s.zones - 1 do
        fit_zone_to_graph s s.zones.(i)
    done;
    s
  in

  let ratio = w /. h in

  let s = {
    zones = [|
        {
            camera = Camera2D.create 
                (Vector2.create 0. 0.)
                (Vector2.create 0. 0.) 0. 1.;
            rect = margin_rect 
                (scalable_rect 0.3 0.0 (1. -. rgui_w) 0.99)
                (-5.) (-5.);
            draw = draw_graph;
            process_input = input_graph;
            panning = false
        };
        {
            camera = Camera2D.create 
                (Vector2.create 0. 0.)
                (Vector2.create 0. 0.) 0. 0.05;
            rect = 
                (let z_w = 0.05 in
                let z_h = ratio *. z_w in
                scalable_rect (1. -. z_w) (1.-. z_h) z_w z_h);
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
                    text = "Directed";
                    value = (fun s -> s.directed);
                    set_value = fun s b -> { s with directed = b }
                };
                Slider {
                    text = "vtx";
                    min = 10.;
                    max = 500.0;
                    value = (fun s -> s.vertex_radius);
                    set_value = (fun s v -> { s with vertex_radius = v })
                };
                Slider {
                    text = "edge";
                    min = 0.1;
                    max = 50.0;
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
                    text = (fun s -> "BFS" ^ (if s.search_type =
                        Graph.BFS then " (*) " else ""));
                    click = (fun s -> {s with search_type = Graph.BFS } )
                };
                Button {
                    text = (fun s -> "DFS" ^ (if s.search_type =
                        Graph.DFS then " (*) " else ""));
                    click = (fun s -> {s with search_type = Graph.DFS} )
                };
                Button {
                    text = (fun s -> "DFS Recursive" ^ (if s.search_type =
                        Graph.DFS_rec then " (*) " else ""));
                    click = (fun s -> {s with search_type = Graph.DFS_rec } )
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
                    max = 0.5;
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
                        ("Grid 7", set_graph (Graph.grid 7));
                        ("Exemple", set_graph (Graph.exemple));
                        ("Divisors 11", set_graph (Graph.divisors 11));
                        ("Divisors 99", set_graph (Graph.divisors 99));
                    |];
                    active = false;
                    selected = 0
                }
            |];
            displayed = true
        }
    |];
    layout = Layout.optimal_parameters g;
    iterate = false;
    edge_thickness = 1.;
    vertex_radius = 10.;
    graph_radius = graph_radius;
    search_type = Graph.DFS_rec;
    search_trace = None;
    current_search_trace_steps = 0;
    directed = false
  } in
  set_graph (Graph.complet 5) s

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
            let pos = get_screen_to_world_2d input.mouse zone.camera in
            Camera2D.set_zoom zone.camera (m *. (Camera2D.zoom zone.camera))
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

    let rgui = scalable_rect 0.0 0.0 rgui_w 1. in
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

            
      for i = 0 to Array.length s.zones - 1 do
          let zone = s.zones.(i) in
          draw_rectangle_gradient_ex 
            (margin_rect zone.rect 1. 1.)
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
