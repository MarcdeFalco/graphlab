let w, h, ch = 800., 800., 20.
let g = Graph.hypercube 3
let pos = Layout.init g
let vertex_radius = 20.

type state = {
    auto_fit : bool;
    iterate : bool;
    dragged_vertex : int option; 
    zoom : float;
    offset : float * float;
    panning : bool;
    curved : bool;
    arrow : bool;
    c1 : float;
    c2 : float;
    c3 : float;
    c4 : float
}

let margin = 10.
let gui_panel_width = max 100. (0.2 *. w)
let gui_panel_height = h -. 2. *. margin
let ctrl_h = 20.
let ctrl_margin = 30.

let ul_x = (margin *. 2. +. gui_panel_width)
let ul_y = margin
let g_w = w -. gui_panel_width -. 3. *. margin
let g_h = gui_panel_height
let br_x = ul_x +. g_w
let br_y = ul_y +. g_h

let fit_graph s =
    let (min_x, min_y), (max_x, max_y) = Layout.bbox pos in
    let d_x = max_x -. min_x in
    let d_y = max_y -. min_y in
    let zoom = max ( (g_w -. 2.4 *. vertex_radius) /. d_x) 
                   ( (g_h -. 2.4 *. vertex_radius) /. d_y) in
    { s with
        zoom = zoom;
        offset = (-. min_x *. zoom +. 1.2 *. vertex_radius, 
                  -. min_y *. zoom +. 1.2 *. vertex_radius) }

let setup () =
  let iw = int_of_float w in
  let ih = int_of_float h in
  Raylib.init_window iw ih "Graph layout";
  Raylib.set_target_fps 60;
  fit_graph {
      auto_fit = true;
      iterate = false;
      panning = false;
      dragged_vertex = None;
      offset = (0.5 *. w, 0.5 *. h);
      zoom = 1.;
      curved = false;
      arrow = true;
      c1 = 2.;
      c2 = 1.;
      c3 = 1.;
      c4 = 0.1
  }


let draw_gui s = 
    let open Raylib in
    let open Raygui in

    panel (Rectangle.create 
        margin margin gui_panel_width gui_panel_height);

    let ctrl_rect n =
          (Rectangle.create 
          (margin +. ctrl_margin)
          (margin +. ctrl_h +. (float_of_int n) *. 1.5 *. ctrl_h) 
          (gui_panel_width -. 2. *. ctrl_margin) ctrl_h)
    in

    let zoom = ref s.zoom in

    let do_reset = Raygui.(button 
        (ctrl_rect 0) "Reset") in


    let iterate = Raygui.(toggle 
        (ctrl_rect 1)
        "Place" s.iterate) in
    let auto_fit = Raygui.(toggle 
        (ctrl_rect 2)
        "Auto. Fit" s.auto_fit) in

    let curved = Raygui.(toggle 
        (ctrl_rect 3)
        "Curved" s.curved) in
    let arrow = Raygui.(toggle 
        (ctrl_rect 4)
        "Arrow on edge" s.arrow) in

    let n = Graph.nvertices g in
    if iterate
    then begin
        Array.blit (Layout.iterate g pos 
        (s.c1, s.c2, s.c3, s.c4) ) 0 pos 0 n
    end;
    if do_reset
    then begin
        Array.blit (Layout.init g) 0 pos 0 n
    end;

    let sl = 5 in
    let c1 = slider 
      (ctrl_rect sl)
      "c1" (Printf.sprintf "%0.2f" s.c1) s.c1 ~min:0.1 ~max:3. in
    let c2 = Raygui.(slider 
      (ctrl_rect (sl+1))
      "c2" (Printf.sprintf "%0.2f" s.c2) s.c2 ~min:0.1 ~max:3.) in
    let c3 = Raygui.(slider 
      (ctrl_rect (sl+2))
        "c3" (Printf.sprintf "%0.2f" s.c3) s.c3 ~min:0.1 ~max:3.) in
    let c4 = Raygui.(slider 
      (ctrl_rect (sl+3))
        "c4" (Printf.sprintf "%0.3f" s.c4) s.c4 ~min:0.001 ~max:0.1) in

    let s = { s with 
        c1 = c1; 
        c2 = c2; 
        c3 = c3; 
        c4 = c4; 
        iterate = iterate;
        curved = curved;
        arrow = arrow;
        auto_fit = auto_fit 
    } in

    if auto_fit
    then fit_graph s
    else s

let draw_graph s =
      let open Raylib in
      let n = Graph.nvertices g in

      let wheel = get_mouse_wheel_move () in
      let zoom = s.zoom in
      let o_x, o_y = s.offset in

            let mpos = get_mouse_position () in
            let mpos_x, mpos_y = Vector2.x mpos -. ul_x, 
                                 Vector2.y mpos -. ul_y in

      let zoom, o_x, o_y =
          if wheel <> 0.
          then begin
            let m = (1. +. wheel *. 0.1) in
            let m_x = (mpos_x -. o_x) /. zoom in
            let m_y = (mpos_y -. o_y) /. zoom in
            let o_x = mpos_x -. m_x *. zoom *. m in
            let o_y = mpos_y -. m_y *. zoom *. m in
            m *. zoom, o_x, o_y
          end else zoom, o_x, o_y in

      let panning, o_x, o_y = if s.panning
          then begin
              if is_mouse_button_released MouseButton.Middle
              then false, o_x, o_y
              else begin
                  let v = get_mouse_delta () in
                  true, o_x +. Vector2.x v, o_y +. Vector2.y v
              end
          end else is_mouse_button_down MouseButton.Middle, o_x, o_y
      in

      let r_g = Rectangle.create ul_x ul_y g_w g_h in

      let (min_x, min_y), (max_x, max_y) = Layout.bbox pos in
      let p2w (x, y) = 
          x *. zoom +. o_x, y *. zoom +. o_y
      in
      let w2p (x, y) =
          (x -. o_x) /. zoom, (y -. o_y) /. zoom
      in

      let dragged_vertex = ref s.dragged_vertex in
      if is_mouse_button_released MouseButton.Left
      then dragged_vertex := None
      else if is_mouse_button_down MouseButton.Left
      then begin
          match s.dragged_vertex with
          | None -> begin
              for i = 0 to n-1 do
                  let x, y = p2w pos.(i) in
                  let d = (x -. mpos_x) ** 2. 
                        +. (y -. mpos_y) ** 2. in
                  if d < vertex_radius ** 2.0
                  then dragged_vertex := Some i
              done
          end
          | Some v -> 
                let x, y = w2p (mpos_x, mpos_y) in
                pos.(v) <- (x, y)
      end;

    Raygui.panel r_g;

    (* Draw edges *)
    let draw_edge x y x' y' =
        let v = Vector2.create (ul_x +. x) (ul_y +. y) in
        let v' = Vector2.create (ul_x +. x') (ul_y +. y') in
        (if s.curved then draw_line_bezier else draw_line_ex) 
            v v' (vertex_radius /. 5.)
            (Color.create 0 0 0 100);
        let vv' = Vector2.subtract v' v in
        let l = Vector2.length vv' in
        let dvv' = Vector2.normalize vv' in
        let perp = Vector2.rotate dvv' (3.14 /. 2.) in
        let arrow_height = vertex_radius *. 0.7 in
        let v'' = Vector2.add v' 
            (Vector2.scale dvv' (-. vertex_radius)) in
        let v1 = Vector2.add 
            (Vector2.add v' 
            (Vector2.scale dvv' (-. vertex_radius *. 2. )))
            (Vector2.scale perp arrow_height) in
        let v2 = Vector2.add 
            (Vector2.add v' 
            (Vector2.scale dvv' (-. vertex_radius *. 2. )))
            (Vector2.scale perp (-. arrow_height)) in
        if s.arrow
        then draw_triangle v'' v2 v1 
                (Color.create 0 0 0 128)
    in


      for i = 0 to n-1 do
        let x, y = p2w pos.(i) in
        if 0. <= x && x < g_w && 0. <= y && y < g_h
        then
            for j = 0 to n-1 do
                let x', y' = p2w pos.(j) in
                if (* 0. <= x' && x' < g_w 
                    && 0. <= y' && y' < g_h && *) g.Graph.mat.(i).(j)
                    && (i < j || not g.Graph.mat.(j).(i))
                then begin
                    draw_edge x y x' y'
                end
            done
      done;
      (* Draw vertices *)
      let draw_vertex n x y =
          let px = int_of_float (x +. ul_x) in
          let py = int_of_float (y +. ul_y) in
          draw_circle_gradient px py vertex_radius
            (Color.create 255 200 200 255)
            (Color.create 200 0 0 255) ;
        let fs = (int_of_float (vertex_radius *. 1.0)) in
        let sn = Printf.sprintf "%d" n in
        let tw = measure_text sn fs in
          draw_text sn (px-tw/2) (py-fs/2) fs (Color.white)
      in

      for i = 0 to n-1 do
          let x, y = p2w pos.(i) in
        if 0. <= x && x < g_w -. 0. 
           && 0. <= y && y < g_h -. 0.
        then draw_vertex g.Graph.vtx.(i) x y
      done;

      { s with dragged_vertex = !dragged_vertex;
        panning = panning; zoom = zoom; offset = (o_x, o_y) }

let rec loop s =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      begin_drawing ();
      clear_background Color.gray;

      let s = draw_gui s in
      let s = draw_graph s in

      end_drawing ();
      loop s

let () = setup () |> loop
