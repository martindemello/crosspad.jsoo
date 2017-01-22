open Xword.Types
open Lwt.Infix

type key_direction = [`Left | `Right | `Up | `Down ]

module Coords = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Pervasives.compare x0 x1 with
      0 -> Pervasives.compare y0 y1
    | c -> c
end

module CSet = Set.Make(Coords)

let set_of_list xs =
  List.fold_left (fun set elem -> CSet.add elem set)
    CSet.empty xs

module Model = struct
  open Cursor

  type t = {
    xw : xword;
    cursor : Cursor.t;
    current_dir : word_direction;
    current_word : CSet.t;
    current_ac : int;
    current_dn : int
  }

  (* updates *)
  let renumber model =
    Xword.renumber model.xw;
    model

  let update_current_word model =
    let get_word = match model.current_dir with
      | `Across -> Xword.word_ac
      | `Down -> Xword.word_dn
    in
    let c = get_word model.xw model.cursor.x model.cursor.y in
    let s = set_of_list c in
    { model with current_word = s }

  (* cursor *)
  let set_current_dir d model =
    { model with current_dir = d }

  let toggle_current_dir model =
    let d = match model.current_dir with `Across -> `Down | `Down -> `Across in
    { model with current_dir = d }
    |> update_current_word

  let set_cursor x y model =
    if x = model.cursor.x && y = model.cursor.y then
      model |> toggle_current_dir
    else
      let cursor' = { model.cursor with x; y } in
      { model with cursor = cursor' }
      |> update_current_word

  let move_cursor ?wrap:(wrap=true) (d : direction) model =
    let cursor' = Cursor.move model.cursor ~wrap d in
    { model with cursor = cursor' }

  let advance_cursor model =
    move_cursor ~wrap:false (model.current_dir :> direction) model

  let backspace_cursor model =
    let dir' = match model.current_dir with
      | `Across -> `Bksp_Ac | `Down -> `Bksp_Dn
    in
    move_cursor ~wrap:false dir' model

  let movement_key (d : key_direction) model =
    let dir' : word_direction = match d with
      | `Left | `Right -> `Across
      | `Up | `Down -> `Down
    in
    model
    |> move_cursor ~wrap:false (d :> direction)
    |> set_current_dir dir'
    |> update_current_word

  (* clues *)
  let set_current_clue d n model =
    match d with
    | `Across -> { model with current_ac = n }
    | `Down -> { model with current_dn = n }

  (* grid *)

  (* set a letter only if the current square is white *)
  let set_current_letter s model =
    let xw = model.xw in
    let x, y = model.cursor.x, model.cursor.y in
    let _ = match s with
    | Some s -> Xword.set_letter xw x y s
    | None -> Xword.delete_letter xw x y
    in
    model

  let toggle_black model =
    let xw = model.xw in
    let x, y = model.cursor.x, model.cursor.y in
    if Xword.toggle_black xw x y then Xword.renumber xw;
    model
    |> advance_cursor
    |> update_current_word

  let set_letter s model =
    model
    |> set_current_letter (Some s)
    |> advance_cursor

  let delete_letter model =
    model
    |> set_current_letter None

  let backspace_letter model =
    model
    |> backspace_cursor
    |> delete_letter

  (* init *)
  let init rows cols =
    let xw = Xword.make rows cols in
    let clues = { across = [(1, "hello"); (2, "world")];
                  down = [(10, "foo"); 20, "bar"]
                }
    in
    let xw = { xw with clues } in
    { xw;
      cursor = Cursor.make rows cols;
      current_dir = `Across;
      current_word = CSet.empty;
      current_ac = 0;
      current_dn = 0;
    }
    |> renumber
    |> update_current_word

end


module Action = struct
  type action =
    | SetCursor of int * int
    | MoveCursor of key_direction
    | SetDirection of word_direction
    | SetLetter of string
    | ToggleBlack
    | Backspace
    | Delete
    | SetClue of word_direction * int
    | Nothing
end

module Controller = struct
  let update action (model, f) =
    let open Action in
    let open Model in
    let model =
      match action with
      | SetCursor (x, y) -> set_cursor x y model
      | MoveCursor d -> movement_key d model
      | SetDirection d -> set_current_dir d model
      | ToggleBlack -> toggle_black model
      | SetLetter s -> set_letter s model
      | Backspace -> backspace_letter model
      | Delete -> delete_letter model
      | SetClue (d, n) -> set_current_clue d n model
      | Nothing -> model
    in
    f model
end

module View = struct
  open Model
  open Cursor
  open Tyxml_js

  (* constants *)
  let top_left_x = 0
  let top_left_y = 0

  let square_size = 32

  (* utils *)
  let px x = (x, Some `Px)

  let letter_of_code k =
    if k >= 65 && k <= 90 then
      Some (String.make 1 @@ Char.chr k)
    else if k >= 97 && k <= 122 then
      Some (String.make 1 @@ Char.chr (k - 32))
    else
      None

  (* events *)
  let add_keyboard_handlers (model, f) =
    let w = Dom_html.window in
    let fn evt =
      let open Dom_html.Keyboard_code in
      let code = evt##.keyCode in
      let key = Dom_html.Keyboard_code.of_event evt in
      let action = match key with
      | Space -> Action.ToggleBlack
      | ArrowLeft -> Action.MoveCursor `Left
      | ArrowRight -> Action.MoveCursor `Right
      | ArrowUp -> Action.MoveCursor `Up
      | ArrowDown -> Action.MoveCursor `Down
      | Backspace -> Action.Backspace
      | Delete -> Action.Delete
      | _ -> begin
          match letter_of_code code with
          | Some s -> Action.SetLetter s
          | None -> Action.Nothing
        end
      in
      match action with
      | Action.Nothing -> Js._true
      | a -> begin
          Controller.update a (model, f);
          Js._false
        end
    in
    w##.onkeypress := Dom_html.handler fn ;
    w##.onkeydown := Dom_html.handler fn

  (* Grid display *)

  let cellstyle x y model =
    let cell = Xword.get_cell model.xw x y in
    let is_cur = model.cursor.x = x && model.cursor.y = y in
    let is_word = CSet.mem (x, y) model.current_word in
    let bg =
      if is_cur then match cell with
      | Black -> "crosspad-cursor-black"
      | _ -> "crosspad-cursor-white"
      else if is_word then
       "crosspad-word"
      else match cell with
      | Black -> "crosspad-black"
      | _ -> "crosspad-white"
    in
    [bg; "crosspad-square"]

  let letter_of_cell = function
    | Letter s -> s
    | Rebus r -> r.display_char
    | _ -> ""

  let display_num = function
    | 0 -> ""
    | n -> string_of_int n

  (* Grid accessors *)

  let square x y model =
    Xword.get model.xw x y

  let letter x y model =
    letter_of_cell (square x y model).cell

  let number x y model =
    display_num (square x y model).num

  (* Grid SVG fragments *)

  let svg_text cls x y txt =
    let open Svg in
    text ~a:[a_class cls; a_x_list [(px x)]; a_y_list [(px y)]] txt

  let svg_rect x y w h cls =
    let open Svg in
    rect ~a:[a_class cls;
             a_x (px x); a_y (px y);
             a_width (px w); a_height (px h)] []

  let cell x y (model, f) =
    let s = square_size in
    let x0 = top_left_x + x * s |> float in
    let y0 = top_left_y + y * s |> float in
    let s = s |> float in
    let num_x = x0 +. 1. in
    let num_y = y0 +. (s /. 3.) in
    let let_x = x0 +. (s /. 2.) in
    let let_y = y0 +. s -. 5. in
    let letter' = [ Svg.pcdata @@ letter x y model ] in
    let number' = [ Svg.pcdata @@ number x y model ] in
    let t_num' = svg_text ["crosspad-number"] num_x num_y number' in
    let t_let' = svg_text ["crosspad-letter"] let_x let_y letter' in
    let rect' = Svg.(rect ~a:[
        a_onclick (fun _ ->
            Controller.update (Action.SetCursor (x, y)) (model, f);
            true);
        Svg.a_class @@ cellstyle x y model;
        a_x (px x0); a_y (px y0);
        a_width (px s); a_height (px s)] [])
    in
    Svg.g [ t_num'; t_let'; rect' ]

  let cells (model, f) =
    let out = ref [] in
    for y = 0 to model.xw.cols - 1 do
      for x = 0 to model.xw.rows - 1 do
        out := (cell x y (model, f)) :: !out
      done
    done;
    List.rev !out

  let svg_grid (model, f) =
    let s = square_size in
    let x0 = top_left_x |> float in
    let y0 = top_left_y |> float in
    let w = model.xw.cols * s |> float in
    let h = model.xw.rows * s |> float in
    let w_px = px w in
    let h_px = px h in
    let svg_w_px = px (w +. 1.) in
    let svg_h_px = px (h +. 1.) in
    let box = Svg.rect ~a:[
        Svg.a_class ["crosspad-grid-rect"];
        Svg.a_x (px x0);
        Svg.a_y (px y0);
        Svg.a_width  w_px;
        Svg.a_height h_px;
      ] []
    in
    Svg.svg ~a:[
      Svg.a_width  svg_w_px;
      Svg.a_height svg_h_px
    ] [
      Svg.g ~a:[Svg.a_transform [`Translate (0.5, Some 0.5)]]
        [
          Svg.g [box];
          Svg.g (cells (model, f))
        ]
    ]

  (* Clues *)

  let clue dir current (n, c) (model, f) =
    let cls = if current = n then [ "crosspad-clue-current" ] else [] in
    let open Html5 in
    li ~a: [ a_class cls ; a_onclick (fun _ ->
        Controller.update (Action.SetClue (dir, n)) (model, f);
        true)]
      [ div ~a:[ a_class ["crosspad-clue-number"] ]
          [ pcdata (string_of_int n) ];
        div ~a:[ a_class ["crosspad-clue-text"] ]
          [ pcdata c ]
      ]

  let clue_box (model, f) =
    let open Html5 in
    let clues = model.xw.clues in
    let list dir current cs =
      div ~a:[ a_class ["crosspad-clue-section"] ]
        [ ul ~a:[ a_class ["crosspad-clue-list"] ]
            (List.map (fun c -> (clue dir current c (model, f))) cs) ]
    in
    let lbl str =
      p ~a:[ a_class ["crosspad-clue-label"] ]
        [ pcdata str ]
    in
    div ~a:[ a_class ["crosspad-clues-container"] ]
      [ lbl "Across"
      ; list `Across model.current_ac clues.across
      ; lbl "Down"
      ; list `Down model.current_dn clues.down
      ]


  (* Main view *)

  let view (model, f) =
    add_keyboard_handlers (model, f);
    let open Html5 in
    let g =
      div
        [ svg ~a:Svg.([a_width (px 600.0); a_height (px 600.0)])
            [ svg_grid (model, f) ]
        ]
    in
    let clues = clue_box (model, f) in
    div ~a:[ a_id "view-main" ]
      [ div ~a:[ a_class ["crosspad-main"] ]
          [ div ~a:[ a_class ["crosspad-grid-container"] ] [ g ]
          ; clues
          ]
      ]
end

let main _ =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById(Js.string "main"))
      (fun () -> assert false)
  in
  let rec f model =
    let elt = Tyxml_js.To_dom.of_div (View.view (model, f)) in
    Js.Opt.iter parent##.lastChild (Dom.replaceChild parent elt) in
  f (Model.init 15 15);
  Lwt.return ()

let _ = Lwt_js_events.onload () >>= main
