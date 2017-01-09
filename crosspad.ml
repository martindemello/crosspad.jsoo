open Xword.Types
open Lwt.Infix

type coords = { x : int; y : int }

module Model = struct
  open Cursor

  type t = {
    xw : xword;
    cursor : Cursor.t
  }

  let init rows cols =
    let xw = Xword.make rows cols in
    Xword.renumber xw;
    { xw;
      cursor = Cursor.make rows cols
    }

  let set_cursor model x y =
    let cursor' = { model.cursor with x; y } in
    { model with cursor = cursor' }
end


module Action = struct
  type action =
    | SetCursor of int * int
end

module Controller = struct
  let update action (model, f) =
    let open Action in
    let open Model in
    let model =
      match action with
      | SetCursor (x, y) -> set_cursor model x y
    in
    f model
end

module View = struct
  open Model
  open Cursor
  open Action
  open Tyxml_js

  let px x = (x, Some `Px)

  let top_left_x = 0
  let top_left_y = 0

  let square_size = 32

  let cellstyle x y model =
    let cell = Xword.get_cell model.xw x y in
    let is_cur = model.cursor.x = x && model.cursor.y = y in
    let bg = match is_cur, cell with
      | true, Black -> "crosspad-cursor-black"
      | true, _ -> "crosspad-cursor-white"
      | false, Black -> "crosspad-black"
      | false, _ -> "crosspad-white"
    in
    [bg; "crosspad-square"]

  let letter_of_cell = function
    | Letter s -> s
    | Rebus r -> r.display_char
    | _ -> ""

  let display_num = function
    | 0 -> ""
    | n -> string_of_int n

  (* Accessors *)
  let square x y model =
    Xword.get model.xw x y

  let letter x y model =
    letter_of_cell (square x y model).cell

  let number x y model =
    display_num (square x y model).num

  (* SVG fragments *)
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
        a_onclick (fun _ -> Controller.update (SetCursor (x, y)) (model, f); true);
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

  let view (model, f) =
    let open Html5 in
    div [
      div [pcdata "hello world"];
      div [ svg ~a:Svg.([a_width (px 600.0); a_height (px 600.0)])
              [ svg_grid (model, f) ] ]
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
