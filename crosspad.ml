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

type rs = Model.t React.signal
type rf = ?step:React.step -> Model.t -> unit
type rp = rs * rf

module Controller = struct
  let update action ((r, f) : rp) =
    let open Action in
    let open Model in
    let model = React.S.value r in
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

  let cell x y (signal, f) =
    let s = square_size in
    let x0 = top_left_x + x * s |> float in
    let y0 = top_left_y + y * s |> float in
    let s = s |> float in
    let num_x = x0 +. 1. in
    let num_y = y0 +. (s /. 3.) in
    let let_x = x0 +. (s /. 2.) in
    let let_y = y0 +. s -. 5. in
    let letter' = [ R.Svg.pcdata @@ React.S.map ~eq:String.equal (letter x y) signal ] in
    let number' = [ R.Svg.pcdata @@ React.S.map ~eq:String.equal (number x y) signal ] in
    let t_num' = svg_text ["crosspad-number"] num_x num_y number' in
    let t_let' = svg_text ["crosspad-letter"] let_x let_y letter' in
    let rect' = Svg.(rect ~a:[
        a_onclick (fun _ -> Controller.update (SetCursor (x, y)) (signal, f); true);
        R.Svg.a_class @@ React.S.map
          ~eq:(fun a b -> String.equal (List.hd a) (List.hd b))
          (cellstyle x y) signal;
        a_x (px x0); a_y (px y0);
        a_width (px s); a_height (px s)] [])
    in
    Svg.g [ t_num'; t_let'; rect' ]

  let cells (signal, f) =
    let list_of_model model =
      let out = ref [] in
      for y = 0 to model.xw.cols - 1 do
        for x = 0 to model.xw.rows - 1 do
          out := (cell x y (signal, f)) :: !out
        done
      done;
      List.rev !out
    in
    (* we don't expect xword size to change *)
    let eq x y = true in
    ReactiveData.RList.from_signal (React.S.map ~eq list_of_model signal)

  let svg_grid (signal, f) =
    let s = square_size in
    let x0 = top_left_x |> float in
    let y0 = top_left_y |> float in
    let w model = model.xw.cols * s |> float in
    let h model = model.xw.rows * s |> float in
    let w_px model = px (w model) in
    let h_px model = px (h model) in
    let svg_w_px model = px (w model +. 1.) in
    let svg_h_px model = px (h model +. 1.) in
    let size_eq (x, _) (y, _) = x = y in
    let box = Svg.rect ~a:[
        Svg.a_class ["crosspad-grid-rect"];
        Svg.a_x (px x0);
        Svg.a_y (px y0);
        R.Svg.a_width  (React.S.map ~eq:size_eq w_px signal);
        R.Svg.a_height (React.S.map ~eq:size_eq h_px signal);
      ] []
    in
    Svg.svg ~a:[
      R.Svg.a_width  (React.S.map ~eq:size_eq svg_w_px signal);
      R.Svg.a_height (React.S.map ~eq:size_eq svg_h_px signal)
    ] [
      Svg.g ~a:[Svg.a_transform [`Translate (0.5, Some 0.5)]]
        [
          Svg.g [box];
          R.Svg.g (cells (signal, f))
        ]
    ]

  let view ((signal, f) : rp) =
    let open Html5 in
    div [
      div [pcdata "hello world"];
      div [ svg ~a:Svg.([a_width (px 600.0); a_height (px 600.0)])
              [ svg_grid (signal, f) ] ]
    ]
end

let main _ =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById(Js.string "main"))
      (fun () -> assert false)
  in
  let m = Model.init 15 15 in
  let rp = React.S.create m in
  Dom.appendChild parent (Tyxml_js.To_dom.of_div (View.view rp)) ;
  Lwt.return ()

let _ = Lwt_js_events.onload () >>= main

