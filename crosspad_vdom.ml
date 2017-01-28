open Vdom
open Crosspad_model
open Xword.Types
open Printf

type coords = { x : int; y : int }

module Html = struct
  let px_prop k v = str_prop "x" (sprintf "%f" v)
  let a_width v = Property ("width", Float v)
  let a_height = float_prop "height"
  let a_class xs = class_ (String.concat " " xs)
  let pcdata = text
end

module Svg = struct
  let svg = svg_elt "svg"

  let g ?key ?a l = svg_elt "g" ?key ?a l
  let rect ?key ?a l = svg_elt "rect" ?key ?a l
  let text_ ?key ?a l = svg_elt "text" ?key ?a l

  let a_width = float_attr "width"
  let a_height = float_attr "height"
  let a_x = float_attr "x"
  let a_y = float_attr "y"
  let a_transform = attr "transform"
  let translate x y = sprintf "translate(%f,%f)" x y
  
  let a_class xs = attr "class" (String.concat " " xs)
  let pcdata = text
end

module View = struct
  open Model
  open Cursor

  let px x = (x, Some `Px)

  let top_left = {x = 0; y = 0}

  let square_size = 32

  let cellstyle model x y =
    let cell = Xword.get_cell model.xw x y in
    let is_cur = model.cursor.x == x && model.cursor.y == y in
    let bg = match cell with Black -> "black" | _ -> "white" in
    let cursor = if is_cur then "cursor-" else "" in
    "crosspad-" ^ cursor ^ bg

  let svg_text cls x y s =
    let open Svg in
    text_ ~a:[a_class cls; a_x x; a_y y] [pcdata s]

  let svg_rect cls x y w h =
    let open Svg in
    rect ~a:[
      a_class cls;
      a_x x; a_y y;
      a_width w; a_height h
    ] [] 

  let letter_of_cell = function
    | Letter s -> s
    | Rebus r -> r.display_char
    | _ -> ""

  let display_num = function
    | 0 -> ""
    | n -> string_of_int n

  let cell model x y =
    let s = square_size in
    let x0 = top_left.x + x * s |> float in
    let y0 = top_left.y + y * s |> float in
    let s = s |> float in
    let num_x = x0 +. 1. in
    let num_y = y0 +. (s /. 3.) in
    let let_x = x0 +. (s /. 2.) in
    let let_y = y0 +. s -. 5. in
    let xw = model.xw in
    let cstyle = ["crosspad-square"; cellstyle model x y] in
    let sq = Xword.get xw x y in
    let letter = letter_of_cell sq.cell in
    let number = display_num sq.num in
    let open Svg in
    let t_num = svg_text ["crosspad-number"] num_x num_y number in
    let t_let = svg_text ["crosspad-letter"] let_x let_y letter in
    let r = rect ~a:[
      a_class cstyle;
      a_x x0; a_y y0;
      a_width s; a_height s;
      onclick (Action.SetCursor (x, y))
    ] []
    in
    g [ t_num; t_let; r ]

  let cells model =
    let out = ref [] in
    Xword.iteri model.xw (fun _ x y _ ->
        out := (cell model x y) :: !out);
    List.rev !out

  let svg_grid model =
    let s = square_size in
    let w = model.xw.cols * s |> float in
    let h = model.xw.rows * s |> float in
    let x0 = top_left.x |> float in
    let y0 = top_left.y |> float in
    let svg_w = w +. 1. in
    let svg_h = h +. 1. in
    let open Svg in
    let box = svg_rect ["crosspad-grid-rect"] x0 y0 w h in
    svg ~a:[a_width svg_w; a_height svg_h]
      [ g ~a:[a_transform (translate 0.5 0.5)]
          ( box :: (cells model) )
      ]

  let letter_of_code k =
    if k >= 65 && k <= 90 then
      Some (String.make 1 @@ Char.chr k)
    else if k >= 97 && k <= 122 then
      Some (String.make 1 @@ Char.chr (k - 32))
    else
      None

  let action_of_key e =
    let open Js_event.Keyboard_code in
    let code = e.which in
    let key = of_key_code code in
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
    action

  let view model =
    let open Html in
    div [
      div [pcdata "Vdom"];
      div [pcdata model.debug];
      div ~a:[a_class ["crosspad-grid-container"]; onkeydown action_of_key; int_prop "tabIndex" 0; autofocus]
        [ svg_grid model  ]
    ]
end

let init = return (Model.init 15 15)

let update model action =
  let f (x : Model.t) = x in
  let model = Controller.update action (model, f) in
  return model

let my_app = app ~init ~update ~view:View.view ()


open Js_browser

let run () =
  Vdom_blit.run my_app   (* run the application *)
  |> Vdom_blit.dom    (* get its root DOM container *)
  |> Element.append_child (Document.body document)   (* insert the DOM in the document *)

let () = Window.set_onload window run
