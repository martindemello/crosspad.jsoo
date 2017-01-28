open Xword.Types

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
    current_dn : int;
    debug : string
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
      debug = ""
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
    | SetDebug of string
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
      | SetDebug s -> { model with debug = s }
      | Nothing -> model
    in
    f model
end
