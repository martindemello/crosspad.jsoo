module Keyboard_code = struct
  type t =
    | Unidentified
    (* Alphabetic Characters *)
    | KeyA                | KeyB                | KeyC                | KeyD
    | KeyE                | KeyF                | KeyG                | KeyH
    | KeyI                | KeyJ                | KeyK                | KeyL
    | KeyM                | KeyN                | KeyO                | KeyP
    | KeyQ                | KeyR                | KeyS                | KeyT
    | KeyU                | KeyV                | KeyW                | KeyX
    | KeyY                | KeyZ
    (* Digits *)
    | Digit0              | Digit1              | Digit2              | Digit3
    | Digit4              | Digit5              | Digit6              | Digit7
    | Digit8              | Digit9              | Minus               | Equal
    (* Whitespace *)
    | Tab                 | Enter               | Space
    (* Editing *)
    | Escape              | Backspace           | Insert              | Delete
    | CapsLock
    (* Misc Printable *)
    | BracketLeft         | BracketRight        | Semicolon           | Quote
    | Backquote           | Backslash           | Comma               | Period
    | Slash
    (* Function keys *)
    | F1                  | F2                  | F3                  | F4
    | F5                  | F6                  | F7                  | F8
    | F9                  | F10                 | F11                 | F12
    (* Numpad keys *)
    | Numpad0             | Numpad1             | Numpad2             | Numpad3
    | Numpad4             | Numpad5             | Numpad6             | Numpad7
    | Numpad8             | Numpad9             | NumpadMultiply      | NumpadSubtract
    | NumpadAdd           | NumpadDecimal       | NumpadEqual         | NumpadEnter
    | NumpadDivide        | NumLock
    (* Modifier keys *)
    | ControlLeft         | ControlRight        | MetaLeft            | MetaRight
    | ShiftLeft           | ShiftRight          | AltLeft             | AltRight
    (* Arrow keys *)
    | ArrowLeft           | ArrowRight          | ArrowUp             | ArrowDown
    (* Navigation *)
    | PageUp              | PageDown            | Home                | End
    (* Sound *)
    | VolumeMute          | VolumeDown          | VolumeUp
    (* Media *)
    | MediaTrackPrevious  | MediaTrackNext      | MediaPlayPause      | MediaStop
    (* Browser special *)
    | ContextMenu         | BrowserSearch       | BrowserHome         | BrowserFavorites
    | BrowserRefresh      | BrowserStop         | BrowserForward      | BrowserBack
    (* Misc *)
    | OSLeft              | OSRight             | ScrollLock          | PrintScreen
    | IntlBackslash       | IntlYen             | Pause

  let try_key_code_left = function
    | 16 -> ShiftLeft
    | 17 -> ControlLeft
    | 18 -> AltLeft
    | 91 -> MetaLeft
    | _  -> Unidentified

  let try_key_code_right = function
    | 16 -> ShiftRight
    | 17 -> ControlRight
    | 18 -> AltRight
    | 91 -> MetaRight
    | _  -> Unidentified

  let try_key_code_numpad = function
    | 46  -> NumpadDecimal
    | 45  -> Numpad0
    | 35  -> Numpad1
    | 40  -> Numpad2
    | 34  -> Numpad3
    | 37  -> Numpad4
    | 12  -> Numpad5
    | 39  -> Numpad6
    | 36  -> Numpad7
    | 38  -> Numpad8
    | 33  -> Numpad9
    | 13  -> NumpadEnter
    | 111 -> NumpadDivide
    | 107 -> NumpadAdd
    | 109 -> NumpadSubtract
    | 106 -> NumpadMultiply
    | 110 -> NumpadDecimal
    | 96  -> Numpad0
    | 97  -> Numpad1
    | 98  -> Numpad2
    | 99  -> Numpad3
    | 100 -> Numpad4
    | 101 -> Numpad5
    | 102 -> Numpad6
    | 103 -> Numpad7
    | 104 -> Numpad8
    | 105 -> Numpad9
    | _   -> Unidentified

  let try_key_code_normal = function
    | 27  -> Escape
    | 112 -> F1
    | 113 -> F2
    | 114 -> F3
    | 115 -> F4
    | 116 -> F5
    | 117 -> F6
    | 118 -> F7
    | 119 -> F8
    | 120 -> F9
    | 121 -> F10
    | 122 -> F11
    | 123 -> F12
    | 42  -> PrintScreen
    | 145 -> ScrollLock
    | 19  -> Pause
    | 192 -> Backquote
    | 49  -> Digit1
    | 50  -> Digit2
    | 51  -> Digit3
    | 52  -> Digit4
    | 53  -> Digit5
    | 54  -> Digit6
    | 55  -> Digit7
    | 56  -> Digit8
    | 57  -> Digit9
    | 48  -> Digit0
    | 189 -> Minus
    | 187 -> Equal
    | 8   -> Backspace
    | 9   -> Tab
    | 81  -> KeyQ
    | 87  -> KeyW
    | 69  -> KeyE
    | 82  -> KeyR
    | 84  -> KeyT
    | 89  -> KeyY
    | 85  -> KeyU
    | 73  -> KeyI
    | 79  -> KeyO
    | 80  -> KeyP
    | 219 -> BracketLeft
    | 221 -> BracketRight
    | 220 -> Backslash
    | 20  -> CapsLock
    | 65  -> KeyA
    | 83  -> KeyS
    | 68  -> KeyD
    | 70  -> KeyF
    | 71  -> KeyG
    | 72  -> KeyH
    | 74  -> KeyJ
    | 75  -> KeyK
    | 76  -> KeyL
    | 186 -> Semicolon
    | 222 -> Quote
    | 13  -> Enter
    | 90  -> KeyZ
    | 88  -> KeyX
    | 67  -> KeyC
    | 86  -> KeyV
    | 66  -> KeyB
    | 78  -> KeyN
    | 77  -> KeyM
    | 188 -> Comma
    | 190 -> Period
    | 191 -> Slash
    | 32  -> Space
    | 93  -> ContextMenu
    | 45  -> Insert
    | 36  -> Home
    | 33  -> PageUp
    | 46  -> Delete
    | 35  -> End
    | 34  -> PageDown
    | 37  -> ArrowLeft
    | 40  -> ArrowDown
    | 39  -> ArrowRight
    | 38  -> ArrowUp
    | _   -> Unidentified

  let run_next value f = function
    | Unidentified -> f value
    | v -> v

  let of_key_code code =
    Unidentified
    |> run_next code try_key_code_left
    |> run_next code try_key_code_right
    |> run_next code try_key_code_numpad
    |> run_next code try_key_code_normal
end
