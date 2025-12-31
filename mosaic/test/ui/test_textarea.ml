open Mosaic_ui

let render_boxed ?(width = 20) ?(height = 3) element =
  let content =
    box ~id:"outer" ~border:true
      ~size:(size ~width:(width + 2) ~height:(height + 2))
      [ element ]
  in
  print_newline ();
  print ~colors:false ~width:(width + 2) ~height:(height + 2) content

let%expect_test "empty textarea shows placeholder" =
  render_boxed ~width:20 ~height:3
    (textarea ~id:"ta" ~placeholder:"Type here..."
       ~size:(size ~width:20 ~height:3) ());
  [%expect_exact
    {|
┌────────────────────┐
│Type here...        │
│                    │
│                    │
└────────────────────┘
||}] [@@ocamlformat "disable"]

let%expect_test "textarea with single line value" =
  render_boxed ~width:20 ~height:3
    (textarea ~id:"ta" ~value:"Hello world"
       ~size:(size ~width:20 ~height:3) ());
  [%expect_exact
    {|
┌────────────────────┐
│Hello world         │
│                    │
│                    │
└────────────────────┘
||}] [@@ocamlformat "disable"]

let%expect_test "textarea with multi-line value" =
  render_boxed ~width:20 ~height:3
    (textarea ~id:"ta" ~value:"Line 1\nLine 2\nLine 3"
       ~size:(size ~width:20 ~height:3) ());
  [%expect_exact
    {|
┌────────────────────┐
│Line 1              │
│Line 2              │
│Line 3              │
└────────────────────┘
||}] [@@ocamlformat "disable"]

let%expect_test "textarea with word wrap" =
  render_boxed ~width:15 ~height:3
    (textarea ~id:"ta" ~value:"This is a very long line that should wrap"
       ~wrap_mode:`Word ~size:(size ~width:15 ~height:3) ());
  [%expect_exact
    {|
┌───────────────┐
│This is a very │
│long line that │
│should wrap    │
└───────────────┘
||}] [@@ocamlformat "disable"]

let%expect_test "textarea empty without placeholder" =
  render_boxed ~width:15 ~height:3
    (textarea ~id:"ta" ~size:(size ~width:15 ~height:3) ());
  [%expect_exact
    {|
┌───────────────┐
│               │
│               │
│               │
└───────────────┘
||}] [@@ocamlformat "disable"]

let%expect_test "set_value updates content" =
  render_boxed ~width:15 ~height:3
    (textarea ~id:"ta" ~value:"Initial"
       ~on_mount:(fun ta -> Textarea.set_value ta "Changed\nMulti\nLine")
       ~size:(size ~width:15 ~height:3) ());
  [%expect_exact
    {|
┌───────────────┐
│Changed        │
│Multi          │
│Line           │
└───────────────┘
||}] [@@ocamlformat "disable"]

let%expect_test "set_placeholder updates placeholder text" =
  render_boxed ~width:20 ~height:3
    (textarea ~id:"ta" ~placeholder:"Original"
       ~on_mount:(fun ta -> Textarea.set_placeholder ta "Updated placeholder")
       ~size:(size ~width:20 ~height:3) ());
  [%expect_exact
    {|
┌────────────────────┐
│Updated placeholder │
│                    │
│                    │
└────────────────────┘
||}] [@@ocamlformat "disable"]

let%expect_test "set_max_length truncates existing content" =
  render_boxed ~width:15 ~height:3
    (textarea ~id:"ta" ~value:"Hello World\nSecond Line"
       ~on_mount:(fun ta -> Textarea.set_max_length ta 5)
       ~size:(size ~width:15 ~height:3) ());
  [%expect_exact
    {|
┌───────────────┐
│Hello          │
│               │
│               │
└───────────────┘
||}] [@@ocamlformat "disable"]

let%expect_test "textarea with no wrap mode shows long line" =
  render_boxed ~width:15 ~height:3
    (textarea ~id:"ta" ~value:"This is a very long line that should not wrap"
       ~wrap_mode:`None ~size:(size ~width:15 ~height:3) ());
  [%expect_exact
    {|
┌───────────────┐
│This is a very │
│               │
│               │
└───────────────┘
||}] [@@ocamlformat "disable"]

let%expect_test "cursor_pos returns correct row and column" =
  render_boxed ~width:20 ~height:3
    (textarea ~id:"ta" ~value:"Line 1\nLine 2\nLine 3"
       ~on_mount:(fun ta ->
         let pos = Textarea.cursor_pos ta in
         (* Cursor defaults to end, which is row 2, col 6 (after "Line 3") *)
         assert (pos.row = 2);
         assert (pos.col = 6);
         Printf.printf "Cursor at row %d, col %d\n" pos.row pos.col)
       ~size:(size ~width:20 ~height:3) ());
  [%expect_exact
    {|
┌────────────────────┐
│Line 1              │
│Line 2              │
│Line 3              │
└────────────────────┘
Cursor at row 2, col 6
|}]

