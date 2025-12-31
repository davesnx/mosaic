type cursor_style = [ `Block | `Line | `Underline ]

type wrap_mode = [ `None | `Char | `Word ]

let default_max_length = 10000

module Props = struct
  type t = {
    background : Ansi.Color.t;
    text_color : Ansi.Color.t;
    focused_background : Ansi.Color.t;
    focused_text_color : Ansi.Color.t;
    placeholder : string;
    placeholder_color : Ansi.Color.t;
    cursor_color : Ansi.Color.t;
    cursor_style : cursor_style;
    cursor_blinking : bool;
    max_length : int;
    max_rows : int option;
    wrap_mode : wrap_mode;
    value : string;
    autofocus : bool;
  }

  let make ?background ?text_color ?focused_background ?focused_text_color
      ?(placeholder = "") ?(placeholder_color = Ansi.Color.of_rgb 102 102 102)
      ?(cursor_color = Ansi.Color.of_rgb 255 255 255)
      ?(cursor_style = (`Block : cursor_style)) ?(cursor_blinking = true)
      ?(max_length = default_max_length) ?max_rows ?(wrap_mode = (`Word : wrap_mode))
      ?(value = "") ?(autofocus = false) () =
    let transparent = Ansi.Color.of_rgba 0 0 0 0 in
    let white = Ansi.Color.of_rgb 255 255 255 in
    let default_focused_bg = Ansi.Color.of_rgb 26 26 26 in
    let background_val = Option.value background ~default:transparent in
    let text_color_val = Option.value text_color ~default:white in
    let focused_background_val =
      match focused_background with
      | Some c -> c
      | None -> (
          match background with Some c -> c | None -> default_focused_bg)
    in
    let focused_text_color_val =
      match focused_text_color with
      | Some c -> c
      | None -> ( match text_color with Some c -> c | None -> white)
    in
    let max_length =
      if max_length <= 0 then default_max_length else max_length
    in
    {
      background = background_val;
      text_color = text_color_val;
      focused_background = focused_background_val;
      focused_text_color = focused_text_color_val;
      placeholder;
      placeholder_color;
      cursor_color;
      cursor_style;
      cursor_blinking;
      max_length;
      max_rows = Option.value max_rows ~default:None;
      wrap_mode;
      value;
      autofocus;
    }

  let default = make ()

  let equal a b =
    Ansi.Color.equal a.background b.background
    && Ansi.Color.equal a.text_color b.text_color
    && Ansi.Color.equal a.focused_background b.focused_background
    && Ansi.Color.equal a.focused_text_color b.focused_text_color
    && String.equal a.placeholder b.placeholder
    && Ansi.Color.equal a.placeholder_color b.placeholder_color
    && Ansi.Color.equal a.cursor_color b.cursor_color
    && a.cursor_style = b.cursor_style
    && Bool.equal a.cursor_blinking b.cursor_blinking
    && Int.equal a.max_length b.max_length
    && Option.equal Int.equal a.max_rows b.max_rows
    && a.wrap_mode = b.wrap_mode
    && String.equal a.value b.value
    && Bool.equal a.autofocus b.autofocus
end

let clamp v ~min ~max = if v < min then min else if v > max then max else v

let grapheme_count s =
  let count = ref 0 in
  Glyph.iter_graphemes (fun ~offset:_ ~len:_ -> incr count) s;
  !count

let byte_offset_of_index s index =
  if index <= 0 then 0
  else
    let len = String.length s in
    let result = ref len in
    let count = ref 0 in
    try
      Glyph.iter_graphemes
        (fun ~offset ~len:_ ->
          if !count = index then (
            result := offset;
            raise Exit)
          else incr count)
        s;
      !result
    with Exit -> !result

let clamp_index s idx = clamp idx ~min:0 ~max:(grapheme_count s)

let substring_by_graphemes s ~start ~stop =
  if start >= stop then ""
  else
    let start = clamp_index s start in
    let stop = clamp_index s stop in
    if start >= stop then ""
    else
      let byte_start = byte_offset_of_index s start in
      let byte_stop = byte_offset_of_index s stop in
      String.sub s byte_start (byte_stop - byte_start)

let insert_text_at s index text =
  let index = clamp_index s index in
  let prefix = substring_by_graphemes s ~start:0 ~stop:index in
  let suffix = substring_by_graphemes s ~start:index ~stop:(grapheme_count s) in
  prefix ^ text ^ suffix

let remove_range s ~start ~stop =
  let start = clamp_index s start in
  let stop = clamp_index s stop in
  if start >= stop then s
  else
    let prefix = substring_by_graphemes s ~start:0 ~stop:start in
    let suffix =
      substring_by_graphemes s ~start:stop ~stop:(grapheme_count s)
    in
    prefix ^ suffix

(* Convert flat grapheme index to line/column position *)
let index_to_line_col text index =
  let lines = String.split_on_char '\n' text in
  let rec find_line acc idx remaining =
    match remaining with
    | [] -> (acc, idx)
    | line :: rest ->
        let line_graphemes = grapheme_count line in
        if idx < line_graphemes then (acc, idx)
        else find_line (acc + 1) (idx - line_graphemes - 1) rest
  in
  find_line 0 index lines

(* Convert line/column position to flat grapheme index *)
let line_col_to_index text row col =
  let lines = String.split_on_char '\n' text in
  let rec accumulate acc row_idx remaining =
    match remaining with
    | [] -> acc
    | line :: rest ->
        if row_idx = 0 then
          let line_graphemes = grapheme_count line in
          acc + clamp col ~min:0 ~max:line_graphemes
        else
          let line_graphemes = grapheme_count line in
          accumulate (acc + line_graphemes + 1) (row_idx - 1) rest
  in
  accumulate 0 row lines

(* Get line count *)
let line_count text =
  if text = "" then 1
  else
    let count = ref 1 in
    String.iter (fun c -> if c = '\n' then incr count) text;
    !count

(* Get grapheme count for a specific line *)
let line_grapheme_count text line_idx =
  let lines = String.split_on_char '\n' text in
  if line_idx < 0 || line_idx >= List.length lines then 0
  else grapheme_count (List.nth lines line_idx)

let placeholder_style color =
  Ansi.Style.make ~fg:color ~bg:(Ansi.Color.of_rgba 0 0 0 0) ()

type callbacks = {
  mutable on_input : (string -> unit) list;
  mutable on_change : (string -> unit) list;
  mutable on_submit : (string -> unit) list;
}

let callbacks () = { on_input = []; on_change = []; on_submit = [] }

let notify handlers variant value =
  let callbacks =
    match variant with
    | `Input -> handlers.on_input
    | `Change -> handlers.on_change
    | `Submit -> handlers.on_submit
  in
  List.iter (fun f -> f value) (List.rev callbacks)

type cursor_pos = { row : int; col : int }

type t = {
  surface : Text_surface.t;
  mutable props : Props.t;
  mutable prop_value : string;
  mutable value : string;
  mutable graphemes : int;
  mutable cursor : int; (* flat grapheme index *)
  mutable preferred_col : int; (* column hint for Up/Down navigation *)
  mutable view_offset : cursor_pos; (* viewport scroll position *)
  mutable last_committed : string;
  callbacks : callbacks;
  mutable was_focused : bool;
  mutable buffer_dirty : bool;
  mutable placeholder_cached_source : string;
  mutable placeholder_cached_width : int;
  mutable placeholder_cached_text : string;
}

let clamp_index_cached t idx = clamp idx ~min:0 ~max:t.graphemes
let surface_node t = Text_surface.node t.surface
let node t = surface_node t
let request_render t = Text_surface.request_render t.surface
let text_buffer t = Text_surface.buffer t.surface
let text_view t = Text_surface.view t.surface

let update_buffer t =
  let buffer = text_buffer t in
  Text_buffer.reset buffer;
  if t.value <> "" then (
    let chunk =
      Text_buffer.Chunk.
        {
          text = Bytes.of_string t.value;
          fg = Some t.props.text_color;
          bg = None;
          attrs = Ansi.Attr.empty;
          link = None;
        }
    in
    ignore (Text_buffer.write_chunk buffer chunk);
    t.buffer_dirty <- true)

let measure t ~known_dimensions ~available_space ~style:_ =
  let width_hint =
    match known_dimensions with
    | Toffee.Geometry.Size.{ width = Some w; _ } when w > 0. -> Some w
    | _ -> (
        match available_space with
        | Toffee.Geometry.Size.{ width; _ } ->
            Toffee.Available_space.to_option width)
  in
  let height_hint =
    match known_dimensions with
    | Toffee.Geometry.Size.{ height = Some h; _ } when h > 0. -> Some h
    | _ -> (
        match available_space with
        | Toffee.Geometry.Size.{ height; _ } ->
            Toffee.Available_space.to_option height)
  in
  let measured_width =
    match width_hint with
    | Some w when w > 0. -> Float.floor w |> int_of_float |> max 1
    | _ -> 1
  in
  let measured_height =
    match height_hint with
    | Some h when h > 0. -> Float.floor h |> int_of_float |> max 1
    | _ -> (
        match t.props.max_rows with
        | Some rows -> rows
        | None -> max 1 (line_count t.value))
  in
  Toffee.Geometry.Size.
    { width = float measured_width; height = float measured_height }

let compute_placeholder t maxw =
  if t.props.placeholder = "" || maxw <= 0 then ""
  else if
    t.placeholder_cached_width = maxw
    && String.equal t.placeholder_cached_source t.props.placeholder
  then t.placeholder_cached_text
  else
    let placeholder = t.props.placeholder in
    let acc = ref 0 in
    let end_offset = ref 0 in
    (try
       Glyph.iter_graphemes
         (fun ~offset ~len ->
           let grapheme = String.sub placeholder offset len in
           let w =
             Glyph.measure ~width_method:`Unicode ~tab_width:2 grapheme |> max 1
           in
           if !acc + w <= maxw then (
             acc := !acc + w;
             end_offset := offset + len)
           else raise Exit)
         placeholder
     with Exit -> ());
    let text = String.sub placeholder 0 !end_offset in
    t.placeholder_cached_source <- placeholder;
    t.placeholder_cached_width <- maxw;
    t.placeholder_cached_text <- text;
    text

let draw_placeholder t ~x ~y ~width grid =
  if t.props.placeholder = "" then ()
  else
    let style = placeholder_style t.props.placeholder_color in
    let maxw = max 0 (width - 1) in
    let text = compute_placeholder t maxw in
    if text <> "" then Grid.draw_text ~style grid ~x ~y ~text

let notify_input t = notify t.callbacks `Input t.value
let notify_change t = notify t.callbacks `Change t.value
let notify_submit t = notify t.callbacks `Submit t.value
let focus t = Renderable.focus (surface_node t)

let blur t =
  if String.compare t.value t.last_committed <> 0 then (
    t.last_committed <- t.value;
    notify_change t);
  Renderable.blur (surface_node t)

let max_length_limit t =
  if t.props.max_length <= 0 then default_max_length else t.props.max_length

let truncate_to_max t value =
  let max_length = max_length_limit t in
  substring_by_graphemes value ~start:0 ~stop:max_length

let set_value_internal t value ~notify =
  let value = truncate_to_max t value in
  if String.equal value t.value then ()
  else (
    t.value <- value;
    t.graphemes <- grapheme_count value;
    t.cursor <- clamp_index_cached t t.cursor;
    update_buffer t;
    request_render t;
    if notify then notify_input t)

let set_value t value =
  set_value_internal t value ~notify:true

(* Update viewport to keep cursor visible *)
let update_viewport_for_cursor t =
  let row, col = index_to_line_col t.value t.cursor in
  let rnode = surface_node t in
  let viewport_height = Renderable.height rnode in
  let viewport_width = Renderable.width rnode in
  if viewport_height <= 0 || viewport_width <= 0 then ()
  else (
    (* Vertical scrolling: keep cursor row visible *)
    let current_view_row = t.view_offset.row in
    let view_end_row = current_view_row + viewport_height - 1 in
    let new_view_row =
      if row < current_view_row then row
      else if row > view_end_row then max 0 (row - viewport_height + 1)
      else current_view_row
    in
    t.view_offset <- { t.view_offset with row = new_view_row };
    (* Horizontal scrolling: keep cursor col visible (if wrap_mode is None) *)
    (match t.props.wrap_mode with
    | `None ->
        let current_view_col = t.view_offset.col in
        let view_end_col = current_view_col + viewport_width - 1 in
        let new_view_col =
          if col < current_view_col then col
          else if col > view_end_col then max 0 (col - viewport_width + 1)
          else current_view_col
        in
        t.view_offset <- { t.view_offset with col = new_view_col }
    | `Char | `Word -> ());
    (* Update Text_surface viewport *)
    Text_buffer_view.set_viewport (text_view t)
      (Some
         {
           Text_buffer_view.x = t.view_offset.col;
           y = t.view_offset.row;
           width = viewport_width;
           height = viewport_height;
         }))

let hardware_cursor t =
  let rnode = surface_node t in
  if not (Renderable.focused rnode) then None
  else
    let lx = Renderable.x rnode in
    let ly = Renderable.y rnode in
    let lw = Renderable.width rnode in
    let lh = Renderable.height rnode in
    if lw <= 0 || lh <= 0 then None
    else
      let row, col = index_to_line_col t.value t.cursor in
      let view_row = row - t.view_offset.row in
      let view_col = col - t.view_offset.col in
      if view_row < 0 || view_row >= lh || view_col < 0 || view_col >= lw then
        None
      else
        Some
          ( lx + view_col + 1,
            ly + view_row + 1,
            t.props.cursor_color,
            t.props.cursor_style,
            t.props.cursor_blinking )

let insert_text t text =
  if text <> "" then
    let current = t.graphemes in
    let inserted = grapheme_count text in
    let capacity =
      let limit = max_length_limit t in
      limit - current
    in
    if capacity <= 0 then ()
    else
      let to_insert = if inserted <= capacity then text else "" in
      if to_insert <> "" then (
        let new_value = insert_text_at t.value t.cursor to_insert in
        set_value_internal t new_value ~notify:true;
        t.cursor <- clamp_index_cached t (t.cursor + grapheme_count to_insert);
        t.preferred_col <- (let _, c = index_to_line_col t.value t.cursor in c))

let insert_newline t =
  let newline = "\n" in
  let current = t.graphemes in
  let capacity =
    let limit = max_length_limit t in
    limit - current
  in
  if capacity > 0 then (
    let new_value = insert_text_at t.value t.cursor newline in
    set_value_internal t new_value ~notify:true;
    t.cursor <- clamp_index_cached t (t.cursor + 1);
    t.preferred_col <- 0)

let delete_backward t =
  if t.cursor > 0 then (
    let original_cursor = t.cursor in
    let new_value =
      remove_range t.value ~start:(original_cursor - 1) ~stop:original_cursor
    in
    set_value_internal t new_value ~notify:true;
    t.cursor <- clamp_index_cached t (original_cursor - 1);
    (* Update preferred_col if we deleted a newline *)
    let _, c = index_to_line_col t.value t.cursor in
    t.preferred_col <- c)

let delete_forward t =
  if t.cursor < t.graphemes then (
    let new_value = remove_range t.value ~start:t.cursor ~stop:(t.cursor + 1) in
    set_value_internal t new_value ~notify:true;
    ())

let commit_value t =
  if String.compare t.value t.last_committed <> 0 then (
    t.last_committed <- t.value;
    notify_change t)

(* Move cursor up one line, preserving column hint *)
let move_up t =
  let row, _ = index_to_line_col t.value t.cursor in
  if row > 0 then (
    let new_row = row - 1 in
    let new_col = min t.preferred_col (line_grapheme_count t.value new_row) in
    let new_index = line_col_to_index t.value new_row new_col in
    t.cursor <- clamp_index_cached t new_index;
    request_render t)

(* Move cursor down one line, preserving column hint *)
let move_down t =
  let row, _ = index_to_line_col t.value t.cursor in
  let total_lines = line_count t.value in
  if row < total_lines - 1 then (
    let new_row = row + 1 in
    let new_col = min t.preferred_col (line_grapheme_count t.value new_row) in
    let new_index = line_col_to_index t.value new_row new_col in
    t.cursor <- clamp_index_cached t new_index;
    request_render t)

(* Move cursor to start of current line *)
let move_home t =
  let row, _ = index_to_line_col t.value t.cursor in
  let new_index = line_col_to_index t.value row 0 in
  t.cursor <- clamp_index_cached t new_index;
  t.preferred_col <- 0;
  request_render t

(* Move cursor to end of current line *)
let move_end t =
  let row, _ = index_to_line_col t.value t.cursor in
  let line_cols = line_grapheme_count t.value row in
  let new_index = line_col_to_index t.value row line_cols in
  t.cursor <- clamp_index_cached t new_index;
  t.preferred_col <- line_cols;
  request_render t

(* Move cursor to start of text *)
let move_home_all t =
  t.cursor <- 0;
  t.preferred_col <- 0;
  request_render t

(* Move cursor to end of text *)
let move_end_all t =
  t.cursor <- t.graphemes;
  let _, c = index_to_line_col t.value t.cursor in
  t.preferred_col <- c;
  request_render t

(* Move cursor up by viewport height *)
let move_page_up t =
  let rnode = surface_node t in
  let page_size = max 1 (Renderable.height rnode) in
  let row, _ = index_to_line_col t.value t.cursor in
  let new_row = max 0 (row - page_size) in
  let new_col = min t.preferred_col (line_grapheme_count t.value new_row) in
  let new_index = line_col_to_index t.value new_row new_col in
  t.cursor <- clamp_index_cached t new_index;
  request_render t

(* Move cursor down by viewport height *)
let move_page_down t =
  let rnode = surface_node t in
  let page_size = max 1 (Renderable.height rnode) in
  let row, _ = index_to_line_col t.value t.cursor in
  let total_lines = line_count t.value in
  let new_row = min (total_lines - 1) (row + page_size) in
  let new_col = min t.preferred_col (line_grapheme_count t.value new_row) in
  let new_index = line_col_to_index t.value new_row new_col in
  t.cursor <- clamp_index_cached t new_index;
  request_render t

let handle_key t (event : Event.key) =
  let event = Event.Key.data event in
  let is_ascii_printable s =
    let len = String.length s in
    if len <> 1 then false
    else
      let c = int_of_char s.[0] in
      c >= 32 && c <= 126
  in
  match event.event_type with
  | Release -> false
  | Press | Repeat -> (
      match event.key with
      | Char _uchar
        when (not event.modifier.ctrl) && (not event.modifier.alt)
             && (not event.modifier.meta)
             && event.associated_text <> "" ->
          if is_ascii_printable event.associated_text then
            insert_text t event.associated_text;
          true
      | Char uchar
        when (not event.modifier.ctrl) && (not event.modifier.alt)
             && not event.modifier.meta ->
          let buf = Stdlib.Buffer.create 4 in
          Stdlib.Buffer.add_utf_8_uchar buf uchar;
          let s = Stdlib.Buffer.contents buf in
          if is_ascii_printable s then insert_text t s;
          true
      | Backspace ->
          delete_backward t;
          true
      | Delete ->
          delete_forward t;
          true
      | Left ->
          if t.cursor > 0 then (
            t.cursor <- clamp_index_cached t (t.cursor - 1);
            let _, c = index_to_line_col t.value t.cursor in
            t.preferred_col <- c;
            request_render t);
          true
      | Right ->
          if t.cursor < t.graphemes then (
            t.cursor <- clamp_index_cached t (t.cursor + 1);
            let _, c = index_to_line_col t.value t.cursor in
            t.preferred_col <- c;
            request_render t);
          true
      | Up ->
          move_up t;
          true
      | Down ->
          move_down t;
          true
      | Home ->
          if event.modifier.ctrl then move_home_all t else move_home t;
          true
      | End ->
          if event.modifier.ctrl then move_end_all t else move_end t;
          true
      | Page_up ->
          move_page_up t;
          true
      | Page_down ->
          move_page_down t;
          true
      | Enter | KP_enter | Line_feed ->
          if event.modifier.ctrl then (
            commit_value t;
            notify_submit t)
          else insert_newline t;
          true
      | _ -> false)

let render_textarea t renderable grid ~delta:_ =
  let lx = 0 in
  let ly = 0 in
  let lw = Renderable.width renderable in
  let lh = Renderable.height renderable in
  if lw <= 0 || lh <= 0 then ()
  else
    let focused = Renderable.focused renderable in
    if focused <> t.was_focused then (
      if (not focused) && t.was_focused then commit_value t;
      t.was_focused <- focused);
    update_viewport_for_cursor t;
    let buffer = text_buffer t in
    let view = text_view t in
    if t.buffer_dirty then (
      Text_buffer_view.set_wrap_mode view t.props.wrap_mode;
      Text_buffer_view.set_wrap_width view
        (match t.props.wrap_mode with
        | `None -> None
        | `Char | `Word -> Some lw);
      Text_buffer.finalise buffer;
      t.buffer_dirty <- false);
    let bg =
      if focused then t.props.focused_background else t.props.background
    in
    Grid.fill_rect grid ~x:lx ~y:ly ~width:lw ~height:lh ~color:bg;
    if t.value = "" then draw_placeholder t ~x:lx ~y:ly ~width:lw grid
    else
      (* Use Text_surface's rendering - it already handles multi-line *)
      let virtual_lines = Text_buffer_view.virtual_lines view in
      let viewport = t.view_offset in
      let start_line = viewport.row in
      let end_line = min (Array.length virtual_lines) (start_line + lh) in
      let widths = Text_buffer.drawing_widths buffer in
      let chars = Text_buffer.drawing_chars buffer in
      let fg =
        if focused then t.props.focused_text_color else t.props.text_color
      in
      for line_idx = start_line to end_line - 1 do
        if line_idx < Array.length virtual_lines then (
          let vline = virtual_lines.(line_idx) in
          let dest_y = ly + (line_idx - start_line) in
          if dest_y >= 0 && dest_y < lh then (
            let rec loop i column =
              if i >= vline.Text_buffer.Virtual_line.length || column >= lw then
                ()
              else
                let idx = vline.Text_buffer.Virtual_line.start_index + i in
                let code = Bigarray.Array1.unsafe_get chars idx in
                let width =
                  let w = Bigarray.Array1.unsafe_get widths idx in
                  if w <= 0 then 1 else w
                in
                let dest_x = lx + column in
                if dest_x >= 0 && dest_x < Grid.width grid then (
                  Grid.set_cell_alpha grid ~x:dest_x ~y:dest_y ~code ~fg ~bg
                    ~attrs:Ansi.Attr.empty ();
                  loop (i + 1) (column + width))
            in
            loop 0 0))
      done

let mount ?(props = Props.default) (rnode : Renderable.t) =
  let default_style = Ansi.Style.make ~fg:props.text_color () in
  let surface =
    Text_surface.mount
      ~props:
        (Text_surface.Props.make ~wrap_mode:props.wrap_mode ~default_style ())
      rnode
  in
  let callbacks = callbacks () in
  let initial_cursor = grapheme_count props.value in
  let _, initial_col = index_to_line_col props.value initial_cursor in
  let textarea =
    {
      surface;
      props;
      prop_value = props.value;
      value = props.value;
      graphemes = grapheme_count props.value;
      cursor = initial_cursor;
      preferred_col = initial_col;
      view_offset = { row = 0; col = 0 };
      last_committed = props.value;
      callbacks;
      was_focused = false;
      buffer_dirty = true;
      placeholder_cached_source = "";
      placeholder_cached_width = -1;
      placeholder_cached_text = "";
    }
  in
  update_buffer textarea;
  let renderable = surface_node textarea in
  Renderable.set_render renderable (render_textarea textarea);
  Renderable.set_measure renderable (Some (measure textarea));
  Renderable.set_buffer renderable `Self;
  Renderable.set_focusable renderable true;
  Renderable.set_default_key_handler renderable
    (Some (fun event -> ignore (handle_key textarea event)));
  Renderable.set_hardware_cursor_provider renderable
    (Some
       (fun _ ->
         match hardware_cursor textarea with
         | None -> None
         | Some (x, y, color, style, blinking) ->
             Some { Renderable.x; y; color; style; blinking }));
  (match textarea.props.autofocus with
  | true -> ignore (Renderable.focus renderable)
  | false -> ());
  request_render textarea;
  textarea

let value t = t.value
let cursor t = t.cursor

type cursor_position = { row : int; col : int }

let cursor_pos t =
  let row, col = index_to_line_col t.value t.cursor in
  { row; col }

let set_placeholder t placeholder =
  if t.props.placeholder <> placeholder then (
    t.props <- { t.props with placeholder };
    t.placeholder_cached_source <- "";
    t.placeholder_cached_width <- -1;
    t.placeholder_cached_text <- "";
    request_render t)

let set_cursor t index =
  let index = clamp_index_cached t index in
  if t.cursor <> index then (
    t.cursor <- index;
    let _, c = index_to_line_col t.value t.cursor in
    t.preferred_col <- c;
    request_render t)

let set_max_length t max_length =
  let normalized = if max_length <= 0 then default_max_length else max_length in
  t.props <- { t.props with max_length = normalized };
  if t.graphemes > normalized then
    let truncated = substring_by_graphemes t.value ~start:0 ~stop:normalized in
    set_value_internal t truncated ~notify:false

let set_background t color =
  if not (Ansi.Color.equal t.props.background color) then (
    t.props <- { t.props with background = color };
    request_render t)

let set_focused_background t color =
  if not (Ansi.Color.equal t.props.focused_background color) then (
    t.props <- { t.props with focused_background = color };
    request_render t)

let set_text_color t color =
  if not (Ansi.Color.equal t.props.text_color color) then (
    t.props <- { t.props with text_color = color };
    Text_surface.set_default_style t.surface (Ansi.Style.make ~fg:color ());
    update_buffer t;
    request_render t)

let set_focused_text_color t color =
  if not (Ansi.Color.equal t.props.focused_text_color color) then (
    t.props <- { t.props with focused_text_color = color };
    request_render t)

let set_placeholder_color t color =
  if not (Ansi.Color.equal t.props.placeholder_color color) then (
    t.props <- { t.props with placeholder_color = color };
    request_render t)

let set_cursor_color t color =
  if not (Ansi.Color.equal t.props.cursor_color color) then (
    t.props <- { t.props with cursor_color = color };
    if Renderable.focused (surface_node t) then request_render t)

let set_cursor_style t style =
  if t.props.cursor_style <> style then (
    t.props <- { t.props with cursor_style = style };
    if Renderable.focused (surface_node t) then request_render t)

let set_cursor_blinking t blinking =
  if t.props.cursor_blinking <> blinking then (
    t.props <- { t.props with cursor_blinking = blinking };
    if Renderable.focused (surface_node t) then request_render t)

let set_wrap_mode t mode =
  if t.props.wrap_mode <> mode then (
    t.props <- { t.props with wrap_mode = mode };
    Text_surface.set_wrap_mode t.surface mode;
    t.buffer_dirty <- true;
    request_render t)

let set_callbacks t ?on_input ?on_change ?on_submit () =
  let to_list = function None -> [] | Some f -> [ f ] in
  t.callbacks.on_input <- to_list on_input;
  t.callbacks.on_change <- to_list on_change;
  t.callbacks.on_submit <- to_list on_submit

let on_input t handler = t.callbacks.on_input <- handler :: t.callbacks.on_input

let on_change t handler =
  t.callbacks.on_change <- handler :: t.callbacks.on_change

let on_submit t handler =
  t.callbacks.on_submit <- handler :: t.callbacks.on_submit

let apply_props t (props : Props.t) =
  if t.props.autofocus <> props.autofocus then
    t.props <- { t.props with autofocus = props.autofocus };
  set_background t props.background;
  set_text_color t props.text_color;
  set_focused_background t props.focused_background;
  set_focused_text_color t props.focused_text_color;
  set_placeholder t props.placeholder;
  set_placeholder_color t props.placeholder_color;
  set_cursor_color t props.cursor_color;
  set_cursor_style t props.cursor_style;
  set_cursor_blinking t props.cursor_blinking;
  set_max_length t props.max_length;
  set_wrap_mode t props.wrap_mode;
  if not (String.equal props.value t.prop_value) then (
    t.prop_value <- props.value;
    set_value t props.value)

