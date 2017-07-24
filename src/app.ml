open Sexplib.Std

module Reactjs = Caelm.Reactjs.Make_with_require (struct
    let require = Js.Unsafe.variable "require"
  end)

module Todo = struct
  type state = Editing of string | View [@@deriving sexp]
  type t =
    { description : string
    ; done_ : bool
    ; id : int
    ; state : state
    } [@@deriving sexp]

  let id = ref 0

  let create description =
    let id' = !id in
    incr id;
    { description; done_ = false; id = id'; state = View }

  let set_next_id id' = id := id'

  let is_editing todo = todo.state <> View

  let id todo = todo.id

  let description todo = todo.description

  let done_ todo = todo.done_

  let state todo = todo.state

  let toggled todo = { todo with done_ = not todo.done_ }

  let begin_edit todo = match todo.state with
    | Editing _ -> todo
    | View -> { todo with state = Editing todo.description }

  let commit_edit todo = match todo.state with
    | Editing _ -> { todo with state = View }
    | View -> todo

  let discard_edit todo = match todo.state with
    | Editing description -> { todo with description; state = View }
    | View -> todo
end

module State = struct
  type filter = All | Active | Completed
  type todos = Todo.t list [@@deriving sexp]
  type t =
    { filter : filter
    ; input : string
    ; todos : todos
    }

  type message =
    | Add_todo of Todo.t
    | Remove_todo of Todo.t
    | Replace_todo of (Todo.t * Todo.t)
    | Set_filter of filter
    | Todo_saved
    | Update_input of string

  type command = message Lwt.t

  let local_storage =
    Dom_html.window##.localStorage
    |> Caelm.Reactjs_wrapper.Unsafe.optdef_get_exn

  let storage_key = Js.string "todos-caelm"

  let load_todos () =
    let todos =
      let deserialize s =
        Js.to_string s |> Sexplib.Sexp.of_string |> todos_of_sexp in
      Js.Opt.case
        local_storage##(getItem storage_key)
        (fun () -> [])
        deserialize in
    let latest_id = List.map Todo.id todos |> List.fold_left max 0 in
    Todo.set_next_id (latest_id + 1);
    todos

  let save_todos todos =
    let serialized =
      sexp_of_todos todos |> Sexplib.Sexp.to_string_mach |> Js.string in
    local_storage##(setItem storage_key serialized);
    Lwt.return Todo_saved

  let create () = { filter = All; input = ""; todos = load_todos () }

  let equal = ( = )

  let replace todos old new_ =
    List.map (fun todo -> if todo = old then new_ else todo) todos

  let update ({ filter; input; todos } as state) = function
    | Add_todo todo ->
      let todos = todo :: todos in
      { state with input = ""; todos }, Some (save_todos todos)
    | Remove_todo todo ->
      let todos = List.filter (fun t -> t <> todo) todos in
      { state with todos }, Some (save_todos todos)
    | Replace_todo (old, new_) ->
      let todos = replace todos old new_ in
      let command =
        if Todo.is_editing new_ then None
        else Some (save_todos todos) in
      { state with todos }, command
    | Set_filter filter -> { state with filter }, None
    | Todo_saved -> state, None (* No-op. *)
    | Update_input input -> { state with input }, None
end

module View = struct
  module Tyxml = Caelm_reactjs_tyxml.Make (Reactjs)
  module Wrapper = Caelm.Reactjs_wrapper.Make (Reactjs)

  open State
  open Tyxml.Html

  let cons_if b x xs = if b then x :: xs else xs

  let filter_todos ~filter ts =
    let f = match filter with
      | All -> fun _ -> true
      | Active -> fun t -> not (Todo.done_ t)
      | Completed -> fun t -> Todo.done_ t in
    List.filter f ts

  let header_control ~add_todo ~update_input ~input:input' =
    let add_todo e =
      let input = String.trim input' in
      let key = Js.to_string e##.key in
      if input <> "" && key = "Enter" then add_todo (Todo.create input) in
    let update_input e = update_input (Js.to_string e##.target##.value) in
    header ~a:[ a_class [ "header" ] ]
      [ h1 [ pcdata "todos" ]
      ; input ~a:[ a_autofocus true
                 ; a_class [ "new-todo" ]
                 ; a_placeholder "What needs to be done?"
                 ; a_value input'
                 ; a_oninput update_input
                 ; a_onkeydown add_todo
                 ] ()
      ]

  let todo ~remove_todo ~replace_todo t =
    let open Todo in
    let { description; done_; _ } = t in
    let editing = is_editing t in
    let replace = replace_todo t in
    let toggle_check _ = replace (toggled t) in
    let class_names =
      [] |> cons_if editing "editing" |> cons_if done_ "completed" in
    li ~a:[ a_class class_names ]
      [ if editing then
          let update_description e =
            replace { t with description = Js.to_string e##.target##.value} in
          let commit_edit _ =
            let description = String.trim description in
            if description = "" then remove_todo t
            else replace (commit_edit { t with description }) in
          let commit_edit_if_enter e =
            if Js.to_string e##.key = "Enter" then commit_edit () in
          let discard_edit_if_escape e =
            if Js.to_string e##.key = "Escape" then replace (discard_edit t) in
          input ~a:[ a_class [ "edit" ]
                   ; a_input_type `Text
                   ; a_autofocus true
                   ; a_value description
                   ; a_onblur commit_edit
                   ; a_oninput update_description
                   ; a_onkeydown discard_edit_if_escape
                   ; a_onkeypress commit_edit_if_enter
                   ] ()
        else
          let begin_edit _ = replace (begin_edit t) in
          div ~a:[ a_class [ "view" ] ]
            [ input ~a:[ a_class [ "toggle" ]
                       ; a_input_type `Checkbox
                       ; a_checked done_
                       ; a_onchange toggle_check
                       ] ()
            ; label ~a:[ a_ondoubleclick begin_edit ] [ pcdata description ]
            ; button ~a:[ a_class [ "destroy" ]
                        ; a_onclick @@ fun _ -> remove_todo t
                        ] []
            ]
      ]

  let main_control ~remove_todo ~replace_todo ~filter ts =
    let all_completed = List.for_all Todo.done_ ts in
    let toggle_all _ =
      let done_ = not all_completed in
      List.iter (fun t -> replace_todo t { t with Todo.done_ }) ts in
    section ~a:[ a_class @@ cons_if (List.length ts = 0) "hidden" [ "main" ] ]
      [ input ~a:[ a_id "toggle-all"
                 ; a_class [ "toggle-all" ]
                 ; a_input_type `Checkbox
                 ; a_checked all_completed
                 ; a_onchange toggle_all
                 ] ()
      ; label ~a:[ a_label_for "toggle-all" ]
          [ pcdata "Mark all as complete" ]
      ; ul ~a:[ a_class [ "todo-list" ] ]
          (ts |> filter_todos ~filter
           |> List.rev_map (todo ~remove_todo ~replace_todo))
      ]

  let footer_control ~remove_todo ~set_filter ~filter:selected ts =
    let completed, left = List.partition Todo.done_ ts in
    let num_todos = List.length ts in
    let num_left = List.length left in
    let filter_switch filter =
      let button_text, href = match filter with
        | All -> "All", Uri.of_string "#/"
        | Active -> "Active", Uri.of_string "#/active"
        | Completed -> "Completed", Uri.of_string "#/completed" in
      li [ a ~a:([ a_href href
                 ; a_onclick @@ fun _ -> set_filter filter
                 ] |> cons_if (filter = selected) (a_class [ "selected" ]))
             [ pcdata button_text ]
         ] in
    footer ~a:[ a_class @@ cons_if (num_todos = 0) "hidden" [ "footer" ] ]
      [ span ~a:[ a_class [ "todo-count" ] ]
          [ strong [ pcdata @@ string_of_int num_left ]
          ; pcdata @@ if num_left = 1 then " item left" else " items left"
          ]
      ; ul ~a:[ a_class [ "filters" ] ]
          (List.map filter_switch [ All; Active; Completed ])
      ; button ~a:[ a_class @@
                    cons_if (completed = []) "hidden" [ "clear-completed" ]
                  ; a_onclick @@ fun _ -> List.iter remove_todo completed
                  ]
          [ pcdata "Clear completed" ]
      ]

  let render ~send ~container { State.filter; input; todos } =
    let add_todo todo = send (Add_todo todo) in
    let remove_todo todo = send (Remove_todo todo) in
    let replace_todo old new_ = send (Replace_todo (old, new_)) in
    let update_input input = send (Update_input input) in
    let set_filter filter = send (Set_filter filter) in
    div [ header_control ~add_todo ~update_input ~input
        ; main_control ~remove_todo ~replace_todo ~filter todos
        ; footer_control ~remove_todo ~set_filter ~filter todos
        ]
    |> to_react_element
    |> Wrapper.render ~container
end

module App = Caelm_lwt.Make (State) (View)

(* TODO: Support routing *)
let () =
  let container =
    Dom_html.document##
      (getElementsByClassName (Js.string "todoapp"))##
      (item 0)
    |> Caelm.Reactjs_wrapper.Unsafe.opt_get_exn in
  ignore @@ App.run ~container (State.create ())
