open Lwt
open Gui

type game_t = {
  gui : game_gui;
  s : State.state option;
  json_fname : string option;
}

(** [reset_game g] is a game with no state and no JSON file name. *)
let reset_game g = {g with s = None; json_fname = None}

(* TODO: finish implementation *)
let rec handle_events g = function
  | [] -> g
  | Init::t ->
      let g' = reset_game g in
      g'.gui#activate_init_modal ();
      handle_events g' t
  | JsonFnameTurnOn::t ->
      (* event given by the init modal, and the game is already reset *)
      g.gui#activate_json_fname_modal ();
      handle_events g t
  | JsonFname fname::t ->
      (* event given by the json modal, and the game is already reset *)
      let g' = {g with json_fname = Some fname} in
      g.gui#activate_pselect_modal ();
      handle_events g' t
  | PSelectTurnOn::t ->
      g.gui#activate_pselect_modal ();
      handle_events g t
  | PSelect p_cnts::t -> handle_pselect g p_cnts t
  | Comm c::t -> handle_comm g c t

(** [handle_pselect g p_cnts] handles event [PSelect p_cnts]. *)
and handle_pselect g p_cnts t =
  match g.json_fname with
  | Some fname ->
    begin
      try
        if List.length p_cnts = 0 then raise (Failure "Select at least one player.") else ();
        let s = State.init_state (Yojson.Basic.from_file fname) p_cnts ~random:true in
        let g' = {g with s = Some s} in
        g'.gui#update_state (Some s);
        g'.gui#activate_pdisplay_modal ();
        handle_events g' t
      with
      | Failure s ->
          g.gui#activate_error_modal
          ~disp:"Error in player selection."
          s (); reset_game g
      | Sys_error s ->
        g.gui#activate_error_modal
          ~disp:"The JSON file couldn't be found in this directory."
          s (); reset_game g
      | Yojson.Basic.Util.Type_error (s, _) ->
        g.gui#activate_error_modal
          ~disp:"The JSON file is not a properly formatted board."
          s (); reset_game g
      | Yojson.Json_error s ->
        g.gui#activate_error_modal
          ~disp:"The JSON file doesn't contain a properly formatted adventure."
          s (); reset_game g
    end
  | None ->
    g.gui#activate_error_modal
      ~disp:"Aborted. Program tried selecting players before inputting game board."
      "" (); reset_game g

(** [handle_comm g c t] handles event [Comm c] *)
and handle_comm g c t =
  match g.s with
  | Some st ->
    begin
      g.gui#update_history (State.get_current_player st |> fst) c;
      let s' = State.do' c st in
      let g' = {g with s = Some s'} in
      g'.gui#update_state (Some s');
      g'.gui#activate_game_modal ();
      handle_events g' t
    end
  | None ->
      g.gui#activate_error_modal
      ~disp:"State cannot be accessed while evaluating the command."
      "" (); g

(** [run_game g n ()] executes the game [g], polls for events in [g.gui],
  * handles them, and updates the game state. [n] is the nth time [run_game]
  * is called, and is used to display the initializing information in the
  * [g.gui]. *)
let rec run_game (g : game_t) n () : unit Lwt.t =
  let inps = g.gui#get_events in
  let g' = handle_events g inps in
  (* display the init modal first. For some reason, n is not 0, but is 1. *)
  if n = 1 then g'.gui#activate_init_modal () else ();
  g'.gui#update_state g'.s; return g'
  >>= fun _ -> Lwt_unix.sleep 0.04 >>= run_game g' (n+1)

(** [main ()] is the main game loop for the game. It sets up the GUI
  * and runs the game. *)
let main () =
  let do_run, push_layer, pop_layer, exit' =
    LTerm_widget.prepare_simple_run () in
  let executer term =
    let gui_ob = new game_gui push_layer pop_layer exit' in
    gui_ob#setup;
    (* start the game *)
    let g = {
      gui = gui_ob;
      s = None;
      json_fname = None;
    } in
    async (run_game g 0);
    do_run g.gui
  in Lazy.force LTerm.stdout >>= executer

let () = Lwt_main.run (main ())
