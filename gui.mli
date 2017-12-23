(** [gui_event_t] represents events from the GUI that were generated due to
  * the players' interactions and actions in the game. These events can be
  *  polled to start different phases of the game and update those phases. *)
type gui_event_t =
  | Init
  | JsonFnameTurnOn (* event to start asking the user to input the JSON file name*)
  | JsonFname of string (* user submitted the JSON file name *)
  | PSelectTurnOn 
  | PSelect of (string * State.controller) list
  | Comm of Command.command

(** [char_or_special] is the type of key that can be bound to different actions
  * on a [LTerm_widget.frame f] if f also inherits [key_binding_functions]. *)
type char_or_special = 
  | Ch of char 
  | Special of LTerm_key.code

(** [key_binding_t] is a key binding where a key is associated to a description
  * that can be displayed, and a actor function that triggers some event or 
  * action in the GUI. *)
type key_binding_t = {
  key : char_or_special;
  desc : string;
  act : unit -> unit;
}

(** [key_binding_functions] provides key bindings to frames in the GUI
  * and functions to handle events on those key bindings. *)
class key_binding_functions : object

  (** [bind_keys lst] binds keys in list [lst] to a frame by appending [lst]
    * to the existing bindings in that frame. *)
  method bind_keys : key_binding_t list -> unit

  (** [bind_scroller adj] binds up and down arrow keys to a frame, which trigger
    * scrolling in [adj]. *)
  method bind_scroller : LTerm_widget.scrollable -> unit

  (** [add_action_labels box] creates labels in [box] of the character and
    * descriptions present in this instance's key bindings. *)
  method add_action_labels : LTerm_widget.box -> unit

  (** [add_false_action_labels box lst] creates "false" action labels
    * in the [box] without actually associating keys to the frame. Can
    * be useful when you don't want to associate key bindings [box]
    * but do want to display them. This can be useful if a child widget binds 
    * those bindings. *)
  method add_false_action_labels : LTerm_widget.box -> (string * string) list -> unit
end

(** [game_frame]'s instance is a [LTerm_widget.frame f] in the GUI. 
  * Use [f#bind_keys lst] to associate key bindings in [lst] to [f]. *)
class game_frame : object
  inherit LTerm_widget.frame
  inherit key_binding_functions
end

(** [game_frame]'s instance is a [LTerm_widget.modal_frame f] in the GUI. 
  * Use [f#bind_keys lst] to associate key bindings in [lst] to [f]. *)
class game_modal_frame : object
  inherit LTerm_widget.modal_frame
  inherit key_binding_functions
end

(** [game_gui] is the GUI for the game. It activates different frames 
  * in correspondence with users' actions, and listens to events based on
  * their actions to play the game. 
  * Reference: Sitar Harel's powder-shell GUI implementation at
  * https://github.com/sitarharel/powder-shell. *)
class game_gui : (game_modal_frame -> unit -> unit)
  -> (unit -> unit) -> (unit -> unit) -> object
  inherit game_frame

  (** [update_state s_opt] updates the copy of [State.state] stored in
    * this object instance. *)
  method update_state : State.state option -> unit

  (** [update_history pid c] adds a rough description of player [pid] giving
    * command [c] to the GUI's history. Used to display history of the game. *)
  method update_history : string -> Command.command -> unit

  (** [get_events] is the list of events that were registered since the
    * last time [get_events] was called. The events on the GUI are reset
    * to empty when [get_events] is called. *)
  method get_events : gui_event_t list

  (** [activate_json_fname_modal ()] activates the modal frame to input the
    * JSON file name. *)
  method activate_json_fname_modal : unit -> unit

  (** [activate_pselect_modal ()] activates the modal frame through which the
    * user can input types of players in the game. Should be called
    * only after JSON file name input has been received. *)
  method activate_pselect_modal : unit -> unit

  (** [activate_init_modal ()] activates the modal frame that shows the
    * initialization actions to initialize the full game. *)
  method activate_init_modal : unit -> unit

  (** [activate_error_modal thrown_err_msg ~disp ()] activates the modal
    * frame that displays [disp]. [thrown_err_msg] is also displayed in the frame
    * if [String.length thrown_err_msg <> 0]. *)
  method activate_error_modal : string -> disp:string -> unit -> unit

  (** [activate_pdisplay_modal ()] activates the modal frame that displays the
   * players in the game. If [data.s = None] then the error modal frame is
   * activated. *)
  method activate_pdisplay_modal : unit -> unit

  (** [activate_game_modal ()] activates modal frames depending on the current
   * player in the game. In other words, it presents different modal frames based
   * on what the current player's controller is.
   * requires:
   *   - at least one [LTerm_widget.frame] is present before this is called. *)
  method activate_game_modal : unit -> unit


  (** [setup] configures the key inputs for the game and sets up the mouse events
    * for modals that do not change throughout the game. *)
  method setup : unit
end