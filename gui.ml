open Lwt
open LTerm_widget
open LTerm_geom
open LTerm_key
open LTerm_event

type gui_event_t =
  | Init
  | JsonFnameTurnOn
  | JsonFname of string
  | PSelectTurnOn
  | PSelect of (string * State.controller) list
  | Comm of Command.command

(* [trade_mon_prop_t] represents the money and the properties in the offer. *)
type trade_mon_prop_t = {
  mon : int;
  prop_lst : string list;
}

(** [text_input]'s objects can be used to make editor widgets inside
  * [LTerm_widget.modal_frame]. These editors will be single-line if the
  * [LTerm_widget.box] containing them doesn't add the frame embedding the editor
  * with free expansion. *)
class text_input = object(self)
  inherit LTerm_edit.edit () as super

  (** [size_request] is the dimensions of the editor window. *)
  method size_request = {rows = 1; cols = 10}
end

(* [gui_data_t] is used to store information in the GUI instance. *)
type gui_data_t = {
  (** [push_modals] is a mapping from the modal's description to a function
    * that pushes the modal. [push_modals] will usually contain unchanging
    * modal frames that can be pushed by [game_gui]'s methods on to the screen.
    * For example, a modal frame for asking the user to input json file name
    * can be pushed on to the screen. *)
  mutable push_modals : (string * (unit -> unit)) list;

  (** [event_buffer] is a list of unprocessed events from the GUI. *)
  mutable event_buffer : gui_event_t list;

  (** [s] is the game's state as referenced by the GUI instance. *)
  mutable s : State.state option;

  (** [player_controllers] is the array containing the Player's controllers that
    * are used to initialize the game state. Its usage should become obsolete once
    * the game state has been initialized. [player_controllers.(i)] is the
    * controller for the ith player in the game. Used by [pselect_modal].
    * restriction for our game: [Array.length player_controllers = 8]. *)
  mutable player_controllers : State.controller option array;

  (** [player_pieces] is the array containing the possible names of the players.
    * Similar to [player_controllers], this array becomes obsolete when the game
    * is initialized. [player_pieces.(i) is the name of ith player. Used by
    * [pselect_modal].
    * restriction for our game: [Array.length player_pieces = 8]. *)
  player_pieces : string array;

  (** [turns] is the number of turns taken by all players in the game. *)
  mutable turns : int;

  (** [history] is the history of commands directed by players in the game. *)
  mutable history : string list;
}

type char_or_special =
  | Ch of char
  | Special of LTerm_key.code

type key_binding_t = {
  key : char_or_special;
  desc : string;
  act : unit -> unit;
}

(** [string_of_preroll_comm pid c] is [Some s], where [s] is a rough description
  * of [Command.Preroll_command c] in English, annotated with player's name [pid]. *)
let string_of_preroll_comm pid = function
  | Command.Roll -> Some (pid^" rolled.")
  | Command.SellProp s -> Some (pid^" attempted to sell "^s^".")
  | Command.SellAndRoll s -> Some (pid^" attempted to sell "^s^" and rolled.")
  | Command.Develop s -> Some (pid^" attempted to develop "^s^".")
  | Command.DevelopAndRoll s -> Some (pid^" attempted to develop "^s^" and rolled.")
  | Command.Trade (otherp, offer, return) ->
      Some (pid^" wants to trade with "^otherp^".")

(** [string_of_injail_comm pid c] is [Some s], where [s] is a rough description
  * of [Command.Preroll_in_jail_command c] in English, annotated with player's
  * name [pid]. *)
let string_of_injail_comm pid = function
  | Command.Roll_injail -> Some (pid^" tried rolling while in jail.")
  | Command.UseCard -> Some (pid^" tried using their Get-Out-Of-Jail card.")
  | Command.PayJailFee -> Some (pid^" tried paying to get out of jail.")

(** [string_of_trade_offer pid c] is [Some s], where [s] is a rough description
  * of [Command.Trade_offer_command c] in English, annotated with player's name
  * [pid]. *)
let string_of_trade_offer pid = function
  | Command.AcceptTrade -> Some (pid^" accepted the trade.")
  | Command.DeclineTrade -> Some (pid^" declined the trade.")

(** [string_of_unowned_prop pid c] is [Some s], where [s] is a rough description
  * of [Command.Land_unowned_prop_command c] in English, annotated with player's
  * name [pid]. *)
let string_of_unowned_prop pid = function
  | Command.Buy -> Some (pid^" tried buying a property.")
  | Command.NotBuy ->
      Some (pid^" did not buy the property they landed on.")

(** [string_of_other_prop pid c] is [Some s], where [s] is a rough description
  * of [Command.Land_other_prop_command c] in English, annotated with player's
  * name [pid]. *)
let string_of_other_prop pid = function
  | Command.PayRent -> Some (pid^" tried paying rent.")
  | Command.Sell_PayRent s -> Some (pid^" tried selling "^s^" to pay rent.")
  | Command.BankruptRent ->
      Some (pid^" declared bankruptcy while paying rent.")

(** [string_of_transaction pid c] is [Some s], where [s] is a rough description
  * of [Command.Transaction_command c] in English, annotated with player's name
  * [pid]. *)
let string_of_transaction pid = function
  | Command.PayTransaction -> Some (pid^" tried paying the bank.")
  | Command.Sell_PayTransaction s ->
      Some (pid^" tried selling "^s^" to pay the bank.")
  | Command.BankruptTransaction ->
      Some (pid^" declared bankruptcy while paying the bank.")

(** [string_of_command pid c] is [Some s], where [s] is a rough description
  * of [c] in English, annotated with player's name [pid]. If
  * [c = Command.End_turn_command], then [None] is returned. *)
let string_of_command pid = function
  | Command.Preroll_command sub_c -> string_of_preroll_comm pid sub_c
  | Command.Preroll_injail_command sub_c -> string_of_injail_comm pid sub_c
  | Command.Trade_offer_command sub_c -> string_of_trade_offer pid sub_c
  | Command.Land_unowned_prop_command sub_c -> string_of_unowned_prop pid sub_c
  | Command.Land_other_prop_command sub_c -> string_of_other_prop pid sub_c
  | Command.Transaction_command sub_c -> string_of_transaction pid sub_c
  | Command.End_turn_command -> None

(** [key_assoc_opt k key_bindings] is the leftmost [(desc, act)] mapped to key
  * [k] in [key_bindings]. *)
let rec key_assoc_opt k = function
  | [] -> None
  | {key; desc; act}::t ->
      if key = k then Some (desc, act) else key_assoc_opt k t

class key_binding_functions = object(self)
  val mutable key_bindings = []

  method bind_keys lst =
    key_bindings <- key_bindings @ lst

  method bind_scroller (adj : scrollable) =
    self#bind_keys [
      {key=Special Down; desc="Scroll Down"; act=(fun () ->
        adj#set_offset adj#incr)};
      {key=Special Up; desc="Scroll Up"; act=(fun () ->
        adj#set_offset adj#decr)};
    ]

  method add_action_labels (box : LTerm_widget.box) =
    box#add ~expand:false (new hline);
    box#add ~expand:false (new hline);
    List.iter (fun {key; desc; _} ->
        match key with
        | Ch c ->
            let l = new label ("["^(String.make 1 c)^"]    "^desc) in
            l#set_alignment H_align_left;
            box#add ~expand:false l
        | _ -> ())
      key_bindings

  method add_false_action_labels (box : LTerm_widget.box) lst =
    box#add ~expand:false (new hline);
    box#add ~expand:false (new hline);
    List.iter (fun (key, desc) ->
        let l = new label ("["^key^"]    "^desc) in
        l#set_alignment H_align_left;
        box#add ~expand:false l)
      lst

  (** [handle_key_input e] handles event [e] using the key bindings
    * of this instance. Returns [true] if [e] matches a key binding's [key]
    * in the key bindings, otherwise returns [false]. If [e] matches [key],
    * then the corresponding [act] function associated with [key] is applied. *)
  method private handle_key_input = function
    | LTerm_event.Key {code = LTerm_key.Char c} ->
      begin
        let open CamomileLibrary.UChar in
        match key_assoc_opt (Ch (char_of c)) key_bindings with
        | Some (_, f) -> f (); true
        | None -> false
      end
    | LTerm_event.Key {code = c} ->
      begin
        match key_assoc_opt (Special c) key_bindings with
        | Some (_, f) -> f (); true
        | None -> false
      end
    | _ -> false
end

class game_frame = object(self)
  inherit LTerm_widget.frame
  inherit key_binding_functions
  initializer self#on_event self#handle_key_input
end

class game_modal_frame = object(self)
  inherit LTerm_widget.modal_frame
  inherit key_binding_functions
  initializer self#on_event self#handle_key_input
end

(** [scrollable_lst scroll lst] is a vertically-scrollable widget
  * listing the elements of [lst]. *)
class scrollable_lst (scroll : scrollable) lst = object(self)
  (* Reference: Inspiration from
    https://github.com/lambda-term/examples/scroll.ml. *)
  inherit LTerm_widget.t "scrollable list"
  initializer scroll#set_range (List.length lst)

  (** [draw ctx _focused] draws the elements of [lst]. *)
  method draw ctx _ =
    (* this function tries to draw List.nth (0..end) inclusive,
      which throws a failure as List.nth refers to one past the end
      of the lst. Safeguard the lst by checking what to draw *)
    let {rows; _} = LTerm_draw.size ctx in
    for row = 0 to rows - 1 do
      if (row + scroll#offset) < List.length lst
      then LTerm_draw.draw_string ctx row 0
        (List.nth lst (row + scroll#offset)) else ()
    done;
end

class game_gui push_layer pop_layer exit' = object(self)
  inherit game_frame as super

  val data = {
    push_modals = [];
    event_buffer = [];
    s = None;
    player_controllers = Array.make 8 None;
    player_pieces = [|"  battleship "; "   racecar   ";
                      "   top hat   "; "   thimble   ";
                      " wheelbarrow "; "    horse    ";
                      "     shoe    "; "  money bag  "|];
    turns = 0;
    history = [];
  }

  (** [reset_data ()] resets the event buffer, state, # of turns, player
    * controllers, and history in the instance's data fields. *)
    method private reset_data () =
    data.event_buffer <- [];
    data.s <- None;
    Array.iteri (fun i _ ->
      data.player_controllers.(i) <- None)
      data.player_controllers;
    data.turns <- 0;
    data.history <- []

  (** [init_key_binding] is the key-binding to activate the re-initialize
    * the game. *)
  method private init_key_binding = {
    key=Ch 'H'; desc="Go Home"; act=(fun () ->
      pop_layer ();
      self#reset_data ();
      data.event_buffer <- Init::data.event_buffer)
  }

  method update_state s_opt =
    data.s <- s_opt

  method update_history pid c =
    match string_of_command pid c with
    | None -> ()
    | Some mess -> data.history <- mess::data.history

  method get_events =
    let e = data.event_buffer in data.event_buffer <- []; e

  (** [exit_term ()] disables mouse inputs (whether enabled or not)
    * and exits the terminal. *)
  method private exit_term () =
    Lazy.force LTerm.stdout
    >>= (fun term -> LTerm.disable_mouse term) |> ignore; exit' ()

  method setup =
    let get_json_fname_modal = self#one_line_textbox "Enter the Game's File Name"
      ~enter_callback:(fun str -> data.event_buffer <- JsonFname str::data.event_buffer) in

    let modals_for_pushing = [
      "json_fname", push_layer get_json_fname_modal;
      "init", push_layer (self#create_init_modal ());
      "pselect", push_layer (self#create_pselect_modal);
    ] in
    data.push_modals <- modals_for_pushing;

    self#bind_keys [
      {key=Ch 'I'; desc="Initialize Game"; act=(fun () -> data.event_buffer <- Init::data.event_buffer)};
      {key=Ch 'H'; desc="Help"; act=(fun () -> self#activate_help_modal (fun () ->
      pop_layer ();
      self#reset_data ();
      data.event_buffer <- Init::data.event_buffer) ())};
      {key=Ch 'Q'; desc="Quit"; act=(fun () -> self#exit_term ())};
    ];

  (** [create_init_modal ()] creates the init modal frame that shows up during
    * game initialization. *)
  method private create_init_modal () =
    let init_modal = new game_modal_frame in
    let init_box = new vbox in
    init_modal#set init_box;
    self#add_heading init_box "Home";

    let init_mess = "Welcome to Monopoly!\n\n\
    Throughout the game, you can take actions on pop-ups by pressing\n\
    the corresponding key on your keyboard.\n\n" in
    init_box#add (new label init_mess);

    init_modal#bind_keys [
      {key=Ch 'N'; desc="New Game"; act=(fun () ->
        pop_layer (); self#activate_json_fname_modal ())};
      {key=Ch 'R'; desc="Rules"; act=(fun () ->
        pop_layer (); self#activate_help_modal (fun () ->
        pop_layer ();
        self#reset_data ();
        data.event_buffer <- Init::data.event_buffer) ())};
      {key=Ch 'Q'; desc="Quit"; act=(fun () -> self#exit_term ())};
    ];
    init_modal#add_action_labels init_box;
    init_modal

  (** [create_pselect_modal] creates a view that will prompt the use to
    * assign controllers to the game pieces before the game starts. The
    * options are:
    * 1) Reset : Reset the player-controller bindings
    * 2) Submit : Submit the player-controller bindings
    * 3) Go Home : Return to the home screen *)
  method private create_pselect_modal =
    let vbox = new vbox in
    let frame = new game_modal_frame in
    frame#set vbox;
    self#add_heading vbox "Select a Game Piece and Assign a Controller to it";

    let key_bindings_activate_cselect =
      Array.mapi (fun i pid -> {
          (* i+1 is mapped to cselect i *)
          key=Ch (char_of_int (49 + i))  (* 49 is 1 in ASCII *);
          desc="Assign controller to "^(String.trim pid);
          act=(fun () -> pop_layer ();
            self#activate_cselect_modal i ();)
        }) data.player_pieces
      |> Array.fold_left (fun a x -> x::a) [] |> List.rev in
    frame#bind_keys key_bindings_activate_cselect;
    frame#add_action_labels vbox;
    vbox#add ~expand:false (new hline);

    (** [create_row_elts row_num col_cnt gui_element_type] creates a row
      * of [col_cnt] radiobuttons corresponding to different elements as
      * specified by [gui_element_type]:
      * It returns the created [hbox] which represents the horizontal row.
      * requires:
      *   - [row_num, col_cnt] such that [row_num * col_cnt + i], where
      *       [0 <= i < col_cnt], is a valid index of [radio_grp, ed_frames_grp]. *)
    let create_row_elts row_num col_cnt gui_element_type =
      let hbox = new hbox in
      for col = 0 to (col_cnt - 1) do
        begin
          match gui_element_type with
          | `Piece -> hbox#add (new label
              (data.player_pieces.(row_num * col_cnt + col)))
          | `Text ->
            let controller_to_string = function
              | None -> "   "
              | Some cnt -> State.string_of_controller cnt
            in hbox#add (new label (
              controller_to_string data.player_controllers.(row_num * col_cnt + col)))
        end;
        if col <> (col_cnt - 1) then hbox#add ~expand:false (new vline) else ()
      done; hbox in

    (* create rows of elements *)
    for row = 0 to 1 do
      create_row_elts row 4 `Piece |> vbox#add; (* player labels *)
      create_row_elts row 4 `Text |> vbox#add;  (* controller labels *)
      vbox#add ~expand:false (new hline);
    done;
    vbox#add (new t "glue");  (* glues the vbox together *)

    (* add more key bindings *)
    frame#bind_keys [
      {key=Ch 'S'; desc="Submit"; act=(fun () -> pop_layer ();
        (* make a list out of player_controllers and player_pieces *)
        let p_cnts_lst =
          (* first create an array of players' names and controllers *)
          let p_cnts_arr =
            Array.map2 (fun p cnt_opt ->
              (String.trim p, cnt_opt)) data.player_pieces data.player_controllers in
          (* then create a list out of the array, ignoring controllers that were
            chosen *)
          Array.fold_left (fun a (p, cnt_opt) ->
            match cnt_opt with
            | None -> a
            | Some cnt -> (p, cnt)::a) [] p_cnts_arr |> List.rev in
        data.event_buffer <- PSelect p_cnts_lst::data.event_buffer)};
      {key=Ch 'R'; desc="Reset"; act=(fun () -> pop_layer ();
        for i = 0 to 7 do
          data.player_controllers.(i) <- None
        done;
        push_layer self#create_pselect_modal ())};
      self#init_key_binding;
    ];
    frame#add_false_action_labels vbox [
      "S", "Submit";
      "R", "Reset";
      "H", "Go Home";
    ];
    frame

  (** [create_cselect_modal pnum] creates a view that will prompt the user
    * to assign a controller to player with id [pnum]. The options are :
    * 1) Human Player : Human/GUI
    * 2) AI1 : AI2
    * 3) AI2 : AI2
    * 4) AI3 : AI3
    * 5) None : No controller *)
  method private create_cselect_modal pnum =
    let cselect_modal = new game_modal_frame in
    let vbox = new vbox in
    cselect_modal#set vbox;

    let text =
      (* show pnum + 1 because humans like indices from 1 *)
      "Assign a Controller type to Player " ^ (string_of_int (pnum+1)) in
    self#add_heading vbox text;

    cselect_modal#bind_keys [
      {key=Ch 'h'; desc="Human Player"; act=(fun () -> pop_layer ();
        data.player_controllers.(pnum) <- (Some State.GUI);
        push_layer self#create_pselect_modal ())};
      {key=Ch '1'; desc="AI1"; act=(fun () -> pop_layer ();
        data.player_controllers.(pnum) <- (Some State.AI1);
        push_layer self#create_pselect_modal ())};
      {key=Ch '2'; desc="AI2"; act=(fun () -> pop_layer ();
        data.player_controllers.(pnum) <- (Some State.AI2);
        push_layer self#create_pselect_modal ())};
      {key=Ch '3'; desc="AI3"; act=(fun () -> pop_layer ();
        data.player_controllers.(pnum) <- (Some State.AI3);
        push_layer self#create_pselect_modal ())};
      {key=Ch 'n'; desc="None"; act=(fun () -> pop_layer ();
        data.player_controllers.(pnum) <- None;
        push_layer self#create_pselect_modal ())};
    ];
    cselect_modal#add_action_labels vbox;
    cselect_modal

  (** [create_pdisplay_modal st] creates the modal frame that displays
    * the players' names and controllers that were used to initialize
    * the game. *)
  method private create_pdisplay_modal st =
    let pdisplay_modal = new game_modal_frame in
    let pdisplay_box = new vbox in
    pdisplay_modal#set pdisplay_box;
    self#add_heading pdisplay_box "Players in the Game";

    State.get_all_players st
    |> List.iteri (fun i (pid, cnt) ->
        let p_mess = pid^" with a "^(State.string_of_controller cnt)^" controller." in
        (* humans like indices starting from 1 *)
        let l = new label ((string_of_int (i+1))^": "^p_mess) in
        l#set_alignment H_align_left;
        pdisplay_box#add l);
    pdisplay_box#add (new label "");

    pdisplay_modal#bind_keys [
      {key=Ch 'S'; desc="Start Game"; act=(fun () ->
        (* activate_game_modal requires that a modal layer is present previously,
          let pdisplay be that layer. I.e., don't pop it here. *)
        self#activate_game_modal ();)};
      {key=Ch 'P'; desc="Select Players Again"; act=(fun () -> pop_layer ();
        self#reset_data ();
        data.event_buffer <- PSelectTurnOn::data.event_buffer)};
      self#init_key_binding;
    ];
    pdisplay_modal#add_action_labels pdisplay_box;
    pdisplay_modal

  method activate_game_modal () =
    pop_layer (); (* pop the layer that was previously pushed*)
    match data.s with
    | None -> self#activate_error_modal
                ~disp:"State cannot be accessed" "" ()
    | Some st ->
      begin
        if List.length (State.get_all_players st) = 1 then
          let (pid, cnt) = State.get_all_players st |> List.hd in
          self#activate_game_over_modal pid st ();
        else begin
        data.turns <- data.turns + 1;
        match State.get_current_player st with
        | pid, State.GUI ->
            self#activate_human_modal pid st ();
        | pid, State.AI1 ->
          begin
            let (c, s) = Controllers.AI1.eval st in
            self#activate_ai_modal pid State.AI1 s ();
            data.event_buffer <- Comm c::data.event_buffer;
          end
        | pid, State.AI2 ->
          begin
            let (c, s) = Controllers.AI2.eval st in
            self#activate_ai_modal pid State.AI2 s ();
            data.event_buffer <- Comm c::data.event_buffer;
          end
        | pid, State.AI3 ->
          begin
            let (c, s) = Controllers.AI3.eval st in
            self#activate_ai_modal pid State.AI3 s ();
            data.event_buffer <- Comm c::data.event_buffer;
          end
        end
      end

  (** [activate_human_modal pid st ()] activates the modal frame that waits for
    * user input.
    * requires:
    *   - [List.hd (State.get_all_players st) = (pid, GUI). *)
  method private activate_human_modal pid st () =
    let human_modal = new game_modal_frame in
    let human_box = new vbox in
    human_modal#set human_box;

    let info_box = new hbox in

    info_box#add (self#human_info_left_box pid st);
    info_box#add ~expand:false (new vline);
    info_box#add (self#human_info_right_box human_modal pid st);

    human_box#add info_box;
    human_box#add ~expand:false (new hline);

    let comm_box = new hbox in

    comm_box#add (self#hist_preview_box);
    comm_box#add ~expand:false (new vline);

    (* bind keys here *)
    let bind_keys_simple_turntype lst =
      let keys_to_bind =
        List.map (fun (k, d, comm) ->
          {key=k; desc=d; act=(fun () ->
            data.event_buffer <- Comm comm::data.event_buffer;)}) lst in
      (* add these keys to the view *)
      comm_box#add (self#human_comm_box
        (List.map (fun (k, d, _) -> (k, d)) lst));
      (* associate key bindings *)
      human_modal#bind_keys keys_to_bind in
    let bind_keys_complex_turntype lst =
      let keys_to_bind =
        List.map (fun (k, d, thunk) ->
          {key=k; desc=d; act=thunk}) lst in
      (* add these keys to the view *)
      comm_box#add (self#human_comm_box
        (List.map (fun (k, d, _) -> (k, d)) lst));
      (* associate key bindings *)
      human_modal#bind_keys keys_to_bind in
    begin
      let open Command in
      match State.get_turntype st with
      | State.Preroll ->
        begin
          bind_keys_complex_turntype [
            Ch 'r', "Roll", (fun () ->
              data.event_buffer <- Comm (Preroll_command Roll)::data.event_buffer);
            Ch 's', "Sell Property", (fun () -> pop_layer ();
              self#activate_select_from_list pid st (State.get_player_properties pid st)
              `Prop (fun str ->
                data.event_buffer <- Comm (
                  Preroll_command (SellProp str))::data.event_buffer) None ()
              );
            Ch 'd', "Develop Property", (fun () -> pop_layer ();
              self#activate_select_from_list pid st (State.get_player_properties pid st)
              `Prop (fun str ->
                data.event_buffer <- Comm (
                  Preroll_command (Develop str))::data.event_buffer) None ()
              );
            Ch 't', "Start a Trade", (fun () -> pop_layer ();
              self#activate_make_trade pid st None
                {mon=0; prop_lst=[]} {mon=0; prop_lst=[]} ()
              );
          ]
        end
      | State.Preroll_injail ->
        begin
          let jail_fee = State.(get_jail_fee st |> string_of_int) in
          bind_keys_simple_turntype [
            Ch 'u', "Use Get-Out-of-Jail Card", Preroll_injail_command UseCard;
            Ch 'p', "Pay Fee of $"^jail_fee^" to Get Out of Jail", Preroll_injail_command PayJailFee;
            Ch 'r', "Roll", Preroll_injail_command Roll_injail;
          ]
        end
      | State.Trade_offer ->
        begin
          bind_keys_complex_turntype [
            Ch 'v', "View Trade Details", (fun () -> pop_layer ();
              self#activate_view_trade_details pid st (););
            Ch 'a', "Accept Trade", (fun () ->
              data.event_buffer <- Comm (Trade_offer_command
                AcceptTrade)::data.event_buffer);
            Ch 'd', "Decline Trade", (fun () ->
              data.event_buffer <- Comm (Trade_offer_command
                DeclineTrade)::data.event_buffer);
          ]
        end
      | State.Land_unowned_prop ->
        begin
          let curr_location = State.(get_player_location pid st
            |> string_of_board_space) in
          let location_cost = State.(get_property_cost curr_location st
            |> string_of_int) in
          bind_keys_simple_turntype [
            Ch 'b', "Buy This Property for $"^location_cost, Land_unowned_prop_command Buy;
            Ch 'd', "Don't Buy This Property", Land_unowned_prop_command NotBuy;
          ]
        end
      | State.Land_your_prop -> bind_keys_simple_turntype [
            Ch 'n', "Next Turn", End_turn_command;
          ]
      | State.Land_other_prop ->
        begin
          let curr_location = State.(get_player_location pid st
            |> string_of_board_space) in
          let prop_rent = State.(get_property_rent curr_location st
            |> string_of_int) in
          bind_keys_complex_turntype [
            Ch 'p', "Pay Rent of $"^prop_rent, (fun () ->
              data.event_buffer <- Comm (Land_other_prop_command PayRent)::data.event_buffer);
            Ch 's', "Sell Property to Pay", (fun () -> pop_layer ();
              self#activate_select_from_list pid st (State.get_player_properties pid st)
                `Prop (fun str ->
                    data.event_buffer <- Comm (
                        Land_other_prop_command (Sell_PayRent str))::data.event_buffer) None ()
              );
            Ch 'b', "Go Bankrupt", (fun () ->
              data.event_buffer <- Comm (Land_other_prop_command BankruptRent)::data.event_buffer);
          ]
        end
      | State.Land_other_injail_prop -> bind_keys_simple_turntype [
            Ch 'n', "Next Turn", End_turn_command;
          ]
      | State.Transaction i ->
        begin
          if i <= 0 then bind_keys_simple_turntype [
            (* receiving money *)
            Ch 'n', "Next Turn", End_turn_command;
          ]
          else bind_keys_complex_turntype [
            (* has to pay money *)
            Ch 'p', "Pay Transaction", (fun () ->
              data.event_buffer <- Comm (Transaction_command PayTransaction)::data.event_buffer);
            Ch 's', "Sell Property to Pay", (fun () -> pop_layer ();
              self#activate_select_from_list pid st (State.get_player_properties pid st)
                `Prop (fun str ->
                    data.event_buffer <- Comm (
                        Transaction_command (Sell_PayTransaction str))::data.event_buffer) None ()
              );
            Ch 'b', "Go Bankrupt", (fun () ->
              data.event_buffer <- Comm (Transaction_command BankruptTransaction)::data.event_buffer);
          ]
        end
      | State.Land_injail -> bind_keys_simple_turntype [
          Ch 'n', "Next Turn", End_turn_command;
        ]
      | State.Land_jail_visiting -> bind_keys_simple_turntype [
          Ch 'n', "Next Turn", End_turn_command;
        ]
      | State.Draw_GetOutOfJail_Card -> bind_keys_simple_turntype [
          Ch 'n', "Next Turn", End_turn_command;
        ]
    end;
    (* end bind keys *)

    human_box#add comm_box;
    human_modal#bind_keys [
      self#init_key_binding;
      {key=Ch 'R'; desc="Rules"; act=(fun () -> pop_layer ();
        self#activate_help_modal (fun () -> pop_layer ();
        self#activate_human_modal pid st ()) ()
        )}
      ];
    human_modal#add_false_action_labels human_box [
      "R", "Rules";
      "H", "Go Home";
    ];
    push_layer human_modal ();

  (** [human_info_left_box pid st] is the top-left box of the modal frame
    * for human players. This modal displays information such as the current
    * player, their cash, and the number of properties they own. *)
  method private human_info_left_box pid st =
    let info_left_box = new vbox in
    info_left_box#add ~expand:false (new label ("Howdy "^pid^"!\n"));
    let p_money_in_hand = State.get_player_money pid st in
    let p_num_prop = State.get_player_properties pid st |> List.length in
    let p_location =
      if State.get_player_in_jail pid st
      then "In Jail."
      else ("At "^(State.get_player_location pid st
        |> State.string_of_board_space
        |> String.capitalize_ascii)^".") in
    let p_info =
      ("Where: "^p_location^"\n"
       ^"Money-in-hand: $"^(string_of_int p_money_in_hand)^".\n"
       ^"# Properties Owned: "^(string_of_int p_num_prop)^".") in
    info_left_box#add ~expand:false (new label p_info);
    info_left_box

  (** [human_info_left_box pid st] is the top-right box of the modal frame
    * for human players. This modal displays view options such as viewing
    * properties, the board, the message log, etc. *)
  method private human_info_right_box parent_modal pid st =
    let info_right_box = new vbox in
    (match State.get_message st with
    | None -> ()
    | Some curr_mess ->
        info_right_box#add (new label ("Message: "^curr_mess));
        info_right_box#add ~expand:false (new hline)
    );

    parent_modal#bind_keys [
      {key=Ch '1'; desc=""; act=(fun () -> pop_layer ();
        self#activate_select_from_list pid st (State.get_player_properties pid st)
        `Prop (fun str -> pop_layer ();
          self#activate_prop_details str pid st `Player) None ()
        )};
      {key=Ch '2'; desc=""; act=(fun () -> pop_layer ();
        let (ids, cnts) = List.split (State.get_all_players st) in
        self#activate_select_from_list pid st ids
        `Player (fun str -> pop_layer ();
          self#activate_player_details str st) None ()
        )};
      {key=Ch '3'; desc=""; act=(fun () -> pop_layer ();
        let props = State.get_all_properties st in
        self#activate_select_from_list pid st props
        `Player (fun str -> pop_layer ();
          self#activate_prop_details str pid st `All) None ()
        )};
      {key=Ch '4'; desc=""; act=(fun () -> pop_layer ();
        self#activate_board_info st pid ()
        )};
      {key=Ch '5'; desc=""; act=(fun () -> pop_layer ();
        self#activate_game_hist_log st pid ()
        )};
    ];
    parent_modal#add_false_action_labels info_right_box [
      "1", "View My Property Info";
      "2", "View All Player Info";
      "3", "View All Property Info";
      "4", "View Board";
      "5", "View Entire History"
    ];
    info_right_box

  (** [hist_preview_box] is a vbox that displays at most the 5 recent
    * gameplay messages from our log. *)
  method private hist_preview_box =
    let hist_preview_box = new vbox in
    self#add_heading hist_preview_box "Recent History";

    let history_len =
      if List.length data.history >= 5 then 5
      else List.length data.history in

    if history_len = 0 then begin
      hist_preview_box#add ~expand:false (new label "None")
    end
    else begin
      for i = 0 to history_len - 1 do
        let l = new label ("- "^List.nth data.history i) in
        l#set_alignment H_align_left;
        hist_preview_box#add ~expand:false l
      done
    end;
    hist_preview_box

  (** [human_comm_box key_binds] is a box displaying the available commands
    * to the current player based on the type of turn they are in and uses
    * [key_binds] to bind keys to those views. *)
  method private human_comm_box key_binds =
    let main_box = new vbox in

    self#add_heading main_box "Moves";
    for i = 0 to List.length key_binds - 1 do
      match List.nth key_binds i with
      | Special _, _ -> ()
      | Ch c, desc ->
      begin
        let disp = "["^(String.make 1 c)^"]    "^desc in
        let l = new label disp in
        l#set_alignment H_align_left;
        main_box#add l;
      end
    done;
    main_box

  (** [activate_board_info st pid ()] displays a modal that lists
    * all properties on the board as well as the locations of
    * the players on the board. The options are :
    * 1) Back : return to the human_modal *)
  method private activate_board_info st pid () =
    let spaces_modal = new game_frame in
    let list_box = new vbox in
    spaces_modal#set list_box;

    let lst = State.get_all_board_spaces st in
    let (box, adj) = self#scrollable_box `BoardSpaces lst st in
    list_box#add ~expand:false (new hline);
    list_box#add box;
    spaces_modal#bind_scroller adj;
    spaces_modal#bind_keys [
      {key=Ch 'b'; desc="Back"; act=(fun () -> pop_layer (); 
        self#activate_human_modal pid st ())}
    ];
    spaces_modal#add_action_labels list_box;
    push_layer spaces_modal ()

  (** [activate_prop_details prop_id pid st player] displays a modal
    * with the information associated with [prop_id] from [st]. This
    * includes the owner, property group, cost, current rent,
    * development cost, and development stage. The options are :
    * 1) Back : Returns to human_modal *)
  method private activate_prop_details prop_id pid st player =
    let prop_detail = new game_modal_frame in
    let details = new vbox in

    let owner =
      match State.get_property_owner prop_id st with
      | State.Player s -> s
      | State.Bank -> "Bank" in
    let propgroup = State.get_property_group prop_id st
                    |> State.string_of_property_group in
    let cost = State.get_property_cost prop_id st
              |> string_of_int in
    let rent = State.get_property_rent prop_id st
                |> string_of_int in
    let development_cost =
      match State.get_property_dev_cost prop_id st with
      | None -> "Fully Developed"
      | Some i -> string_of_int i in
    let development_stage = State.get_property_dev_stage prop_id st
                            |> string_of_int in

    details#add (new label ("Owner: " ^ owner));
    details#add (new label ("Property Group: " ^ propgroup));
    details#add (new label ("Cost: " ^ cost));
    details#add (new label ("Current Rent: " ^ rent));
    details#add (new label ("Development Cost: " ^ development_cost));
    details#add (new label ("Development Stage: " ^ development_stage));

    prop_detail#set details;

    prop_detail#bind_keys [
      {key=Ch 'b'; desc="Back"; act=(fun () -> pop_layer ();
        match player with
        | `Player ->
          self#activate_select_from_list pid st (State.get_player_properties pid st)
          `Prop (fun str -> pop_layer ();
          self#activate_prop_details str pid st `Player) None ()
        | `All ->
          self#activate_select_from_list pid st (State.get_all_properties st)
          `Prop (fun str -> pop_layer ();
          self#activate_prop_details str pid st `All) None ()
        )};
    ];

    prop_detail#add_action_labels details;
    push_layer prop_detail ()

  (** [activate_player_details pid st] displays a modal
    * with the information associated with [pid] from [st]. This
    * includes the piece, location, cash holdings, jail status,
    * card count, and properties. The options are :
    * 1) Back : Returns to human_modal *)
  method private activate_player_details pid st =
    let player_detail = new game_modal_frame in
    let detail = new vbox in

    let in_jail = State.get_player_in_jail pid st in
    let location = State.get_player_location pid st
                    |> State.string_of_board_space in
    let money = State.get_player_money pid st |> string_of_int in
    let cards = State.get_player_cards pid st |> string_of_int in
    let properties = State.get_player_properties pid st in

    detail#add (new label ("Piece: " ^ pid));
    detail#add (new label ("Location: " ^ location));
    detail#add (new label ("Money: " ^ money));
    detail#add (new label ("In Jail: " ^ (string_of_bool in_jail)));
    detail#add (new label ("Cards: " ^ cards));
    detail#add (new label ("Properties: "));
    let display_props lst =
      match lst with
      | [] -> ()
      | h::t -> detail#add (new label h); in
    display_props properties;

    player_detail#bind_keys [
      {key=Ch 'b'; desc="Go Back"; act=(fun () ->
        pop_layer ();
        let (ids, cnts) = List.split (State.get_all_players st) in
        self#activate_select_from_list pid st ids
        `Player (fun str ->
        pop_layer ();
        self#activate_player_details str st) None ()
        )};
    ];

    player_detail#set detail;
    player_detail#add_action_labels detail;
    push_layer player_detail ()

  (** [activate_game_hist_log st pid ()] displays the entire list of
    * gameplay messages from our log. *)
  method private activate_game_hist_log st pid () =
    let hist_view = new game_frame in
    let list_box = new vbox in
    hist_view#set list_box;
    self#add_heading list_box "Game History";

    let (box, adj) = self#scrollable_box `Message data.history st in
    list_box#add ~expand:false (new hline);
    list_box#add box; (* scrollable box must expand *)
    hist_view#bind_scroller adj;

    hist_view#bind_keys [
      {key=Ch 'b'; desc="Go Back"; act=(fun () -> pop_layer ();
        self#activate_human_modal pid st ())}
    ];

    hist_view#add_action_labels list_box;
    push_layer hist_view ()

  (** [activate_ai_modal pid ai_cnt ()] activates the modal frame that
    * shows the game players that the player with name [pid] and controller
    * [ai_cnt] is playing. The AI modal is activated for at least 3 seconds.
    * requires:
    *   - data.s <> None
    *   - [List.hd (State.get_all_players (extract data.s)) = (pid, ai_cnt). *)
  method private activate_ai_modal pid cnt disp () =
    let ai_modal = new game_modal_frame in
    let ai_box = new vbox in
    ai_modal#set ai_box;

    ai_box#add (new label ("TURN "^(string_of_int data.turns)));
    ai_box#add (new label disp);

    ai_modal#bind_keys [self#init_key_binding;];
    ai_modal#add_action_labels ai_box;
    push_layer ai_modal ();
    (* sleep for 3 sec to allow humans to see what the ai's are doing *)
    Unix.sleepf 3.0

  (** [activate_make_trade pid st pselected offer ret_offer ()] activates the 
    * modal frame when the player with controller GUI requests to make a trade.
    * The modal frame displays the offer details, and can update their offer
    * and return offer. The return offer section is enabled when 
    * [pselected <> None]. A key binding to finish the trade is only enabled when
    * the [offer] and [ret_offer] are non-empty. *)
  method private activate_make_trade pid st pselected 
      ({mon=o_mon; prop_lst=o_props} as offer)
      ({mon=r_mon; prop_lst=r_props} as ret_offer) () =
    let trade_modal = new game_modal_frame in
    let trade_box = new vbox in
    trade_modal#set trade_box;

    self#add_heading trade_box "Make a Trade";
    let l = new label "Steps to make a trade:\n\
      - Select a Player\n\
      - Determine your Offer\n\
      - Determine the Return Offer\n" in
    l#set_alignment H_align_left;
    trade_box#add l;

    let string_of_offer = function
      | `Offer -> "You want to give $"^(string_of_int o_mon)^" in money and "
          ^(if List.length o_props = 0 then "no properties.\n"
            else "these properties: "^String.concat ", " o_props^".\n")
      | `Return -> "You want to receive $"^(string_of_int r_mon)^" in money and "
          ^(if List.length r_props = 0 then "no properties."
            else "these properties: "^String.concat ", " r_props^".\n") in

    let other_players = List.split (State.get_all_players st) |> fst
      |> List.filter (fun s -> s <> pid) in
    let players_not_in_trade =
      match pselected with
      | None -> other_players
      | Some ret_off_p -> List.filter (fun s -> s <> ret_off_p) other_players in

    trade_modal#bind_keys [
      {key=Ch 'p'; desc="Select a Player"; act=(fun () -> pop_layer ();
        self#activate_select_from_list pid st players_not_in_trade
          `Player (fun otherp -> pop_layer ();
            self#activate_make_trade pid st (Some otherp) offer
            {mon=0; prop_lst=[]} ())

            (Some (fun () -> pop_layer ();
              self#activate_make_trade pid st pselected offer ret_offer ())) ())};
      {key=Ch 'o'; desc="Offer to Player"; act=(fun () -> pop_layer ();
        self#select_mon_or_prop pid st "Your Offer" offer (fun off ->
          self#activate_make_trade pid st pselected off ret_offer ()))};
      ];
    (match pselected with
    | None ->
      begin
        trade_box#add (new label
        "\nYou haven't selected who you want to trade with.\n");
        let l_o = new label (string_of_offer `Offer) in
        l_o#set_alignment H_align_left;
        trade_box#add l_o;
      end
    | Some otherp ->
      begin
        trade_box#add (new label
          ("\nYou want to make a trade with "^otherp^".\n"));
        trade_modal#bind_keys [
          {key=Ch 'r'; desc="Get in Return"; act=(fun () -> pop_layer ();
            self#select_mon_or_prop otherp st "Return Offer" ret_offer (fun ret_off ->
              self#activate_make_trade pid st pselected offer ret_off ()))};
        ];
        let l_o = new label (string_of_offer `Offer) in
        l_o#set_alignment H_align_left;
        trade_box#add l_o;
        let l_r = new label (string_of_offer `Return) in
        l_r#set_alignment H_align_left;
        trade_box#add l_r;

        if o_mon <> 0 || List.length o_props <> 0
          || r_mon <> 0 || List.length r_props <> 0
        then trade_modal#bind_keys [
          {key=Ch 'm'; desc="Make Trade"; act=(fun () ->
            let give = [Command.Mon o_mon]
              @ (List.map (fun s -> Command.Prop s) o_props) in
            let take = [Command.Mon r_mon]
              @ (List.map (fun s -> Command.Prop s) r_props) in
            let trade_off = Command.Preroll_command
              (Command.Trade (otherp, give, take)) in
            data.event_buffer <- Comm trade_off::data.event_buffer;)};
        ]
        else ();
      end);

    trade_modal#bind_keys [
      {key=Ch 'c'; desc="Cancel Trade"; act=(fun () -> pop_layer ();
        self#activate_human_modal pid st ())};
    ];
    trade_modal#add_action_labels trade_box;
    push_layer trade_modal ()

  (** [activate_view_trade_details pid st ()] shows a modal frame that displays
    * trade offer details for the player [pid]. Should be only activated when 
    * [State.get_turntype = Trade_offer]. *)
  method private activate_view_trade_details pid st () =
    let view_trade_frame = new game_modal_frame in
    let view_trade_box = new vbox in
    view_trade_frame#set view_trade_box;

    self#add_heading view_trade_box "Trade Details";
    let sum_money = List.fold_left (fun a x -> match x with
      | Command.Mon i -> a + i
      | Command.Prop _ -> a) 0 in

    let collect_props = List.fold_left (fun a x -> match x with
      | Command.Mon _ -> a
      | Command.Prop p -> p::a) [] in

    let (offerer, _, offer, ret_offer) = State.get_trade_info st in
    let offer_money = sum_money offer in
    let offer_props = collect_props offer in
    let return_money = sum_money ret_offer in
    let return_props = collect_props ret_offer in

    let offer_disp = offerer^" is offering you $"
      ^(string_of_int offer_money)^" and "
      ^(if List.length offer_props = 0 then "no properties.\n"
        else String.concat ", " offer_props^".\n") in
    let return_disp = "They are asking for $"
      ^(string_of_int return_money)^" and "
      ^(if List.length offer_props = 0 then "no properties in exchange.\n"
        else "these properties in exchange: "
          ^String.concat ", " return_props^".\n") in

    let l_o = new label offer_disp in
    l_o#set_alignment H_align_left;
    view_trade_box#add l_o;

    let l_r = new label return_disp in
    l_r#set_alignment H_align_left;
    view_trade_box#add l_r;

    view_trade_frame#bind_keys [
      {key=Ch 'b'; desc="Go Back"; act=(fun () -> pop_layer ();
        self#activate_human_modal pid st ())};
    ];
    view_trade_frame#add_action_labels view_trade_box;
    push_layer view_trade_frame ()

  (** [activate_select_money mon callback] pushes a modal that prompts the
    * user to indicate how much money they'd like to offer or receive
    * in a trade. The options are :
    * 1) Incr : Increments the amount of money
    * 2) Decr : Decrements the amount of money
    * 3) Submit : calls [callback] *)
  method private activate_select_money mon callback =
    let money_modal = new game_modal_frame in
    let money_box = new vbox in
    money_modal#set money_box;

    self#add_heading money_box "Add Money";
    let money_label = new label (string_of_int mon) in
    let pm = new label "+    -" in

    money_box#add money_label;
    money_box#add ~expand:false (new hline);
    money_box#add pm;

    money_modal#bind_keys [
      {key=Ch '+'; desc="Incr"; act=(fun () ->
        let curr_val = int_of_string money_label#text in
        let updated_val = if curr_val < max_int - 25 then curr_val + 25
          else max_int in
        money_label#set_text (string_of_int updated_val))};
      {key=Ch '-'; desc="Decr"; act=(fun () ->
        let curr_val = int_of_string money_label#text in
        let updated_val = max (curr_val - 25) 0 in(*  if curr_val > min_int + 25 then curr_val - 25
          else min_int in *)
        money_label#set_text (string_of_int updated_val))};
      {key=Ch 's'; desc="Submit"; act=(fun () ->
        callback (int_of_string money_label#text))};
    ];

    money_modal#add_false_action_labels money_box ["s", "Submit"];
    push_layer money_modal ();

  (** [select_mon_or_prop pid st heading off f_on_submit callback] displays
    * a modal that prompts the user to select what they'd like to add
    * to a offer or return offer. The options are :
    * 1) Add Money : calls [activate_select_money]
    * 2) Add Property : calls [activate_select_from_list]
    * 3) Submit : calls [callback] *)
  method private select_mon_or_prop pid st heading ({mon; prop_lst} as off) callback : unit =
    let mon_or_prop_frame = new game_modal_frame in
    let mon_or_prop_box = new vbox in
    mon_or_prop_frame#set mon_or_prop_box;

    self#add_heading mon_or_prop_box heading;
    let mon_label = new label ("Money: $"^(string_of_int mon)) in
    let prop_label = new label (
      if List.length prop_lst = 0 then "No Properties.\n"
      else "Properties: "^String.concat ", " prop_lst^".\n"
    ) in

    mon_or_prop_box#add mon_label;
    mon_or_prop_box#add prop_label;

    mon_or_prop_frame#bind_keys [
      {key=Ch 'm'; desc="Add Money"; act=(fun () -> pop_layer ();
        self#activate_select_money mon (fun money -> pop_layer ();
          self#select_mon_or_prop pid st heading {off with mon=money} callback ))};
      {key=Ch 'p'; desc="Add Property"; act=(fun () -> pop_layer ();
        self#activate_select_from_list pid st (State.get_player_properties pid st)
          `Prop (fun str -> pop_layer ();
            self#select_mon_or_prop pid st
            heading {off with prop_lst=str::prop_lst} callback)

            (Some (fun () -> pop_layer ();
              self#select_mon_or_prop pid st heading off callback)) ())};
      {key=Ch 's'; desc="Submit"; act=(fun () -> pop_layer ();
        callback off)};
    ];
    mon_or_prop_frame#add_action_labels mon_or_prop_box;
    push_layer mon_or_prop_frame ()

  method activate_json_fname_modal () =
    List.assoc "json_fname" data.push_modals ()

  method activate_pselect_modal () =
    List.assoc "pselect" data.push_modals ()

  method activate_init_modal () =
    List.assoc "init" data.push_modals ()

  (** [activate_cselect_modal num] activates the modal frame that shows the
    * controller select options for player number [num]. *)
  method private activate_cselect_modal num () =
    push_layer (self#create_cselect_modal num) ()

  method activate_error_modal thrown_err_msg ~disp () =
    let error_modal = new game_modal_frame in
    let error_box = new vbox in
    error_modal#set error_box;

    self#reset_data ();

    self#add_heading error_box "Error";
    error_box#add (new label disp);
    if String.length thrown_err_msg <> 0
    then (error_box#add ~expand:false (new hline);
        error_box#add (new label ("This is the error message from the system: "^thrown_err_msg);))
    else ();

    error_modal#bind_keys [
      self#init_key_binding;
    ];
    error_modal#add_action_labels error_box;
    push_layer error_modal ()

  method activate_pdisplay_modal () =
    match data.s with
    | None -> self#activate_error_modal
                ~disp:"State cannot be accessed" "" ()
    | Some st -> push_layer (self#create_pdisplay_modal st) ()

  (** [activate_select_from_list pid st lst elt_type enter_callback
    * escape_callback ()] displays a modular modal that will display
    * the elements of [lst] and prompt the user with the ability to
    * select an element from [lst]. The options are:
    * 1) Enter : calls [enter_callback]
    * 2) Escape : calls [escape_callback] *)
  method private activate_select_from_list pid st lst elt_type 
      enter_callback escape_callback () : unit =
    let select_modal = new game_frame in
    let list_box = new vbox in
    select_modal#set list_box;

    let f = new LTerm_widget.frame in
    let editor = new text_input in

    let disp = "Select an element from the requested list by entering it here." in
    let mess = new label disp in
    editor#bind
      [{control = false; meta = false; shift = false; code = Enter}]
      [LTerm_edit.Custom (fun () ->
        let input = String.lowercase_ascii editor#text in
        if List.mem input (List.map String.lowercase_ascii lst) then (enter_callback input)
        else mess#set_text "ERROR : The entered element is not in the list.")];
    editor#bind
      [{control = false; meta = false; shift = false; code = Escape}]
      [LTerm_edit.Custom (match escape_callback with
        | None -> fun () -> pop_layer (); self#activate_human_modal pid st ()
        | Some f -> f)];

    f#set editor;
    list_box#add ~expand:false mess;
    list_box#add ~expand:false f;

    list_box#add ~expand:false (new hline);
    if List.length lst = 0
    then list_box#add (new label ("There are no available elements to select from."))
    else begin
      list_box#add ~expand:false (new label ("Here are the available elements to select from: "));
      let (box, adj) = self#scrollable_box elt_type lst st in
      list_box#add ~expand:false (new hline);
      list_box#add box; (* scrollable box must expand *)
      select_modal#bind_scroller adj;
    end;

    select_modal#add_false_action_labels list_box [
      "enter", "Submit";
      "escape", "Go Back"
    ];
    push_layer select_modal ();

  (** [create_help_modal ()] creates the modal frame for help message. *)
  method private activate_help_modal callback () =
    let help_modal = new game_frame in
    let help_box = new vbox in
    help_modal#set help_box;
    self#add_heading help_box "Rules";

    let help_message = "
THE BOARD

The file used specifies the board spaces and their order; the cost to buy and
develop properties as well as their rent at each development stage; the decks
and cards in each deck; and the amount of money each player starts with, gets
for completing a loop around the board, and the fee required to leave jail.

OBJECTIVE IN THE GAME

Buy and develop properties in order to collect money from other players. You win
when all other players have gone bankrupt.

ON YOUR TURN

Before you roll to move, you may complete the following actions as many times
as you would like on your turn:
  Sell a property: If selling a developed property, the development stage will
      decrease by 1 and you will receive half of the cost paid to develop to
      that stage. If selling an undeveloped property, the property will be
      given to the bank and you will receive half of the cost to buy that
      property.
  Develop a property: You may develop properties of Color groups when you own
      all the properties in that group. Properties of the Railroad group
      have a development stage based on the number of properties in the group
      that you own, and cannot be developed by paying money. You must have
      enough money to pay for the development of the property you select.
  Make a trade offer: You may offer an amount of money and a set of properties
      to another player, and request an amount of money and set of properties
      from another player. The request will then be set to the other player to
      accept or declin
Once you decide to roll, the rest of your turn depends on what kind of space
you land on:
  Property space:
    - If the property is owned by the Bank, you can decide to buy the property
      for the displayed cost (you must have enough money to pay the cost) or to
      not buy the propert
    - If the property is owned by another playe
      - If that other player is in jail, you do not owe any ren
      - Otherwise, you must pay the rent. You may sell properties in order to
        have enough money to pay rent. If after selling properties you still do
        not have enough money to pay rent, you must go bankrupt (lose the game).
    - If you own the property, you stay without paying rent.
  Transaction space:
    You may be required to pay money to the bank, or you may receive money from
    the bank. You may sell properties in order to have enough money to pay the
    bank. If after selling properties you still do not have enough money to pay
    the bank, you must go bankrupt (lose the game).
  Card space:
    Draw a card. The card will be of the following typ
    - The card may instruct you to go to a property space, in which case you
      move and your turn continues the same as if you had landed on that space
      (see Property space above).
    - The card may instruct you to conduct a transaction with the bank.
      (see Transaction space above).
    - The card may be a Get Out Of Jail Free card, which you hold onto to use
      when you are in jail.
  Jail (just visiting):
    If you land on the jail space, you are just visiting (not in jail) and your
    turn ends.
  Go To Jail:
    You are sent to jail (in jail), and your turn ends.

IN JAIL

While you are in jail, you cannot collect rent from players who land on your
properties, you cannot sell or develop properties, and you cannot make trade
offers (although you may be offered trades by other players which you can accept
or reject). On your turn you may do the following actions to attempt to leave jail:
  Use Get Out of Jail Free Card: If you have a get out of jail free card, you
    can use it to leave jail, and you get to start your turn from just visiting
    jail.
  Pay Fee: If you have enough money, you can pay a fee (specified by the game
    which you are playing) to leave jail. You leave jail and your turn ends.
  Roll: If you roll doubles (two of the same number), you leave jail and move
    the number of spaces that you rolled. Your turn proceeds based on which
    space you land on (see above). If you do not roll doubles, you remain
    in jail and your turn ends.

ROLLING DOUBLES

If you roll two of the same number (doubles) and you are not in jail, you get
to take another turn. However, if you roll doubles three times in a row, you
are immediately sent to jail.\n" in
    let help_str_lst = String.split_on_char '\n' help_message in
    (* this state is a dummy state *)
    let test_state = State.(init_state
      (Yojson.Basic.from_file ("boards" ^ Filename.dir_sep ^ "test_board.json")) ["a", GUI] ~random:true) in
    let (scr_box, adj) = self#scrollable_box `Message help_str_lst test_state in
    help_box#add scr_box;

    help_modal#bind_scroller adj;
    help_modal#bind_keys [
      {key=Ch 'b'; desc="Back"; act=callback}];
    help_modal#add_action_labels help_box;
    push_layer help_modal ()

  (** [activate_game_over_modal pid st ()] displays a modal that tells the user
    * who won the game and the most signification information about that player. *)
  method private activate_game_over_modal pid st () =
    let game_over = new game_frame in
    let message_box = new vbox in
    game_over#set message_box;
    self#add_heading message_box (pid^" Wins!");

    let location = State.get_player_location pid st
      |> State.string_of_board_space in
    let money = State.get_player_money pid st |> string_of_int in
    let cards = State.get_player_cards pid st |> string_of_int in
    let properties = State.get_player_properties pid st in

    message_box#add ~expand:false (new label ("Location: " ^ location));
    message_box#add ~expand:false (new label ("Money: " ^ money));
    message_box#add ~expand:false (new label ("# of Get-Out-Of-Jail Cards: "^cards));
    message_box#add ~expand:false (new label ("# of Properties: "
      ^(List.length properties |> string_of_int)));
    message_box#add ~expand:false (new hline);
    message_box#add ~expand:false (new label "These are the owned properties:");

    let (scr_box, adj) = self#scrollable_box `Prop properties st in
    message_box#add scr_box;
    game_over#bind_scroller adj;
    game_over#bind_keys [
      {key=Ch 'Q'; desc="Quit"; act=(fun () -> self#exit_term ())};
      self#init_key_binding;
    ];
    game_over#add_action_labels message_box;
    push_layer game_over ()

  (** [one_line_textbox disp enter_callback] creates a one-line textbox modal
    * frame with [disp] displayed in the textbox and [callback] associated
    * with the user-entered string. Enter key triggers the [callback]
    * function, and Escape key quits the modal and adds [Init] to the event buffer. *)
  method private one_line_textbox disp ~(enter_callback:string -> unit) =
    let input_modal = new game_modal_frame in
    let input_box = new vbox in
    let input_f = new LTerm_widget.frame in
    let editor = new text_input in
    self#add_heading input_box disp;
    editor#bind
      [{control = false; meta = false; shift = false; code = Escape}]
      [LTerm_edit.Custom (fun () -> pop_layer ();
        data.event_buffer <- Init::data.event_buffer)];
    editor#bind
      [{control = false; meta = false; shift = false; code = Enter}]
      [LTerm_edit.Custom (fun () -> pop_layer ();
        enter_callback editor#text)];
    input_f#set editor;
    input_box#add input_f;
    input_modal#set input_box;
    input_modal#add_false_action_labels input_box [
      "escape", "Go Home";
      "enter", "Submit";
    ];
    input_modal

  (** [add_heading b heading] adds [heading] as a "header" to the box [b]. *)
  method private add_heading (b : box) heading : unit =
    b#add ~expand:false (new label (String.capitalize_ascii heading));
    b#add ~expand:false (new hline);
    b#add ~expand:false (new hline)

  (** [scrollable_prop_box player_or_prop_lst lst st] returns the scrollable
    * box [b] and handle of the vertical scroll bar [adj] with [lst] listed in [b].
    * [player_or_prop_lst = `Player] if the elements of [lst] are player names.
    * [player_or_prop_lst = `Prop] if the elements of [lst] are properties; in this
    * case, groups of those properties are prepended in the listed label.
    * ONLY WORKS IF THE BOX IS ADDED TO A FRAME, AND NOT A MODAL FRAME. *)
  method private scrollable_box player_or_prop_lst lst st =
    (** [add_prop_group props] is the list of groups and properties [lst] sorted in
      * ascending order. A group and property are separated by dots such that the
      * length of all elements of [lst] are equal in length. *)
    let add_prop_group props =
      (* [group_and_prop] is the list of properties and their groups
        (group is fst) in [props] *)
      let group_and_prop = List.map (fun prop ->
        (State.get_property_group prop st
        |> State.string_of_property_group), prop) props in
      (* [sorted_group_and_prop] is the elements of [group_and_prop] but sorted in
        ascending order. *)
      let sorted_group_and_prop = List.sort (fun (g1, p1) (g2, p2) ->
        match Pervasives.compare g1 g2 with
        | 0 -> Pervasives.compare p1 p2
        | c -> c) group_and_prop in
      (* [max_len_group_prop] is the max value where each value is
        [String.length gi + String.length pi] if [(gi, pi)] is in
        [sorted_group_and_prop]. *)
      let max_len_group_prop = List.fold_left (fun a (gi, pi) ->
        max a (String.length gi + String.length pi)) 0 sorted_group_and_prop in
      (* at least five dots between gi and pi *)
      List.map (fun (gi, pi) ->
        let rem_len = max_len_group_prop - String.length gi - String.length pi in
        "Property "^pi^String.make (rem_len + 5) ' '^"in group "^gi)
          sorted_group_and_prop in

    let rec concat_strings lst = String.concat ", " lst in
    let rec clean_up_for_display bs_and_pids =
      match bs_and_pids with
      | [] -> []
      | (b,ps)::t ->
      (b ^ "...................." ^ (concat_strings ps))::(clean_up_for_display t) in
    let rec add_player_to_bs board_spaces pid =
      match board_spaces with
      | [] -> []
      | (b,ps)::t ->
        let bs = State.get_player_location pid st
                  |> State.string_of_board_space in
        if bs = b then (b,pid::ps)::t
        else (b,ps)::(add_player_to_bs t pid)
        in
    let rec add_players_to_bs board_spaces pids =
      List.fold_left add_player_to_bs board_spaces pids in
    let disp_lst = begin
      match player_or_prop_lst with
      | `Message -> lst
      | `Player ->
          List.map String.capitalize_ascii lst |> List.sort Pervasives.compare
      | `Prop -> add_prop_group lst
      | `BoardSpaces ->
        let lst = List.map (fun elt -> (elt,[])) lst in
        let (pids, cnts) = State.get_all_players st |> List.split in
        add_players_to_bs lst pids |> clean_up_for_display
    end in

    let adj = new scrollable in
    let scroll = new vscrollbar adj in
    let lst_handle = new scrollable_lst adj disp_lst in

    let box = new hbox in
    box#add ~expand:true lst_handle;
    box#add ~expand:false scroll;

    adj#on_offset_change (fun _ -> scroll#queue_draw);
    (box, adj)
end
