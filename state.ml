open Command
open Yojson.Basic.Util

type controller =
  | GUI
  | AI1
  | AI2
  | AI3

type player_id = string
type property_id = string
type color = string
type deck_id = string

type player_or_bank =
  | Player of player_id
  | Bank

type property_group =
  | Color of color
  | Railroad

(* record defining the characteristics of a property *)
type property_record = {
  owner: player_or_bank;
  propgroup: property_group;
  cost: int;
  rent: int list;
  development_cost: int list; (* must be exactly one element shorter than rent*)
  development_stage: int; (* must be an index of rent*)
}

(* Variant defining the different types of effects cards can have*)
type card_effect =
  | GoToSpace of property_id
  | ExchangeBank of int
  | GetOutOfJail

(* record for a single card *)
type card = {
  description: string;
  effect: card_effect; (*want to have a command variant of some sort*)
}

(* record for a deck of cards *)
type deck = {
  card_index : int;
  cards : card list;
}

type board_space =
  | Property of property_id
  | TransactionSpace of int * string
  | Jail
  | ToJail
  | CardSpace of deck_id

(* record defining the characteristics of a player.*)
type player_record = {
  plocation : int;
  pmoney: int;
  getoutofjailcards: int;
  in_jail: bool;
}

type turntype =
  | Preroll
  | Preroll_injail
  | Trade_offer
  | Land_unowned_prop
  | Land_your_prop
  | Land_other_prop
  | Land_other_injail_prop
  | Transaction of int
  | Land_injail
  | Land_jail_visiting
  | Draw_GetOutOfJail_Card

type state = {
  current_turntype: turntype;
  message: string option;
  current_player: player_id * controller;
  playerlist : (player_id * controller) list;
  turncounter: int;
  (* indicated the place in playerlist for determining the next player *)
  board: board_space list;
  pass_go_amount: int;
  jail_fee_amount: int;
  playerinfo: (player_id * player_record) list;
  propertyinfo: (property_id * property_record) list;
  ownership: (player_or_bank * (property_id list)) list;
  decks: (deck_id * deck) list;
  trade_info: player_id * player_id * Command.mon_prop list* Command.mon_prop list;
  (*(player offering, player offered to, requested items, offered items*)
  trade_accepted: bool option; (* if some then made a trade accepted, if none *)
  doublescount: int;
  random_roll: bool;
}

(* Coverts json description of a board space to appropriate constructor. *)
let space_of_json j =
  match (j |> member "space" |> to_string) with
  | "Property" -> Property (j |> member "id" |> to_string)
  | "Transaction" ->
    begin
      let amount = j |> member "amount" |> to_int in
      let description = j |> member "description" |> to_string in
      TransactionSpace (amount, description)
    end
  | "Jail" -> Jail
  | "ToJail" -> ToJail
  | "CardSpace" -> CardSpace (j |> member "deck" |> to_string)
  | _ -> failwith "Improperly formatted board"

(* [init_playerinfo start_money p] generates inital player tuple
 * ([p],player_rec). The player starts at the beginning of the board out of
 * jail with [start_money], no get out of jail cards.*)
let init_playerinfo start_money p =
  let record =
    {
      plocation = 0;
      pmoney = start_money;
      getoutofjailcards = 0;
      in_jail = false;
    } in
  (p, record)

(* Converts string representations of property groups to appropriate constructor.*)
let string_to_property_group = function
  | "Railroad" -> Railroad
  | s -> Color (s)

(* Converts json description of property into (property_id, property_record).
 * All properties are initialized to development_stage 0.*)
let property_info_of_json j =
  let propid = j |> member "id" |> to_string in
  let proprecord = {
    owner = Bank;
    propgroup = j |> member "group" |> to_string |> string_to_property_group;
    cost = j |> member "cost" |> to_int;
    rent = j |> member "rent" |> to_list |> List.map to_int;
    development_cost = j |> member "dev_cost" |> to_list |> List.map to_int;
    development_stage = 0;
  } in
  (propid,proprecord)

(* Converts json description of a card into a card record.*)
let card_of_json j =
  {
    description = j |> member "description" |> to_string;
    effect =
      match j |> member "effect" |> to_string with
      | "GoToSpace" -> GoToSpace (j |> member "id" |> to_string)
      | "ExchangeBank" -> ExchangeBank (j |> member "amount" |> to_int)
      | "GetOutOfJail" -> GetOutOfJail
      | _ -> failwith "Improperly formatted board"
  }

(* Converts json description of a deck into (deck_id, deck_record). *)
let deck_of_json j =
  let deckid = j |> member "id" |> to_string in
  let cardlst = j |> member "cards" |> to_list |> List.map card_of_json in
  (deckid, {card_index = 0; cards=cardlst})

let init_state j plst ~(random:bool) =
  let start_money = j |> member "start_money" |> to_int in
  let gameboard = j |> member "board" |> to_list |> List.map space_of_json in
  let gamepropinfo =
    j |> member "properties" |> to_list |> List.map property_info_of_json in
  {
    current_turntype = Preroll;
    message = None;
    current_player = List.hd plst;
    playerlist = plst;
    turncounter = 0;
    board = gameboard;
    pass_go_amount = j |> member "pass_go_amount" |> to_int;
    jail_fee_amount = j |> member "jail_fee_amount" |> to_int;
    playerinfo = plst |> List.map fst |> List.map (init_playerinfo start_money);
    propertyinfo = gamepropinfo;
    ownership =
      (let allpropids = gamepropinfo |> List.map fst in
       [(Bank,allpropids)]);
    decks = j |> member "decks" |> to_list |> List.map deck_of_json;
    trade_info = (let pid = List.hd plst |> fst in (pid, pid, [], []));
    trade_accepted = None;
    doublescount = 0;
    random_roll = random;
  }

let string_of_board_space b =
  match b with
  | Property p -> p
  | TransactionSpace (_,s) -> s
  | Jail -> "Jail"
  | ToJail -> "Go To Jail"
  | CardSpace d -> d ^ ": Draw a card"

let get_all_board_spaces s =
  s.board |> List.map string_of_board_space

let get_all_properties s =
  s.propertyinfo |> List.map fst

let get_all_players s =
  s.playerlist

let get_turntype s =
  s.current_turntype

let get_message s =
  s.message

let get_current_player s =
  s.current_player

let string_of_controller c =
  match c with
  | GUI -> "GUI"
  | AI1 -> "AI1"
  | AI2 -> "AI2"
  | AI3 -> "AI3"

(* [catch_nonplayer id s] is the player record corresponding to player [id].
 * raises: Failure if [id] is not the [id] of any of the players in the game*)
let catch_nonplayer id s=
  match List.assoc_opt id s.playerinfo with
  | None -> failwith "Not a player"
  | Some p -> p

let get_player_location id s =
  (catch_nonplayer id s).plocation |> List.nth s.board

let get_player_money id s=
  (catch_nonplayer id s).pmoney

let get_player_cards id s =
  (catch_nonplayer id s).getoutofjailcards

let get_player_in_jail id s =
  (catch_nonplayer id s).in_jail

let get_player_properties id s =
  match List.assoc_opt (Player id) s.ownership with
  | None -> []
  | Some props -> props

(* [catch_nonprop  s] is the property record corresponding to property [p].
 * raises: Failure if [p] is not the [p] of any of the properties in the game*)
let catch_nonprop p s =
  match List.assoc_opt p s.propertyinfo with
  | None -> failwith "Not a property"
  | Some prop -> prop

let get_property_owner p s=
  (catch_nonprop p s).owner

let string_of_property_group g =
  match g with
  | Color c -> c
  | Railroad -> "Railroad"

let get_properties_of_group g s =
  s.propertyinfo |> List.filter (fun p -> ((snd p).propgroup = g))
  |> List.map fst

let get_player_properties_of_group p g s =
  s.propertyinfo |> List.filter (fun prop -> ((snd prop).propgroup = g))
  |> List.filter (fun prop -> ((snd prop).owner) = Player p)|> List.map fst

let get_player_monopoly p g s =
  List.sort compare (get_player_properties_of_group p g s) =
  List.sort compare (get_properties_of_group g s)

let get_property_group p s =
  (catch_nonprop p s).propgroup

let get_property_cost p s =
  (catch_nonprop p s).cost

let get_property_rent p s =
  let prop = catch_nonprop p s in
  List.nth prop.rent prop.development_stage

let get_property_dev_stage p s =
  let prop = catch_nonprop p s in
  prop.development_stage

let get_property_dev_cost p s =
  let prop = catch_nonprop p s in
  match prop.propgroup with
  | Railroad -> None
  | Color _ ->
    let max_dev_stage = List.length prop.rent - 1 in
    if prop.development_stage = max_dev_stage then None
    else Some (List.nth prop.development_cost prop.development_stage)

let get_property_max_dev_stage p s =
  let prop = catch_nonprop p s in
  List.length prop.development_cost

let get_property_max_rent p s =
  let prop = catch_nonprop p s in
  List.nth prop.rent (get_property_max_dev_stage p s)

let get_trade_acceptance s =
  s.trade_accepted

let get_trade_info s =
  s.trade_info

let get_jail_fee s =
  s.jail_fee_amount

(* [player_enough_money player amount s] is whether [player] has enough money
 * to pay [amount] in state [s].*)
let player_enough_money player amount s =
  (catch_nonplayer player s).pmoney >= amount

(* [transfer_money from_p to_p amount s] returns the state where [amount] money
 * has been deducted from player [from_p] and has been added to player [to_p].
 * requires: [from_p] and [to_p] are valid player ids in [s].*)
let transfer_money from_p to_p amount s =
  let from_p_rec = (catch_nonplayer from_p s) in
  let to_p_rec = (catch_nonplayer to_p s) in
  let from_p_rec' = {from_p_rec with pmoney = (from_p_rec.pmoney - amount)} in
  let to_p_rec' = {to_p_rec with pmoney = (to_p_rec.pmoney + amount)} in
  let playerinfo' =
    (from_p, from_p_rec')::(to_p, to_p_rec')::
    (s.playerinfo |> List.remove_assoc from_p |> List.remove_assoc to_p) in
  {s with
   playerinfo = playerinfo'
  }

(* [money_to_bank amount s] returns the state where [amount] has been deducted
 * from the current player in [s].*)
let money_to_bank amount s =
  let player = fst s.current_player in
  let player_rec = (catch_nonplayer player s) in
  let player_rec' = {player_rec with pmoney = (player_rec.pmoney - amount)} in
  let playerinfo' =
    (player, player_rec')::(s.playerinfo |> List.remove_assoc player) in
  {s with playerinfo = playerinfo'}

(* [update_dev_stage proplst d s] returns the state where the development stage
 * of all properties specified by the ids in [proplst] has been set to [d].*)
let rec update_dev_stage proplst d s =
  match proplst with
  | [] -> s
  | prop::t ->
    let prop_record = catch_nonprop prop s in
    let prop_record' = {prop_record with development_stage = d} in
    let propertyinfo' =
      (prop, prop_record')::(s.propertyinfo |> List.remove_assoc prop) in
    {s with propertyinfo = propertyinfo'} |> update_dev_stage t d

(* [update_railroad s rlstlst] returns the state where the properties in
 * [rlstlst] have been assigned development stages according to the length
 * of the sublist that they are a part of.
 * requires: all properties in [rlstlst] are of group Railroad.*)
let rec update_railroad (s:state) (rlstlst: property_id list list) =
  match rlstlst with
  | [] -> s
  | h::t ->
    let s' = update_dev_stage h ((List.length h) - 1) s in
    update_railroad s' t

(* [develop_railroads s] returns the state where all the properties of group
 * Railroad have been assigned development stages according to the number
 * of Railroad properties owned by the player owner, if the property is owned
 * by a player.*)
let develop_railroads s : state =
  s.playerlist |> List.map fst |>
  List.map (fun id -> get_player_properties_of_group id Railroad s)
  |> update_railroad s

(* [assign_new_owner prevown newonw prop_id] returns the state where property
 * [prop_id] has been takend from [prevown] and given to [newown]
 * requires: [prevown] is the owner of [prop_id] in s.*)
let assign_new_owner prevown newown prop_id s=
  let prop_record = catch_nonprop prop_id s in
  let prop_record' = {prop_record with owner = newown} in
  let propertyinfo' =
    (prop_id, prop_record')::(s.propertyinfo |> List.remove_assoc prop_id) in
  let prevown_props' = List.assoc prevown s.ownership |>
                       List.filter (fun a -> a <> prop_id)  in
  let newown_props =
    begin
      match List.assoc_opt newown s.ownership with
      | None -> []
      | Some props -> props
    end in
  let newown_props' = prop_id :: newown_props in
  let ownership' =
    (prevown, prevown_props')::(newown, newown_props')::
    (s.ownership |> List.remove_assoc prevown |> List.remove_assoc newown) in
  {s with
   propertyinfo = propertyinfo';
   ownership = ownership';
  } |> develop_railroads

(* [do_end_turn s] returns the state from ending the current turn and starting
 * a new one. If the current player rolled doubles and is not in jail, the
 * current player gets to take another turn. Otherwise, it is the next player
 * in the order's turn, and the turntype, message, current_player, turncounter
 * trade_accepted and doublescount are adjusted accordingly.*)
let do_end_turn s =
  if (s.doublescount = 0 || (get_player_in_jail (fst s.current_player) s = true))
  then
    let turncounter' = (s.turncounter + 1) mod (List.length s.playerlist) in
    let current_player' = List.nth s.playerlist turncounter' in
    let current_turntype' =
      if (get_player_in_jail (fst current_player') s) then Preroll_injail
      else Preroll in
    {s with
     current_turntype = current_turntype';
     message = None;
     current_player = current_player';
     turncounter = turncounter';
     trade_accepted = None;
     doublescount = 0;
    }
  else
    {s with
     current_turntype = Preroll;
     message = Some "You rolled a double, so you get to roll again";
     trade_accepted = None;
    }

(* [do_pay_rent s] returns the state after the current player pays the rent for
 * the property that they landed on.
 * requires: the curreny player is at a property, and that property is owned
 * by a different player who is not in jail.*)
let do_pay_rent s =
  let from_p = fst s.current_player in
  let prop = get_player_location from_p s in
  let rent = get_property_rent (string_of_board_space prop) s in
  if player_enough_money from_p rent s
  then
    begin
      let to_p =
        match get_property_owner (string_of_board_space prop) s with
        | Player p -> p
        | Bank -> failwith "Invalid state"
      in
      transfer_money from_p to_p rent s |> do_end_turn
    end
  else
    {s with message = Some "You do not have enough money to pay rent."}

(* [assign_list_props prevown newown prop_id_lst s] returns the state where all
 * the properties in [prop_id_lst] have been transfered from [prevown]
 * to [newown].
 * requires: [prevown] owns all the properties in [prop_id_lst] in [s].*)
let rec assign_list_props prevown newown prop_id_lst s=
  match prop_id_lst with
  | [] -> s
  | h::t -> assign_new_owner prevown newown h s |>
            assign_list_props prevown newown t

(* [acc_mon_prop mon_prop_lst] is [(mon,props)] where mon is the total amount
 * of money specified in [mon_prop_lst] and [props] is the list of all
 * properties specified in [mon_prop_lst].*)
let rec acc_mon_prop mon_prop_lst (mon,props) =
  match mon_prop_lst with
  | [] -> (mon,props)
  | h::t ->
    match h with
    | Mon i -> acc_mon_prop t (mon + i, props)
    | Prop p -> acc_mon_prop t (mon, p::props)

(* [do_sell prop s] is the state resulting from the current player attempting to
 * sell [prop]. If the player does not own prop, the message is updated to
 * inform them. If the player owns [prop], either the development stage of [prop]
 * is decremented or if the development stage is at 0, ownership of the property
 * is transfered to Bank and the player is given money equal to half the cost
 * of development/buying the property respectively.*)
let do_sell prop s =
  let player = fst s.current_player in
  if (Player player) = get_property_owner prop s
  then
    begin
      let prop_record = catch_nonprop prop s in
      let prop_ds = prop_record.development_stage in
      if prop_ds = 0 || prop_record.propgroup = Railroad
      then
        let prop_value = prop_record.cost / 2 in
        assign_new_owner (Player player) Bank prop s
        |> money_to_bank (- prop_value)
      else
        let prop_value = List.nth prop_record.development_cost (prop_ds - 1) in
        let prop_record' = {prop_record with development_stage = prop_ds - 1} in
        let propertyinfo' =
          (prop, prop_record')::(s.propertyinfo |> List.remove_assoc prop) in
        {s with propertyinfo = propertyinfo'} |> money_to_bank (- prop_value)
    end
  else {s with message = Some "You do not own that property."}

(* [do_develop prop s] is the state resulting from the current player attempting
 * to develop [prop]. If the property is not developable (because it is of group
 * Railroad, because they player doesn't have a monopoly on the Color group,
 * because the property is already fully developed, or because the player does
 * not have enough money), the message is updated accordingly. Otherwise, the
 * development_stage of [prop] is increased by 1 and the cost of development
 * is deducted from the current player's money.*)
let do_develop prop s =
  let player = (fst s.current_player) in
  if (Player player) = get_property_owner prop s
  then
    begin
      let prop_record = catch_nonprop prop s in
      let group = prop_record.propgroup in
      match group with
      | Railroad -> {s with message = Some "You cannot pay to develop a Railroad"}
      | Color c ->
        if get_player_monopoly player group s = false
        then
          {s with message = Some "You do not own all the properties in the group"}
        else
          let prop_ds = prop_record.development_stage in
          if prop_ds = get_property_max_dev_stage prop s
          then {s with message = Some "That property is already fully developed."}
          else
            let cost = match get_property_dev_cost prop s with
              | None -> failwith "Incorrectly formatted property"
              | Some i -> i
            in
            if player_enough_money player cost s
            then
              let prop_record' = {prop_record with development_stage = prop_ds + 1}
              in let propertyinfo' =
                   (prop, prop_record')::(s.propertyinfo |> List.remove_assoc prop)
              in {s with propertyinfo = propertyinfo'} |> money_to_bank cost
            else {s with message =
                           Some "You do not have enough money to develop that property."}
    end
  else {s with message = Some "You do not own that property."}

(* [have_proplst player proplst s] is whether player owns all the properties
 * in [proplst].*)
let rec have_proplst player proplst s =
  match proplst with
  | [] -> true
  | h::t ->
    if (Player player) = get_property_owner h s
    then have_proplst player t s
    else false

(* [do_offer_trade (p_req, v_offer, v_req) s] is the state resulting from
 * the current player attempting to offer a trade where they would give the
 * money and properties in [v_offer] to [p_req] and would recive the money
 * and properties in [v_req] from [p_req]. If the trade is not possible because
 * either player would not be able to fulfill the trade, the message is updated
 * to inform the player. Otherwise, the turntype is updated to Trade_offer, the
 * trade offer is recorded, and the current_player is updated the [p_req].*)
let do_offer_trade (p_req, v_offer, v_req) s =
  let p_offer = (fst s.current_player) in
  let (mon_offer, props_offer) = acc_mon_prop v_offer (0,[]) in
  let (mon_req, props_req) = acc_mon_prop v_req (0,[]) in
  let p_offer_rec = catch_nonplayer p_offer s in
  let p_req_rec = catch_nonplayer p_req s in
  if mon_offer > p_offer_rec.pmoney
  then {s with message =
                 Some "You do not have enough money for this trade."}
  else if mon_req > p_req_rec.pmoney
  then {s with message =
                 Some "The other player does not have enough money for this trade."}
  else if (have_proplst p_offer props_offer s) = false
  then {s with message =
                 Some "You do not have the properties required for this trade."}
  else if (have_proplst p_req props_req s) = false
  then {s with message =
                 Some "The other player does not have the properties required for this trade."}
  else
    {s with
     current_turntype = Trade_offer;
     current_player = (p_req, List.assoc p_req s.playerlist);
     trade_info = ((fst s.current_player), p_req, v_offer, v_req);
     message = Some "You have been offered a trade.";
    }

(* [out_of_jail s] returns the state in which the current player is out of jail.*)
let out_of_jail s =
  let player = (fst s.current_player) in
  let p_rec = catch_nonplayer player s in
  let p_rec' = {p_rec with in_jail = false} in
  let playerinfo' = (player, p_rec')::
                    (s.playerinfo |> List.remove_assoc player) in
  {s with
   playerinfo = playerinfo';
   message = Some "You got out of jail."}

(* [do_use_card s] is the state resulting from the current player attempting to
 * use a get out of jail free card. If the player does not have any get out of
 * jail free cards, the message is updated to inform them. Otherwise, the player
 * is updated to not be in jail and their number of get out of jail free cards
 * is decremented by 1.*)
let do_use_card s =
  let player = fst s.current_player in
  let p_rec = catch_nonplayer player s in
  let cards = p_rec.getoutofjailcards in
  if cards = 0 then
    {s with message = Some "You do not have any Get Out of Jail Free cards."}
  else
    let p_rec' = {p_rec with getoutofjailcards = cards - 1} in
    let playerinfo' = (player, p_rec')::
                      (s.playerinfo |> List.remove_assoc player) in
    {s with
     playerinfo = playerinfo';
     current_turntype = Preroll}
    |> out_of_jail

(* [do_pay_jail fee s] is the state resulting from the current player attempting
 * to pay the fee to leave jail. If the player does not have enough money to pay
 * the fee, the message is updated to inform them. Otherwise, the player is
 * updated to not be in jail and their money is decremnted by the jail fee
 * specified in [s].*)
let do_pay_jail_fee s =
  let player = fst s.current_player in
  let amount = s.jail_fee_amount in
  if player_enough_money player amount s
  then
    {s with current_turntype = Preroll} |> money_to_bank amount |> out_of_jail
  else
    {s with message = Some "You do not have enough money to pay the jail fee."}

(* [do_pay_transaction s] is the state resulting from the current player
 * attempting to pay the transaction specified by the turntype if [s]. If the
 * player does not have enough money to pay the transaction, the message is
 * updated to inform them. Otherwise, the money is deducted from the player's
 * money and their turn ends.
 * requires: the turntype of [s] is Transaction i*)
let do_pay_transaction s =
  let player = fst s.current_player in
  let amount =
    match s.current_turntype with
    | Transaction i -> i
    | _ -> failwith "Invalid state" in
  if player_enough_money player amount s
  then money_to_bank amount s |> do_end_turn
  else
    {s with
     message = Some "You do not have enought money to pay the fee."}

(* [do_buy_prop s] is the state resulting from the current player attempting to
 * buy the Bank property where they are located. If the player does not have
 * enough money to pay the cost, the message is updated to inform them. Otherwise,
 * the property is transfered from the Bank to the current player and the cost
 * of the property is deducted from the player's money.
 * requires: the location of the current player is a property owned by Bank.*)
let do_buy_prop s =
  let player = (fst s.current_player) in
  let prop = (get_player_location player s) in
  let cost = get_property_cost (string_of_board_space prop) s
  in if player_enough_money player cost s
  then
    s |> money_to_bank cost
    |> assign_new_owner Bank (Player player) (string_of_board_space prop) |> do_end_turn
  else
    {s with message = Some "You do not have enough money to buy the property."}

(* [bankrupt s] is the state resulting from the current player declaring
 * bankrupcy. The player is removed from the turn order, ad all of their
 * properties are transferred to the bank. Play continues with the next player
 * in the turn order.*)
let bankrupt s =
  let player = fst s.current_player in
  let s' = assign_list_props (Player player) Bank
      (get_player_properties player s) s in
  let playerlist' = List.filter (fun p -> fst p <> player) s.playerlist in
  let turncounter' = (s.turncounter) mod (List.length playerlist') in
  let playerinfo' = List.remove_assoc player s.playerinfo in
  let current_player' = List.nth playerlist' turncounter' in
  let current_turntype' =
    if (get_player_in_jail (fst current_player') s) then Preroll_injail
    else Preroll in
  {s' with
   current_turntype = current_turntype';
   message = None;
   current_player = current_player';
   playerlist = playerlist';
   turncounter = turncounter';
   playerinfo = playerinfo';
   trade_accepted = None;
   doublescount = 0;
  }

(* [do_accept_trade s] is the state resulting from the player accepting the
 * trade offered to them. For [s.trade_info] = (p_offer, p _req, v_offer, v_req)
 * the properties and money in [v_offer] are transfered from [p_offer] to [p_req]
 * and the properties and money in [v_req] are transferred from [p_req] to
 * [p_offer]. The message and trade_accepted are updated to inform the offering
 * player that their offer was accepted. The current player is reverted back to
 * [p_offer].
 * requires: the trade specified in [s.trade_info] is valid.*)
let do_accept_trade s =
  let (p_offer, p_req, v_offer, v_req) = s.trade_info in
  let (mon_offer, props_offer) = acc_mon_prop v_offer (0,[]) in
  let (mon_req, props_req) = acc_mon_prop v_req (0,[]) in
  let s' = s |> transfer_money p_offer p_req mon_offer
           |> transfer_money p_req p_offer mon_req
           |> assign_list_props (Player p_offer) (Player p_req) props_offer
           |> assign_list_props (Player p_req) (Player p_offer) props_req in
  {s' with
   current_turntype = Preroll;
   message = Some "Your trade offer was accepted and the trade was performed.";
   current_player = (p_offer, List.assoc p_offer s.playerlist);
   trade_accepted = Some true}

(* [do_decline_trade s] is the state resulting from the player declining the
 * trade offered to them. The message and trade_accepted are updated to inform
 * the offering player that their offer was declined. The current player is
 * revered back to the offering player.
 * requires: the trade specified in [s.trade_info] is valid.*)
let do_decline_trade s =
  {s with
   current_turntype = Preroll;
   message = Some "Your trade offer was declined";
   current_player =
     begin
       match s.trade_info with
       | (op,_,_,_) -> (op, List.assoc op s.playerlist);
     end;
   trade_accepted = Some false
  }

let roll_die () =
  Random.self_init ();
  (Random.int 6) + 1

(* [loc_visiting_jail board acc] is the index of the first [Jail] space in
 * [board] plus [acc].
 * reuqires: [board] contains at least one [Jail] space.*)
let rec loc_visiting_jail board acc =
  match board with
  | [] -> failwith "Invalid board configuration"
  | h::t ->
    if h = Jail then acc
    else loc_visiting_jail t (acc + 1)

(* [loc_property p board acc] is the index of Property p in [board] plus [acc].
 * requires: [p] is a property listed in [board].*)
let rec loc_property p board acc =
  match board with
  | [] -> failwith "Not a property location"
  | h::t ->
    if h = (Property p) then acc
    else loc_property p t (acc + 1)

(* [land_prop prop s] returns the state resulting from the current player
 * landing on property [prop]. If the Bank owns the property, the turntype
 * is Land_unowned_prop. If the current player owns the property, the turntype
 * is Land_your_prop. If the property is owned by onther player, if that player
 * is in jail, the turntype is Land_other_injail_prop, otherwise it is
 * Land_other_prop.*)
let land_prop prop s =
  begin
    match get_property_owner prop s with
    | Bank -> {s with current_turntype = Land_unowned_prop}
    | Player o ->
      begin
        if o = (fst s.current_player) then {s with current_turntype = Land_your_prop}
        else if get_player_in_jail o s then
          {s with current_turntype = Land_other_injail_prop}
        else {s with current_turntype = Land_other_prop}
      end
  end

(* [do_to_jail s] is the state resulting from moving the current player to the
 * jail space and making them in jail.*)
let do_to_jail s =
  let p = (fst s.current_player) in
  let p_rec = catch_nonplayer p s in
  let jail_loc = loc_visiting_jail s.board 0 in
  let p_rec' = {p_rec with plocation = jail_loc; in_jail = true} in
  let playerinfo' = (p, p_rec')::
                    (s.playerinfo |> List.remove_assoc p) in
  {s with
   playerinfo = playerinfo';
   current_turntype = Land_injail;
   message = Some "Go Directly to Jail."}

(* [move_spaces n s] is the state resulting from the current player moving
 * forward [n] spaces on the board. The state is updated to the corresponding
 * turntype based on the type of space landed on (see the Roll section of the
 * do' spec in the mli for details).
 * requires: [n] is a positive number.*)
let move_spaces n s =
  let p = (fst s.current_player) in
  let p_rec = catch_nonplayer p s in
  let p_loc = p_rec.plocation in
  let p_loc' = (p_loc + n) mod List.length s.board in
  let passed_go = (p_loc + n) / (List.length) s.board in
  let p_rec' = {p_rec with plocation = p_loc'} in
  let playerinfo' = (p, p_rec')::
                    (s.playerinfo |> List.remove_assoc p) in
  let s' =
    {s with playerinfo = playerinfo'}
    |> money_to_bank (-s.pass_go_amount * passed_go) in
  match get_player_location p s' with
  | Property prop ->
    land_prop prop s'
  | TransactionSpace (i,desc) ->
    {s' with
     current_turntype = Transaction (i);
     message = Some desc
    }
  | Jail ->
    {s' with
     current_turntype = Land_jail_visiting;
     message = Some "You are visiting jail."}
  | ToJail -> do_to_jail s'
  | CardSpace d ->
    let deck = List.assoc d s'.decks in
    let i = deck.card_index in
    let card = List.nth deck.cards i in
    let description = card.description in
    let deck' = {deck with card_index = (i + 1) mod (List.length deck.cards)} in
    let s'' = {s' with decks = (d,deck')::(s'.decks |> List.remove_assoc d)} in
    match card.effect with
    | GoToSpace prop ->
      let prop_loc = loc_property prop s''.board 0 in
      let p_rec'' = {p_rec' with plocation = prop_loc} in
      let passed_go = if prop_loc < p_rec'.plocation then 1 else 0 in
      let playerinfo'' = (p, p_rec'')::
                         (s''.playerinfo |> List.remove_assoc p) in
      {s'' with
       playerinfo = playerinfo'';
       message = Some description} |> land_prop prop
      |> money_to_bank (-s'.pass_go_amount * passed_go)
    | ExchangeBank i ->
      {s'' with
       current_turntype = Transaction i;
       message = Some description}
    | GetOutOfJail ->
      let p_rec'' = {p_rec' with getoutofjailcards = p_rec'.getoutofjailcards + 1}
      in let playerinfo' = (p, p_rec'')::
                           (s''.playerinfo |> List.remove_assoc p) in
      {s'' with
       current_turntype = Draw_GetOutOfJail_Card;
       playerinfo = playerinfo';
       message = Some description}

(*[do_roll s] is the state resulting from the current player rolling the dice.
 * if [s] is set to random, the spaces moved are based on a simulated rolling
 * of two six sided dice, while if [s] is set to not random, the current player
 * always moves forward one space.*)
let do_roll s =
  let d1 = if s.random_roll then roll_die () else 1 in
  let d2 = if s.random_roll then roll_die () else 0 in
  let total = (d1 + d2) in
  let doublescount' = if d1 = d2 then s.doublescount + 1 else 0 in
  let s' = {s with doublescount = doublescount'} in
  if doublescount' = 3 then do_to_jail s'
  else move_spaces total s'

(*[do_roll s] is the state resulting from the current player rolling the dice to
 * attempt to leave jail. if [s] is set to random, the spaces moved are based on
 * a simulated rolling of two six sided dice, while if [s] is set to not random,
 * rolling always fails to achieve doubles. If the player rolls doubles, They
 * leave jail and move the number of spaces they rolled. *)
let do_roll_injail s =
  let d1 = if s.random_roll then roll_die () else 1 in
  let d2 = if s.random_roll then roll_die () else 0 in
  if d1 = d2 then
    s |> out_of_jail |> move_spaces (d1 + d2)
  else do_end_turn s

let do' c s =
  match c with
  | Preroll_command cmnd ->
    begin
      match cmnd with
      | Roll -> do_roll s
      | SellProp prop -> do_sell prop s
      | SellAndRoll prop -> do_sell prop s |> do_roll
      | Develop prop -> do_develop prop s
      | DevelopAndRoll prop -> do_develop prop s |> do_roll
      | Trade (p_req,v_offer,v_req) -> do_offer_trade (p_req, v_offer, v_req) s
    end
  | Preroll_injail_command cmnd ->
    begin
      match cmnd with
      | Roll_injail -> do_roll_injail s
      | UseCard -> do_use_card s
      | PayJailFee -> do_pay_jail_fee s
    end
  | Trade_offer_command cmnd ->
    begin
      match cmnd with
      | AcceptTrade -> do_accept_trade s
      | DeclineTrade -> do_decline_trade s
    end
  | Land_unowned_prop_command cmnd ->
    begin
      match cmnd with
      | Buy -> do_buy_prop s
      | NotBuy -> do_end_turn s
    end
  | Land_other_prop_command cmnd ->
    begin
      match cmnd with
      | PayRent -> do_pay_rent s
      | Sell_PayRent prop -> do_sell prop s
      | BankruptRent -> bankrupt s
    end
  | Transaction_command cmnd ->
    begin
      match cmnd with
      | PayTransaction -> do_pay_transaction s
      | Sell_PayTransaction prop -> do_sell prop s
      | BankruptTransaction -> bankrupt s
    end
  | End_turn_command -> do_end_turn s
