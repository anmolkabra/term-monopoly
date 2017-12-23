module type AIControllerFunctions = sig
  val preroll_eval : State.state -> Command.preroll_command * string
  val preroll_injail_eval : State.state -> Command.preroll_injail_command * string
  val trade_offer_eval : State.state -> Command.trade_offer_command * string
  val land_unowned_prop_eval : State.state -> Command.land_unowned_prop_command * string
  val land_other_prop_eval : State.state -> Command.land_other_prop_command * string
  val transaction_eval : State.state -> int -> Command.transaction_command * string
end

module type AIController = sig
  val eval : State.state -> Command.command * string
end


let current_player = ref "id"

let command_roll id =
  Command.Roll,
  ("[" ^ id ^ "]" ^ " just rolled a dice.")

let command_dev id pid =
  Command.Develop pid,
  ("[" ^ id ^ "]" ^ " just developed property [" ^ pid ^ "]")

let command_roll_injail id =
  Command.Roll_injail,
  ("[" ^ id ^ "]" ^ " just rolled a dice in the jail.")

let command_pay_jail_fee id =
  Command.PayJailFee,
  ("[" ^ id ^ "]" ^ " just got out of jail by paying the fee.")

let command_use_card id =
  Command.UseCard,
  ("[" ^ id ^ "]" ^ " just got out of jail by using a card.")

let command_accept_trade id id2 =
  Command.AcceptTrade,
  ("[" ^ id ^ "]" ^ " just accepted the trade offer from [" ^ id2 ^ "].")

let command_decline_trade id id2 =
  Command.DeclineTrade,
  ("[" ^ id ^ "]" ^ " just declined the trade offer from [" ^ id2 ^ "].")

let command_buy id location =
  Command.Buy,
  ("[" ^ id ^ "]" ^ " just bought the property [" ^ location ^ "].")

let command_not_buy id location =
  Command.NotBuy,
  ("[" ^ id ^ "]" ^ " refused to buy the property [" ^ location ^ "].")

let command_pay_rent id location =
  Command.PayRent,
  ("[" ^ id ^ "]" ^ " just paid the rent [" ^ location ^ "].")

let command_sell_pay_rent id s location =
  Command.Sell_PayRent s,
  ("[" ^ id ^ "]" ^ " just paid the rent [" ^ location ^ "] after selling " ^
  "the property [" ^ s ^ "].")

let command_bankrupt_rent id location =
  Command.BankruptRent,
  ("[" ^ id ^ "]" ^ " just called a bankruptcy because it couldn't pay the" ^
  " rent [" ^ location ^ "].")

let command_pay_transaction t id location =
  Command.PayTransaction,
  ("[" ^ id ^ "]" ^ " just " ^ (if t >= 0 then "paid" else "received") ^
  " the transaction [" ^ location ^ "].")

let command_sell_pay_transaction id s location =
  Command.Sell_PayTransaction s,
  ("[" ^ id ^ "]" ^ " just paid the transaction [" ^ location ^ "] after " ^ 
  "selling the property [" ^ s ^ "].")

let command_bankrupt_transaction id location =
  Command.BankruptTransaction,
  ("[" ^ id ^ "]" ^ " just called a bankruptcy because it couldn't pay the" ^
  " transaction [" ^ location ^ "].")    


module AI1Functions : AIControllerFunctions = struct

  let get_my_id st =
    let id, _ = State.get_current_player st in
    current_player := id; id

  let get_random_element lst =
    let ind = Random.int (List.length lst) in
    let rec helper dec = function
    | [] -> failwith "Index Out of Bound"
    | h::t -> if dec = 0 then h else helper (dec-1) t in
    helper ind lst

  let preroll_eval st =

    let id = get_my_id st in
    match State.get_player_properties id st with
    | [] -> command_roll id
    | lst -> let pid = get_random_element lst in
    let pgroup = State.get_property_group pid st in
    if not (State.get_player_monopoly id pgroup st)
    then command_roll id else
    let dev_cost = State.get_property_dev_cost pid st in
    begin
      match dev_cost with
      | None -> command_roll id
      | Some dev -> if (dev <= State.get_player_money id st) &&
      (State.get_property_dev_stage pid st <>
      State.get_property_max_dev_stage pid st) then command_dev id pid
      else command_roll id
    end

  let preroll_injail_eval st =
    let id = get_my_id st in
    let num_cards = State.get_player_cards id st in
    match num_cards with
    | 0 -> if State.get_jail_fee st <= State.get_player_money id st
    then command_pay_jail_fee id else command_roll_injail id
    | _ -> command_use_card id

  let trade_offer_eval st =
    match State.get_trade_info st with
    | _,id2,_,_ ->
    let id = get_my_id st in command_decline_trade id id2

  let land_cost_prop_eval st get_cost return_exe =
    let id = get_my_id st in
    let bt = State.get_player_location id st in
    let location = State.string_of_board_space bt in
    let cost = get_cost location st in
    let budget = State.get_player_money id st in
    let d = if budget >= cost then false else true in
    return_exe id location d
  
  let return_unowned_prop id location = function
  | false -> command_buy id location
  | true -> command_not_buy id location

  let return_other_prop id location = function
  | false -> command_pay_rent id location
  | true -> command_bankrupt_rent id location

  let return_transaction t id location = function
  | false -> command_pay_transaction t id location
  | true -> command_bankrupt_transaction id location

  let land_unowned_prop_eval st =
    land_cost_prop_eval st State.get_property_cost
    return_unowned_prop

  let land_other_prop_eval st =
    land_cost_prop_eval st State.get_property_rent
    return_other_prop

  let transaction_eval st t =
    land_cost_prop_eval st (fun x y -> t)
    (return_transaction t)

end

(* AI2 implementation *)
module AI2Functions : AIControllerFunctions = struct

  let get_my_id st =
    let id, _ = State.get_current_player st in
    current_player := id; id

  let get_random_element lst =
    let ind = Random.int (List.length lst) in
    let rec helper dec = function
    | [] -> failwith "Index Out of Bound"
    | h::t -> if dec = 0 then h else helper (dec-1) t in
    helper ind lst

  let property_estimate id st = st

  let player_ids st =
    let f,s = List.split (State.get_all_players st) in f
  
  let player_properties_assoc st =
    let rec helper ids acc =
      match ids with
      | [] -> acc
      | h::t -> helper t ((h, State.get_player_properties h st)::acc) in
    let plids = player_ids st in
    List.rev (helper plids [])
  
  let opponent_properties id st =
    let rec helper ids acc =
      match ids with
      | [] -> acc
      | h::t -> if h=id then helper t acc
      else helper t (State.get_player_properties h st) @ acc in
    let plids = player_ids st in
    helper plids []

  let expected_gain id st =
    let num_opponents = List.length (State.get_all_players st) - 1 in
    let my_properties = State.get_player_properties id st in
    let rec rent_sum acc = function
    | [] -> acc
    | h::t -> rent_sum (acc + State.get_property_rent h st) t in
    let rent_sum = rent_sum 0 my_properties in
    int_of_float (float_of_int (num_opponents) *.
    float_of_int (rent_sum) /. 7.0)

  let expected_loss id st =
    let op_properties = opponent_properties id st in
    let rec rent_sum acc = function
    | [] -> acc
    | h::t -> rent_sum (acc + State.get_property_rent h st) t in
    let rent_sum = rent_sum 0 op_properties in
    int_of_float (float_of_int (rent_sum) /. 7.0)

  let preroll_eval st =
    let id = get_my_id st in

    match State.get_player_properties id st with
    | [] -> command_roll id
    | lst -> let pid = get_random_element lst in
    let pgroup = State.get_property_group pid st in
    if not (State.get_player_monopoly id pgroup st)
    then command_roll id else
    let dev_cost = State.get_property_dev_cost pid st in
    begin
      match dev_cost with
      | None -> command_roll id
      | Some dev -> if (dev <= State.get_player_money id st) &&
      (State.get_property_dev_stage pid st <>
      State.get_property_max_dev_stage pid st) then command_dev id pid
      else command_roll id
    end

  let willing_to_escape_jail id st =
    let el = expected_loss id st in
    let eg = expected_gain id st in
    eg >= el

  let preroll_injail_eval st =
    let id = get_my_id st in
    if willing_to_escape_jail id st then
    let num_cards = State.get_player_cards id st in
    match num_cards with
    | 0 -> if State.get_jail_fee st <= State.get_player_money id st
    then command_pay_jail_fee id else command_roll_injail id
    | _ -> command_use_card id
    else command_roll_injail id

  let trade_offer_eval st =
    match State.get_trade_info st with
    | _,id2,_,_ ->
    let id = get_my_id st in command_decline_trade id id2

  let land_unowned_prop_eval st =
    let id = get_my_id st in

    let bt = State.get_player_location id st in
    let location = State.string_of_board_space bt in
    let cost = State.get_property_cost location st in
    
    let budget = State.get_player_money id st in

    if budget >= cost then command_buy id location
    else command_not_buy id location

  let rec sellable_property lst st =
    match lst with
    | [] -> None
    | h::t -> Some h
  
  let land_payment_location st get_cost comm_pay comm_bankrupt comm_sell =
    let id = get_my_id st in

    let bt = State.get_player_location id st in
    let location = State.string_of_board_space bt in
    let cost = get_cost location st in
    
    let budget = State.get_player_money id st in

    let my_properties = State.get_player_properties id st in

    if budget >= cost then comm_pay id location
    else match sellable_property my_properties st with
    | None -> comm_bankrupt id location
    | Some s -> comm_sell id s location


  let land_other_prop_eval st =
    land_payment_location st State.get_property_rent
    command_pay_rent command_bankrupt_rent command_sell_pay_rent


  let transaction_eval st t =
    land_payment_location st (fun x y -> t)
    (command_pay_transaction t) command_bankrupt_transaction
    command_sell_pay_transaction

end

(* AI3 implementation *)
module AI3Functions : AIControllerFunctions = struct

  let get_my_id st =
    let id, _ = State.get_current_player st in
    current_player := id; id

  let get_random_element lst =
    let ind = Random.int (List.length lst) in
    let rec helper dec = function
    | [] -> failwith "Index Out of Bound"
    | h::t -> if dec = 0 then h else helper (dec-1) t in
    helper ind lst
    
  let abs f =
    if f > 0.0 then f else (f *. -1.0)

  let player_ids st =
    let f,s = List.split (State.get_all_players st) in f
  
  let player_properties_assoc st =
    let rec helper ids acc =
      match ids with
      | [] -> acc
      | h::t -> helper t ((h, State.get_player_properties h st)::acc) in
    let plids = player_ids st in
    List.rev (helper plids [])
  
  let opponent_properties id st =
    let rec helper ids acc =
      match ids with
      | [] -> acc
      | h::t -> if h=id then helper t acc
      else helper t (State.get_player_properties h st) @ acc in
    let plids = player_ids st in
    helper plids []

  let expected_gain id st =
    let num_opponents = List.length (State.get_all_players st) - 1 in
    let my_properties = State.get_player_properties id st in
    let rec rent_sum acc = function
    | [] -> acc
    | h::t -> rent_sum (acc + State.get_property_rent h st) t in
    let rent_sum = rent_sum 0 my_properties in
    int_of_float (float_of_int (num_opponents) *.
    float_of_int (rent_sum) /. 7.0)

  let expected_loss id st =
    let op_properties = opponent_properties id st in
    let rec rent_sum acc = function
    | [] -> acc
    | h::t -> rent_sum (acc + State.get_property_rent h st) t in
    let rent_sum = rent_sum 0 op_properties in
    int_of_float (float_of_int (rent_sum) /. 7.0)

  let float_division_by_zero f1 f2 =
    if f2 = 0.0 then 0.0 else f1 /. f2

  let remaining_properties_until_monopoly id pid st =
    let pgroup = State.get_property_group pid st in
    if State.get_player_monopoly id pgroup st then 0
    else let s1 = State.get_properties_of_group pgroup st in
    let s2 = State.get_player_properties_of_group id pgroup st in
    List.length s1 - List.length s2

  let property_value id pid st =
    let board_size = List.length (State.get_all_board_spaces st) in
    let p = 7.0 /. (float_of_int board_size) in
    let rem_mon= remaining_properties_until_monopoly id pid st -
    State.get_property_dev_stage pid st in
    let t = 1.0 /. p in
    let turns_until_dev = t ** (float_of_int rem_mon) in
    let max_rent = float_of_int (State.get_property_max_rent pid st) in
    turns_until_dev +. (float_division_by_zero 1.0 max_rent)

  let property_at_max_dev pid st =
    State.get_property_dev_stage pid st =
    State.get_property_max_dev_stage pid st

  let rec property_time id lst st acc =
    match lst with
    | [] -> acc
    | h::t -> let v = property_value id h st in
    property_time id t st ((h, Some v)::acc)
  
  let rec developable_property id lst st (pidp,p) =
    match lst with
    | [] -> (pidp,p)
    | (pidh, h)::t -> begin
      match h,p with
      | None, None -> developable_property id t st (pidh,h)
      | Some v, None ->
      if property_at_max_dev pidh st
      then ("",None) else developable_property id t st (pidh,h)
      | None, Some v ->
      if property_at_max_dev pidp st
      then ("",None) else developable_property id t st (pidp,p)
      | Some vh, Some vp ->
      let hmax = property_at_max_dev pidh st in
      let pmax = property_at_max_dev pidp st in
      begin
        match hmax, pmax with
        | false, false ->
        if vh >= vp
        then developable_property id t st (pidp,p)
        else developable_property id t st (pidh,h)
        | true, false -> developable_property id t st (pidp,p)
        | false, true -> developable_property id t st (pidh,h)
        | true, true -> developable_property id t st ("", None)
      end
    end

  let rec sellable_property lst st (pidp,p) =
    match lst with
    | [] -> (pidp,p)
    | (pidh, h)::t -> begin
      match h,p with
      | _, None -> sellable_property t st (pidh,h)
      | None, Some v -> sellable_property t st (pidp,p)
      | Some vh, Some vp -> if vh <= vp then sellable_property t st (pidp,p)
      else sellable_property t st (pidh,h)
    end

  let preroll_eval st =

    let id = get_my_id st in
    match State.get_player_properties id st with
    | [] -> command_roll id
    | lst -> let my_properties = State.get_player_properties id st in
    let pt = property_time id my_properties st [] in
    let pid, pval = developable_property id pt st ("", None) in
    begin
      match pval with
      | None -> command_roll id
      | Some _ -> let pgroup = State.get_property_group pid st in
      if not (State.get_player_monopoly id pgroup st)
      then command_roll id else
      let dev_cost = State.get_property_dev_cost pid st in
      begin
        match dev_cost with
        | None -> command_roll id
        | Some dev -> if (dev <= State.get_player_money id st) &&
        (State.get_property_dev_stage pid st <>
        State.get_property_max_dev_stage pid st) then command_dev id pid
        else command_roll id
      end
    end

  let willing_to_escape_jail id st =
    let el = expected_loss id st in
    let eg = expected_gain id st in
    eg >= el

  let preroll_injail_eval st =
    let id = get_my_id st in
    if willing_to_escape_jail id st then
    let num_cards = State.get_player_cards id st in
    match num_cards with
    | 0 -> if State.get_jail_fee st <= State.get_player_money id st
    then command_pay_jail_fee id else command_roll_injail id
    | _ -> command_use_card id
    else command_roll_injail id
  
  let rec net_trade_offer st lst acc =
    match lst with
    | [] -> acc
    | h::t -> begin
      match h with
      | Command.Mon n -> net_trade_offer st t (acc +. float_of_int n)
      | Command.Prop p -> net_trade_offer st t
      (acc +. float_of_int (State.get_property_rent p st))
    end
    
  let trade_offer_eval st =
    let id = get_my_id st in
    let tinfo = State.get_trade_info st in
    match tinfo with
    | _,id2,o,r -> let oval = net_trade_offer st o 0.0 in
    let rval = net_trade_offer st r 0.0 in
    let abs_diff = abs (oval -. rval) in
    if oval >= rval then command_accept_trade id id2
    else if oval < rval && abs_diff /. rval <= 0.15
    then command_accept_trade id id2
    else command_decline_trade id id2

  let land_unowned_prop_eval st =
    let id = get_my_id st in

    let bt = State.get_player_location id st in
    let location = State.string_of_board_space bt in
    let cost = State.get_property_cost location st in
    
    let budget = State.get_player_money id st in

    if budget >= cost then command_buy id location
    else command_not_buy id location

  let land_payment_location st get_cost comm_pay comm_bankrupt comm_sell =
    let id = get_my_id st in

    let bt = State.get_player_location id st in
    let location = State.string_of_board_space bt in
    let cost = get_cost location st in
    
    let budget = State.get_player_money id st in

    let my_properties = State.get_player_properties id st in
    let sell_prop_val = property_time id my_properties st [] in

    if budget >= cost then comm_pay id location
    else match sellable_property sell_prop_val st ("", None) with
    | _, None -> comm_bankrupt id location
    | s, Some _ -> comm_sell id s location


  let land_other_prop_eval st =
    land_payment_location st State.get_property_rent
    command_pay_rent command_bankrupt_rent command_sell_pay_rent


  let transaction_eval st t =
    land_payment_location st (fun x y -> t)
    (command_pay_transaction t) command_bankrupt_transaction
    command_sell_pay_transaction


end

module MakeAIController (M : AIControllerFunctions) = struct
  let eval st = match State.get_turntype st with
    | State.Preroll -> let com, msg = M.preroll_eval st in
    Command.Preroll_command com, msg
    | State.Preroll_injail -> let com, msg = M.preroll_injail_eval st in
      Command.Preroll_injail_command com, msg
    | State.Trade_offer -> let com, msg = M.trade_offer_eval st in
    Command.Trade_offer_command com, msg
    | State.Land_unowned_prop -> let com, msg = M.land_unowned_prop_eval st in
      Command.Land_unowned_prop_command com, msg
    | State.Land_your_prop -> Command.End_turn_command,
    "[" ^ !current_player ^ "] landed on own property. End current turn."
    | State.Land_other_prop -> let com, msg = M.land_other_prop_eval st in
      Command.Land_other_prop_command com, msg
    | State.Land_other_injail_prop -> Command.End_turn_command,
    "[" ^ !current_player ^ "] landed on jailed player's property. " ^
    "End current turn."
    | State.Transaction i -> let com, msg = M.transaction_eval st i in
      Command.Transaction_command com, msg
    | State.Land_injail -> Command.End_turn_command,
    "[" ^ !current_player ^ "] landed on go-to-jail space. End current turn."
    | State.Land_jail_visiting -> Command.End_turn_command,
    "[" ^ !current_player ^ "] landed on jail. End current turn."
    | State.Draw_GetOutOfJail_Card -> Command.End_turn_command,
    "[" ^ !current_player ^ "] drew get-out-of-jail card. End current turn."
end

module AI1 : AIController = MakeAIController (AI1Functions)

module AI2 : AIController = MakeAIController (AI2Functions)

module AI3 : AIController = MakeAIController (AI3Functions)
