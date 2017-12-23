(* Defines the current state of the game. Information accessed through getter
 * functions. *)
type state

(* Provided to help distinguish between the different identifiers represented
 * as strings.*)
type player_id = string
type property_id = string
type color = string
type deck_id = string

(* Variant describing the two kinds of owners of properties.*)
type player_or_bank =
  | Player of player_id
  | Bank

(* Variant describing the types of property groups. Properties of [Color]
 * groups can be developed when a player owns all the properties in a group.
 * Properties of the group [Railroad] are "automatically" developed depending
 * on how many the player owns.*)
type property_group =
  | Color of color
  | Railroad

(* A variation defining the possible controllers which may be used to determine
 * each player's action on a turn.*)
type controller =
  | GUI
  | AI1
  | AI2
  | AI3

(* Variant describing the types of board spaces.*)
type board_space =
  | Property of property_id
  | TransactionSpace of int * string
  | Jail
  | ToJail
  | CardSpace of deck_id

(* Variant describing the turntpes. The turntype determines which commands are
 * valid inputs for the state.*)
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

(* [init_state fjson playerlst] is the intial state of a game described by
 * the JSON file [fjson] for the players with controlers specified in
 * [playerlst].
 * Setting random to true uses the simulated rolling of two 6-sided dice.
 * Setting it to false has the players move one space upon rolling.
 * Raises errors if the [fjson] is not formatted properly. *)
val init_state : Yojson.Basic.json -> (string * controller) list -> random:bool -> state

(* [do' c s] is the updated state [s'] resulting from performing command [c] on
 * state [s], according to the following specifications for the changes from
 * [s] to [s'] that [c] dictates:
 * Roll
 *    The number of spaces to move the player forward is determined by the
 *    the simulated rolling of 2 fair 6-sided dice. The player's location is
 *    updated to the location that number of spaced forward on the board.
 *    If the player completes a circuit of the board (passes the GO space),
 *    their money is increased by an amount specified by the state. If they roll
 *    two of the same number (doubles), they are allowed to take another turn,
 *    but upon rolling their third double in a row, they are immediately sent
 *    to jail.
 *    The turntype is updated as follows:
 *      - Land_unowned_prop if the player lands on a property owned by the bank
 *      - Land_your_prop if the player lands on a property that they own
 *      - Land_other_prop if the player lands on a propery owned by another
 *        player, and that other player is not in jail
 *      - Land_other_injail_prop if the player land on a property owned by
 *        another player, and that other player is in jail
 *      - Transaction i if the player lands on a Transaction space, where i
 *        is the amount of money to be transacted with the bank. Positive i
 *        indicates the player owes the bank money, while negative i
 *        indicates that the player recieves money from the bank.
 *        Additionally, the message is updated with the description of the
 *        transaction.
 *      - Land_injail if the player landed on the ToJail space
 *        Additionally, the message is notified to tell the player they are
 *        in jail, the player's location is updated to Jail and the
 *        in_jail field of that player's record is updated to true.
 *      - Land_jail_visiting if the player landed on the Jail space
 *        Additionally, the message is updated to inform the player they are
 *        just visiting jail.
 *      - If the player lands on a CardSpace d, the turntype depends on the
 *        effect of the next card in deck d. In all cases, the message is
 *        updated to the card description.
 *          * if the effect is GoToSpace p, then the player location is updated
 *            to be p and the turntype is updated to the relevant Land_x_prop
 *            depending on the owner.
 *          * if the effect is ExchangeBank i, then the turntype is updated to
 *            Transaction i
 *          * if the effect is GetOutOfJail, the player's getoutofjailcards is
 *            incremented by 1, and the turntype is Draw_GetOutofJail_Card
 * SellProp p
 *    - If p is not a property id, raises failure
 *    - If p is a property id but is not owned by the current player
 *      the message is is updated to inform the player.
 *    - If p is a property id and is owned by the player:
 *      * if the property is at a development state > 0, the property's
 *        development state is decreased by 1 and half the cost of developing
 *        the property to that stage is added to the player's money
 *      * if the propery is at development state 0, ownership of the property is
 *        transferred to the bank and half the cost of the property is added to
 *        the player's money.
 * Develop p
 *    - If p is not a property id, raises failure
 *    - If p is a property id but is not owned by the current player
 *      the message is is updated to inform the player.
 *    - If p is a property id and is owned by the current player
 *      * If the property cannot be updated further, the message is updated to
 *        inform the player. Otherwise:
 *      * if the player does not have enough money to pay for the development,
 *        the message is updated to inform the player
 *      * if the player has enough money to pay for the development, the cost
 *        of development is subtracted from the player's money and the
 *        development stage of the property is incremented by 1
 * Trade (p2, o, r) (current player offering a trade with [p2] where the current
 *    player would give [p2] the properties and money listed in [o] and [p2]
 *    would give the current player the properties and money listed in [r])
 *    requires: the money amounts are positive.
 *    - If the trade is not possible the message is updated with Information
 *      about why (i.e., properties don't exist, players do not own those
 *      properties, players do not have enough money)
 *    - If the trade is possible, the current player is updated to p2,
 *      the tradeinfo is updated to (current, p2, o, r), and the and the turntype
 *      is updated to Trade_offer
 * AcceptTrade
 *   Based on tradeinfo (p1,p2,o,r), the current player is updated to [p1], the
 *   trade_accepted is updated to Some true, the properties and money in
 *   lst [o] are transferred from [p1] to [p2], the properties and money
 *   in lst [r] are transferred from [p2] to [p1], and the turntype is updated
 *   to Preroll. The message is updated to let the player know the trade was
 *   accepted, and trade_accepted is updated to Some true.
 * DeclineTrade
 *   The current player is updated to [p1], the trade_accepted is updated
 *   to Some false, and the turntype is updated to Preroll. The message is
 *   updated to let the player know the trade was declined.
 * Roll_injail
 *   Simulate the rolling of 2 fair two-sided dice.
 *   - If the dice are different, the player remains in jail. The current_player
 *     is updated to the next player in the turn order, the turntype is updated
 *     to Preroll, and the trade_accepted is updated to None.
 *   - If the dice are the same, the player's in_jail updated to false, then the
 *     player moves the number of spaces on the dice (starting from Jail)
 *     and the player location and turntype are updated accordingly (see Roll).
 * UseCard
 *   - If the player does not have any getoutofjailcards, the message is updated
 *     to inform them.
 *   - If the player does have a getoutofjailcard, then decrement the number by
 *     1. The player's in_jail is update to false, and the turntype is updated
 *     to Preroll.
 * PayJailFee
 *   - If the player does not have enough money to pay the fee, the message is
 *     updated to inform them.
 *   - If the player does have enough money, then the money is deducted from the
 *     player's money, the player's in_jail is updated to false, and the
 *     turntype is updated to Preroll.
 * Buy
 *   - If the current player does not have enough money to pay for the property,
 *     the message is updated to inform the player.
 *   - If the current player has enough money, the cost of the property is
 *     deducted from the player's money, and ownership is transferred from the
 *     bank to the player. The current_player is updated to the next player in
 *     the turn order, the turntype is updated to Preroll, and the
 *     trade_accepted is updated to None.
 * NotBuy
 *   - The current_player is updated to the next player in the turn order,
 *     the turntype is updated to Preroll, and the trade_accepted is updated
 *     to None.
 * PayRent
 *   - If the curent player does not have enough money to pay the rent, the
 *     message is updated to inform the player.
 *   - If the current player has enough money, the money is tranferred from the
 *     current player to the player who owns the property. The current_player is
 *     updated to the next player in the turn order, the turntype is updated to
 *     Preroll, and the trade_accepted is updated to None.
 * Sell_PayRent p
 *    - If p is not a property id, the message is updated to inform the player
 *    - If p is a property id but is not owned by the current player
 *      the message is is updated to inform the player.
 *    - If p is a property id and is owned by the player:
 *      * if the property is at a development state > 0, the property's
 *        development state is decreased by 1 and half the cost of developing
 *        the property to that stage is added to the player's money
 *      * if the propery is at development state 0, ownership of the property is
 *        transferred to the bank and half the cost of the property is added to
 *        the player's money.
 * BankruptRent
 *    - Intended to be invoked only when the player does not have enough money
 *      and cannot sell off property in order to pay the rent. All of the
 *      player's properties are transferred to the bank, and the player is
 *      is removed from the player list. The current_player is updated to the
 *      next player in the turn order, the turntype is updated to Preroll, and
 *      the trade_accepted is updated to None. The turncounter must be adjusted
 *      accordingly to maintain the proper current_player.
 * PayTransaction
 *   - If the curent player does not have enough money to pay the transaction,
 *     the message is updated to inform the player.
 *   - If the current player has enough money, the money is deducted from the
 *     player's money. The current_player is updated to the next player in the
 *     turn order, the turntype is updated to Preroll, and the trade_accepted is
 *     updated to None.
 * Sell_PayTransaction p
 *    - If p is not a property id, the message is updated to inform the player
 *    - If p is a property id but is not owned by the current player
 *      the message is is updated to inform the player.
 *    - If p is a property id and is owned by the player:
 *      * if the property is at a development state > 0, the property's
 *        development state is decreased by 1 and half the cost of developing
 *        the property to that stage is added to the player's money
 *      * if the propery is at development state 0, ownership of the property is
 *        transferred to the bank and half the cost of the property is added to
 *        the player's money.
 * BankruptTransaction
 *    - Intended to be invoked only when the player does not have enough money
 *      and cannot sell off property in order to pay the transaction. All of the
 *      player's properties are transferred to the bank, and the player is
 *      is removed from the player list. The current_player is updated to the
 *      next player in the turn order, the turntype is updated to Preroll, and
 *      the trade_accepted is updated to None. The turncounter must be adjusted
 *      accordingly to maintain the proper current_player.
 * End_turn_command
 *    The message is cleared, the current_player is the next player
 *    as determined by the player list, the turntype is Preroll, and the
 *    trade_accepted is None.
 * requires: The type of command [c] must match the turntype of [s].
 * for example if [get_turntype s] is Preroll,
 * then [c] must be of type Command.preroll_command.*)
val do' : Command.command -> state -> state

(* [get_X] is an accessor function *)

(* [get_all_board_spaces s] is the list of the string representations of all
 * board spaces in [s], starting from the starting location and proceding in
 * order. *)
val get_all_board_spaces : state -> string list

(* [get_all_properties s] is the list of the ids of all properties in [s]. Order
 * is not guaranteed.*)
val get_all_properties : state -> property_id list

(* [get_all_players s] is the list of (id,countroller) for all active players
 * (not including any players who may have gone bankrupt over the course of
 * the game). They players are in order of their turns, starting with the player
 * with the earliest first turn.*)
val get_all_players: state -> (player_id * controller) list

(* [get_turntype s] is the current turntype of s*)
val get_turntype : state -> turntype

(* [get_message s] is None if there is no message for the current player
 * or [Some str] where str is a message to the player*)
val get_message : state -> string option

(* [string_of_controller c] is the string representing controller [c].*)
val string_of_controller : controller -> string

(* [string_of_board_space s] is the string representing board space [s].*)
val string_of_board_space : board_space -> string

(* [string_of_property_group g] is the string represeting property group [p]*)
val string_of_property_group : property_group -> string

(* [get_current_player s] is (id,cont) where [id] is the id of the player
 * whose turn it is and [cont] is the type of controller that player uses. *)
val get_current_player : state -> player_id * controller

(* [get_player_location id s] is the location of the player with [id].
 * requires: [id] is the id of a player of the game [s].*)
val get_player_location : player_id -> state -> board_space

(* [get_player_money id s] is the money that the player with [id] has.
 * requires: [id] is the id of a player of the game [s].*)
val get_player_money : player_id -> state -> int

(* [get_player_cards id s] is the number of Get Out of Jail Free cards that
 * player [s] has.
 * requires: [id] is the id of a player of the game [s].*)
val get_player_cards : player_id -> state -> int

(* [get_player_in_jail id s] is whether or not player [id] is in jail
 * requires: [id] is the id of a player of the game [s].*)
val get_player_in_jail : player_id -> state -> bool

(* [get_player_properties id s] is the list of property_ids owned by
 * player [id]
 * requires: [id] is the id of a player of the game [s].*)
val get_player_properties : player_id -> state -> property_id list

(* [get_player_properties_of_group id g s] is the list of all property ids owned
 * by player [id] with group [g]
 * requires: [id] is the id of a player of the game [s].*)
val get_player_properties_of_group :
  player_id -> property_group -> state -> property_id list

(* [get_player_monopoly p g s] is whether player [p] owns all the properties
 * in group [g]
 * requires: [id] is the id of a player of the game [s].*)
val get_player_monopoly : player_id -> property_group -> state -> bool

(* [get_player_property p s] is [Player id] where [id] is of the player who owns
 * property [p], or [Bank] if no player owns [p].
 * requires: [p] is the id of a property in the game [s].*)
val get_property_owner : property_id -> state -> player_or_bank

(* [get property_group p s] is the group of property [p]
 * requires: [p] is the id of a property in the game [s].*)
val get_property_group : property_id -> state -> property_group

(* [get_properties_of_group g s] is the list of all property ids which have
 * the group [g]*)
val get_properties_of_group : property_group -> state -> property_id list

(* [get_property_cost p s] is the cost to purchase [p] from the bank.
 * requires: [p] is the id of a property in the game [s].*)
val get_property_cost : property_id -> state -> int

(* [get_property_rent p s] is the rent owed by a player who lands on [p].
 * requires: [p] is the id of a property in the game [s].*)
val get_property_rent : property_id -> state -> int

(* [get_property_dev_cost p s] is [None] if the property cannot be developed
 * by paying or Some [c] where c is the cost tso develop property [p] by one
 * state. Note that properties of group [Railroad] as well as
 * fully developed cannot be developed by paying.
 * requires: [p] is the id of a property in the game [s].*)
val get_property_dev_cost : property_id -> state -> int option

(* [get_property_dev_stage p s] is the current development stage of property [p]
 * requires: [p] is the id of a property in game [s].*)
val get_property_dev_stage : property_id -> state -> int

(* [get_property_max_dev_stage p s] is the maximum development stage of property
 * [p].
 * requires: [p] is the id of a property in game [s].*)
val get_property_max_dev_stage : property_id -> state ->int

(* [get_property_max_rent p s] is the rent that would be owed by a player who
 * lands on property [p] if [p] was at its maximum development stage.
 * requires: [p] is the id of a property in game [s].*)
val get_property_max_rent : property_id -> state -> int

(* [get_trade_acceptance s] is [None] if the player has not proposed a trade
 * on this turn, [Some true] if the player's proposed trade has been accepted,
 * and [Some false] if the player's proposed trade has been rejected.*)
val get_trade_acceptance : state -> bool option

(* [get_trade_info s] is [(p1,p2,o,r)] is the info of the last proposed trade,
   where [p1] is the id of the player offering the trade,
   [p2] is the id of the player being offered the trade,
   [o] is the list of money and property being offered by [p1], and
   [r] is the list of money and property being requested of [p2].
   requires: [get_trade_acceptance s] is not [None].
*)
val get_trade_info :
  state -> player_id * player_id * Command.mon_prop list * Command.mon_prop list

(* [get_jail_fee s] is the amount of money a player would need to pay to get out
 * of jail in game [s].*)
val get_jail_fee : state -> int
