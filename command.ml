type mon_prop =
  | Mon of int
  | Prop of string

type preroll_command =
  | Roll
  | SellProp of string (*property_id*)
  | SellAndRoll of string (*property_id*)
  | Develop of string (*property_id*)
  | DevelopAndRoll of string (*property_id*)
  | Trade of string * mon_prop list * mon_prop list

type preroll_injail_command =
  | Roll_injail
  | UseCard
  | PayJailFee

type trade_offer_command =
  | AcceptTrade
  | DeclineTrade

type land_unowned_prop_command =
  | Buy
  | NotBuy

type land_other_prop_command =
  | PayRent
  | Sell_PayRent of string
  | BankruptRent

type transaction_command =
  | PayTransaction
  | Sell_PayTransaction of string
  | BankruptTransaction

type command =
  | Preroll_command of preroll_command
  | Preroll_injail_command of preroll_injail_command
  | Trade_offer_command of trade_offer_command
  | Land_unowned_prop_command of land_unowned_prop_command
  | Land_other_prop_command of land_other_prop_command
  | Transaction_command of transaction_command
  | End_turn_command
