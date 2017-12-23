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

module AI1 : AIController

module AI2 : AIController

module AI3 : AIController
