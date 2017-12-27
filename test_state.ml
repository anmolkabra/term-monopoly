open OUnit2
open State
open Command

let test_board = Yojson.Basic.from_file "boards/test_board.json"
let test_board_2 = Yojson.Basic.from_file "boards/test_board_2.json"
let test_board_r = Yojson.Basic.from_file "boards/test_board_railroads.json"
let test_board_j = Yojson.Basic.from_file "boards/test_board_jail.json"

let one_player = [("p1",GUI)]
let two_players = [("p1",GUI);("p2",AI1)]
let three_players = [("p1",GUI);("p2",AI1);("p3",GUI)]

let test_game_1p = init_state test_board one_player ~random:false
let test_game_2p = init_state test_board two_players ~random:false
let test_game_3p = init_state test_board three_players ~random:false

let test_2_game_2p = init_state test_board_2 two_players ~random:false

let test_game_r = init_state test_board_r two_players ~random:false
let test_game_j = init_state test_board_j two_players ~random:false

let test_board_spaces = ["prop1"; "Pay $200"; "Jail"; "Go To Jail";
                         "Chance: Draw a card"; "railroad";"utility";]
let init_tests = [
  "init_test_game_board_spaces" >::
  (fun _ -> assert_equal test_board_spaces (get_all_board_spaces test_game_1p));
  "init_test_all_properties" >::
  (fun _ -> assert_equal ["prop1"; "railroad"; "utility"] (get_all_properties test_game_1p));
  "init_test_all_players_1p" >::
  (fun _ -> assert_equal [("p1",GUI)] (get_all_players test_game_1p));
  "init_test_all_players_2p" >::
  (fun _ -> assert_equal [("p1",GUI);("p2",AI1)] (get_all_players test_game_2p));
  "init_test_all_players_3p" >::
  (fun _ -> assert_equal [("p1",GUI);("p2",AI1);("p3",GUI)] (get_all_players test_game_3p));
  "init_test_turntype" >::
  (fun _ -> assert_equal Preroll (get_turntype test_game_1p));
  "init_test_message" >::
  (fun _ -> assert_equal None (get_message test_game_1p));
  "init_test_current_player_1p" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player test_game_1p));
  "init_test_current_player_2p" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player test_game_2p));
  "init_test_current_player_3p" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player test_game_3p));
  "init_test_player_loc_1p" >::
  (fun _ -> assert_equal (Property "prop1") (get_player_location "p1" test_game_1p));
  "init_test_player_loc_2p" >::
  (fun _ -> assert_equal (Property "prop1") (get_player_location "p2" test_game_2p));
  "init_test_player_loc_3p" >::
  (fun _ -> assert_equal (Property "prop1") (get_player_location "p3" test_game_3p));
  "init_test_player_money" >::
  (fun _ -> assert_equal 1500 (get_player_money "p1" test_game_2p));
  "init_test_player_cards" >::
  (fun _ -> assert_equal 0 (get_player_cards "p1" test_game_2p));
  "init_test_player_in_jail" >::
  (fun _ -> assert_equal false (get_player_in_jail "p1" test_game_2p));
  "init_test_player_properties" >::
  (fun _ -> assert_equal [] (get_player_properties "p1" test_game_2p));
  "init_test_player_properties_of_group" >::
  (fun _ -> assert_equal [] (get_player_properties_of_group "p1" (Color "Purple") test_game_2p));
  "init_test_player_monopoly" >::
  (fun _ -> assert_equal false (get_player_monopoly "p1" (Color "Purple") test_game_2p));
  "init_test_property_owner_1" >::
  (fun _ -> assert_equal Bank (get_property_owner "prop1" test_game_2p));
  "init_test_property_owner_r" >::
  (fun _ -> assert_equal Bank (get_property_owner "railroad" test_game_2p));
  "init_test_property_owner_u" >::
  (fun _ -> assert_equal Bank (get_property_owner "utility" test_game_2p));
  "init_test_property_group_1" >::
  (fun _ -> assert_equal (Color "Purple") (get_property_group "prop1" test_game_2p));
  "init_test_property_group_r" >::
  (fun _ -> assert_equal (Railroad) (get_property_group "railroad" test_game_2p));
  "init_test_property_group_u" >::
  (fun _ -> assert_equal (Color "Utility") (get_property_group "utility" test_game_2p));
  "init_test_properties_of_group_1" >::
  (fun _ -> assert_equal ["prop1"] (get_properties_of_group (Color "Purple") test_game_2p));
  "init_test_properties_of_group_r" >::
  (fun _ -> assert_equal ["railroad"] (get_properties_of_group (Railroad) test_game_2p));
  "init_test_properties_of_group_u" >::
  (fun _ -> assert_equal ["utility"] (get_properties_of_group (Color "Utility") test_game_2p));
  "init_test_property_cost_1" >::
  (fun _ -> assert_equal 100 (get_property_cost "prop1" test_game_2p));
  "init_test_property_cost_r" >::
  (fun _ -> assert_equal 200 (get_property_cost "railroad" test_game_2p));
  "init_test_property_cost_u" >::
  (fun _ -> assert_equal 300 (get_property_cost "utility" test_game_2p));
  "init_test_property_rent_1" >::
  (fun _ -> assert_equal 5 (get_property_rent "prop1" test_game_2p));
  "init_test_property_rent_r" >::
  (fun _ -> assert_equal 20 (get_property_rent "railroad" test_game_2p));
  "init_test_property_rent_u" >::
  (fun _ -> assert_equal 30 (get_property_rent "utility" test_game_2p));
  "init_test_property_dev_cost_1" >::
  (fun _ -> assert_equal (Some 100) (get_property_dev_cost "prop1" test_game_2p));
  "init_test_property_dev_cost_r" >::
  (fun _ -> assert_equal None (get_property_dev_cost "railroad" test_game_2p));
  "init_test_property_dev_cost_u" >::
  (fun _ -> assert_equal None (get_property_dev_cost "utility" test_game_2p));
  "init_test_property_dev_stage_1" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "prop1" test_game_2p));
  "init_test_property_dev_stage_r" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "railroad" test_game_2p));
  "init_test_property_dev_stage_u" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "utility" test_game_2p));
  "init_test_property_dev_stage_r" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "railroad" test_game_2p));
  "init_test_property_max_dev_stag_1" >::
  (fun _ -> assert_equal 3 (get_property_max_dev_stage "prop1" test_game_2p));
  "init_test_property_max_dev_stag_r" >::
  (fun _ -> assert_equal 0 (get_property_max_dev_stage "railroad" test_game_2p));
  "init_test_property_max_dev_stag_u" >::
  (fun _ -> assert_equal 0 (get_property_max_dev_stage "utility" test_game_2p));
  "init_test_property_max_rent_1" >::
  (fun _ -> assert_equal 20 (get_property_max_rent "prop1" test_game_2p));
  "init_test_property_max_rent_r" >::
  (fun _ -> assert_equal 20 (get_property_max_rent "railroad" test_game_2p));
  "init_test_property_max_rent_u" >::
  (fun _ -> assert_equal 30 (get_property_max_rent "utility" test_game_2p));
  "init_test_trade_acceptance" >::
  (fun _ -> assert_equal None (get_trade_acceptance test_game_2p));
  "init_test_jail_fee" >::
  (fun _ -> assert_equal 50 (get_jail_fee test_game_2p));
]

let move_1 = test_game_2p |> do' (Preroll_command Roll)
let move_1_pay = move_1 |> do' (Transaction_command PayTransaction)
let move_1_br = move_1 |> do' (Transaction_command (BankruptTransaction))
let move_2_pay = move_1_pay |> do' (Preroll_command Roll)
                 |> do' (Transaction_command PayTransaction)
let move_3 = move_2_pay |> do' (Preroll_command Roll)
let move_5 = move_3 |> do' (End_turn_command) |> do' (Preroll_command Roll)
             |> do' (End_turn_command) |> do' (Preroll_command Roll)
let move_7 = move_5 |> do' (End_turn_command) |> do' (Preroll_command Roll)
             |> do' (End_turn_command)
let move_7_roll = move_7 |> do' (Preroll_injail_command Roll_injail)
let move_7_pay = move_7 |> do' (Preroll_injail_command PayJailFee)

let roll_test_board_tests = [
  "move_1_turntype" >::
  (fun _ -> assert_equal (Transaction 200) (get_turntype move_1));
  "move_1_message" >::
  (fun _ -> assert_equal (Some "Pay $200") (get_message move_1));
  "move_1_current_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player  move_1));
  "move_1_player_loc_p1" >::
  (fun _ -> assert_equal (TransactionSpace (200,"Pay $200"))
      (get_player_location "p1" move_1));
  "move_1_player_loc_p2" >::
  (fun _ -> assert_equal (Property "prop1") (get_player_location "p2" move_1));
  "move_1_player_money" >::
  (fun _ -> assert_equal 1500 (get_player_money "p1" move_1));
  "move_1_pay_turntype" >::
  (fun _ -> assert_equal Preroll (get_turntype move_1_pay));
  "move_1_pay_message" >::
  (fun _ -> assert_equal (None) (get_message move_1_pay));
  "move_1_pay_current_player" >::
  (fun _ -> assert_equal ("p2",AI1) (get_current_player  move_1_pay));
  "move_1_pay_player_loc_p1" >::
  (fun _ -> assert_equal (TransactionSpace (200,"Pay $200"))
      (get_player_location "p1" move_1_pay));
  "move_1_pay_player_loc_p2" >::
  (fun _ -> assert_equal (Property "prop1") (get_player_location "p2" move_1_pay));
  "move_1_pay_player_money_p1" >::
  (fun _ -> assert_equal 1300 (get_player_money "p1" move_1_pay));
  "move_1_pay_player_money_p2" >::
  (fun _ -> assert_equal 1500 (get_player_money "p2" move_1_pay));
  "move1_br_turntype" >::
  (fun _ -> assert_equal (Preroll) (get_turntype move_1_br));
  "move1_br_message" >::
  (fun _ -> assert_equal None (get_message move_1_br));
  "move1_br_players" >::
  (fun _ -> assert_equal [("p2",AI1)] (get_all_players move_1_br));
  "move1_br_current_player" >::
  (fun _ -> assert_equal ("p2",AI1) (get_current_player move_1_br));
  "move1_br_player_loc_p2" >::
  (fun _ -> assert_equal (Property "prop1") (get_player_location "p2" move_1_br));
  "move_2_pay_turntype" >::
  (fun _ -> assert_equal Preroll (get_turntype move_2_pay));
  "move_2_pay_message" >::
  (fun _ -> assert_equal (None) (get_message move_2_pay));
  "move_2_pay_current_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player  move_2_pay));
  "move_2_pay_player_loc_p1" >::
  (fun _ -> assert_equal (TransactionSpace (200,"Pay $200"))
      (get_player_location "p1" move_2_pay));
  "move_2_pay_player_loc_p2" >::
  (fun _ -> assert_equal (TransactionSpace (200,"Pay $200"))
      (get_player_location "p2" move_2_pay));
  "move_2_pay_player_money_p1" >::
  (fun _ -> assert_equal 1300 (get_player_money "p1" move_2_pay));
  "move_2_pay_player_money_p1" >::
  (fun _ -> assert_equal 1300 (get_player_money "p2" move_2_pay));
  "move_3_turntype" >::
  (fun _ -> assert_equal (Land_jail_visiting) (get_turntype move_3));
  "move_3_message" >::
  (fun _ -> assert_equal (Some "You are visiting jail.") (get_message move_3));
  "move_3_current_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player  move_3));
  "move_3_player_loc_p1" >::
  (fun _ -> assert_equal (Jail) (get_player_location "p1" move_3));
  "move_3_p1_in_jail" >::
  (fun _ -> assert_equal false (get_player_in_jail "p1" move_3));
  "move_3_player_loc_p2" >::
  (fun _ -> assert_equal (TransactionSpace (200,"Pay $200"))
      (get_player_location "p2" move_3));
  "move_5_turntype" >::
  (fun _ -> assert_equal (Land_injail) (get_turntype move_5));
  "move_5_message" >::
  (fun _ -> assert_equal (Some "Go Directly to Jail.") (get_message move_5));
  "move_5_current_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player  move_5));
  "move_5_player_loc_p1" >::
  (fun _ -> assert_equal (Jail) (get_player_location "p1" move_5));
  "move_5_player_loc_p2" >::
  (fun _ -> assert_equal (Jail)
      (get_player_location "p2" move_5));
  "move_5_p1_in_jail" >::
  (fun _ -> assert_equal true (get_player_in_jail "p1" move_5));
  "move_5_p2_in_jail" >::
  (fun _ -> assert_equal false (get_player_in_jail "p2" move_5));
  "move_7_turntype" >::
  (fun _ -> assert_equal (Preroll_injail) (get_turntype move_7));
  "move_7_message" >::
  (fun _ -> assert_equal None (get_message move_7));
  "move_7_current_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player  move_7));
  "move_7_player_loc_p1" >::
  (fun _ -> assert_equal (Jail) (get_player_location "p1" move_7));
  "move_7_player_loc_p2" >::
  (fun _ -> assert_equal (Jail)
      (get_player_location "p2" move_7));
  "move_7_p1_in_jail" >::
  (fun _ -> assert_equal true (get_player_in_jail "p1" move_7));
  "move_7_p2_in_jail" >::
  (fun _ -> assert_equal true (get_player_in_jail "p2" move_7));
  "move_7_roll_turntype" >::
  (fun _ -> assert_equal (Preroll_injail) (get_turntype move_7_roll));
  "move_7_roll_message" >::
  (fun _ -> assert_equal None (get_message move_7_roll));
  "move_7_roll_current_player" >::
  (fun _ -> assert_equal ("p2",AI1) (get_current_player  move_7_roll));
  "move_7_roll_player_loc_p1" >::
  (fun _ -> assert_equal (Jail) (get_player_location "p1" move_7_roll));
  "move_7_roll_player_loc_p2" >::
  (fun _ -> assert_equal (Jail)
      (get_player_location "p2" move_7_roll));
  "move_7_roll_p1_in_jail" >::
  (fun _ -> assert_equal true (get_player_in_jail "p1" move_7_roll));
  "move_7_roll_p2_in_jail" >::
  (fun _ -> assert_equal true (get_player_in_jail "p2" move_7_roll));
  "move_7_turntype" >::
  (fun _ -> assert_equal (Preroll_injail) (get_turntype move_7));
  "move_7_message" >::
  (fun _ -> assert_equal None (get_message move_7));
  "move_7_current_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player  move_7));
  "move_7_player_loc_p1" >::
  (fun _ -> assert_equal (Jail) (get_player_location "p1" move_7));
  "move_7_player_loc_p2" >::
  (fun _ -> assert_equal (Jail)
      (get_player_location "p2" move_7));
  "move_7_p1_in_jail" >::
  (fun _ -> assert_equal true (get_player_in_jail "p1" move_7));
  "move_7_p2_in_jail" >::
  (fun _ -> assert_equal true (get_player_in_jail "p2" move_7));
  "move_7_pay_turntype" >::
  (fun _ -> assert_equal (Preroll) (get_turntype move_7_pay));
  "move_7_pay_message" >::
  (fun _ -> assert_equal (Some "You got out of jail.") (get_message move_7_pay));
  "move_7_pay_current_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player  move_7_pay));
  "move_7_pay_player_loc_p1" >::
  (fun _ -> assert_equal (Jail) (get_player_location "p1" move_7_pay));
  "move_7_pay_player_loc_p2" >::
  (fun _ -> assert_equal (Jail)
      (get_player_location "p2" move_7_pay));
  "move_7_pay_p1_in_jail" >::
  (fun _ -> assert_equal false (get_player_in_jail "p1" move_7_pay));
  "move_7_pay_p2_in_jail" >::
  (fun _ -> assert_equal true (get_player_in_jail "p2" move_7_pay));
  "move_7_pay_player_money_p1" >::
  (fun _ -> assert_equal 1250 (get_player_money "p1" move_7_pay));
]

let move2_1 = test_2_game_2p |> do' (Preroll_command Roll)
let move2_1_nb = move2_1 |> do' (Land_unowned_prop_command NotBuy)
let move2_1_buy = move2_1 |> do' (Land_unowned_prop_command Buy)

let move2_2 = move2_1_buy |> do' (Preroll_command Roll)
let move2_2_pay_rent = move2_2 |> do' (Land_other_prop_command PayRent)
let move2_2_bankrupt = move2_2 |> do' (Land_other_prop_command BankruptRent)

let move2_3_dev = move2_2_pay_rent |> do' (Preroll_command (Develop "prop1"))
let move2_3_sell = move2_2_pay_rent |> do' (Preroll_command (SellProp "prop1"))
let move2_3 = move2_2_pay_rent |> do' (Preroll_command Roll)
let move2_3_transell = move2_3
                       |> do' (Transaction_command (Sell_PayTransaction "prop1"))
let move2_3_br = move2_3
                 |> do' (Transaction_command BankruptTransaction)

let move2_7 = move2_3 |> do' (Transaction_command PayTransaction)
           |> do' (Preroll_command Roll)
           |> do' (Transaction_command PayTransaction)
           |> do' (Preroll_command Roll)
           |> do' (Land_unowned_prop_command Buy)
           |> do' (Preroll_command Roll)
           |> do' (Land_other_prop_command PayRent)
let move2_7_dev = move2_7 |> do' (Preroll_command (Develop "prop1"))

let move2_7_over_dev = move2_7_dev |> do' (Preroll_command (Develop "prop1"))
                       |> do' (Preroll_command (Develop "prop1"))
                       |> do' (Preroll_command (Develop "prop1"))
let move2_7_exp_dev = move2_7_dev |> do' (Preroll_command (Develop "prop2"))

let roll_test_board_2_tests = [
  "move2_purple_properties" >::
  (fun _ -> assert_equal (["prop1"; "prop2"])
    (get_properties_of_group (Color "Purple") move2_1));
  "move2_1_turntype" >::
  (fun _ -> assert_equal (Land_unowned_prop) (get_turntype move2_1));
  "move2_1_message" >::
  (fun _ -> assert_equal None (get_message move2_1));
  "move2_1_current_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player  move2_1));
  "move2_1_player_loc_p1" >::
  (fun _ -> assert_equal (Property "prop1")
      (get_player_location "p1" move2_1));
  "move2_1_player_loc_p2" >::
  (fun _ -> assert_equal (Jail) (get_player_location "p2" move2_1));
  "move2_1_player_money" >::
  (fun _ -> assert_equal 1500 (get_player_money "p1" move2_1));
  "move2_1_nb_turntype" >::
  (fun _ -> assert_equal (Preroll) (get_turntype move2_1_nb));
  "move2_1_nb_message" >::
  (fun _ -> assert_equal None (get_message move2_1_nb));
  "move2_1_nb_current_player" >::
  (fun _ -> assert_equal ("p2",AI1) (get_current_player  move2_1_nb));
  "move2_1_nb_player_loc_p1" >::
  (fun _ -> assert_equal (Property "prop1")
      (get_player_location "p1" move2_1_nb));
  "move2_1_nb_player_loc_p2" >::
  (fun _ -> assert_equal (Jail) (get_player_location "p2" move2_1_nb));
  "move2_1_nb_player_money" >::
  (fun _ -> assert_equal 1500 (get_player_money "p1" move2_1_nb));
  "move2_1_buy_turntype" >::
  (fun _ -> assert_equal (Preroll) (get_turntype move2_1_buy));
  "move2_1_buy_message" >::
  (fun _ -> assert_equal None (get_message move2_1_nb));
  "move2_1_buy_current_player" >::
  (fun _ -> assert_equal ("p2",AI1) (get_current_player  move2_1_buy));
  "move2_1_buy_player_loc_p1" >::
  (fun _ -> assert_equal (Property "prop1")
      (get_player_location "p1" move2_1_buy));
  "move2_1_buy_player_loc_p2" >::
  (fun _ -> assert_equal (Jail) (get_player_location "p2" move2_1_buy));
  "move2_1_buy_player_money" >::
  (fun _ -> assert_equal 1400 (get_player_money "p1" move2_1_buy));
  "move2_1_buy_player_properties" >::
  (fun _ -> assert_equal ["prop1"] (get_player_properties "p1" move2_1_buy));
  "move2_1_buy_property_owner" >::
  (fun _ -> assert_equal (Player "p1") (get_property_owner "prop1" move2_1_buy));
  "move2_1_buy_monopoly" >::
  (fun _ -> assert_equal false (get_player_monopoly "p1" (Color "Purple") move2_1_buy));
  "move2_2_turntype" >::
  (fun _ -> assert_equal (Land_other_prop) (get_turntype move2_2));
  "move2_2_message" >::
  (fun _ -> assert_equal None (get_message move2_2));
  "move2_2_current_player" >::
  (fun _ -> assert_equal ("p2",AI1) (get_current_player  move2_2));
  "move2_2_player_loc_p1" >::
  (fun _ -> assert_equal (Property "prop1") (get_player_location "p1" move2_2));
  "move2_2_player_loc_p2" >::
  (fun _ -> assert_equal (Property "prop1") (get_player_location "p2" move2_2));
  "move2_2_player_money" >::
  (fun _ -> assert_equal 1500 (get_player_money "p2" move2_2));
  "move2_2_pr_turntype" >::
  (fun _ -> assert_equal (Preroll) (get_turntype move2_2_pay_rent));
  "move2_2_pr_message" >::
  (fun _ -> assert_equal None (get_message move2_2_pay_rent));
  "move2_2_pr_current_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player move2_2_pay_rent));
  "move2_2_pr_player_loc_p1" >::
  (fun _ -> assert_equal (Property "prop1") (get_player_location "p1" move2_2_pay_rent));
  "move2_2_pr_player_loc_p2" >::
  (fun _ -> assert_equal (Property "prop1") (get_player_location "p2" move2_2_pay_rent));
  "move2_2_pr_player_money_p1" >::
  (fun _ -> assert_equal 1405 (get_player_money "p1" move2_2_pay_rent));
  "move2_2_pr_player_money_p2" >::
  (fun _ -> assert_equal 1495 (get_player_money "p2" move2_2_pay_rent));
  "move2_2_br_turntype" >::
  (fun _ -> assert_equal (Preroll) (get_turntype move2_2_bankrupt));
  "move2_2_br_message" >::
  (fun _ -> assert_equal None (get_message move2_2_bankrupt));
  "move2_2_br_players" >::
  (fun _ -> assert_equal [("p1",GUI)] (get_all_players move2_2_bankrupt));
  "move2_2_br_current_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player move2_2_bankrupt));
  "move2_2_br_player_loc_p1" >::
  (fun _ -> assert_equal (Property "prop1") (get_player_location "p1" move2_2_bankrupt));
  "move2_3_dev_message" >::
  (fun _ -> assert_equal (Some "You do not own all the properties in the group")
      (get_message move2_3_dev));
  "move2_3_dev_player_props" >::
  (fun _ -> assert_equal ["prop1"] (get_player_properties "p1" move2_3_dev));
  "move2_3_dev_prop_owner" >::
  (fun _ -> assert_equal (Player "p1") (get_property_owner "prop1" move2_3_dev));
  "move2_3_dev_stage" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "prop1" move2_3_dev));
  "move2_3_sell_message" >::
  (fun _ -> assert_equal None
      (get_message move2_3_sell));
  "move2_3_sell_player_props" >::
  (fun _ -> assert_equal [] (get_player_properties "p1" move2_3_sell));
  "move2_3_sell_prop_owner" >::
  (fun _ -> assert_equal Bank (get_property_owner "prop1" move2_3_sell));
  "move2_3_sell_dev_stage" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "prop1" move2_3_sell));
  "move2_3_sell_money" >::
  (fun _ -> assert_equal 1455 (get_player_money "p1" move2_3_sell));
  "move2_3_turntype" >::
  (fun _ -> assert_equal (Transaction 200) (get_turntype move2_3));
  "move2_3_message" >::
  (fun _ -> assert_equal (Some "Pay $200") (get_message move2_3));
  "move2_3_current_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player  move2_3));
  "move2_3_player_loc_p1" >::
  (fun _ -> assert_equal (TransactionSpace (200,"Pay $200"))
      (get_player_location "p1" move2_3));
  "move2_3__transell_turntype" >::
  (fun _ -> assert_equal (Transaction 200) (get_turntype move2_3_transell));
  "move2_3_transell_message" >::
  (fun _ -> assert_equal (Some "Pay $200") (get_message move2_3_transell));
  "move2_3_transell_player_props" >::
  (fun _ -> assert_equal [] (get_player_properties "p1" move2_3_transell));
  "move2_3_transell_prop_owner" >::
  (fun _ -> assert_equal Bank (get_property_owner "prop1" move2_3_transell));
  "move2_3_transell_dev_stage" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "prop1" move2_3_transell));
  "move2_3_transell_money" >::
  (fun _ -> assert_equal 1455 (get_player_money "p1" move2_3_transell));
  "move2_3_br_turntype" >::
  (fun _ -> assert_equal (Preroll) (get_turntype move2_3_br));
  "move2_3_br_message" >::
  (fun _ -> assert_equal None (get_message move2_3_br));
  "move2_3_br_players" >::
  (fun _ -> assert_equal [("p2",AI1)] (get_all_players move2_3_br));
  "move2_3_br_current_player" >::
  (fun _ -> assert_equal ("p2",AI1) (get_current_player move2_3_br));
  "move2_3_br_property" >::
  (fun _ -> assert_equal Bank (get_property_owner "prop1" move2_3_br));
  "move2_7_turntype" >::
  (fun _ -> assert_equal (Preroll) (get_turntype move2_7));
  "move2_7_message" >::
  (fun _ -> assert_equal None (get_message move2_7));
  "move2_7_current_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player  move2_7));
  "move2_7_player_loc_p1" >::
  (fun _ -> assert_equal (Property "prop2") (get_player_location "p1" move2_7));
  "move2_7_player_money" >::
  (fun _ -> assert_equal 1101 (get_player_money "p1" move2_7));
  "move2_7_dev_turntype" >::
  (fun _ -> assert_equal (Preroll) (get_turntype move2_7_dev));
  "move2_7_dev_message" >::
  (fun _ -> assert_equal None (get_message move2_7_dev));
  "move2_7_dev_current_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player  move2_7_dev));
  "move2_7_dev_player_loc_p1" >::
  (fun _ -> assert_equal (Property "prop2") (get_player_location "p1" move2_7_dev));
  "move2_7_dev_player_money" >::
  (fun _ -> assert_equal 1001 (get_player_money "p1" move2_7_dev));
  "move2_7_dev_property_stage" >::
  (fun _ -> assert_equal 1 (get_property_dev_stage "prop1" move2_7_dev));
  "move2_7_dev_property_rent" >::
  (fun _ -> assert_equal 10 (get_property_rent "prop1" move2_7_dev));
  "move2_7_over_dev_turntype" >::
  (fun _ -> assert_equal (Preroll) (get_turntype move2_7_over_dev));
  "move2_7_over_dev_message" >::
  (fun _ -> assert_equal (Some "That property is already fully developed.")
      (get_message move2_7_over_dev));
  "move2_7_over_dev_current_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player move2_7_over_dev));
  "move2_7_over_dev_player_loc_p1" >::
  (fun _ -> assert_equal (Property "prop2") (get_player_location "p1" move2_7_over_dev));
  "move2_7_over_dev_player_money" >::
  (fun _ -> assert_equal 501 (get_player_money "p1" move2_7_over_dev));
  "move2_7_over_dev_property_stage" >::
  (fun _ -> assert_equal 3 (get_property_dev_stage "prop1" move2_7_over_dev));
  "move2_7_over_dev_property_rent" >::
  (fun _ -> assert_equal 20 (get_property_rent "prop1" move2_7_over_dev));
  "move2_7_dev_turntype" >::
  (fun _ -> assert_equal (Preroll) (get_turntype move2_7_exp_dev));
  "move2_7_exp_dev_message" >::
  (fun _ -> assert_equal (Some "You do not have enough money to develop that property.")
      (get_message move2_7_exp_dev));
  "move2_7_exp_dev_current_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player  move2_7_exp_dev));
  "move2_7_exp_dev_player_loc_p1" >::
  (fun _ -> assert_equal (Property "prop2") (get_player_location "p1" move2_7_exp_dev));
  "move2_7_exp_dev_player_money" >::
  (fun _ -> assert_equal 1001 (get_player_money "p1" move2_7_exp_dev));
  "move2_7_exp_dev_property_stage" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "prop2" move2_7_exp_dev));
  "move2_7_exp_dev_property_rent" >::
  (fun _ -> assert_equal 6 (get_property_rent "prop2" move2_7_exp_dev));
]

let one_r = test_game_r |> do' (Preroll_command Roll)
            |> do' (Land_unowned_prop_command Buy)
let two_r = one_r |> do' (Preroll_command Roll)
            |> do' (Land_other_prop_command PayRent)
            |> do' (Preroll_command Roll)
            |> do' (Land_unowned_prop_command Buy)
let r_dev_attempt = one_r |> do' (Preroll_command Roll)
                    |> do' (Land_other_prop_command PayRent)
                    |> do' (Preroll_command (Develop "railroad1"))

let railroad_tests =[
  "init_r" >::
  (fun _ -> assert_equal ["railroad1";"railroad2";"railroad3"]
      (get_properties_of_group Railroad test_game_r));
  "one_r_owner" >::
  (fun _ -> assert_equal (Player "p1") (get_property_owner "railroad1" one_r));
  "one_r_dev" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "railroad1" one_r));
  "two_r_owner_1" >::
  (fun _ -> assert_equal (Player "p1") (get_property_owner "railroad1" two_r));
  "two_r_owner_2" >::
  (fun _ -> assert_equal (Player "p1") (get_property_owner "railroad2" two_r));
  "two_r_dev_1" >::
  (fun _ -> assert_equal 1 (get_property_dev_stage "railroad1" two_r));
  "two_r_dev_2" >::
  (fun _ -> assert_equal 1 (get_property_dev_stage "railroad2" two_r));
  "two_r_dev_3" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "railroad3" two_r));
  "r_dev_attempt_message" >::
  (fun _ -> assert_equal (Some "You cannot pay to develop a Railroad")
      (get_message r_dev_attempt));
  "r_dev_attempt_dev" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "railroad1" r_dev_attempt));
]

let j1 = test_game_j |> do' (Preroll_command Roll) |> do' End_turn_command
let j2 = j1 |> do' (Preroll_command Roll) |> do' End_turn_command
         |> do' (Preroll_command Roll) |> do' End_turn_command
let j3 = j2 |> do' (Preroll_command Roll) |>
         do' End_turn_command |> do' (Preroll_injail_command UseCard)

let jail_tests = [
  "j1_cards" >::
  (fun _ -> assert_equal 1 (get_player_cards "p1" j1));
  "j2_jail" >::
  (fun _ -> assert_equal true (get_player_in_jail "p1" j2));
  "j3_cards" >::
  (fun _ -> assert_equal 0 (get_player_cards "p1" j3));
  "j3_jail" >::
  (fun _ -> assert_equal false (get_player_in_jail "p1" j3))
]

let sell_no_props_1 = do' (Preroll_command (SellProp "prop1")) test_game_2p

let sell_no_prop_tests = [
  "sell_no_props_1_message" >::
  (fun _ -> assert_equal (Some "You do not own that property.")
      (get_message sell_no_props_1));
  "sell_no_props_1_player_props" >::
  (fun _ -> assert_equal [] (get_player_properties "p1" sell_no_props_1));
  "sell_no_props_1_prop_owner" >::
  (fun _ -> assert_equal Bank (get_property_owner "prop1" sell_no_props_1));
  "sell_no_props_1_dev_stage" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "prop1" sell_no_props_1));
]

let develop_no_props_1 = do' (Preroll_command (Develop "prop1")) test_game_2p
let develop_no_props_r = do' (Preroll_command (Develop "railroad")) test_game_2p

let develop_no_prop_tests = [
  "develop_no_props_1_message" >::
  (fun _ -> assert_equal (Some "You do not own that property.")
      (get_message develop_no_props_1));
  "develop_no_props_1_player_props" >::
  (fun _ -> assert_equal [] (get_player_properties "p1" develop_no_props_1));
  "develop_no_props_1_prop_owner" >::
  (fun _ -> assert_equal Bank (get_property_owner "prop1" develop_no_props_1));
  "develop_no_props_1_dev_stage" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "prop1" develop_no_props_1));
  "develop_no_props_r_message" >::
  (fun _ -> assert_equal (Some "You do not own that property.")
      (get_message develop_no_props_1));
  "develop_no_props_r_player_props" >::
  (fun _ -> assert_equal [] (get_player_properties "railroad" develop_no_props_r));
  "develop_no_props_r_prop_owner" >::
  (fun _ -> assert_equal Bank (get_property_owner "railroad" develop_no_props_r));
  "develop_no_props_r_dev_stage" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "railroad" develop_no_props_r));
]

let trade_offer_has_no_props = Trade ("p2",[Prop "prop1"],[Mon 50])
let trade_request_has_no_props = Trade ("p2",[Mon 50],[Prop "prop1"])
let trade_offer_not_enough_money = Trade ("p2",[Mon 2000],[Mon 1000])
let trade_request_not_enough_money = Trade ("p2",[Mon 1000],[Mon 2000])
let trade_no_props_1 = do' (Preroll_command (trade_offer_has_no_props)) test_game_2p
let trade_no_props_2 = do' (Preroll_command (trade_request_has_no_props)) test_game_2p
let trade_no_props_3 = do' (Preroll_command (trade_offer_not_enough_money)) test_game_2p
let trade_no_props_4 = do' (Preroll_command (trade_request_not_enough_money)) test_game_2p

let trade_no_prop_tests = [
  "trade_no_props_1_message" >::
  (fun _ -> assert_equal (Some "You do not have the properties required for this trade.")
      (get_message trade_no_props_1));
  "trade_no_props_1_p1_props" >::
  (fun _ -> assert_equal [] (get_player_properties "p1" trade_no_props_1));
  "trade_no_props_1_p2_props" >::
  (fun _ -> assert_equal [] (get_player_properties "p2" trade_no_props_1));
  "trade_no_props_1_prop_owner" >::
  (fun _ -> assert_equal Bank (get_property_owner "prop1" trade_no_props_1));
  "trade_no_props_1_dev_stage" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "prop1" trade_no_props_1));
  "trade_no_props_2_message" >::
  (fun _ -> assert_equal (Some "The other player does not have the properties required for this trade.")
      (get_message trade_no_props_2));
  "trade_no_props_2_p1_props" >::
  (fun _ -> assert_equal [] (get_player_properties "p1" trade_no_props_2));
  "trade_no_props_2_p2_props" >::
  (fun _ -> assert_equal [] (get_player_properties "p2" trade_no_props_2));
  "trade_no_props_2_prop_owner" >::
  (fun _ -> assert_equal Bank (get_property_owner "prop1" trade_no_props_2));
  "trade_no_props_2_dev_stage" >::
  (fun _ -> assert_equal 0 (get_property_dev_stage "prop1" trade_no_props_2));
  "trade_no_props_3_message" >::
  (fun _ -> assert_equal (Some "You do not have enough money for this trade.")
      (get_message trade_no_props_3));
  "trade_no_props_3_p1_mon" >::
  (fun _ -> assert_equal 1500 (get_player_money "p1" trade_no_props_3));
  "trade_no_props_3_p2_mon" >::
  (fun _ -> assert_equal 1500 (get_player_money "p2" trade_no_props_3));
  "trade_no_props_4_message" >::
  (fun _ -> assert_equal (Some "The other player does not have enough money for this trade.")
      (get_message trade_no_props_4));
  "trade_no_props_4_p1_mon" >::
  (fun _ -> assert_equal 1500 (get_player_money "p1" trade_no_props_4));
  "trade_no_props_4_p2_mon" >::
  (fun _ -> assert_equal 1500 (get_player_money "p2" trade_no_props_4));
]

let trade_get_money = Trade ("p2",[],[Mon 1000])
let trade_offer_get_money = do' (Preroll_command (trade_get_money)) test_game_2p
let trade_decline_get_money = trade_offer_get_money
                              |> do' (Trade_offer_command DeclineTrade)
let trade_accept_get_money = trade_offer_get_money
                             |> do' (Trade_offer_command AcceptTrade)


let trade_money_tests =[
  "trade_offer_get_money_message" >::
  (fun _ -> assert_equal (Some "You have been offered a trade.")
      (get_message trade_offer_get_money));
  "trade_offer_turntype" >::
  (fun _ -> assert_equal (Trade_offer) (get_turntype trade_offer_get_money));
  "trade_offer_player" >::
  (fun _ -> assert_equal ("p2",AI1) (get_current_player trade_offer_get_money));
  "trade_no_props_4_p1_mon" >::
  (fun _ -> assert_equal 1500 (get_player_money "p1" trade_offer_get_money));
  "trade_no_props_4_p2_mon" >::
  (fun _ -> assert_equal 1500 (get_player_money "p2" trade_offer_get_money));
  "trade_decline_get_money_message" >::
  (fun _ -> assert_equal (Some "Your trade offer was declined")
      (get_message trade_decline_get_money));
  "trade_decline_get_money_turntype" >::
  (fun _ -> assert_equal (Preroll) (get_turntype trade_decline_get_money));
  "trade_decline_get_money_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player trade_decline_get_money));
  "trade_decline_get_money_p1_mon" >::
  (fun _ -> assert_equal 1500 (get_player_money "p1" trade_decline_get_money));
  "trade_decline_get_money_p2_mon" >::
  (fun _ -> assert_equal 1500 (get_player_money "p2" trade_decline_get_money));
  "trade_decline_get_money_message" >::
  (fun _ -> assert_equal (Some "Your trade offer was accepted and the trade was performed.")
      (get_message trade_accept_get_money));
  "trade_accept_get_money_turntype" >::
  (fun _ -> assert_equal (Preroll) (get_turntype trade_accept_get_money));
  "trade_accept_get_money_player" >::
  (fun _ -> assert_equal ("p1",GUI) (get_current_player trade_accept_get_money));
  "trade_accept_get_money_p1_mon" >::
  (fun _ -> assert_equal 2500 (get_player_money "p1" trade_accept_get_money));
  "trade_accept_get_money_p2_mon" >::
  (fun _ -> assert_equal 500 (get_player_money "p2" trade_accept_get_money));
]

let tests =
  "state test suite"  >::: List.flatten [
    init_tests;
    roll_test_board_tests;
    roll_test_board_2_tests;
    railroad_tests;
    jail_tests;
    sell_no_prop_tests;
    develop_no_prop_tests;
    trade_no_prop_tests;
    trade_money_tests;
  ]

let _ = run_test_tt_main tests
