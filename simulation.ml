type game_t = {
  s : State.state option;
  json_fname : string option;
}

let rec run_one_sim s turns =
  if List.length (State.get_all_players s) = 1 then begin
    (* game has ended *)
    print_string "winner: ";
    print_endline State.(get_current_player s |> fst)
  end
  else if turns == 30000 then begin
    (*game has tied*)
    print_endline "game tied";
  end
  else begin
    match State.get_current_player s with
    (* game is not over *)
    | pid, State.GUI -> failwith "GUI is in state, shouldn't be."
    | pid, State.AI1 ->
        let (c, _) = Controllers.AI1.eval s in
        run_one_sim (State.do' c s) (turns+1)
    | pid, State.AI2 ->
        let (c, _) = Controllers.AI2.eval s in
        run_one_sim (State.do' c s) (turns+1)
    | pid, State.AI3 ->
        let (c, _) = Controllers.AI3.eval s in
        run_one_sim (State.do' c s) (turns+1)
  end

let run_sim num_sims s =
  for i = 1 to num_sims do
    (* run a simulation *)
    print_endline ("simulation "^(string_of_int i));
    let end_res = run_one_sim s 0 in
    (* end_res is the result *)
    ()
  done

let rec take_lines fchan n acc =
  if n = 0 then acc
  else begin
    take_lines fchan (n-1) (input_line fchan::acc)
  end

let controller_of_string = function
  | "AI1" -> State.AI1
  | "AI2" -> State.AI2
  | "AI3" -> State.AI3
  | _ -> failwith "Incorrect controller entered."

(** [main sim_fname] is the main game loop for the game. It sets up the GUI
  * and runs the game. *)
let main sim_fname =
  try
    let fchan = open_in sim_fname in
    let json_fname = input_line fchan in
    let num_sims = input_line fchan |> int_of_string in
    let num_players = input_line fchan |> int_of_string in
    let players = take_lines fchan num_players [] |> List.rev in
    let formatted_players =
      List.map (fun s -> 
        let player_info_lst = String.split_on_char ':' s in
        match player_info_lst with
        | name::cnt::[] -> name, controller_of_string cnt
        | _ -> failwith "Badly formatted players and controllers") 
      players in
    let s = State.init_state (Yojson.Basic.from_file json_fname) formatted_players ~random:true in
    close_in fchan;
    run_sim num_sims s
  with
  | e -> raise e

let () = print_endline "Please enter the name of the simulation file you want to run.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> print_endline "End of file."; exit 0
  | file_name -> try
      main ("sim_files/" ^ file_name)
    with
    | _ -> print_endline "Invalid simulation file or file path."; exit 0

