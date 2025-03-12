let prompt s =
  print_string s;
  read_line ()

let create_dir_if_does_not_exist s =
  if BatSys.file_exists s then
    ()
  else
    BatUnix.mkdir s 0o700

let get_token_or_ask_user_for_one () =
  let open BatSys in
  let open BatPervasives in
  let home =
    let env = BatUnix.environment () in
    env
    |> Array.find_opt (String.starts_with ~prefix:"HOME=")
    |? "HOME=/home/marik"
    |> BatString.lchop ~n:5
  in
  let config = home ^ "/.config" in
  let telebot_token = config ^ "/telebot_token.txt" in
  let dir_exists s =
    file_exists s && is_directory s
  in
  if dir_exists config && file_exists telebot_token then begin
      BatString.trim @@ BatPervasives.input_file telebot_token
  end else begin
    let token = prompt "Provide telegram bot token: " in
    create_dir_if_does_not_exist config;
    BatFile.with_file_out telebot_token (
      fun f -> BatIO.write_line f token
    );
    token
  end