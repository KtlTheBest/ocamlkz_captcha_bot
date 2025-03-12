open Ocamlkz_captcha_bot

let () = 
  let token = Utils.get_token_or_ask_user_for_one () in
  let open Lwt in
  let open Telegram in
  let module BotToken = Token(struct 
      let x = token 
    end) 
  in
  let module Bot = LwtHttpBot(BotToken) in
  Bot.switch_debug_on ();
  (*
  let echo_f (upd : Telegram_types.update) =
    let open Bot in
    match upd.message with
    | None -> Printf.printf "Update is not a message...\n"; return ()
    | Some(msg) ->
      let msg_str = Telegram_types.show_message msg in
      Lwt_io.printf "DEBUG:\n%s\n" msg_str |> ignore;
      send_message @@ (
        send_message_req_of msg
        |> with_text ("```\n" ^ msg_str ^ "\n```")
        |> as_markdownv2
      ) >>= fun res ->
      Lwt_io.printf "%s\n" res |> ignore;
      send_message @@ (
        send_message_req_of msg
        |> with_text ("```\n" ^ res ^ "\n```")
        |> as_markdownv2
      ) >>= fun _ ->
      return ()
    (*
    match upd with
    | Message(msg_upd) ->
      let chat_id = msg_upd.chat_id in
      let contents = msg_upd.contents in
      (match contents with
      | Text(s) ->
        Bot.sendMessageToUser chat_id s >>= fun res ->
        Printf.printf "%s\n" res;
        return ()
      | UnknownContents -> return ())
    | Unknown(_) ->
      Printf.printf "%s\n" (Bot.show_update upd); return ()
    *)
  in
  *)
  let echo_f2 (upd : Telegram_types.ergonomic_message_update) (upd_raw : Telegram_types.update ) =
    let open Bot in
    match upd with
    | TextMessage(chat, _, _) ->
      (*
      let user_info =
        let ({ first_name; _ } : Telegram_types.user) = user in
        first_name
      in
      *)
      let rp = Telegram.ReplyParameters.(
        BatOption.bind (upd_raw.message) (fun x -> Some (reply_to_message x))
      ) in
      let kb = Telegram.InlineKeyboard.(
        let button_a = Telegram.InlineKeyboardButton.(
          basic_button
          |> with_text "Button A"
          |> with_callback_data "A"
        ) in
        let button_b = Telegram.InlineKeyboardButton.(
          basic_button
          |> with_text "Button B"
          |> with_callback_data "B"
        ) in
        keyboard_of [[button_a; button_b]]
      ) in
      send_message @@ (
        SendMessage.(
        send_message_to 
          chat
          |> with_text "Some text"
          |> reply_parameters (BatOption.get rp)
          (* |> with_text (Printf.sprintf "Received a text from user %s: %s" user_info msg) *)
          |> with_keyboard kb)
      ) >>= fun _ -> return ()
    | UnknownMessage(msg) ->
      let msg_str = Telegram_types.show_message msg in
      Lwt_io.printf "DEBUG:\n%s\n" msg_str |> ignore;
      SendMessage.(
      send_message @@ (
        send_message_req_of msg
        |> with_text ("```\n" ^ msg_str ^ "\n```")
        |> as_markdownv2)
      ) >>= fun res ->
      Lwt_io.printf "%s\n" (Telegram_types.show_return_type res) |> ignore;
      SendMessage.(
      send_message @@ (
        send_message_req_of msg
        |> with_text ("```\n" ^ (Telegram_types.show_return_type res) ^ "\n```")
        |> as_markdownv2)
      ) >>= fun _ ->
      return ()
    | _ -> return ()
  in
  (* Bot.poll echo_f *)
  Bot.ergonomic_poll echo_f2
