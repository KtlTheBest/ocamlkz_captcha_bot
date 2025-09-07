[@@@ocaml.warnings "-27-33"]
open Ocamlkz_captcha_bot

module PairII = struct
  type t = (BatInt64.t * BatInt64.t)
  let compare ((aa, ab) : t) ((ba, bb) : t) =
    let res = compare aa ba in
    if res = 0 then
      compare ab bb
    else
      res
end

module PairIII = struct
  type t = (BatInt64.t * BatInt64.t * int)
  let compare ((aa, ab, ac) : t) ((ba, bb, bc) : t) =
    let res = compare aa ba in
    if res = 0 then (
      let res2 = compare ab bb in
      if res2 = 0 then
        compare ac bc
      else
        res2
    )
    else
      res
end

module PairIIIMap = Map.Make(PairIII)
module PairIIMap  = Map.Make(PairII)
module IMap       = Map.Make(BatInt64)

let captcha_answers = ref PairIIIMap.empty
let captcha_timeouts = ref PairIIIMap.empty
let unverified_users_messages = ref PairIIMap.empty


let () =
  let token = Utils.get_token_or_ask_user_for_one () in
  let open Lwt in
  let open Telegram in
  let open Telegram_types in

  let l_ignore v = v >>= fun _ -> return () in

  let module BotToken = Token(struct
      let x = token
    end)
  in
  let module Bot = LwtHttpBot(BotToken) in
  
  let check_command f (message : message) =
    let is_command (x : message_entity) = x._type = BotCommand in
    let extract_commands text entities =
      let for_each_entity { offset; length; _ } = BatString.sub text offset length in
      List.map for_each_entity entities
    in
    if message.entities = [] || message.text = None then
      false
    else
      let open BatOption in
      let text = get message.text in
      let entities = message.entities in
      let commands = List.filter (is_command) entities in
      let extracted_commands = extract_commands text commands in
      f extracted_commands
      (*
      match extracted_commands with
      | ["/start"] -> true
      | _ -> false
      *)
  in
  
  let start_command_message (message : message) =
    let f = function
      | ["/start"] -> true
      | _ -> false
    in
    check_command f message
    (*
    if message.entities = [] || message.text = None then
      false
    else
      let open BatOption in
      let text = get message.text in
      let entities = message.entities in
      let commands = List.filter (is_command) entities in
      let extracted_commands = extract_commands text commands in
      match extracted_commands with
      | ["/start"] -> true
      | _ -> false
    *)
  in
  
  let spam_command_message message =
    let f = function
      | ["/spam"] | ["/s"] -> true
      | _ -> false
    in
    check_command f message
  in
  
  let print_start_help (update : update) =
    let text = "This is a help message" in
    let message = BatOption.get update.message in
    let msg_req = 
      SendMessage.(
      send_message_req_of message
      |> with_text text
      )
    in
    Bot.send_message msg_req
  in

  let trigger_spam_action (message : message) =
    let text = "This is trigger spam action" in
    let msg_req =
      SendMessage.(
      send_message_req_of message
      |> with_text text
      )
    in
    Bot.send_message msg_req
  in

  let captcha_single_new_user user_join_message (new_user : user) =
    let create_mention (new_user : user) =
      let open BatOption in
      let id = new_user.id in
      let name = 
        if is_none (new_user.last_name) then 
          new_user.first_name 
        else 
          new_user.first_name ^ " " ^ get new_user.last_name 
      in
      Printf.sprintf "[%s](tg://user?id=%s)" name (BatInt64.to_string id)
    in
    let mention = create_mention new_user in
    let create_arithmetic_question () =
      let module ArithmeticExpression = 
        struct 
          type t =
            | Add of t * t
            | Sub of t * t
            | Mul of t * t
            | Int of int

          type tt =
            | AddT
            | SubT
            | MulT

          let rec expr_to_string v =
            let f = expr_to_string in
            match v with
            | Int(i) -> Printf.sprintf "%d" i
            | Add(a, b) -> Printf.sprintf "%s \\+ %s" (f a) (f b)
            | Sub(a, b) -> Printf.sprintf "%s \\- %s" (f a) (f b)
            | Mul(Add(aa, ab), Add(ba, bb)) -> Printf.sprintf "\\(%s \\+ %s\\) * \\(%s \\+ %s\\)" (f aa) (f ab) (f ba) (f bb)
            | Mul(Add(aa, ab), Sub(ba, bb)) -> Printf.sprintf "\\(%s \\+ %s\\) * \\(%s \\- %s\\)" (f aa) (f ab) (f ba) (f bb)
            | Mul(Sub(aa, ab), Add(ba, bb)) -> Printf.sprintf "\\(%s \\- %s\\) * \\(%s \\+ %s\\)" (f aa) (f ab) (f ba) (f bb)
            | Mul(Sub(aa, ab), Sub(ba, bb)) -> Printf.sprintf "\\(%s \\- %s\\) * \\(%s \\- %s\\)" (f aa) (f ab) (f ba) (f bb)
            | Mul(a, b) -> Printf.sprintf "%s \\* %s" (f a) (f b)

          let rec calc_answer v =
            match v with
            | Int(i) -> i
            | Add(a, b) -> calc_answer a + calc_answer b
            | Sub(a, b) -> calc_answer a - calc_answer b
            | Mul(a, b) -> calc_answer a * calc_answer b

          let rec create_expr_of_depth depth =
            let resolve_tt v =
              let a, b =
                if Random.bool () then
                  let a = create_expr_of_depth (depth - 1) in
                  let b = create_expr_of_depth (depth - 1) in
                  (a, b)
                else
                  (if Random.bool () then
                    let a = create_expr_of_depth (depth - 1) in
                    let b = Int(Random.int 10) in
                    (a, b)
                  else
                    let a = Int(Random.int 10) in
                    let b = create_expr_of_depth (depth - 1) in
                    (a, b))
              in
              match v with
              | AddT -> Add(a, b)
              | SubT -> Sub(a, b)
              | MulT -> Mul(a, b)
            in
            if depth = 0 then Int(Random.int 10) else
            resolve_tt @@
              BatOption.get @@
                BatEnum.get @@
                  BatRandom.multi_choice 1 @@ 
                  ([ AddT; SubT; MulT ] 
                  |> List.map (BatEnum.singleton) 
                  |> List.fold_left (BatEnum.append) (BatEnum.empty ())
                  )

        end 
      in
      let q = ArithmeticExpression.create_expr_of_depth 1 in
      let a = ArithmeticExpression.calc_answer q in
      let q_as_s = ArithmeticExpression.expr_to_string q in
      (q_as_s, a)
    in
    let captcha_question () =
      let (q, a) = create_arithmetic_question () in
      let rec create_other_answer l =
        let a = List.hd l in
        let variant = BatRandom.int 20 in
        let variant2 = variant - 10 in
        let variant3 = a + variant2 in
        if List.exists (fun x -> x = variant3) l then 
          create_other_answer l 
        else 
          variant3
      in
      let ans1 = create_other_answer [a] in
      let ans2 = create_other_answer [a; ans1] in
      let arr = [a; ans1; ans2] in
      let shuffled_arr = BatList.shuffle arr in
      let timeout = 60 in
      (Printf.sprintf "User %s, please answer the following captcha within %d seconds:\n\n%s" mention timeout q), shuffled_arr, a
    in
    let (q, opts, a) = captcha_question () in
    let keyboard =
      let open InlineKeyboard in
      let open InlineKeyboardButton in
      let make_button x =
        basic_button
        |> with_text (string_of_int x)
        |> with_callback_data (string_of_int x)
      in
      keyboard_of [List.map make_button opts]
    in
    Bot.send_message
    SendMessage.(
      send_message_req_of user_join_message
      |> with_text q
      |> as_markdownv2
      |> with_keyboard keyboard
    ) >>= fun quiz_message ->
    match quiz_message with
    | Error _ -> return ()
    | Message(quiz_message') ->
      let get_combo () =
        let id = quiz_message'.message_id in
        let user_id = new_user.id in
        let chat = quiz_message'.chat in
        let chat_id = chat.id in
        let combo = (chat_id, user_id, id) in
        combo
      in
      (*
      let add_unverified_user () =
        let combo =
          let chat_id = m.chat.id in
          let user_id = new_user.id in
          (chat_id, user_id)
        in
        let cur_map = !unverified_users_messages in
        let cur_messages = PairIIMap.find_opt combo cur_map in
        match cur_messages with
        | Some(_) -> ()
        | None ->
          let new_map = PairIIMap.add combo [] cur_map in
          unverified_users_messages := new_map
      in
      *)
      let add_new_answer () =
        let combo = get_combo () in
        let cur_map = !captcha_answers in
        let new_map = PairIIIMap.add combo a cur_map in
        captcha_answers := new_map
      in
      let add_new_timer () =
        let timeout = 60 (* 1 minute *) in
        let failed_captcha () : unit =
          let message = Printf.sprintf "User %s has failed to accomplish the captcha within %d seconds, kicking the \"user\"" mention timeout in
          let send_req =
            SendMessage.(
            send_message_to quiz_message'.chat
            |> with_text message
            |> as_markdownv2
            )
          in
          let shoot_requests () =
            Bot.send_message send_req >>= fun (sent_message : RS.send_message)->
            match sent_message with
            | Error(x) -> Lwt_io.printf "[ERROR] Couldn't send a notification message: %s\n" x
            | Message(sent_message') ->
            let ban_chat_member_req =
              BanChatMember.(ban_chat_member_of_chat quiz_message'.chat new_user)
            in
            Bot.ban_chat_member ban_chat_member_req >>= fun banned_message ->
            (match banned_message with
            | Error(s) -> Lwt_io.printf "[ERROR] Couldn't ban a user: %s\n" s
            | True ->
            let make_delete_request (msg : message) =
              DeleteMessage.delete_message_of_chat (msg.chat.id) (msg.message_id)
            in
            let cur_map = !unverified_users_messages in
            let combo = (quiz_message'.chat.id, new_user.id) in
            let unverified_messages = PairIIMap.find_opt combo cur_map in
            (
            match unverified_messages with
            | None -> ()
            | Some(messages) ->
              List.map (fun x -> Bot.delete_message @@ make_delete_request x) messages |> ignore;
              let new_map = PairIIMap.remove combo cur_map in
              unverified_users_messages := new_map;
              ()
            )
            ;
            let delete_message_1 = make_delete_request quiz_message' in
            let delete_message_2 = make_delete_request sent_message' in
            let delete_message_3 = make_delete_request user_join_message in
            List.map (Bot.delete_message) [ delete_message_1; delete_message_2; delete_message_3 ] |> ignore;
            return ()
            )
            (* Take a short break of 5 sec and then delete bot messages *)
          in
          shoot_requests () |> ignore;
        in
        let timeout_task = Lwt_timeout.create timeout failed_captcha in
        let combo = get_combo () in
        let cur_map = !captcha_timeouts in
        let new_map = PairIIIMap.add combo timeout_task cur_map in
        captcha_timeouts := new_map;
        Lwt_timeout.start timeout_task
      in
      add_new_answer ();
      (* add_unverified_user (); *)
      add_new_timer ();
      return ()
    (* Lwt_io.printf "%s" (show_return_type res) *)
  in 

  let captcha_new_joined_users message = function
    | [] -> return ()
    | new_users ->
      (* List.map (captcha_single_new_user message) new_users |> ignore |> return *)
      List.map (captcha_single_new_user message) new_users |> ignore |> return
  in

  let is_new_users_joined (update : update) f =
    match update.message with
    | Some(message) -> f message message.new_chat_members
    | None -> return ()
  in

  let callback_query_received (update : update) f =
    match update.callback_query with
    | Some(cbq) -> f cbq
    | None -> return ()
  in

  let any_message_from_user (update : update) f =
    match update.message with
    | Some(m) -> f m
    | None -> return ()
  in

  let any_kick_messages_from_bot bot_id (update : update) f =
    match update.message with
    | Some(m) -> 
      let from_user = m.from in
      let left_chat_member = m.left_chat_member in
      (match from_user with
      | Some(from_user') ->
        (match left_chat_member with
        | Some(_) ->
          (* check that we are the sender of the message *)
          if from_user'.id = bot_id then
            f m
          else
            return ()
        | None -> return ())
      | None -> return ())
    | None -> return ()
  in

  let respond_to_captcha (cbq : callback_query) =
    let open BatOption in
    let user_id = cbq.from.id in
    let message = get (cbq.message) in
    match message with
    | Message(msg) -> 
      let check_from_unverified (msg : message) =
        let chat_id = msg.chat.id in
        let user_id = msg.from in
        match user_id with
        | None -> ()
        | Some(from) -> 
          let combo = (chat_id, from.id) in
          let cur_map = !unverified_users_messages in
          let cur_messages = PairIIMap.find_opt combo cur_map in
          (match cur_messages with
          | None -> ()
          | Some(msgs) ->
            let msgs' = msg :: msgs in
            let new_map = PairIIMap.add combo msgs' cur_map in
            unverified_users_messages := new_map
          )
      in
      check_from_unverified msg;
      let message_id = msg.message_id in
      let chat_id = msg.chat.id in
      let cur_map = !captcha_answers in
      let combo = (chat_id, user_id, message_id) in
      let ans = PairIIIMap.find_opt combo cur_map in
      let data = get cbq.data in
      (
      match ans with
      | Some(ans') -> (
        if string_of_int ans' = data then (
          Bot.send_message
          SendMessage.(
            send_message_to_chat_of_instance chat_id
            |> with_text "Correct!"
          )
          |> l_ignore |> ignore;
          let cur_map = !captcha_timeouts in
          let cur_timer = PairIIIMap.find_opt combo cur_map in
          (
            match cur_timer with
            | None -> return ()
            | Some(t) -> 
              Lwt_timeout.stop t;
              let new_map = PairIIIMap.remove combo cur_map in
              captcha_timeouts := new_map;
              return ()
          )
        )
        else
          Bot.send_message
          SendMessage.(
            send_message_to_chat_of_instance chat_id
            |> with_text "Failure!"
          ) |> l_ignore
        )
      | None ->
        Bot.send_message
        SendMessage.(
          send_message_to_chat_of_instance chat_id
          |> with_text "Failure 2!"
        )
        |> l_ignore
      )
    | InaccessibleMessage(_) -> return ()
  in

  let record_unverified_messages (m : message) =
    let f () =
      let (>>=) = BatOption.bind in
      m.from >>= fun u ->
      let user_id = u.id in
      let chat_id = m.chat.id in
      let cur_map = !unverified_users_messages in
      let user_messages = PairIIMap.find_opt (chat_id, user_id) cur_map in
      let new_user_messages =
        match user_messages with
        | Some(l) -> m :: l
        | None -> []
      in
      let new_map = PairIIMap.add (chat_id, user_id) new_user_messages cur_map in
      unverified_users_messages := new_map;
      BatOption.some ()
    in
    f () |> ignore |> return
  in

  let delete_those_messages message =
    let make_delete_request (msg : message) =
      DeleteMessage.delete_message_of_chat (msg.chat.id) (msg.message_id)
    in
    let delete_request = make_delete_request message in
    Bot.delete_message delete_request |> l_ignore
  in

  Bot.switch_debug_on ();
  let captcha_bot (bot_get_me : user) (upd : Telegram_types.update) =
    let bot_id = bot_get_me.id in
    is_new_users_joined upd (captcha_new_joined_users) |> ignore;
    callback_query_received upd (respond_to_captcha) |> ignore;
    any_message_from_user upd (record_unverified_messages) |> ignore;
    any_kick_messages_from_bot bot_id upd (delete_those_messages) |> ignore;
    match upd.message with
    | Some(message) -> 
      if start_command_message message then print_start_help upd |> l_ignore else
      if spam_command_message message then trigger_spam_action message |> l_ignore else
      let message_str = show_message message in
      Lwt_io.printf "Message:\n%s\n" message_str
    | None ->
      Lwt_io.printf "Update:\n%s\n" (show_update upd)
  in
  Bot.poll_with_get_me captcha_bot
