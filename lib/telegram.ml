module RQ = Telegram_types_reqs
module RS = Telegram_types_resps

module type StringT = sig val x : string end
module type TokenT = sig val token : string end

module Token(Tok: StringT) : TokenT = struct
  let token = Tok.x
end

let yojson_to_true_res body =
  let open Yojson.Safe.Util in
  let body_yojson = Yojson.Safe.(from_string body) in
  let ok = body_yojson |> member "ok" |> to_bool in
  let error = body_yojson |> member "error" |> to_string_option in
  let result = body_yojson |> member "result" |> to_bool_option in
  RS.true_res_of ok error result

let yojson_to_message_res body =
  let open Yojson.Safe.Util in
  let body_yojson = Yojson.Safe.(from_string body) in
  let ok = body_yojson |> member "ok" |> to_bool in
  let error = body_yojson |> member "error" |> to_string_option in
  let result = body_yojson |> member "result" |> Telegram_types_utils.to_message_option in
  RS.message_res_of ok error result

let yojson_to_user_res body =
  let open Yojson.Safe.Util in
  let body_yojson = Yojson.Safe.(from_string body) in
  let ok = body_yojson |> member "ok" |> to_bool in
  let error = body_yojson |> member "error" |> to_string_option in
  let res = body_yojson |> member "result" in
  Printf.printf "DEBUG: %s" (Yojson.Safe.pretty_to_string res);
  let result = body_yojson |> member "result" |> Telegram_types_utils.to_user_option in
  RS.user_res_of ok error result


module KeyboardButton = struct
  type t = Telegram_types.keyboard_button

  let basic_button : Telegram_types.keyboard_button = {
    text = "";
    request_users = None;
    request_chat = None;
    request_contact = None;
    request_location = None;
    request_poll = None;
    web_app = None;
  }

  let with_text txt (kb : t) = 
    { kb with text = txt }

  let with_request_contact (kb : t) =
    { kb with request_contact = Some true }

  let with_request_location (kb : t) =
    { kb with request_location = Some true }
end

module ReplyKeyboardMarkup = struct 
  type t = Telegram_types.reply_keyboard_markup

  let pack kb : Telegram_types.reply_markup_type = ReplyKeyboard kb
  let empty_keyboard : t =
    {
      keyboard = [];
      is_persistent = None;
      resize_keyboard = None;
      one_time_keyboard = None;
      input_field_placeholder = None;
      selective = None;
    }
  
  let with_keyboard kb reply_key : t =
    { reply_key with keyboard = kb }
  
  let with_persistent kb : t =
    { kb with is_persistent = Some true }
  
  let with_resize_keyboard kb : t =
    { kb with resize_keyboard = Some true }
  
  let with_one_time_keyboard kb : t =
    { kb with one_time_keyboard = Some true }
  
  let with_input_field_placeholder s kb : t =
    { kb with input_field_placeholder = Some s }
  
  let with_selective kb : t = 
    { kb with selective = Some true }
end

module InlineKeyboardButton = struct
  type t = Telegram_types.inline_keyboard_button

  let basic_button : t = {
    text = "";
    url = None;
    callback_data = None;
    web_app = None;
    login_url = None;
    switch_inline_query = None;
    switch_inline_query_current_chat = None;
    switch_inline_query_chosen_chat = None;
    copy_text_button = None;
  }

  let with_text s kb : t =
    { kb with text = s }
  
  let with_callback_data data btn : t =
    { btn with callback_data = Some data }
end

module InlineKeyboard = struct
  type t = Telegram_types.inline_keyboard_markup

  let empty_keyboard : t = {
    inline_keyboard = []
  }

  let keyboard_of kb = 
    Telegram_types.InlineKeyboard({ inline_keyboard = kb })

end

module ReplyParameters = struct
  type t = Telegram_types.reply_parameters

  let empty_parameters : t =
    {
      message_id = 0;
      chat_id = None;
      allow_sending_without_reply = None;
      quote = None;
      quote_parse_mode = None;
      quote_entities = None;
      quote_position = None;
    }

  let reply_to_message (msg : Telegram_types.message) : t =
    { empty_parameters with message_id = msg.message_id }

end

module SendMessage = struct
  type rsm = RQ.send_message
  
  let send_message_req_of (msg : Telegram_types.message) : rsm =
    let open Telegram_types in
    let business_connection_id = None in
    let chat_id = Chat(msg.chat.id) in
    let text = "" in
    let message_thread_id = None in
    let parse_mode = None in
    let entities = None in
    let link_preview_options = None in
    let disable_notification = None in
    let protect_content = None in
    let allow_paid_broadcast = None in
    let message_effect_id = None in
    let reply_parameters = None in
    let reply_markup = None in
    { business_connection_id
    ; chat_id
    ; text
    ; message_thread_id
    ; parse_mode
    ; entities
    ; link_preview_options
    ; disable_notification
    ; protect_content
    ; allow_paid_broadcast
    ; message_effect_id
    ; reply_parameters
    ; reply_markup
    }
  
  let send_message_to (chat : Telegram_types.chat) : rsm =
    let open Telegram_types in
    let business_connection_id = None in
    let chat_id = Chat(chat.id) in
    let text = "" in
    let message_thread_id = None in
    let parse_mode = None in
    let entities = None in
    let link_preview_options = None in
    let disable_notification = None in
    let protect_content = None in
    let allow_paid_broadcast = None in
    let message_effect_id = None in
    let reply_parameters = None in
    let reply_markup = None in
    { business_connection_id
    ; chat_id
    ; text
    ; message_thread_id
    ; parse_mode
    ; entities
    ; link_preview_options
    ; disable_notification
    ; protect_content
    ; allow_paid_broadcast
    ; message_effect_id
    ; reply_parameters
    ; reply_markup
    }
  
  let send_message_to_chat_of_instance (chat_instance : BatInt64.t) : rsm =
    let open Telegram_types in
    let business_connection_id = None in
    let chat_id = Chat(chat_instance) in
    let text = "" in
    let message_thread_id = None in
    let parse_mode = None in
    let entities = None in
    let link_preview_options = None in
    let disable_notification = None in
    let protect_content = None in
    let allow_paid_broadcast = None in
    let message_effect_id = None in
    let reply_parameters = None in
    let reply_markup = None in
    { business_connection_id
    ; chat_id
    ; text
    ; message_thread_id
    ; parse_mode
    ; entities
    ; link_preview_options
    ; disable_notification
    ; protect_content
    ; allow_paid_broadcast
    ; message_effect_id
    ; reply_parameters
    ; reply_markup
    }
  
  let with_text t (req : rsm) =
    { req with text = t }
  
  let as_markdownv2 (req : rsm) =
    { req with parse_mode = Some(Telegram_types.MarkDownV2)}
  
  let reply_parameters rp (req : rsm) =
    { req with reply_parameters = Some rp }

  let with_keyboard keyboard (req : rsm) =
    { req with reply_markup = Some(keyboard)}
end

module DeleteMessage = struct
  type rdm = RQ.delete_message
  let delete_message_of_chat (chat_id : BatInt64.t) mess_id : rdm =
    { chat_id = Chat chat_id; message_id = mess_id }
end

module BanChatMember = struct
  type bcm = RQ.ban_chat_member

  let ban_chat_member_of_chat (chat : Telegram_types.chat) (user : Telegram_types.user) : bcm =
    let chat_id = Telegram_types.Chat(chat.id) in
    let user_id = user.id in
    let until_date = None in
    let revoke_messages = None in
    { chat_id
    ; user_id
    ; until_date
    ; revoke_messages
    }

  let with_until_date date bcm : bcm =
    { bcm with until_date = Some(date) }
  
  let revoke_messages bcm : bcm =
    { bcm with revoke_messages = Some(true) }

  let do_not_revoke_messages bcm : bcm =
    { bcm with revoke_messages = Some(false) }
end

module LwtHttpBot(Token: TokenT) = struct
  open Lwt
  open Cohttp
  open Cohttp_lwt_unix

  type debug = Debug | NoDebug
  let debug_mode = ref NoDebug
  let set_debug m =
    debug_mode := m

  let switch_debug_on () = set_debug Debug
  let switch_debug_off () = debug_mode := NoDebug

  let with_forced_debug f =
    let prev_debug_mode = !debug_mode in
    switch_debug_on ();
    f ();
    set_debug prev_debug_mode

  let try_construct_message_update j : Telegram_types.update =
    Telegram_types_utils.json_to_update j

  let try_construct_message_update_list j =
    List.map try_construct_message_update j

  let make_get_basic s =
    Client.get (Uri.of_string s) >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (match !debug_mode with
    | Debug -> begin
        Printf.printf "Response code: %d\n" code;
        Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
        Printf.printf "Body: %s\n" body
      end
    | NoDebug -> ());
    body

  let make_get_basic_with_args s args =
    let combined_args = String.concat "&" (List.map (fun (a, b) -> a ^ "=" ^ b) args) in
    Client.get (Uri.of_string (s ^ "?" ^ combined_args)) >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (match !debug_mode with
    | Debug -> begin
      Printf.printf "Response code: %d\n" code;
      Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
      Printf.printf "Body: %s\n" body
      end
    | NoDebug -> ());
    body
  
  let make_post_basic s =
    Client.post (Uri.of_string s) >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (match !debug_mode with
    | Debug -> begin
        Printf.printf "Response code: %d\n" code;
        Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
        Printf.printf "Body: %s\n" body
      end
    | NoDebug -> ());
    body
  
  let make_post_basic_with_body s req_body =
    let j = Yojson.Safe.to_string req_body in
    let body = Cohttp_lwt.Body.of_string j in
    Client.post ~body (Uri.of_string s) 

  let merge_headers ha hb =
    Cohttp.Header.fold 
      (fun a b acc -> Cohttp.Header.add acc a b) 
      ha 
      hb

  let last_update_id = ref 0
  let json_header = Cohttp.Header.of_list ["Content-type", "application/json"]
  let token = Token.token
  let api_prefix = Printf.sprintf "https://api.telegram.org/bot%s" token
  let api_method s = Printf.sprintf "%s/%s" api_prefix s
  let api_method_with_args s args =
    Printf.sprintf "%s/%s?%s" api_prefix s
    (String.concat "&" (List.map (fun (a, b) -> a ^ "=" ^ b) args))

  let getMe =
    let getMe_string = Printf.sprintf "https://api.telegram.org/bot%s/getMe\n" token in
    make_get_basic getMe_string

  let updates_raw_body_to_yojson body =
    Yojson.Safe.from_string body

  let peekUpdates =
    let peekUpdates_string = api_method "getUpdates" in
    let open BatPervasives in
    let open Lwt in
    make_get_basic peekUpdates_string >>= (updates_raw_body_to_yojson %> Lwt.return)
  
  let getUpdates () =
    let (>>>) (x : unit t) f = x >>= fun _ -> f in
    let getUpdates_string = api_method "getUpdates" in
    let open Lwt in
    Lwt_io.printf "DEBUG: last_update_id: %d\n" !last_update_id >>>
    let args = [("offset", string_of_int (!last_update_id + 1))] in
    make_get_basic_with_args getUpdates_string args >>= fun v ->
    let res = updates_raw_body_to_yojson v in
    Printf.printf "DEBUG:\n%s\n" (Yojson.Safe.pretty_to_string res);
    Lwt.return res
  
  let rec poller f () =
    let (>>>) (x : unit t) f = x >>= fun _ -> f in
    let open Telegram_types in
    Lwt_io.printl "Executing poller" >>>
    getUpdates () >>= fun updates_json ->
    Lwt_io.printf "The update: %s\n" (Yojson.Safe.pretty_to_string updates_json) >>>
    let updates_opt = 
      Telegram_types_utils.yojson_to_message_update_from_results updates_json 
    in
    match updates_opt with
    | None -> Lwt_unix.sleep 5.0 >>= poller f
    | Some(updates) ->
    let highest_update_id =
      let extract_update_id ({ update_id; _ }: update) = update_id in
      updates
      |> List.map extract_update_id
      |> List.fold_left max (!last_update_id)
    in
    last_update_id := highest_update_id;
    updates
    |> List.map f
    |> List.cons (Lwt_unix.sleep 3.0)
    |> Lwt.join
    >>= poller f

  let rec poller_with_get_me (f : Telegram_types.user -> Telegram_types.update -> unit t) get_me () =
    let (>>>) (x : unit t) f = x >>= fun _ -> f in
    let open Telegram_types in
    Lwt_io.printl "Executing poller" >>>
    getUpdates () >>= fun updates_json ->
    Lwt_io.printf "The update: %s\n" (Yojson.Safe.pretty_to_string updates_json) >>>
    let updates_opt = 
      Telegram_types_utils.yojson_to_message_update_from_results updates_json 
    in
    match updates_opt with
    | None -> Lwt_unix.sleep 5.0 >>= poller_with_get_me f get_me
    | Some(updates) ->
    let highest_update_id =
      let extract_update_id ({ update_id; _ }: update) = update_id in
      updates
      |> List.map extract_update_id
      |> List.fold_left max (!last_update_id)
    in
    last_update_id := highest_update_id;
    updates
    |> List.map (f get_me)
    |> List.cons (Lwt_unix.sleep 3.0)
    |> Lwt.join
    >>= (poller_with_get_me f get_me)
  
  let rec ergonomic_poller f () =
    let open Telegram_types in
    getUpdates () >>= fun updates_json ->
    let updates_opt = Telegram_types_utils.yojson_to_message_update_from_results updates_json in
    match updates_opt with
    | None -> Lwt_unix.sleep 5.0 >>= ergonomic_poller f
    | Some(updates) ->
    let highest_update_id =
      let extract_update_id ({ update_id; _ }: update) = update_id in
      updates
      |> List.map extract_update_id
      |> List.fold_left max (!last_update_id - 1)
    in
    last_update_id := highest_update_id + 1;
    updates
    |> List.map 
        (fun x -> 
            (Telegram_types_utils.update_to_ergonomic_message_update x, x))
    |> List.map (fun (a, b) -> f a b)
    |> List.cons (Lwt_unix.sleep 5.0)
    |> Lwt.join
    >>= ergonomic_poller f
  let basic_debug resp body =
    let code = resp |> Response.status |> Code.code_of_status in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (match !debug_mode with
    | Debug -> begin
        (Lwt_io.printf "Response code: %d\n" code >>= fun _ ->
        Lwt_io.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string) >>= fun _ ->
        Lwt_io.printf "Body: %s\n" body >>= return) |> ignore
      end
    | NoDebug -> ());
    body
  
  let get_me () : RS.user_res t =
    let get_me_string = api_method "getMe" in
    let open Lwt in
    Client.get (Uri.of_string get_me_string) >>= fun (resp, body) ->
    basic_debug resp body >>= fun body ->
    return (yojson_to_user_res body)
  
  let poll f =
    let is_webhook_active s =
      let open Yojson.Safe.Util in
      let j = Yojson.Safe.from_string s in
      let webhook_info = j |> member "result" |> Telegram_types_utils.yojson_to_webhook_info in
      (* let webhook_info = Telegram_types_utils.yojson_to_webhook_info j in *)
      if webhook_info.url = "" then false else true
    in
    let disable_webhook () =
      let delete_webhook_url = api_method "deleteWebhook" in
      let drop_pending_updates = true in
      let (delete_webhook_req : Telegram_types_reqs.delete_webhook) = { drop_pending_updates = (BatOption.some drop_pending_updates) } in
      let delete_webhook_json = Telegram_types_reqs_yojson.delete_webhook_to_yojson delete_webhook_req in
      make_post_basic_with_body delete_webhook_url delete_webhook_json >>= fun (_, body) -> 
      Cohttp_lwt.Body.to_string body >>= fun body ->
      let _ = yojson_to_true_res body in
      Lwt.return ()
    in
    let disable_webhook_if_needed =
      let get_webhook_info = api_method "getWebhookInfo" in
      make_get_basic get_webhook_info >>= fun res ->
      if is_webhook_active res then
        disable_webhook ()
      else 
        Lwt.return ()

    in
    Lwt_main.run (disable_webhook_if_needed);
    Lwt_main.run (poller f ())
  let poll_with_get_me (f : Telegram_types.user -> Telegram_types.update -> unit t) =
    let is_webhook_active s =
      let open Yojson.Safe.Util in
      let j = Yojson.Safe.from_string s in
      let webhook_info = j |> member "result" |> Telegram_types_utils.yojson_to_webhook_info in
      (* let webhook_info = Telegram_types_utils.yojson_to_webhook_info j in *)
      if webhook_info.url = "" then false else true
    in
    let disable_webhook () =
      let delete_webhook_url = api_method "deleteWebhook" in
      let drop_pending_updates = true in
      let (delete_webhook_req : Telegram_types_reqs.delete_webhook) = { drop_pending_updates = (BatOption.some drop_pending_updates) } in
      let delete_webhook_json = Telegram_types_reqs_yojson.delete_webhook_to_yojson delete_webhook_req in
      make_post_basic_with_body delete_webhook_url delete_webhook_json >>= fun (_, body) -> 
      Cohttp_lwt.Body.to_string body >>= fun body ->
      let _ = yojson_to_true_res body in
      Lwt.return ()
    in
    let disable_webhook_if_needed =
      let get_webhook_info = api_method "getWebhookInfo" in
      make_get_basic get_webhook_info >>= fun res ->
      if is_webhook_active res then
        disable_webhook ()
      else 
        Lwt.return ()
    in
    let bot_get_me =
      let basic_get_me = Lwt_main.run (get_me ()) in
      match basic_get_me with
      | User(get_me) -> Some(get_me)
      | Error(s) -> Printf.printf "[ERROR] Couldn't perform getMe: %s\n" s; None
    in
    match bot_get_me with
    | Some(get_me') ->
      Lwt_main.run (disable_webhook_if_needed);
      Lwt_main.run (poller_with_get_me f get_me' ())
    | None -> ()
  
  let ergonomic_poll f =
    Lwt_main.run (ergonomic_poller f ())
  
  let send_message req : RS.send_message t =
    let sendMessageToUser_string = api_method "sendMessage" in
    let open BatPervasives in
    let open Lwt in
    let post_body =
      Cohttp_lwt.Body.of_string (
        let open Yojson.Safe in
        to_string @@ Telegram_types_reqs_yojson.send_message_to_yojson req
      )
    in
    Lwt_io.printf 
      "Sending data to %s:\n%s\n" 
        "sendMessage" 
        (Yojson.Safe.pretty_to_string @@ Telegram_types_reqs_yojson.send_message_to_yojson req) >>= fun _ ->
    Client.post (Uri.of_string sendMessageToUser_string)
      ~headers:json_header
      ~body:post_body >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (match !debug_mode with
    | Debug -> begin
        (Lwt_io.printf "Response code: %d\n" code >>= fun _ ->
        Lwt_io.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string) >>= fun _ ->
        Lwt_io.printf "Body: %s\n" body >>= return) |> ignore
      end
    | NoDebug -> ());
    yojson_to_message_res body
    (* Yojson.Safe.pretty_to_string body_yojson *)
  
  let delete_message (req : RQ.delete_message) : RS.delete_message t =
    let deleteMessage_string = api_method "deleteMessage" in
    let open BatPervasives in
    let open Lwt in
    let post_body =
      Cohttp_lwt.Body.of_string (
        let open Yojson.Safe in
        to_string @@ Telegram_types_reqs_yojson.delete_message_to_yojson req
      )
    in
    Client.post (Uri.of_string deleteMessage_string)
      ~headers:json_header
      ~body:post_body >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (match !debug_mode with
    | Debug -> begin
        (Lwt_io.printf "Response code: %d\n" code >>= fun _ ->
        Lwt_io.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string) >>= fun _ ->
        Lwt_io.printf "Body: %s\n" body >>= return) |> ignore
      end
    | NoDebug -> ());
    yojson_to_true_res body

  let ban_chat_member (req : RQ.ban_chat_member) : RS.ban_chat_member t =
    let ban_string = api_method "banChatMember" in
    let open BatPervasives in
    let open Lwt in
    let post_body =
      Cohttp_lwt.Body.of_string (
        let open Yojson.Safe in
        to_string @@ Telegram_types_reqs_yojson.ban_chat_member_to_yojson req
      )
    in
    Client.post (Uri.of_string ban_string)
      ~headers:json_header
      ~body:post_body >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (match !debug_mode with
    | Debug -> begin
        (Lwt_io.printf "Response code: %d\n" code >>= fun _ ->
        Lwt_io.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string) >>= fun _ ->
        Lwt_io.printf "Body: %s\n" body >>= return) |> ignore
      end
    | NoDebug -> ());
    yojson_to_true_res body
  
  
end