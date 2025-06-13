module RQ = Telegram_types_reqs
module RS = Telegram_types_resps

module type StringT = sig val x : string end
module type TokenT = sig val token : string end

module Token(Tok: StringT) : TokenT = struct
  let token = Tok.x
end

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
    let getUpdates_string = api_method "getUpdates" in
    let open BatPervasives in
    let open Lwt in
    Printf.printf "DEBUG: last_update_id: %d\n" !last_update_id;
    let args = [("offset", string_of_int !last_update_id)] in
    make_get_basic_with_args getUpdates_string args >>= 
    (updates_raw_body_to_yojson %> Lwt.return)
  
  let rec poller f () =
    let open Telegram_types in
    getUpdates () >>= fun updates_json ->
    let updates_opt = Telegram_types_utils.yojson_to_message_update_from_results updates_json in
    match updates_opt with
    | None -> Lwt_unix.sleep 5.0 >>= poller f
    | Some(updates) ->
    let highest_update_id =
      let extract_update_id ({ update_id; _ }: update) = update_id in
      updates
      |> List.map extract_update_id
      |> List.fold_left max (!last_update_id - 1)
    in
    last_update_id := highest_update_id + 1;
    updates
    |> List.map f
    |> List.cons (Lwt_unix.sleep 5.0)
    |> Lwt.join
    >>= poller f
  
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
  
  let poll f =
    Lwt_main.run (poller f ())
  
  let ergonomic_poll f =
    Lwt_main.run (ergonomic_poller f ())
  
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

  let send_message req : RS.send_message t =
    let sendMessageToUser_string = api_method "sendMessage" in
    let open BatPervasives in
    let open Lwt in
    let post_body =
      Cohttp_lwt.Body.of_string (
        let open Yojson.Safe in
        to_string @@ Telegram_types_utils.send_message_request_to_yojson req
      )
    in
    Lwt_io.printf "Sending data to %s:\n%s\n" "sendMessage" (Yojson.Safe.pretty_to_string @@ Telegram_types_utils.send_message_request_to_yojson req) >>= fun _ ->
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
        to_string @@ Telegram_types_utils.delete_message_request_to_yojson req
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
    let deleteMessage_string = api_method "banChatMember" in
    let open BatPervasives in
    let open Lwt in
    let post_body =
      Cohttp_lwt.Body.of_string (
        let open Yojson.Safe in
        to_string @@ Telegram_types_utils.ban_chat_member_request_to_yojson req
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
end