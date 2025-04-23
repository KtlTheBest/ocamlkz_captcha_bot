open Telegram_types

let assoc_to_json l : Yojson.Safe.t =
  let l' = List.fold_left (@) [] l in
  `Assoc l'

let formatting_option_to_yojson_string x =
  match x with
  | NoFormat -> None
  | MarkDown -> Some(`String "Markdown")
  | MarkDownV2 -> Some(`String "MarkdownV2")
  | Html -> Some(`String "HTML")

let message_entity_type_to_yojson = function
  | Mention -> `String "mention"
  | Hashtag -> `String "hashtag"
  | Cashtag -> `String "cashtag"
  | BotCommand -> `String "bot_command"
  | Url -> `String "url"
  | Email -> `String "email"
  | PhoneNumber -> `String "phone_number"
  | Bold -> `String "bold"
  | Italic -> `String "italic"
  | Underline -> `String "underline"
  | Strikethrough -> `String "strikethrough"
  | Spoiler -> `String "spoiler"
  | Blockquote -> `String "blockquote"
  | ExpandableBlockquote -> `String "expandable_blockquote"
  | Code -> `String "code"
  | Pre -> `String "pre"
  | TextLink -> `String "text_link"
  | TextMention -> `String "text_mention"
  | CustomEmoji -> `String "custom_emoji"

let user_to_yojson (u : user) =
  let id = [("id", `Intlit (BatInt64.to_string u.id))] in
  let is_bot = [("is_bot", `Bool u.is_bot)] in
  let first_name = [("first_name", `String u.first_name)] in
  let last_name =
    match u.last_name with
    | None -> []
    | Some(ln) -> [("last_name", `String ln)]
  in
  let username =
    match u.username with
    | None -> []
    | Some(un) -> [("username", `String un)]
  in
  let is_premium = [("is_premium", `Bool u.is_premium)] in
  let added_to_attachement_menu = [("added_to_attachment_menu", `Bool u.added_to_attachement_menu)] in
  assoc_to_json [id; is_bot; first_name; last_name; username; is_premium; added_to_attachement_menu]

let message_entity_to_yojson (x: message_entity) =
  let _type = [("type", message_entity_type_to_yojson x._type)] in
  let offset = [("offset", `Int x.offset)] in
  let length = [("length", `Int x.length)] in
  let url =
    match x.url with
    | None -> []
    | Some(u) -> [("url", `String u)]
  in
  let user =
    match x.user with
    | None -> []
    | Some(u) -> [("user", user_to_yojson u)]
  in
  let language =
    match x.language with
    | None -> []
    | Some(l) -> [("language", `String l)]
  in
  let custom_emoji_id =
    match x.custom_emoji_id with
    | None -> []
    | Some(cei) -> [("custom_emoji_id", `String cei)]
  in
  assoc_to_json [_type; offset; length; url; user; language; custom_emoji_id]

let message_entity_list_to_yojson l =
  let rec loop l =
    match l with
    | [] -> []
    | x :: rest ->
      message_entity_to_yojson x :: loop rest
  in
  `List (loop l)

let link_preview_options_to_yojson lpo =
  let is_disabled = [("is_disabled", `Bool lpo.is_disabled)] in
  let url =
    match lpo.url with
    | None -> []
    | Some(u) -> [("url", `String u)]
  in
  let prefer_small_media =
    match lpo.prefer_small_media with
    | true -> [("prefer_small_media", `Bool true)]
    | false -> []
  in
  let prefer_large_media =
    match lpo.prefer_large_media with
    | true -> [("prefer_large_media", `Bool true)]
    | false -> []
  in
  let show_above_text =
    match lpo.show_above_text with
    | true -> [("show_above_text", `Bool true)]
    | false -> []
  in
  assoc_to_json [is_disabled; url; prefer_small_media; prefer_large_media; show_above_text]

let reply_parameters_to_yojson (rp : reply_parameters) =
  let message_id = [("message_id", `Int rp.message_id)] in
  let chat_id =
    match rp.chat_id with
    | None -> []
    | Some(ci) ->
      (match ci with
      | Chat(i) -> [("chat_id", `Intlit (BatInt64.to_string i))]
      | Channel(c) -> [("chat_id", `String c)])
  in
  let allow_sending_without_reply =
    match rp.allow_sending_without_reply with
    | None -> []
    | Some(aswr) -> [("allow_sending_without_reply", `Bool aswr)]
  in
  let quote =
    match rp.quote with
    | None -> []
    | Some(q) -> [("quote", `String q)]
  in
  let quote_parse_mode =
    match rp.quote_parse_mode with
    | None -> []
    | Some(qpm) -> 
      (match formatting_option_to_yojson_string qpm with
      | None -> []
      | Some(res) ->
        [("quote_parse_mode", res)]
      )
  in
  let quote_entities =
    match rp.quote_entities with
    | None -> []
    | Some(qe) -> [("quote_entities", message_entity_list_to_yojson qe)]
  in
  let quote_position =
    match rp.quote_position with
    | None -> []
    | Some(qp) -> [("quote_position", `Int qp)]
  in
  assoc_to_json [message_id; chat_id; allow_sending_without_reply; quote; quote_parse_mode; quote_entities; quote_position]

let web_app_info_to_yojson (wa : web_app_info) =
  assoc_to_json [[("url", `String wa.url)]]

let login_url_to_yojson (lu : login_url) =
  let url = [("url", `String lu.url)] in
  let forward_text =
    match lu.forward_text with
    | None -> []
    | Some(ft) -> [("forward_text", `String ft)]
  in
  let bot_username =
    match lu.bot_username with
    | None -> []
    | Some(bu) -> [("bot_username", `String bu)]
  in
  let request_write_access =
    match lu.request_write_access with
    | None -> []
    | Some(rwa) -> [("request_write_access", `Bool rwa)]
  in
  assoc_to_json [url; forward_text; bot_username; request_write_access]

let switch_inline_query_chosen_chat_to_yojson (siqcc : switch_inline_query_chosen_chat) =
  let query =
    match siqcc.query with
    | None -> []
    | Some(q) -> [("query", `String q)]
  in
  let allow_user_chats =
    match siqcc.allow_user_chats with
    | None -> []
    | Some(auc) -> [("allow_user_chats", `Bool auc)]
  in
  let allow_bot_chats =
    match siqcc.allow_bot_chats with
    | None -> []
    | Some(abc) -> [("allow_bot_chats", `Bool abc)]
  in
  let allow_group_chats =
    match siqcc.allow_group_chats with
    | None -> []
    | Some(agc) -> [("allow_group_chats", `Bool agc)]
  in
  let allow_channel_chats =
    match siqcc.allow_channel_chats with
    | None -> []
    | Some(acc) -> [("allow_channel_chats", `Bool acc)]
  in
  assoc_to_json [query; allow_user_chats; allow_bot_chats; allow_group_chats; allow_channel_chats]

let copy_text_button_to_yojson (ctb : copy_text_button) =
  assoc_to_json [[("text", `String ctb.text)]]

let inline_keyboard_button_to_yojson (ikb : inline_keyboard_button) =
  let text = [("text", `String ikb.text)] in
  let url =
    match ikb.url with
    | None -> []
    | Some(u) -> [("url", `String u)]
  in
  let callback_data =
    match ikb.callback_data with
    | None -> []
    | Some(cd) -> [("callback_data", `String cd)]
  in
  let web_app =
    match ikb.web_app with
    | None -> []
    | Some(wa) -> [("web_app", web_app_info_to_yojson wa)]
  in
  let login_url =
    match ikb.login_url with
    | None -> []
    | Some(lu) -> [("login_url", login_url_to_yojson lu)]
  in
  let switch_inline_query =
    match ikb.switch_inline_query with
    | None -> []
    | Some(siq) -> [("switch_inline_query", `String siq)]
  in
  let switch_inline_query_current_chat =
    match ikb.switch_inline_query_current_chat with
    | None -> []
    | Some(siqcc) -> [("switch_inline_query_current_chat", `String siqcc)]
  in
  let switch_inline_query_chosen_chat =
    match ikb.switch_inline_query_chosen_chat with
    | None -> []
    | Some(siqcc) -> [("switch_inline_query_chosen_chat", switch_inline_query_chosen_chat_to_yojson siqcc)]
  in
  let copy_text_button =
    match ikb.copy_text_button with
    | None -> []
    | Some(ctb) -> [("copy_text_button", copy_text_button_to_yojson ctb)]
  in
  assoc_to_json [
    text;
    url;
    callback_data;
    web_app;
    login_url;
    switch_inline_query;
    switch_inline_query_current_chat;
    switch_inline_query_chosen_chat;
    copy_text_button;
  ]

let inline_keyboard_button_list_to_yojson ikbl =
  `List (List.map inline_keyboard_button_to_yojson ikbl)

let inline_keyboard_button_list_list_to_yojson ikbll =
  `List (List.map inline_keyboard_button_list_to_yojson ikbll)

let keyboard_button_request_users_to_yojson (ru : keyboard_button_request_users) =
  let request_id = [("request_id", `Int ru.request_id)] in
  let user_is_bot =
    match ru.user_is_bot with
    | None -> []
    | Some(uib) -> [("user_is_bot", `Bool uib)]
  in
  let user_is_premium =
    match ru.user_is_premium with
    | None -> []
    | Some(uip) -> [("user_is_premium", `Bool uip)]
  in
  let max_quantity =
    match ru.max_quantity with
    | None -> []
    | Some(mq) -> [("max_quantity", `Int mq)]
  in
  let request_name =
    match ru.request_name with
    | None -> []
    | Some(rn) -> [("request_name", `Bool rn)]
  in
  let request_username =
    match ru.request_username with
    | None -> []
    | Some(ru) -> [("request_username", `Bool ru)]
  in
  let request_photo =
    match ru.request_photo with
    | None -> []
    | Some(rp) -> [("request_photo", `Bool rp)]
  in
  assoc_to_json [
    request_id;
    user_is_bot;
    user_is_premium;
    max_quantity;
    request_name;
    request_username;
    request_photo
  ]

let chat_administrator_rights_to_yojson (car : chat_administrator_rights) =
  let is_anonymous = [("is_anonymous", `Bool car.is_anonymous)] in
  let can_manage_chat = [("can_manage_chat", `Bool car.can_manage_chat)] in
  let can_delete_messages = [("can_delete_messages", `Bool car.can_delete_messages)] in
  let can_manage_video_chats = [("can_manage_video_chats", `Bool car.can_manage_video_chats)] in
  let can_restrict_members = [("can_restrict_members", `Bool car.can_restrict_members)] in
  let can_promote_members = [("can_promote_members", `Bool car.can_promote_members)] in
  let can_change_info = [("can_change_info", `Bool car.can_change_info)] in
  let can_invite_users = [("can_invite_users", `Bool car.can_invite_users)] in
  let can_post_stories = [("can_post_stories", `Bool car.can_post_stories)] in
  let can_edit_stories = [("can_edit_stories", `Bool car.can_edit_stories)] in
  let can_delete_stories = [("can_delete_stories", `Bool car.can_delete_stories)] in
  let can_post_messages =
    match car.can_post_messages with
    | None -> []
    | Some(cpm) -> [("can_post_messages", `Bool cpm)]
  in
  let can_edit_messages =
    match car.can_edit_messages with
    | None -> []
    | Some(cem) -> [("can_edit_messages", `Bool cem)]
  in
  let can_pin_messages =
    match car.can_pin_messages with
    | None -> []
    | Some(cpm) -> [("can_pin_messages", `Bool cpm)]
  in
  let can_manage_topics =
    match car.can_manage_topics with
    | None -> []
    | Some(cmt) -> [("can_manage_topics", `Bool cmt)]
  in
  assoc_to_json [
    is_anonymous;
    can_manage_chat;
    can_delete_messages;
    can_manage_video_chats;
    can_restrict_members;
    can_promote_members;
    can_change_info;
    can_invite_users;
    can_post_stories;
    can_edit_stories;
    can_delete_stories;
    can_post_messages;
    can_edit_messages;
    can_pin_messages;
    can_manage_topics;
  ]

let keyboard_button_request_chat_to_yojson (rc : keyboard_button_request_chat) =
  let request_id = [("request_id", `Int rc.request_id)] in
  let chat_is_channel = [("chat_is_channel", `Bool rc.chat_is_channel)] in
  let chat_is_forum =
    match rc.chat_is_forum with
    | None -> []
    | Some(cif) -> [("chat_is_forum", `Bool cif)]
  in
  let chat_has_username =
    match rc.chat_has_username with
    | None -> []
    | Some(chu) -> [("chat_has_username", `Bool chu)]
  in
  let chat_is_created =
    match rc.chat_is_created with
    | None -> []
    | Some(cic) -> [("chat_is_created", `Bool cic)]
  in
  let user_administrator_rights =
    match rc.user_administrator_rights with
    | None -> []
    | Some(uar) -> [("user_administrator_rights", chat_administrator_rights_to_yojson uar)]
  in
  let bot_administrator_rights =
    match rc.bot_administrator_rights with
    | None -> []
    | Some(bar) -> [("bot_administrator_rights", chat_administrator_rights_to_yojson bar)]
  in
  let bot_is_member =
    match rc.bot_is_member with
    | None -> []
    | Some(bim) -> [("bot_is_member", `Bool bim)]
  in
  let request_title =
    match rc.request_title with
    | None -> []
    | Some(rt) -> [("request_title", `Bool rt)]
  in
  let request_username =
    match rc.request_username with
    | None -> []
    | Some(ru) -> [("request_username", `Bool ru)]
  in
  let request_photo =
    match rc.request_photo with
    | None -> []
    | Some(rp) -> [("request_photo", `Bool rp)]
  in
  assoc_to_json [
    request_id;
    chat_is_channel;
    chat_is_forum;
    chat_has_username;
    chat_is_created;
    user_administrator_rights;
    bot_administrator_rights;
    bot_is_member;
    request_title;
    request_username;
    request_photo;
  ]

let keyboard_button_poll_type_to_yojson (rp : keyboard_button_poll_type) =
  match rp._type with
  | None -> None
  | Some(s) -> Some(assoc_to_json [[("type", `String s)]])

let keyboard_button_to_yojson (kb : keyboard_button) =
  let text = [("text", `String kb.text)] in
  let request_users =
    match kb.request_users with
    | None -> []
    | Some(ru) -> [("request_users", keyboard_button_request_users_to_yojson ru)]
  in
  let request_chat =
    match kb.request_chat with
    | None -> []
    | Some(rc) -> [("request_chat", keyboard_button_request_chat_to_yojson rc)]
  in
  let request_contact =
    match kb.request_contact with
    | None -> []
    | Some(rc) -> [("request_contact", `Bool rc)]
  in
  let request_location =
    match kb.request_location with
    | None -> []
    | Some(rl) -> [("request_location", `Bool rl)]
  in
  let request_poll =
    match kb.request_poll with
    | None -> []
    | Some(rp) -> 
      (match keyboard_button_poll_type_to_yojson rp with
      | None -> []
      | Some(res) -> [("request_poll", res)])
  in
  let web_app =
    match kb.web_app with
    | None -> []
    | Some(wa) -> [("web_app", web_app_info_to_yojson wa)]
  in
  assoc_to_json [
    text;
    request_users;
    request_chat;
    request_contact;
    request_location;
    request_poll;
    web_app
  ]

let keyboard_button_list_to_yojson kbl =
  `List (List.map keyboard_button_to_yojson kbl)

let keyboard_button_list_list_to_yojson kbll =
  `List (List.map keyboard_button_list_to_yojson kbll)

let reply_keyboard_markup_to_yojson (rkm : reply_keyboard_markup) =
  let keyboard = [("keyboard", keyboard_button_list_list_to_yojson rkm.keyboard)] in
  let is_persistent =
    match rkm.is_persistent with
    | None -> []
    | Some(ip) -> [("is_persistent", `Bool ip)]
  in
  let resize_keyboard =
    match rkm.resize_keyboard with
    | None -> []
    | Some(rk) -> [("resize_keyboard", `Bool rk)]
  in
  let one_time_keyboard =
    match rkm.one_time_keyboard with
    | None -> []
    | Some(otk) -> [("one_time_keyboard", `Bool otk)]
  in
  let input_field_placeholder =
    match rkm.input_field_placeholder with
    | None -> []
    | Some(ifp) -> [("input_field_placeholder", `String ifp)]
  in
  let selective =
    match rkm.selective with
    | None -> []
    | Some(s) -> [("selective", `Bool s)]
  in
  assoc_to_json [
    keyboard;
    is_persistent;
    resize_keyboard;
    one_time_keyboard;
    input_field_placeholder;
    selective;
  ]

let reply_keyboard_remove_to_yojson (rkr : reply_keyboard_remove) =
  let remove_keyboard = [("remove_keyboard", `Bool rkr.remove_keyboard)] in
  let selective =
    match rkr.selective with
    | None -> []
    | Some(s) -> [("selective", `Bool s)]
  in
  assoc_to_json [remove_keyboard; selective]

let force_reply_to_yojson (fr : force_reply) =
  let force_reply = [("force_reply", `Bool fr.force_reply)] in
  let input_field_placeholder =
    match fr.input_field_placeholder with
    | None -> []
    | Some(ifp) -> [("input_field_placeholder", `String ifp)]
  in
  let selective =
    match fr.selective with
    | None -> []
    | Some(s) -> [("selective", `Bool s)]
  in
  assoc_to_json [
    force_reply;
    input_field_placeholder;
    selective;
  ]

let inline_keyboard_markup_to_yojson ikm =
  let ikbll = ikm.inline_keyboard in
  `Assoc [( "inline_keyboard", inline_keyboard_button_list_list_to_yojson ikbll )]

let reply_markup_to_yojson rmk =
  match rmk with
  | InlineKeyboard(ikbll) -> inline_keyboard_markup_to_yojson ikbll
  | ReplyKeyboard(rk) -> reply_keyboard_markup_to_yojson rk
  | ReplyKeyboardRemove(rkr) -> reply_keyboard_remove_to_yojson rkr
  | ForceReply(fr) -> force_reply_to_yojson fr

let send_message_request_to_yojson (sm : send_message_type) : Yojson.Safe.t =
  let chat_id =
    match sm.chat_id with
    | Chat(i) -> [("chat_id", `Intlit (BatInt64.to_string i))]
    | Channel(s) -> [("chat_id", `String s)]
  in
  let text = [("text", `String sm.text)] in
  let message_thread_id =
    match sm.message_thread_id with
    | None -> []
    | Some(x) -> [("message_thread_id", `Int x)]
  in
  let parse_mode =
    match sm.parse_mode with
    | None -> []
    | Some(x) -> 
      (match formatting_option_to_yojson_string x with
      | None -> []
      | Some(res) -> [("parse_mode", res)])
  in
  let entities =
    match sm.entities with
    | None -> []
    | Some([]) -> []
    | Some(l) ->
      [("entities", message_entity_list_to_yojson l)]
  in
  let link_preview_options =
    match sm.link_preview_options with
    | None -> []
    | Some(lpo) -> [("link_preview_options", link_preview_options_to_yojson lpo)]
  in
  let disable_notification =
    match sm.disable_notification with
    | None -> []
    | Some(dn) -> [("disable_notification", `Bool dn)]
  in
  let protect_content =
    match sm.protect_content with
    | None -> []
    | Some(pc) -> [("protect_content", `Bool pc)]
  in
  let allow_paid_broadcast =
    match sm.allow_paid_broadcast with
    | None -> []
    | Some(apb) -> [("allow_paid_broadcast", `Bool apb)]
  in
  let message_effect_id =
    match sm.message_effect_id with
    | None -> []
    | Some(mei) -> [("message_effect_id", `String mei)]
  in
  let reply_parameters = 
    match sm.reply_parameters with
    | None -> []
    | Some(rp) -> [("reply_parameters", reply_parameters_to_yojson rp)] 
  in
  let reply_markup_keyboard =
    match sm.reply_markup with
    | None -> []
    | Some(rmk) -> [("reply_markup", reply_markup_to_yojson rmk)]
  in
  assoc_to_json [
    chat_id;
    text;
    message_thread_id;
    parse_mode;
    entities;
    link_preview_options;
    disable_notification;
    protect_content;
    allow_paid_broadcast;
    message_effect_id;
    reply_parameters;
    reply_markup_keyboard;
  ]

let delete_message_request_to_yojson (dm : delete_message_type) : Yojson.Safe.t =
  let chat_id =
    match dm.chat_id with
    | Chat(v) -> [("chat_id", `Intlit (BatInt64.to_string v))]
    | Channel(v) -> [("chat_id", `String v)]
  in
  let message_id = [("message_id", `Int dm.message_id)] in
  assoc_to_json [
    chat_id;
    message_id;
  ]

let ban_chat_member_request_to_yojson (bcm : ban_chat_member_type) : Yojson.Safe.t =
  let chat_id =
    match bcm.chat_id with
    | Chat(v) -> [("chat_id", `Intlit (BatInt64.to_string v))]
    | Channel(v) -> [("chat_id", `String v)]
  in
  let user_id = [("user_id", `Intlit (BatInt64.to_string bcm.user_id))] in
  let until_date =
    match bcm.until_date with
    | None -> []
    | Some(date) -> [("until_date", `Int date)]
  in
  let revoke_messages =
    match bcm.revoke_messages with
    | None -> []
    | Some(rm) -> [("revoke_messages", `Bool rm)]
  in
  assoc_to_json [
    chat_id;
    user_id;
    until_date;
    revoke_messages;
  ]

let to_user_option j : user option =
  let open Yojson.Safe.Util in
  let open BatOption in
  let to_bool_option' x =
    x |> to_bool_option |> default false 
  in
  match j with
  | `Null -> None
  | _ ->
  let id = j |> member "id" |> to_int64 in
  let is_bot = j |> member "is_bot" |> to_bool_option |> default false in
  let first_name = j |> member "first_name" |> to_string in
  let last_name = j |> member "last_name" |> to_string_option in
  let username = j |> member "username" |> to_string_option in
  let is_premium = j |> member "is_premium" |> to_bool_option' in
  let added_to_attachement_menu = j |> member "added_to_attachement_menu" |> to_bool_option' in
  Some({
    id;
    is_bot;
    first_name;
    last_name;
    username;
    is_premium;
    added_to_attachement_menu;
  })

let to_user j =
  let open BatOption in
  j |> to_user_option |> get

let to_user_list j =
  let open Yojson.Safe.Util in
  let open BatOption in
  match j with
  | `Null -> []
  | `List _ ->
    j 
    |> to_list 
    |> List.map to_user_option 
    |> List.filter (is_some) 
    |> List.map get
  | _ -> failwith "to_user_list: do not know what to do"

let to_chat_option j : chat option =
  let open Yojson.Safe.Util in
  let open BatOption in
  let to_bool_option' x =
    x |> to_bool_option |> default false
  in
  match j with
  | `Null -> None
  | _ ->
    let id = j |> member "id" |> to_int64 in
    let _type = 
      match j |> member "type" |> to_string with
      | "private" -> Private
      | "group" -> Group
      | "supergroup" -> Supergroup
      | "channel" -> Channel
      | x -> failwith @@ Printf.sprintf "Unhandled chat type: %s" x
    in
    let title = j |> member "title" |> to_string_option in
    let username = j |> member "username" |> to_string_option in
    let first_name = j |> member "first_name" |> to_string_option in
    let last_name = j |> member "last_name" |> to_string_option in
    let is_forum = j |> member "is_forum" |> to_bool_option' in
    Some({
      id;
      _type;
      title;
      username;
      first_name;
      last_name;
      is_forum
    })

let to_chat j =
  let open BatOption in
  get (to_chat_option j)

let to_chat_list (j : Yojson.Safe.t) : chat list =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> []
  | `List _ ->
    j
    |> to_list
    |> List.map to_chat
  | _ -> failwith "to_chat_list: do not know what to do"

let to_message_origin_user j : message_origin_user =
  let open Yojson.Safe.Util in
  let _type = "user" in
  let date = j |> member "date" |> to_int in
  let sender_user = j |> member "sender_user" |> to_user in
  { _type; date; sender_user }

let to_message_origin_hidden_user j : message_origin_hidden_user =
  let open Yojson.Safe.Util in
  let _type = "hidden_user" in
  let date = j |> member "date" |> to_int in
  let sender_user_name = j |> member "sender_user_name" |> to_string in 
  { _type; date; sender_user_name }

let to_message_origin_chat j : message_origin_chat =
  let open Yojson.Safe.Util in
  let _type = "chat" in
  let date = j |> member "date" |> to_int in
  let sender_chat = j |> member "chat" |> to_chat in
  let author_signature = j |> member "author_signature" |> to_string_option in
  { _type; date; sender_chat; author_signature }

let to_message_origin_channel j : message_origin_channel =
  let open Yojson.Safe.Util in
  let _type = "channel" in
  let date = j |> member "date" |> to_int in
  let chat = j |> member "chat" |> to_chat in
  let message_id = j |> member "message_id" |> to_int in
  let author_signature = j |> member "author_signature" |> to_string_option in
  { _type; date; chat; message_id; author_signature }

let to_message_origin_option j =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
    let _type = j |> member "type" |> to_string in
    match _type with
    | "user" -> Some(MessageOriginUser(to_message_origin_user j))
    | "hidden_user" -> Some(MessageOriginHiddenUser(to_message_origin_hidden_user j))
    | "chat" -> Some(MessageOriginChat(to_message_origin_chat j))
    | "channel" -> Some(MessageOriginChannel(to_message_origin_channel j))
    | _ -> failwith @@ Printf.sprintf "Unsupported message origin: %s" _type

let to_message_origin j =
  let open BatOption in
  get (to_message_origin_option j)

let to_link_preview_options_option j : link_preview_options option =
  let open Yojson.Safe.Util in
  let open BatOption in
  let to_bool_option' x =
    x |> to_bool_option |> default false
  in
  match j with
  | `Null -> None
  | _ ->
  let is_disabled = j |> member "is_disabled" |> to_bool_option' in
  let url = j |> member "url" |> to_string_option in
  let prefer_small_media = j |> member "prefer_small_media" |> to_bool_option' in
  let prefer_large_media = j |> member "prefer_large_media" |> to_bool_option' in
  let show_above_text = j |> member "show_above_text" |> to_bool_option' in
  Some({ is_disabled; url; prefer_small_media; prefer_large_media; show_above_text })

let to_photosize_option j : photosize option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let file_id = j |> member "file_id" |> to_string in
  let file_unique_id = j |> member "file_unique_id" |> to_string in
  let width = j |> member "width" |> to_int in
  let height = j |> member "heigth" |> to_int in
  let file_size = j |> member "file_size" |> to_int_option in
  Some({ file_id; file_unique_id; width; height; file_size })

let to_photosize j : photosize =
  let open BatOption in
  get (to_photosize_option j)

let to_animation_option j : animation option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let file_id = j |> member "file_id" |> to_string in
  let file_unique_id = j |> member "file_unique_id" |> to_string in
  let width = j |> member "width" |> to_int in
  let height = j |> member "heigth" |> to_int in
  let duration =  j |> member "duration" |> to_int in
  let thumbnail = j |> member "thumbnail" |> to_photosize_option in
  let file_name = j |> member "file_name" |> to_string_option in
  let mime_type = j |> member "mime_type" |> to_string_option in
  let file_size = j |> member "file_size" |> to_int64_option in
  Some({
    file_id;
    file_unique_id;
    width;
    height;
    duration;
    thumbnail;
    file_name;
    mime_type;
    file_size
  })

let to_audio_option j : audio option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let file_id = j |> member "file_id" |> to_string in
  let file_unique_id = j |> member "file_unique_id" |> to_string in
  let duration = j |> member "duration" |> to_int in
  let performer = j |> member "performer" |> to_string_option in
  let title = j |> member "title" |> to_string_option in
  let file_name = j |> member "file_name" |> to_string_option in
  let mime_type = j |> member "mime_type" |> to_string_option in
  let file_size = j |> member "file_size" |> to_int64_option in
  let thumbnail = j |> member "thumbnail" |> to_photosize_option in
  Some({
    file_id;
    file_unique_id;
    duration;
    performer;
    title;
    file_name;
    mime_type;
    file_size;
    thumbnail;
  })

let to_document_option j : document option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let file_id = j |> member "file_id" |> to_string in
  let file_unique_id = j |> member "file_unique_id" |> to_string in
  let thumbnail = j |> member "thumbnail" |> to_photosize_option in
  let file_name = j |> member "file_name" |> to_string_option in
  let mime_type = j |> member "mime_type" |> to_string_option in
  let file_size = j |> member "file_size" |> to_int64_option in
  Some({
    file_id;
    file_unique_id;
    thumbnail;
    file_name;
    mime_type;
    file_size;
  })

let to_document j : document =
  let open BatOption in
  get (to_document_option j)

let to_photosize_list (j : Yojson.Safe.t) : photosize list =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> []
  | `List _ ->
    j |> to_list |> List.map (to_photosize) 
  | _ -> failwith "to_photosize_list: do not know what to do"

let to_file_option j : file option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let file_id = j |> member "file_id" |> to_string in
  let file_unique_id = j |> member "file_unique_id" |> to_string in
  let file_size = j |> member "file_size" |> to_int64_option in
  let file_path = j |> member "file_path" |> to_string_option in
  Some({ file_id; file_unique_id; file_size; file_path })

let to_mask_position_option j : mask_position option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let point = j |> member "point" |> to_string in
  let x_shift = j |> member "x_shift" |> to_float in
  let y_shift = j |> member "y_shift" |> to_float in
  let scale = j |> member "scale" |> to_float in
  Some({ point; x_shift; y_shift; scale })

let to_sticker_option j : sticker option =
  let open Yojson.Safe.Util in
  let open BatOption in
  let to_bool_option' x =
    x |> to_bool_option |> default false
  in
  match j with
  | `Null -> None
  | _ ->
  let file_id = j |> member "file_id" |> to_string in
  let file_unique_id = j |> member "file_unique_id" |> to_string in
  let (_type: sticker_type) = 
    match j |> member "type" |> to_string with
    | "regular" -> Regular
    | "mask" -> Mask
    | "custom_emoji" -> CustomEmoji
    | x -> failwith @@ Printf.sprintf "Unhandled sticker type: %s" x
  in
  let width = j |> member "width" |> to_int in
  let height = j |> member "height" |> to_int in
  let is_animated = j |> member "is_animated" |> to_bool in
  let is_video = j |> member "is_video" |> to_bool in
  let thumbnail = j |> member "thumbnail" |> to_photosize_option in
  let emoji = j |> member "emoji" |> to_string_option in
  let set_name = j |> member "set_name" |> to_string_option in
  let premium_animation = j |> member "premium_animation" |> to_file_option in
  let mask_position = j |> member "mask_position" |> to_mask_position_option in
  let custom_emoji_id = j |> member "custom_emoji_id" |> to_string_option in
  let needs_repainting = j |> member "needs_repainting" |> to_bool_option' in
  let file_size = j |> member "file_size" |> to_int_option in
  Some({
    file_id;
    file_unique_id;
    _type;
    width;
    height;
    is_animated;
    is_video;
    thumbnail;
    emoji;
    set_name;
    premium_animation;
    mask_position;
    custom_emoji_id;
    needs_repainting;
    file_size;
  })

let to_story_option j : story option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let chat = j |> member "chat" |> to_chat in
  let id = j |> member "id" |> to_int in
  Some({ chat; id })
  
let to_video_option j : video option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let file_id = j |> member "file_id" |> to_string in
  let file_unique_id = j |> member "file_unique_id" |> to_string in
  let width = j |> member "width" |> to_int in
  let height = j |> member "height" |> to_int in
  let duration = j |> member "duration" |> to_int in
  let thumbnail = j |> member "thumbnail" |> to_photosize_option in
  let file_name = j |> member "file_name" |> to_string_option in
  let mime_type = j |> member "mime_type" |> to_string_option in
  let file_size = j |> member "file_size" |> to_int64_option in
  Some({
    file_id;
    file_unique_id;
    width;
    height;
    duration;
    thumbnail;
    file_name;
    mime_type;
    file_size;
  })

let to_video j =
  let open BatOption in
  get (to_video_option j)
  
let to_video_note_option j : video_note option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let file_id = j |> member "file_id" |> to_string in
  let file_unique_id = j |> member "file_unique_id" |> to_string in
  let length = j |> member "length" |> to_int in
  let duration = j |> member "duration" |> to_int in
  let thumbnail = j |> member "thumbnail" |> to_photosize_option in
  let file_size = j |> member "file_size" |> to_int_option in
  Some({
    file_id;
    file_unique_id;
    length;
    duration;
    thumbnail;
    file_size;
  })
  
let to_voice_option j : voice option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let file_id = j |> member "file_id" |> to_string in
  let file_unique_id = j |> member "file_unique_id" |> to_string in
  let duration = j |> member "duration" |> to_int in
  let mime_type = j |> member "mime_type" |> to_string_option in
  let file_size = j |> member "file_size" |> to_int64_option in
  Some({
    file_id;
    file_unique_id;
    duration;
    mime_type;
    file_size;
  })
  
let to_contact_option j : contact option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let phone_number = j |> member "phone_number" |> to_string in
  let first_name = j |> member "first_name" |> to_string in
  let last_name = j |> member "last_name" |> to_string_option in
  let user_id = j |> member "user_id" |> to_int64_option in
  let vcard = j |> member "vcard" |> to_string_option in
  Some({
    phone_number;
    first_name;
    last_name;
    user_id;
    vcard;
  })
  
let to_dice_option j : dice option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let emoji = j |> member "emoji" |> to_string in
  let value = j |> member "value" |> to_int in
  Some({ emoji; value })
  
let to_giveaway_option j : giveaway option =
  let open Yojson.Safe.Util in
  let open BatOption in
  let to_bool_option' x =
    x |> to_bool_option |> default false
  in
  let to_string_list x =
    match x with
    | `Null -> []
    | `List _ ->
      x
      |> to_list
      |> List.map (to_string_option)
      |> List.filter is_some
      |> List.map get
    | _ -> failwith "to_string_list: do not know what to do"
  in
  match j with
  | `Null -> None
  | _ ->
  let chats = j |> member "chats" |> to_chat_list in
  let winners_selection_date = j |> member "winners_selection_date" |> to_int in
  let winner_count = j |> member "winner_count" |> to_int in
  let only_new_members = j |> member "only_new_members" |> to_bool_option' in
  let has_public_winners = j |> member "has_public_winners" |> to_bool_option' in
  let prize_description = j |> member "prize_description" |> to_string_option in
  let country_codes = j |> member "country_codes" |> to_string_list in
  let prize_star_count = j |> member "prize_star_count" |> to_int_option in
  let premium_subscription_month_count = j |> member "premium_subscription_month_count" |> to_int_option in
  Some({
    chats;
    winners_selection_date;
    winner_count;
    only_new_members;
    has_public_winners;
    prize_description;
    country_codes;
    prize_star_count;
    premium_subscription_month_count
  })
  
let to_giveaway_winners_option j : giveaway_winners option =
  let open Yojson.Safe.Util in
  let open BatOption in
  let to_bool_option' x =
    x |> to_bool_option |> default false
  in
  let to_user_list x =
    match x with
    | `Null -> []
    | `List _ ->
      x |> to_list |> List.map to_user
    | _ -> failwith "to_user_list: do not know what to do"
  in
  match j with
  | `Null -> None
  | _ ->
  let chat = j |> member "chat" |> to_chat in
  let giveaway_message_id = j |> member "giveaway_message_id" |> to_int in
  let winners_selection_date = j |> member "winners_selection_date" |> to_int in
  let winner_count = j |> member "winner_count" |> to_int in
  let winners = j |> member "winners" |> to_user_list in
  let additional_chat_count = j |> member "additional_chat_count" |> to_int_option in
  let prize_star_count = j |> member "prize_star_count" |> to_int_option in
  let premium_subscription_month_count = j |> member "premium_subscription_month_count" |> to_int_option in
  let unclaimed_prize_count = j |> member "unclaimed_prize_count" |> to_int_option in
  let only_new_members = j |> member "only_new_members" |> to_bool_option' in
  let was_refunded = j |> member "was_refunded" |> to_bool_option' in
  let prize_description = j |> member "prize_description" |> to_string_option in
  Some({
    chat;
    giveaway_message_id;
    winners_selection_date;
    winner_count;
    winners;
    additional_chat_count;
    prize_star_count;
    premium_subscription_month_count;
    unclaimed_prize_count;
    only_new_members;
    was_refunded;
    prize_description;
  })

let to_location_option j : location option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let latitude = j |> member "latitude" |> to_float in
  let longtitude = j |> member "longtitude" |> to_float in
  let horizontal_accuracy = j |> member "horizontal_accuracy" |> to_float_option in
  let live_period = j |> member "live_period" |> to_int_option in
  let heading = j |> member "heading" |> to_int_option in
  let proximity_alert_radius = j |> member "proximity_alert_radius" |> to_int_option in
  Some({
    latitude;
    longtitude;
    horizontal_accuracy;
    live_period;
    heading;
    proximity_alert_radius
  })

let to_message_entity_option j : message_entity option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let _type =
    match j |> member "type" |> to_string with
    | "mention" -> Mention
    | "hashtag" -> Hashtag
    | "cashtag" -> Cashtag
    | "bot_command" -> BotCommand
    | "url" -> Url
    | "email" -> Email
    | "phone_number" -> PhoneNumber
    | "bold" -> Bold
    | "italic" -> Italic
    | "underline" -> Underline
    | "strikethrough" -> Strikethrough
    | "spoiler" -> Spoiler
    | "blockquote" -> Blockquote
    | "expandable_blockquote" -> ExpandableBlockquote
    | "code" -> Code
    | "pre" -> Pre
    | "text_link" -> TextLink
    | "text_mention" -> TextMention
    | "custom_emoji" -> CustomEmoji
    | x -> failwith @@ Printf.sprintf "Unsupported message entity type: %s" x
  in
  let offset = j |> member "offset" |> to_int in
  let length = j |> member "length" |> to_int in
  let url = j |> member "url" |> to_string_option in
  let user = j |> member "user" |> to_user_option in
  let language = j |> member "language" |> to_string_option in
  let custom_emoji_id = j |> member "custom_emoji_id" |> to_string_option in
  Some({
    _type;
    offset;
    length;
    url;
    user;
    language;
    custom_emoji_id;
  })

let to_message_entity j =
  let open BatOption in
  get (to_message_entity_option j)

let to_message_entity_list j =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> []
  | `List _ ->
    j
    |> to_list
    |> List.map to_message_entity
  | _ -> failwith "to_message_entity_list: do not know what to do"
  
let to_poll_opt_option j : poll_option option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let text = j |> member "text" |> to_string in
  let text_entities = j |> member "text_entities" |> to_message_entity_list in
  let voter_count = j |> member "voter_count" |> to_int in
  Some({ text; text_entities; voter_count })
  
let to_poll_opt j =
  let open BatOption in
  get (to_poll_opt_option j)

let to_poll_opt_list j =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> []
  | `List _ ->
    j
    |> to_list
    |> List.map to_poll_opt
  | _ -> failwith "to_poll_opt_list: do not know what to do"
  
let to_poll_option j : poll option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let id = j |> member "id" |> to_string in
  let question = j |> member "question" |> to_string in
  let question_entities = j |> member "question_entities" |> to_message_entity_list in
  let options = j |> member "options" |> to_poll_opt_list in
  let total_voter_count = j |> member "total_voter_count" |> to_int in
  let is_closed = j |> member "is_closed" |> to_bool in
  let is_anonymous = j |> member "is_anonymous" |> to_bool in
  let _type = 
    match j |> member "type" |> to_string with 
    | "regular" -> Regular
    | "quiz" -> Quiz
    | x -> failwith @@ Printf.sprintf "Unsupported poll type: %s" x
  in
  let allows_multiple_answers = j |> member "allows_multiple_answers" |> to_bool in
  let correct_option_id = j |> member "correct_option_id" |> to_int_option in
  let explanation = j |> member "explanation" |> to_string_option in
  let explanation_entities = j |> member "explanation_entities" |> to_message_entity_list in
  let open_period = j |> member "open_period" |> to_int_option in
  let close_date = j |> member "close_date" |> to_int_option in
  Some({
    id;
    question;
    question_entities;
    options;
    total_voter_count;
    is_closed;
    is_anonymous;
    _type;
    allows_multiple_answers;
    correct_option_id;
    explanation;
    explanation_entities;
    open_period;
    close_date;
  })
  
let to_external_reply_info_option j : external_reply_info option =
  let open Yojson.Safe.Util in
  let open BatOption in
  let to_bool_option' x =
    x |> to_bool_option |> default false
  in
  match j with
  | `Null -> None
  | _ ->
  let origin = j |> member "origin" |> to_message_origin in
  let chat = j |> member "chat" |> to_chat_option in
  let message_id = j |> member "message_id" |> to_int_option in
  let link_preview_options = j |> member "link_preview_options" |> to_link_preview_options_option in
  let animation = j |> member "animation" |> to_animation_option in
  let audio = j |> member "audio" |> to_audio_option in
  let document = j |> member "document" |> to_document_option in
  let photo = j |> member "photo" |> to_photosize_list in
  let sticker = j |> member "sticker" |> to_sticker_option in
  let story = j |> member "story" |> to_story_option in
  let video = j |> member "video" |> to_video_option in
  let video_note = j |> member "video_note" |> to_video_note_option in
  let voice = j |> member "voice" |> to_voice_option in
  let has_media_spoiler = j |> member "has_media_spoiler" |> to_bool_option' in
  let contact = j |> member "contact" |> to_contact_option in
  let dice = j |> member "dice" |> to_dice_option in
  let giveaway = j |> member "giveaway" |> to_giveaway_option in
  let giveaway_winners = j |> member "giveaway_winners" |> to_giveaway_winners_option in
  let location = j |> member "location" |> to_location_option in
  let poll = j |> member "poll" |> to_poll_option in
  Some({
    origin;
    chat;
    message_id;
    link_preview_options;
    animation;
    audio;
    document;
    photo;
    sticker;
    story;
    video;
    video_note;
    voice;
    has_media_spoiler;
    contact;
    dice;
    giveaway;
    giveaway_winners;
    location;
    poll;
  })

let to_text_quote_option j : text_quote option =
  let open Yojson.Safe.Util in
  let open BatOption in
  let to_bool_option' x =
    x |> to_bool_option |> default false
  in
  match j with
  | `Null -> None
  | _ ->
  let text = j |> member "text" |> to_string in
  let entities = j |> member "entities" |> to_message_entity_list in
  let position = j |> member "position" |> to_int in
  let is_manual = j |> member "is_manual" |> to_bool_option' in
  Some({
    text;
    entities;
    position;
    is_manual;
  })

let to_paid_media_preview j =
  let open Yojson.Safe.Util in
  let _type = j |> member "type" |> to_string in
  let width = j |> member "width" |> to_int_option in
  let height = j |> member "height" |> to_int_option in
  let duration = j |> member "duration" |> to_int_option in
  { _type; width; height; duration }

let to_paid_media_photo j : paid_media_photo =
  let open Yojson.Safe.Util in
  let _type = j |> member "type" |> to_string in
  let photo = j |> member "photo" |> to_photosize_list in
  { _type; photo }

let to_paid_media_video j : paid_media_video =
  let open Yojson.Safe.Util in
  let _type = j |> member "type" |> to_string in
  let video = j |> member "video" |> to_video in
  { _type; video }

let to_paid_media_option j : paid_media option =
  let open Yojson.Safe.Util in
  match j with 
  | `Null -> None
  | _ ->
  let _type = j |> member "type" |> to_string in
  match _type with
  | "preview" -> Some(PaidMediaPreview(to_paid_media_preview j))
  | "photo" -> Some(PaidMediaPhoto(to_paid_media_photo j))
  | "video" -> Some(PaidMediaVideo(to_paid_media_video j))
  | _ -> failwith @@ Printf.sprintf "Unexpected paid media type: %s" _type

let to_paid_media j =
  let open BatOption in
  get (to_paid_media_option j)

let to_paid_media_info_option j : paid_media_info option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let star_count = j |> member "star_count" |> to_int in
  let paid_media = j |> member "paid_media" |> to_paid_media in
  Some({ star_count; paid_media })

let to_game_option j : game option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let title = j |> member "title" |> to_string in
  let description = j |> member "description" |> to_string in
  let photo = j |> member "photo" |> to_photosize_list in
  let text = j |> member "text" |> to_string_option in
  let text_entities = j |> member "text_entities" |> to_message_entity_list in
  let animation = j |> member "animation" |> to_animation_option in
  Some({
    title;
    description;
    photo;
    text;
    text_entities;
    animation;
  })

let to_location j =
  let open BatOption in
  get (to_location_option j)

let to_venue_option j : venue option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let location = j |> member "location" |> to_location in
  let title = j |> member "title" |> to_string in
  let address = j |> member "address" |> to_string in
  let foursquare_id = j |> member "foursquare_id" |> to_string_option in
  let foursquare_type = j |> member "foursquare_type" |> to_string_option in
  let google_place_id = j |> member "google_place_id" |> to_string_option in
  let google_place_type = j |> member "google_place_type" |> to_string_option in
  Some({
    location;
    title;
    address;
    foursquare_id;
    foursquare_type;
    google_place_id;
    google_place_type;
  })

let to_message_auto_delete_timer_changed_option j : message_auto_delete_timer_changed option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let message_auto_delete_time = j |> member "message_auto_delete_time" |> to_int in
  Some({ message_auto_delete_time })

let to_inaccessible_message j : inaccessible_message =
  let open Yojson.Safe.Util in
  let chat = j |> member "chat" |> to_chat in
  let message_id = j |> member "message_id" |> to_int in
  let date = 0 (* always 0 in case of inaccessible *) in
  { chat; message_id; date }

let to_shared_user_option j : shared_user option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let user_id = j |> member "user_id" |> to_int64 in
  let first_name = j |> member "first_name" |> to_string_option in
  let last_name = j |> member "last_name" |> to_string_option in
  let username = j |> member "username" |> to_string_option in
  let photo = j |> member "photo" |> to_photosize_list in
  Some({
    user_id;
    first_name;
    last_name;
    username;
    photo;
  })

let to_shared_user_list j : shared_user list =
  let open Yojson.Safe.Util in
  let open BatOption in
  match j with
  | `Null -> []
  | `List _ ->
    j
    |> to_list
    |> List.map to_shared_user_option
    |> List.filter is_some
    |> List.map get
  | _ -> failwith "to_shared_user_list: do not know what to do"

let to_users_shared_option j : users_shared option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let request_id = j |> member "request_id" |> to_int in
  let users = j |> member "users" |> to_shared_user_list in
  Some({ request_id; users })

let to_chat_shared_option j : chat_shared option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let request_id = j |> member "request_id" |> to_int in
  let chat_id = j |> member "chat_id" |> to_int64 in
  let title = j |> member "title" |> to_string_option in
  let username = j |> member "username" |> to_string_option in
  let photo = j |> member "photo" |> to_photosize_list in
  Some({
    request_id;
    chat_id;
    title;
    username;
    photo;
  })

let to_write_access_allowed_option j : write_access_allowed option =
  let open Yojson.Safe.Util in
  let open BatOption in
  let to_bool_option' x =
    x |> to_bool_option |> default false
  in
  match j with
  | `Null -> None
  | _ ->
  let from_request = j |> member "from_request" |> to_bool_option' in
  let web_app_name = j |> member "web_app_name" |> to_string_option in
  let from_attachment_menu = j |> member "from_attachment_menu" |> to_bool_option' in
  Some({
    from_request;
    web_app_name;
    from_attachment_menu;
  })

let to_passport_data_option j : passport_data option =
  match j with
  | `Null -> None
  | _ -> Some(PassportData)

let to_proximity_alert_triggered_option j : proximity_alert_triggered option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let traveler = j |> member "traveler" |> to_user in
  let watcher = j |> member "watcher" |> to_user in
  let distance = j |> member "distance" |> to_int in
  Some({
    traveler;
    watcher;
    distance;
  })

let to_chat_boost_added_option j : chat_boost_added option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let boost_count = j |> member "boost_count" |> to_int in
  Some({ boost_count })

let to_background_fill_solid j : background_fill_solid =
  let open Yojson.Safe.Util in
  let _type = j |> member "type" |> to_string in
  let color = j |> member "color" |> to_int in
  { _type; color }

let to_background_fill_gradient j : background_fill_gradient =
  let open Yojson.Safe.Util in
  let _type = j |> member "type" |> to_string in
  let top_color = j |> member "top_color" |> to_int in
  let bottom_color = j |> member "bottom_color" |> to_int in
  let rotation_angle = j |> member "rotation_angle" |> to_int in
  { _type; top_color; bottom_color; rotation_angle }

let to_background_fill_freeform_gradient j : background_fill_freeform_gradient =
  let open Yojson.Safe.Util in
  let _type = j |> member "type" |> to_string in
  let colors = j |> member "colors" |> to_list |> List.map to_int in
  { _type; colors }

let to_background_fill j : background_fill =
  let open Yojson.Safe.Util in
  let _type = j |> member "type" |> to_string in
  match _type with
  | "solid" -> BackgroundFillSolid(to_background_fill_solid j)
  | "gradient" -> BackgroundFillGradient(to_background_fill_gradient j)
  | "freeform_gradient" -> BackgroundFillFreeformGradient(to_background_fill_freeform_gradient j)
  | _ -> failwith @@ Printf.sprintf "Unexpected background fill type: %s" _type

let to_background_type_fill j : background_type_fill =
  let open Yojson.Safe.Util in
  let _type = j |> member "type" |> to_string in
  let fill = j |> member "fill" |> to_background_fill in
  let dark_theme_dimming = j |> member "dark_theme_dimming" |> to_int in
  { _type; fill; dark_theme_dimming }

let to_background_type_wallpaper j : background_type_wallpaper =
  let open Yojson.Safe.Util in
  let open BatOption in
  let to_bool_option' x =
    x |> to_bool_option |> default false
  in
  let _type = j |> member "type" |> to_string in
  let document = j |> member "document" |> to_document in
  let dark_theme_dimming = j |> member "dark_theme_dimming" |> to_int in
  let is_blurred = j |> member "is_blurred" |> to_bool_option' in
  let is_moving = j |> member "is_moving" |> to_bool_option' in
  {
    _type;
    document;
    dark_theme_dimming;
    is_blurred;
    is_moving;
  }

let to_background_type_pattern j : background_type_pattern =
  let open Yojson.Safe.Util in
  let open BatOption in
  let to_bool_option' x =
    x |> to_bool_option |> default false
  in
  let _type = j |> member "type" |> to_string in
  let document = j |> member "document" |> to_document in
  let fill = j |> member "fill" |> to_background_fill in
  let intensity = j |> member "intensity" |> to_int in
  let is_inverted = j |> member "is_inverted" |> to_bool_option' in
  let is_moving = j |> member "is_moving" |> to_bool_option' in
  {
    _type;
    document;
    fill;
    intensity;
    is_inverted;
    is_moving;
  }

let to_background_type_chat_theme j : background_type_chat_theme =
  let open Yojson.Safe.Util in
  let _type = j |> member "type" |> to_string in
  let theme_name = j |> member "theme_name" |> to_string in
  { _type; theme_name }

let to_chat_background_option j : chat_background option =
  let open Yojson.Safe.Util in
  let open BatOption in
  match j with
  | `Null -> None
  | _ ->
  let _type = j |> member "type" |> to_string in
  let res = 
    match _type with
    | "fill" -> BackgroundTypeFill(to_background_type_fill j)
    | "wallpaper" -> BackgroundTypeWallpaper(to_background_type_wallpaper j)
    | "pattern" -> BackgroundTypePattern(to_background_type_pattern j)
    | "theme" -> BackgroundTypeChatTheme(to_background_type_chat_theme j)
    | _ -> failwith @@ Printf.sprintf "Unexpected background type: %s" _type
  in
  some ({ _type = res } : chat_background)

let to_forum_topic_created_option j : forum_topic_created option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let name = j |> member "name" |> to_string in
  let icon_color = j |> member "icon_color" |> to_int in
  let icon_custom_emoji_id = j |> member "icon_custom_emoji_id" |> to_string_option in
  Some({
    name;
    icon_color;
    icon_custom_emoji_id;
  })

let to_forum_topic_edited_option j : forum_topic_edited option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let name = j |> member "name" |> to_string_option in
  let icon_custom_emoji_id = j |> member "icon_custom_emoji_id" |> to_string_option in
  Some({ name; icon_custom_emoji_id })

let to_forum_topic_closed_option (j : Yojson.Safe.t) : forum_topic_closed option =
  match j with
  | `Null -> None
  | _ -> Some(ForumTopicClosed)

let to_forum_topic_reopened_option (j : Yojson.Safe.t) : forum_topic_reopened option =
  match j with
  | `Null -> None
  | _ -> Some(ForumTopicReopened)

let to_general_topic_forum_hidden_option (j : Yojson.Safe.t) : general_topic_forum_hidden option =
  match j with
  | `Null -> None
  | _ -> Some(GeneralTopicForumHidden)

let to_general_topic_forum_unhidden_option (j : Yojson.Safe.t) : general_topic_forum_unhidden option =
  match j with
  | `Null -> None
  | _ -> Some(GeneralTopicForumUnhidden)

let to_giveaway_created_option j : giveaway_created option = 
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let prize_star_count = j |> member "prize_star_count" |> to_int_option in
  Some({ prize_star_count })

let to_video_chat_scheduled_option j : video_chat_scheduled option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let start_date = j |> member "start_date" |> to_int in
  Some({ start_date })

let to_video_chat_ended_option j : video_chat_ended option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let duration = j |> member "duration" |> to_int in
  Some({ duration })

let to_video_chat_participants_invited_option j : video_chat_participants_invited option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let users = j |> member "users" |> to_user_list in
  Some({ users })

let to_web_app_data_option j : web_app_data option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let data = j |> member "data" |> to_string in
  let button_text = j |> member "button_text" |> to_string in
  Some({ data; button_text })

let to_web_app_info_option j : web_app_info option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let url = j |> member "url" |> to_string in
  Some({ url })

let to_login_url_option j : login_url option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let url = j |> member "url" |> to_string in
  let forward_text = j |> member "forward_text" |> to_string_option in
  let bot_username = j |> member "bot_username" |> to_string_option in
  let request_write_access = j |> member "request_write_access" |> to_bool_option in
  Some({
    url;
    forward_text;
    bot_username;
    request_write_access;
  })

let to_switch_inline_query_chosen_chat_option j : switch_inline_query_chosen_chat option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let query = j |> member "query" |> to_string_option in
  let allow_user_chats = j |> member "allow_user_chats" |> to_bool_option in
  let allow_bot_chats = j |> member "allow_bot_chats" |> to_bool_option in
  let allow_group_chats = j |> member "allow_group_chats" |> to_bool_option in
  let allow_channel_chats = j |> member "allow_channel_chats" |> to_bool_option in
  Some({
    query;
    allow_user_chats;
    allow_bot_chats;
    allow_group_chats;
    allow_channel_chats;
  })

let to_copy_text_button_option j : copy_text_button option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let text = j |> member "text" |> to_string in
  Some({ text })

let to_inline_keyboard_button j : inline_keyboard_button =
  let open Yojson.Safe.Util in
  let text = j |> member "text" |> to_string in
  let url = j |> member "url" |> to_string_option in
  let callback_data = j |> member "callback_data" |> to_string_option in
  let web_app = j |> member "web_app" |> to_web_app_info_option in
  let login_url = j |> member "login_url" |> to_login_url_option in
  let switch_inline_query = j |> member "switch_inline_query" |> to_string_option in
  let switch_inline_query_current_chat = j |> member "switch_inline_query_current_chat" |> to_string_option in
  let switch_inline_query_chosen_chat = j |> member "switch_inline_query_chosen_chat" |> to_switch_inline_query_chosen_chat_option in
  let copy_text_button = j |> member "copy_text_button" |> to_copy_text_button_option in
  {
    text;
    url;
    callback_data;
    web_app;
    login_url;
    switch_inline_query;
    switch_inline_query_current_chat;
    switch_inline_query_chosen_chat;
    copy_text_button;
  }

let to_inline_keyboard_markup_option j : inline_keyboard_markup option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let f x = 
    x 
    |> to_list 
    |> List.map (to_list) 
    |> List.map (List.map to_inline_keyboard_button)
  in
  let inline_keyboard = j |> member "inline_keyboard" |> f in
  Some( { inline_keyboard = inline_keyboard } )
  (* | _ -> failwith @@ Printf.sprintf "to_inline_keyboard_markup_option: do not know what to do: %s" (Yojson.Safe.pretty_to_string j) *)

let rec to_message_option j : message option =
  let open Yojson.Safe.Util in
  let open BatOption in
  let to_bool_option' x =
    x |> to_bool_option |> default false
  in
  let to_maybe_inaccessible_message_option j : maybe_inaccessible_message option =
    let open Yojson.Safe.Util in
    let open BatOption in
    match j with
    | `Null -> None
    | _ ->
    let date = j |> member "date" |> to_int in
    match date with
    | 0 -> Some(InaccessibleMessage(to_inaccessible_message j))
    | _ -> Some(Message(get (to_message_option j)))
  in
  let to_giveaway_completed_option j : giveaway_completed option =
    let open Yojson.Safe.Util in
    match j with
    | `Null -> None
    | _ ->
    let winner_count = j |> member "winner_count" |> to_int in
    let unclaimed_prize_count = j |> member "unclaimed_prize_count" |> to_int_option in
    let giveaway_message = j |> member "giveaway_message" |> to_message_option in
    let is_star_giveaway = j |> member "is_star_giveaway" |> to_bool_option' in
    Some({
      winner_count;
      unclaimed_prize_count;
      giveaway_message;
      is_star_giveaway;
    })
  in
  match j with
  | `Null -> None
  | _ ->
  let message_id = j |> member "message_id" |> to_int in
  let message_thread_id = j |> member "message_thread_id" |> to_int_option in
  let from = j |> member "from" |> to_user_option in
  let sender_chat = j |> member "sender_chat" |> to_chat_option in
  let sender_boost_count = j |> member "sender_boost_count" |> to_int_option in
  let sender_business_bot = j |> member "sender_business_bot" |> to_user_option in
  let date = j |> member "date" |> to_int in
  let business_connection_id = j |> member "business_connection_id" |> to_string_option in
  let chat = j |> member "chat" |> to_chat in
  let forward_origin = j |> member "forward_origin" |> to_message_origin_option in
  let is_topic_message = j |> member "is_topic_message" |> to_bool_option' in
  let is_automatic_forward = j |> member "is_automatic_forward" |> to_bool_option' in
  let reply_to_message = j |> member "reply_to_message" |> to_message_option in
  let external_reply = j |> member "external_reply" |> to_external_reply_info_option in
  let quote = j |> member "quote" |> to_text_quote_option in
  let reply_to_story = j |> member "reply_to_story" |> to_story_option in
  let via_bot = j |> member "via_bot" |> to_user_option in
  let edit_date = j |> member "edit_date" |> to_int_option in
  let has_protected_content = j |> member "has_protected_content" |> to_bool_option' in
  let is_from_offline = j |> member "is_from_offline" |> to_bool_option' in
  let media_group_id = j |> member "media_group_id" |> to_string_option in
  let author_signature = j |> member "author_signature" |> to_string_option in
  let text = j |> member "text" |> to_string_option in
  let entities = j |> member "entities" |> to_message_entity_list in
  let link_preview_options = j |> member "link_preview_options" |> to_link_preview_options_option in
  let effect_id = j |> member "effect_id" |> to_string_option in
  let animation = j |> member "animation" |> to_animation_option in
  let audio = j |> member "audio" |> to_audio_option in
  let document = j |> member "document" |> to_document_option in
  let paid_media = j |> member "paid_media" |> to_paid_media_info_option in
  let photo = j |> member "photo" |> to_photosize_list in
  let sticker = j |> member "sticker" |> to_sticker_option in
  let story = j |> member "story" |> to_story_option in
  let video = j |> member "video" |> to_video_option in
  let video_note = j |> member "video_note" |> to_video_note_option in
  let voice = j |> member "voice" |> to_voice_option in
  let caption = j |> member "caption" |> to_string_option in
  let caption_entities = j |> member "caption_entities" |> to_message_entity_list in
  let show_caption_above_media = j |> member "show_caption_above_media" |> to_bool_option' in
  let has_media_spoiler = j |> member "has_media_spoiler" |> to_bool_option' in
  let contact = j |> member "contact" |> to_contact_option in
  let dice = j |> member "dice" |> to_dice_option in
  let game = j |> member "game" |> to_game_option in
  let poll = j |> member "poll" |> to_poll_option in
  let venue = j |> member "venue" |> to_venue_option in
  let location = j |> member "location" |> to_location_option in
  let new_chat_members = j |> member "new_chat_members" |> to_user_list in
  let left_chat_member = j |> member "left_chat_member" |> to_user_option in
  let new_chat_title = j |> member "new_chat_title" |> to_string_option in
  let new_chat_photo = j |> member "new_chat_photo" |> to_photosize_list in
  let delete_chat_photo = j |> member "delete_chat_photo" |> to_bool_option' in
  let group_chat_created = j |> member "group_chat_created" |> to_bool_option' in
  let supergroup_chat_created = j |> member "supergroup_chat_created" |> to_bool_option' in
  let channel_chat_created = j |> member "channel_chat_created" |> to_bool_option' in
  let message_auto_delete_timer_changed = j |> member "message_auto_delete_timer_changed" |> to_message_auto_delete_timer_changed_option in
  let migrate_to_chat_id = j |> member "migrate_to_chat_id" |> to_int64_option in
  let migrate_from_chat_id = j |> member "migrate_to_chat_id" |> to_int64_option in
  let pinned_message = j |> member "pinned_message" |> to_maybe_inaccessible_message_option in
  let users_shared = j |> member "users_shared" |> to_users_shared_option in
  let chat_shared = j |> member "chat_shared" |> to_chat_shared_option in
  let connected_website = j |> member "connected_website" |> to_string_option in
  let write_access_allowed = j |> member "write_access_allowed" |> to_write_access_allowed_option in
  let passport_data = j |> member "passport_data" |> to_passport_data_option in
  let proximity_alert_triggered = j |> member "proximity_alert_triggered" |> to_proximity_alert_triggered_option in
  let boost_added = j |> member "boost_added" |> to_chat_boost_added_option in
  let chat_background_set = j |> member "chat_background_set" |> to_chat_background_option in
  let forum_topic_created = j |> member "forum_topic_created" |> to_forum_topic_created_option in
  let forum_topic_edited = j |> member "forum_topic_edited" |> to_forum_topic_edited_option in
  let forum_topic_closed = j |> member "forum_topic_closed" |> to_forum_topic_closed_option in
  let forum_topic_reopened = j |> member "forum_topic_reopened" |> to_forum_topic_reopened_option in
  let general_topic_forum_hidden = j |> member "general_topic_forum_hidden" |> to_general_topic_forum_hidden_option in
  let general_topic_forum_unhidden = j |> member "general_topic_forum_unhidden" |> to_general_topic_forum_unhidden_option in
  let giveaway_created = j |> member "giveaway_created" |> to_giveaway_created_option in
  let giveaway = j |> member "giveaway" |> to_giveaway_option in
  let giveaway_winners = j |> member "giveaway_winners" |> to_giveaway_winners_option in
  let giveaway_completed = j |> member "giveaway_completed" |> to_giveaway_completed_option in
  let video_chat_scheduled = j |> member "video_chat_scheduled" |> to_video_chat_scheduled_option in
  let video_chat_ended = j |> member "video_chat_ended" |> to_video_chat_ended_option in
  let video_chat_participants_invited = j |> member "video_chat_participants_invited" |> to_video_chat_participants_invited_option in
  let web_app_data = j |> member "web_app_data" |> to_web_app_data_option in
  let reply_markup = j |> member "reply_markup" |> to_inline_keyboard_markup_option in
  Some({
    message_id;
    message_thread_id;
    from;
    sender_chat;
    sender_boost_count;
    sender_business_bot;
    date;
    business_connection_id;
    chat;
    forward_origin;
    is_topic_message;
    is_automatic_forward;
    reply_to_message;
    external_reply;
    quote;
    reply_to_story;
    via_bot;
    edit_date;
    has_protected_content;
    is_from_offline;
    media_group_id;
    author_signature;
    text;
    entities;
    link_preview_options;
    effect_id;
    animation;
    audio;
    document;
    paid_media;
    photo;
    sticker;
    story;
    video;
    video_note;
    voice;
    caption;
    caption_entities;
    show_caption_above_media;
    has_media_spoiler;
    contact;
    dice;
    game;
    poll;
    venue;
    location;
    new_chat_members;
    left_chat_member;
    new_chat_title;
    new_chat_photo;
    delete_chat_photo;
    group_chat_created;
    supergroup_chat_created;
    channel_chat_created;
    message_auto_delete_timer_changed;
    migrate_to_chat_id;
    migrate_from_chat_id;
    pinned_message;
    users_shared;
    chat_shared;
    connected_website;
    write_access_allowed;
    passport_data;
    proximity_alert_triggered;
    boost_added;
    chat_background_set;
    forum_topic_created;
    forum_topic_edited;
    forum_topic_closed;
    forum_topic_reopened;
    general_topic_forum_hidden;
    general_topic_forum_unhidden;
    giveaway_created;
    giveaway;
    giveaway_winners;
    giveaway_completed;
    video_chat_scheduled;
    video_chat_ended;
    video_chat_participants_invited;
    web_app_data;
    reply_markup;
  })

let to_message j =
  let open BatOption in
  get (to_message_option j)

let to_business_connection_option j : business_connection option =
  let open Yojson.Safe.Util in
  let open BatOption in
  let to_bool_option' x =
    x |> to_bool_option |> default false
  in
  match j with
  | `Null -> None
  | _ ->
  let id = j |> member "id" |> to_string in
  let user = j |> member "user" |> to_user in
  let user_chat_id = j |> member "user_chat_id" |> to_int64 in
  let date = j |> member "date" |> to_int in
  let can_reply = j |> member "can_reply" |> to_bool_option' in
  let is_enabled = j |> member "is_enabled" |> to_bool_option' in
  Some({
    id;
    user;
    user_chat_id;
    date;
    can_reply;
    is_enabled;
  })

let to_business_messages_deleted_option j : business_messages_deleted option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let business_connection_id = j |> member "business_connection_id" |> to_string in
  let chat = j |> member "chat" |> to_chat in
  let message_ids = j |> member "message_ids" |> to_list |> List.map to_int in
  Some({
    business_connection_id;
    chat;
    message_ids;
  })

let to_reaction_type_emoji j : reaction_type_emoji =
  let open Yojson.Safe.Util in
  let _type = j |> member "type" |> to_string in
  let emoji = j |> member "emoji" |> to_string in
  { _type; emoji }

let to_reaction_type_custom_emoji j : reaction_type_custom_emoji =
  let open Yojson.Safe.Util in
  let _type = j |> member "type" |> to_string in
  let custom_emoji_id = j |> member "custom_emoji_id" |> to_string in
  { _type; custom_emoji_id }

let to_reaction_type_paid j : reaction_type_paid =
  let open Yojson.Safe.Util in
  let _type = j |> member "type" |> to_string in
  { _type } 

let to_reaction_type_option j : reaction_type option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let _type = j |> member "type" |> to_string in
  match _type with
  | "emoji" -> Some(ReactionTypeEmoji(to_reaction_type_emoji j))
  | "custom_emoji" -> Some(ReactionTypeCustomEmoji(to_reaction_type_custom_emoji j))
  | "paid" -> Some(ReactionTypePaid(to_reaction_type_paid j))
  | _ -> failwith @@ Printf.sprintf "Unhandled reaction type: %s" _type

let to_reaction_type j : reaction_type =
  let open BatOption in
  get (to_reaction_type_option j)

let to_reaction_type_list j : reaction_type list =
  match j with
  | `Null -> []
  | `List(l) ->
    List.map to_reaction_type l
  | _ -> []

let to_message_reaction_updated_option j : message_reaction_updated option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let chat = j |> member "chat" |> to_chat in
  let message_id = j |> member "message_id" |> to_int in
  let user = j |> member "user" |> to_user_option in
  let actor_chat = j |> member "actor_chat" |> to_chat_option in
  let date = j |> member "date" |> to_int in
  let old_reaction = j |> member "old_reaction" |> to_reaction_type_list in
  let new_reaction = j |> member "new_reaction" |> to_reaction_type_list in
  Some({
    chat;
    message_id;
    user;
    actor_chat;
    date;
    old_reaction;
    new_reaction;
  })

let to_reaction_count_option j : reaction_count option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let _type = j |> member "type" |> to_reaction_type in
  let total_count = j |> member "total_count" |> to_int in
  Some({ _type; total_count })

let to_reaction_count j =
  let open BatOption in
  get (to_reaction_count_option j)

let to_message_reaction_count_updated_option j : message_reaction_count_updated option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let chat = j |> member "chat" |> to_chat in
  let message_id = j |> member "message_id" |> to_int in
  let date = j |> member "date" |> to_int in
  let reactions = j |> member "reactions" |> to_list |> List.map to_reaction_count in
  Some({
    chat;
    message_id;
    date;
    reactions;
  })

let to_inline_query_option j : inline_query option =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> None
  | _ ->
  let id = j |> member "id" |> to_string in
  let from = j |> member "from" |> to_user in
  let query = j |> member "query" |> to_string in
  let offset = j |> member "offset" |> to_string in
  let chat_type = j |> member "chat_type" |> to_string_option in
  let location = j |> member "location" |> to_location_option in
  Some({
    id;
    from;
    query;
    offset;
    chat_type;
    location;
  })

let to_yojson_option f j =
  match j with
  | `Null -> None
  | _ -> Some(f j)

let to_chosen_inline_result_option j : chosen_inline_result option =
  let open Yojson.Safe.Util in
  let f j : chosen_inline_result =
    let result_id = j |> member "result_id" |> to_string in 
    let from = j |> member "from" |> to_user in
    let location = j |> member "location" |> to_location_option in
    let inline_message_id = j |> member "inline_message_id" |> to_string_option in
    let query = j |> member "query" |> to_string in
    {
      result_id;
      from;
      location;
      inline_message_id;
      query;
    }
  in
  to_yojson_option f j

let to_maybe_inaccessible_message_option j =
  let f j : maybe_inaccessible_message =
    let open Yojson.Safe.Util in
    let date = j |> member "date" |> to_int in
    match date with
    | 0 -> InaccessibleMessage(to_inaccessible_message j)
    | _ -> Message(to_message j)
  in
  to_yojson_option f j

let to_callback_query_option j =
  let open Yojson.Safe.Util in
  let f j : callback_query =
    let id = j |> member "id" |> to_string in
    let from = j |> member "from" |> to_user in
    let message = j |> member "message" |> to_maybe_inaccessible_message_option in
    let inline_message_id = j |> member "inline_message_id" |> to_string_option in
    let chat_instance = j |> member "chat_instance" |> to_string_option in
    let data = j |> member "data" |> to_string_option in
    let game_short_name = j |> member "game_short_name" |> to_string_option in
    {
      id;
      from;
      message;
      inline_message_id;
      chat_instance;
      data;
      game_short_name;
    }
  in
  to_yojson_option f j

let to_shipping_address j : shipping_address =
  let open Yojson.Safe.Util in
  let country = j |> member "country" |> to_string in
  let state = j |> member "state" |> to_string in
  let city = j |> member "city" |> to_string in
  let street_line1 = j |> member "street_line1" |> to_string in
  let street_line2 = j |> member "street_line2" |> to_string in
  let post_code = j |> member "post_code" |> to_string in
  {
    country;
    state;
    city;
    street_line1;
    street_line2;
    post_code;
  }

let to_shipping_address_option j =
  to_yojson_option (to_shipping_address) j

let to_shipping_query j : shipping_query = 
  let open Yojson.Safe.Util in
  let id = j |> member "id" |> to_string in
  let from = j |> member "from" |> to_user in
  let invoice_payload = j |> member "invoice_payload" |> to_string in
  let shipping_address = j |> member "shipping_address" |> to_shipping_address in
  {
    id;
    from;
    invoice_payload;
    shipping_address;
  }

let to_shipping_query_option j =
  to_yojson_option (to_shipping_query) j

let to_order_info j : order_info =
  let open Yojson.Safe.Util in
  let name = j |> member "name" |> to_string_option in
  let phone_number = j |> member "phone_number" |> to_string_option in
  let email = j |> member "email" |> to_string_option in
  let shipping_address = j |> member "shipping_address" |> to_shipping_address_option in
  {
    name;
    phone_number;
    email;
    shipping_address;
  }

let to_order_info_option j =
  to_yojson_option (to_order_info) j

let to_pre_checkout_query j : pre_checkout_query =
  let open Yojson.Safe.Util in
  let id = j |> member "id" |> to_string in
  let from = j |> member "from" |> to_user in
  let currency = j |> member "currency" |> to_string in
  let total_amount = j |> member "total_amount" |> to_int in
  let invoice_payload = j |> member "invoice_payload" |> to_string in
  let shipping_option_id = j |> member "shipping_option_id" |> to_string_option in
  let order_info = j |> member "order_info" |> to_order_info_option in
  {
    id;
    from;
    currency;
    total_amount;
    invoice_payload;
    shipping_option_id;
    order_info;
  }

let to_pre_checkout_query_option j =
  to_yojson_option (to_pre_checkout_query) j

let to_paid_media_purchased j : paid_media_purchased =
  let open Yojson.Safe.Util in
  let from = j |> member "from" |> to_user in
  let paid_media_payload = j |> member "paid_media_payload" |> to_string in
  { from; paid_media_payload }

let to_paid_media_purchased_option j =
  to_yojson_option (to_paid_media_purchased) j

let to_poll_answer j : poll_answer =
  let open Yojson.Safe.Util in
  let poll_id = j |> member "poll_id" |> to_string in
  let voter_chat = j |> member "voter_chat" |> to_chat_option in
  let user = j |> member "user" |> to_user_option in
  let option_ids = j |> member "option_ids" |> to_list |> List.map to_int in
  {
    poll_id;
    voter_chat;
    user;
    option_ids;
  }

let to_poll_answer_option j =
  to_yojson_option (to_poll_answer) j

let to_bool_option' x =
  let open Yojson.Safe.Util in
  let open BatOption in
  x |> to_bool_option |> default false

let to_chat_member_owner j : chat_member_owner =
  let open Yojson.Safe.Util in
  let status = j |> member "status" |> to_string in
  let user = j |> member "user" |> to_user in
  let is_anonymous = j |> member "is_anonymous" |> to_bool_option' in
  let custom_title = j |> member "custom_title" |> to_string_option in
  {
    status;
    user;
    is_anonymous;
    custom_title;
  }

let to_chat_member_administrator j : chat_member_administrator =
  let open Yojson.Safe.Util in
  let status = j |> member "status" |> to_string in
  let user = j |> member "user" |> to_user in
  let can_be_edited = j |> member "can_be_edited" |> to_bool_option' in
  let is_anonymous = j |> member "is_anonymous" |> to_bool_option' in
  let can_manage_chat = j |> member "can_manage_chat" |> to_bool_option' in
  let can_delete_messages = j |> member "can_delete_messages" |> to_bool_option' in
  let can_manage_video_chats = j |> member "can_manage_video_chats" |> to_bool_option' in
  let can_restrict_messages = j |> member "can_restrict_messages" |> to_bool_option' in
  let can_promote_members = j |> member "can_promote_members" |> to_bool_option' in
  let can_change_info = j |> member "can_change_info" |> to_bool_option' in
  let can_invite_users = j |> member "can_invite_users" |> to_bool_option' in
  let can_post_stories = j |> member "can_post_stories" |> to_bool_option' in
  let can_edit_stories = j |> member "can_edit_stories" |> to_bool_option' in
  let can_delete_stories = j |> member "can_delete_stories" |> to_bool_option' in
  let can_post_messages = j |> member "can_post_messages" |> to_bool_option' in
  let can_edit_messages = j |> member "can_edit_messages" |> to_bool_option' in
  let can_pin_messages = j |> member "can_pin_messages" |> to_bool_option' in
  let can_manage_topics = j |> member "can_manage_topics" |> to_bool_option' in
  let custom_title = j |> member "custom_title" |> to_string_option in
  {
    status;
    user;
    can_be_edited;
    is_anonymous;
    can_manage_chat;
    can_delete_messages;
    can_manage_video_chats;
    can_restrict_messages;
    can_promote_members;
    can_change_info;
    can_invite_users;
    can_post_stories;
    can_edit_stories;
    can_delete_stories;
    can_post_messages;
    can_edit_messages;
    can_pin_messages;
    can_manage_topics;
    custom_title;
  }

let to_chat_member_member j : chat_member_member =
  let open Yojson.Safe.Util in
  let status = j |> member "status" |> to_string in
  let user = j |> member "user" |> to_user in
  let until_date = j |> member "until_date" |> to_int_option in
  {
    status;
    user;
    until_date;
  }

let to_chat_member_restricted j : chat_member_restricted =
  let open Yojson.Safe.Util in
  let status = j |> member "status" |> to_string in
  let user = j |> member "user" |> to_user in
  let is_member = j |> member "is_member" |> to_bool_option' in
  let can_send_messages = j |> member "can_send_messages" |> to_bool_option' in
  let can_send_audios = j |> member "can_send_audios" |> to_bool_option' in
  let can_send_documents = j |> member "can_send_documents" |> to_bool_option' in
  let can_send_photos = j |> member "can_send_photos" |> to_bool_option' in
  let can_send_videos = j |> member "can_send_videos" |> to_bool_option' in
  let can_send_video_notes = j |> member "can_send_video_notes" |> to_bool_option' in
  let can_send_voice_notes = j |> member "can_send_voice_notes" |> to_bool_option' in
  let can_send_polls = j |> member "can_send_polls" |> to_bool_option' in
  let can_send_other_messages = j |> member "can_send_other_messages" |> to_bool_option' in
  let can_add_web_page_previews = j |> member "can_add_web_page_previews" |> to_bool_option' in
  let can_change_info = j |> member "can_change_info" |> to_bool_option' in
  let can_invite_users = j |> member "can_invite_users" |> to_bool_option' in
  let can_pin_messages = j |> member "can_pin_messages" |> to_bool_option' in
  let can_manage_topics = j |> member "can_manage_topics" |> to_bool_option' in
  let until_date = j |> member "until_date" |> to_int_option in
  {
    status;
    user;
    is_member;
    can_send_messages;
    can_send_audios;
    can_send_documents;
    can_send_photos;
    can_send_videos;
    can_send_video_notes;
    can_send_voice_notes;
    can_send_polls;
    can_send_other_messages;
    can_add_web_page_previews;
    can_change_info;
    can_invite_users;
    can_pin_messages;
    can_manage_topics;
    until_date;
  }

let to_chat_member_left j : chat_member_left =
  let open Yojson.Safe.Util in
  let status = j |> member "status" |> to_string in
  let user = j |> member "user" |> to_user in
  { status; user }

let to_chat_member_banned j : chat_member_banned =
  let open Yojson.Safe.Util in
  let status = j |> member "status" |> to_string in
  let user = j |> member "user" |> to_user in
  let until_date = j |> member "until_date" |> to_int_option in
  {
    status;
    user;
    until_date;
  }

let to_chat_member j : chat_member =
  let open Yojson.Safe.Util in
  let status = j |> member "status" |> to_string in
  match status with
  | "creator" -> ChatMemberOwner(to_chat_member_owner j)
  | "administrator" -> ChatMemberAdministrator(to_chat_member_administrator j)
  | "member" -> ChatMemberMember(to_chat_member_member j)
  | "restricted" -> ChatMemberRestricted(to_chat_member_restricted j)
  | "left" -> ChatMemberLeft(to_chat_member_left j)
  | "kicked" -> ChatMemberBanned(to_chat_member_banned j)
  | _ -> failwith @@ Printf.sprintf "Unknown chat member: %s" status

let to_chat_member_updated j : chat_member_updated =
  let open Yojson.Safe.Util in
  let chat = j |> member "chat" |> to_chat in
  let from = j |> member "from" |> to_user in
  let date = j |> member "date" |> to_int in
  let old_chat_member = j |> member "old_chat_member" |> to_chat_member in
  let new_chat_member = j |> member "new_chat_member" |> to_chat_member in
  let via_join_request = j |> member "via_join_request" |> to_bool_option' in
  let via_chat_folder_invite_link = j |> member "via_chat_folder_invite_link" |> to_bool_option' in
  {
    chat;
    from;
    date;
    old_chat_member;
    new_chat_member;
    via_join_request;
    via_chat_folder_invite_link;
  }

let to_chat_member_updated_option j =
  to_yojson_option (to_chat_member_updated) j

let to_chat_invite_link j : chat_invite_link =
  let open Yojson.Safe.Util in
  let invite_link = j |> member "invite_link" |> to_string in
  let creator = j |> member "creator" |> to_user in
  let creates_join_request = j |> member "creates_join_request" |> to_bool_option' in
  let is_primary = j |> member "is_primary" |> to_bool_option' in
  let is_revoked = j |> member "is_revoked" |> to_bool_option' in
  let name = j |> member "name" |> to_string_option in
  let expire_date = j |> member "expire_date" |> to_int_option in
  let member_limit = j |> member "member_limit" |> to_int_option in
  let pending_join_request_count = j |> member "pending_join_request_count" |> to_int_option in
  let subscription_period = j |> member "subscription_period" |> to_int_option in
  let subscription_price = j |> member "subscription_price" |> to_int_option in
  {
    invite_link;
    creator;
    creates_join_request;
    is_primary;
    is_revoked;
    name;
    expire_date;
    member_limit;
    pending_join_request_count;
    subscription_period;
    subscription_price;
  }

let to_chat_invite_link_option j =
  to_yojson_option (to_chat_invite_link) j

let to_chat_join_request j : chat_join_request =
  let open Yojson.Safe.Util in
  let chat = j |> member "chat" |> to_chat in
  let from = j |> member "from" |> to_user in
  let user_chat_id = j |> member "user_chat_id" |> to_int64 in
  let date = j |> member "date" |> to_int in
  let bio = j |> member "bio" |> to_string_option in
  let invite_link = j |> member "to_invite_link" |> to_chat_invite_link_option in
  {
    chat;
    from;
    user_chat_id;
    date;
    bio;
    invite_link;
  }

let to_chat_join_request_option j =
  to_yojson_option (to_chat_join_request) j

let to_chat_boost_source_premium j : chat_boost_source_premium =
  let open Yojson.Safe.Util in
  let source = j |> member "source" |> to_string in
  let user = j |> member "user" |> to_user in
  { source; user }

let to_chat_boost_source_gift_code j : chat_boost_source_gift_code =
  let open Yojson.Safe.Util in
  let source = j |> member "source" |> to_string in
  let user = j |> member "user" |> to_user in
  { source; user }

let to_chat_boost_source_giveaway j : chat_boost_source_giveaway =
  let open Yojson.Safe.Util in
  let source = j |> member "source" |> to_string in
  let giveaway_message_id = j |> member "giveaway_message_id" |> to_int in
  let user = j |> member "user" |> to_user_option in
  let prize_star_count = j |> member "prize_star_count" |> to_int_option in
  let is_unclaimed = j |> member "is_unclaimed" |> to_bool_option' in
  {
    source;
    giveaway_message_id;
    user;
    prize_star_count;
    is_unclaimed;
  }

let to_chat_boost_source j : chat_boost_source =
  let open Yojson.Safe.Util in
  let source = j |> member "source" |> to_string in
  match source with
  | "premium" -> ChatBoostSourcePremium(to_chat_boost_source_premium j)
  | "gift_code" -> ChatBoostSourceGiftCode(to_chat_boost_source_gift_code j)
  | "giveaway" -> ChatBoostSourceGiveaway(to_chat_boost_source_giveaway j)
  | _ -> failwith @@ Printf.sprintf "Unknown chat boost source: %s" source

let to_chat_boost j : chat_boost =
  let open Yojson.Safe.Util in
  let boost_id = j |> member "boost_id" |> to_string in
  let add_date = j |> member "add_date" |> to_int in
  let expiration_date = j |> member "expiration_date" |> to_int in
  let source = j |> member "source" |> to_chat_boost_source in
  {
    boost_id;
    add_date;
    expiration_date;
    source;
  }

let to_chat_boost_updated j : chat_boost_updated =
  let open Yojson.Safe.Util in
  let chat = j |> member "chat" |> to_chat in
  let boost = j |> member "boost" |> to_chat_boost in
  { chat; boost }

let to_chat_boost_updated_option j =
  to_yojson_option (to_chat_boost_updated) j

let json_to_update j : update =
  let open Yojson.Safe.Util in
  let update_id = j |> member "update_id" |> to_int in
  let message = j |> member "message" |> to_message_option in
  let edited_message = j |> member "edited_message" |> to_message_option in
  let channel_post = j |> member "channel_post" |> to_message_option in
  let edited_channel_post = j |> member "edited_channel_post" |> to_message_option in
  let business_connection = j |> member "business_connection" |> to_business_connection_option in
  let business_message = j |> member "business_message" |> to_message_option in
  let editted_business_message = j |> member "editted_business_message" |> to_message_option in
  let deleted_business_messages = j |> member "deleted_business_messages" |> to_business_messages_deleted_option in
  let message_reaction = j |> member "message_reaction" |> to_message_reaction_updated_option in
  let message_reaction_count = j |> member "message_reaction_count" |> to_message_reaction_count_updated_option in
  let inline_query = j |> member "inline_query" |> to_inline_query_option in
  let chosen_inline_result = j |> member "chosen_inline_result" |> to_chosen_inline_result_option in
  let callback_query = j |> member "callback_query" |> to_callback_query_option in
  let shipping_query = j |> member "shipping_query" |> to_shipping_query_option in
  let pre_checkout_query = j |> member "pre_checkout_query" |> to_pre_checkout_query_option in
  let purchased_paid_media = j |> member "purchased_paid_media" |> to_paid_media_purchased_option in
  let poll = j |> member "poll" |> to_poll_option in
  let poll_answer = j |> member "poll_answer" |> to_poll_answer_option in
  let my_chat_member = j |> member "my_chat_member" |> to_chat_member_updated_option in
  let chat_member = j |> member "chat_member" |> to_chat_member_updated_option in
  let chat_join_request = j |> member "chat_join_request" |> to_chat_join_request_option in
  let chat_boost = j |> member "chat_boost" |> to_chat_boost_updated_option in
  let removed_chat_boost = j |> member "removed_chat_boost" |> to_chat_boost_updated_option in
  { update_id
  ; message 
  ; edited_message
  ; channel_post
  ; edited_channel_post
  ; business_connection
  ; business_message
  ; editted_business_message
  ; deleted_business_messages
  ; message_reaction
  ; message_reaction_count
  ; inline_query
  ; chosen_inline_result
  ; callback_query
  ; shipping_query
  ; pre_checkout_query
  ; purchased_paid_media
  ; poll
  ; poll_answer
  ; my_chat_member
  ; chat_member
  ; chat_join_request
  ; chat_boost
  ; removed_chat_boost
  }

let yojson_to_message_update_from_results j =
  let open Yojson.Safe.Util in
  let ok = j |> member "ok" |> to_bool in
  match ok with
  | false -> None
  | true ->
    Printf.printf "DEBUG: Attempting to convert result to list\n";
    let result = j |> member "result" |> to_list in
    Printf.printf "DEBUG: Converted result to list\n";
    let updates = List.map json_to_update result in
    Some(updates)

type ergonomic_message_update =
  | TextMessage of chat * user * string
  | AddedUsersToChat of chat * user * user list
  | RemovedUserFromChat of chat * user * user
  | UserJoined of chat * user
  | ManyUsersJoined of chat * user list
  | UserLeft of chat * user
  | GroupCreated of chat
  | UnknownMessage of message
  | UnknownUpdate of update

(*
type errtype =
  | UnknownError of string
[@@deriving show]

type return_type =
  | Ok of string
  | Fail of errtype
[@@deriving show]
*)

let message_to_ergonomic_message_update (msg : message) =
  let open BatOption in
  if List.length msg.new_chat_members = 1 && is_none msg.from then
    let user = List.hd msg.new_chat_members in
    let chat = msg.chat in
    UserJoined(chat, user)
  else if msg.new_chat_members <> [] && is_some msg.from then
    let user = get msg.from in
    let chat = msg.chat in
    let new_users = msg.new_chat_members in
    AddedUsersToChat(chat, user, new_users)
  else if is_some msg.from && is_some msg.left_chat_member then
    let removed_by = get msg.from in
    let removed = get msg.left_chat_member in
    let chat = msg.chat in
    RemovedUserFromChat(chat, removed_by, removed)
  else if is_none msg.from && is_some msg.left_chat_member then
    let left = get msg.left_chat_member in
    let chat = msg.chat in
    UserLeft(chat, left)
  else if msg.group_chat_created = true then
    GroupCreated(msg.chat)
  else if is_some msg.from && is_some msg.text && msg.entities = [] then
    let text = get msg.text in
    let user = get msg.from in
    let chat = msg.chat in
    TextMessage(chat, user, text)
  else
    UnknownMessage msg
  
let update_to_ergonomic_message_update (upd : update) =
  let open BatOption in
  if is_some upd.message then
    message_to_ergonomic_message_update (get upd.message)
  else
    UnknownUpdate upd