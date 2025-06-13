open Telegram_types
open Telegram_types_reqs
open Yojson.Safe

type tl = (string * t) list

let hole () = failwith "HOLE"

let lift_opt f n v : tl =
  match v with
  | None -> []
  | Some(v) -> f n v

let list_to_assoc l : t = `Assoc l
let build_assoc l : t =
  l
  |> BatList.flatten
  |> list_to_assoc

let wrap_in_list (n : string) f v : tl = [(n, f v)]
let to_wrap_l f = fun n v -> wrap_in_list n f v

let to_y_string s : t = `String s
let to_y_string_l n s = wrap_in_list n to_y_string s
let to_y_int_option_l n i : tl =
  match i with
  | None -> []
  | Some(v) -> [(n, `Int v)]

let to_y_int64_option_l n i : tl =
  match i with
  | None -> []
  | Some(v) -> [(n, `Intlit (BatInt64.to_string v))]

let target_chat_to_yojson (tc : target_chat) : t =
  match tc with
  | Chat(msg_id) -> `Intlit (BatInt64.to_string msg_id)
  | Channel(channel) -> `String channel


let bool_to_yojson x : t = `Bool x
let string_to_yojson x : t = `String x
let formatting_option_to_yojson = Telegram_types_utils.formatting_option_to_yojson_string
let message_entity_list_to_yojson = Telegram_types_utils.message_entity_list_to_yojson
let link_preview_options_to_yojson = Telegram_types_utils.link_preview_options_to_yojson
let reply_parameters_to_yojson = Telegram_types_utils.reply_parameters_to_yojson
let reply_markup_type_to_yojson = Telegram_types_utils.reply_markup_to_yojson

let bool_to_yojson_l = to_wrap_l bool_to_yojson
let string_to_yojson_l = to_wrap_l string_to_yojson
let target_chat_to_yojson_l = to_wrap_l target_chat_to_yojson
let formatting_option_to_yojson_l = to_wrap_l formatting_option_to_yojson 
let message_entity_list_to_yojson_l = to_wrap_l message_entity_list_to_yojson
let link_preview_options_to_yojson_l = to_wrap_l link_preview_options_to_yojson
let reply_parameters_to_yojson_l = to_wrap_l reply_parameters_to_yojson
let reply_markup_type_to_yojson_l = to_wrap_l reply_markup_type_to_yojson

let bool_option_to_yojson_l = lift_opt bool_to_yojson_l
let string_option_to_yojson_l = lift_opt string_to_yojson_l
let formatting_option_option_to_yojson_l = lift_opt formatting_option_to_yojson_l
let message_entity_list_option_to_yojson_l = lift_opt message_entity_list_to_yojson_l
let link_preview_options_option_t_to_yojson = lift_opt link_preview_options_to_yojson_l
let reply_parameters_option_t_to_yojson_l = lift_opt reply_parameters_to_yojson_l
let reply_markup_type_option_to_yojson_l = lift_opt reply_markup_type_to_yojson_l

let business_connection_id_option_to_yojson_l bci = string_option_to_yojson_l "business_connection_id" bci
let chat_id_to_yojson_l c_id = target_chat_to_yojson_l "chat_id" c_id
let text_to_yojson_l text = to_y_string_l "text" text
let message_thread_id_option_to_yojson_l v = to_y_int_option_l "message_thraed_id" v
let parse_mode_option_to_yojson_l pm = formatting_option_option_to_yojson_l "parse_mode" pm
let quote_parse_mode_option_to_yojson_l qpm = formatting_option_option_to_yojson_l "quote_parse_mode" qpm
let entities_option_to_yojson_l e = message_entity_list_option_to_yojson_l "entities" e
let link_preview_options_option_to_yojson_l lpo = link_preview_options_option_t_to_yojson "link_preview_options" lpo
let disable_notification_option_to_yojson_l dn = bool_option_to_yojson_l "disable_notification" dn
let protect_content_option_to_yojson_l pc = bool_option_to_yojson_l "protect_content" pc
let allow_paid_broadcast_option_to_yojson_l apb = bool_option_to_yojson_l "allow_paid_broadcast" apb
let message_effect_id_option_to_yojson_l mei = string_option_to_yojson_l "message_effect_id" mei
let reply_paramenters_option_to_yojson_l rp = reply_parameters_option_t_to_yojson_l "reply_parameters" rp
let reply_markup_option_to_yojson_l rm = reply_markup_type_option_to_yojson_l "reply_markup" rm

let send_message_to_yojson (sm: send_message) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l sm.business_connection_id in
  let chat_id = chat_id_to_yojson_l sm.chat_id in
  let text = text_to_yojson_l sm.text in
  let message_thread = message_thread_id_option_to_yojson_l sm.message_thread_id in
  let parse_mode = parse_mode_option_to_yojson_l sm.parse_mode in
  let entities = entities_option_to_yojson_l sm.entities in
  let link_preview_options = link_preview_options_option_to_yojson_l sm.link_preview_options in
  let disable_notification = disable_notification_option_to_yojson_l sm.disable_notification in
  let protect_content = protect_content_option_to_yojson_l sm.protect_content in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l sm.allow_paid_broadcast in
  let message_effect_id = message_effect_id_option_to_yojson_l sm.message_effect_id in
  let reply_parameters = reply_paramenters_option_to_yojson_l sm.reply_parameters in
  let reply_markup = reply_markup_option_to_yojson_l sm.reply_markup in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; text
  ; message_thread
  ; parse_mode
  ; entities
  ; link_preview_options
  ; disable_notification
  ; protect_content
  ; allow_paid_broadcast
  ; message_effect_id
  ; reply_parameters
  ; reply_markup
  ]