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
let true_to_yojson () = bool_to_yojson true
let int_to_yojson x : t = `Int x
let int64_to_yojson x : t = `Intlit (BatInt64.to_string x)
let int_list_to_yojson l : t = `List (List.map int_to_yojson l)
let float_to_yojson x : t = `Float x
let string_to_yojson x : t = `String x
let formatting_option_to_yojson = Telegram_types_utils.formatting_option_to_yojson_string
let message_entity_list_to_yojson = Telegram_types_utils.message_entity_list_to_yojson
let link_preview_options_to_yojson = Telegram_types_utils.link_preview_options_to_yojson
let reply_parameters_to_yojson = Telegram_types_utils.reply_parameters_to_yojson
let reply_markup_type_to_yojson = Telegram_types_utils.reply_markup_to_yojson
let input_file_or_string_type_to_yojson = Telegram_types_utils.input_file_or_string_type_to_yojson
let input_file_or_string_type_list_to_yojson x : t = `List (List.map input_file_or_string_type_to_yojson x)
let input_poll_option_to_yojson x : t = Telegram_types_utils.input_poll_option_to_yojson x
let input_poll_option_list_to_yojson x : t = `List (List.map input_poll_option_to_yojson x)
let poll_type_to_yojson = Telegram_types_utils.poll_type_to_yojson
let chat_action_to_yojson = Telegram_types_utils.chat_action_to_yojson
let reaction_type_to_yojson = Telegram_types_utils.reaction_type_to_yojson
let reaction_type_list_to_yojson x = `List (List.map reaction_type_to_yojson x)
let chat_permissions_to_yojson = Telegram_types_utils.chat_permissions_to_yojson

let bool_to_yojson_l = to_wrap_l bool_to_yojson
let int_to_yojson_l = to_wrap_l int_to_yojson
let int_list_to_yojson_l = to_wrap_l int_list_to_yojson
let float_to_yojson_l = to_wrap_l float_to_yojson
let string_to_yojson_l = to_wrap_l string_to_yojson
let target_chat_to_yojson_l = to_wrap_l target_chat_to_yojson
let formatting_option_to_yojson_l = to_wrap_l formatting_option_to_yojson 
let message_entity_list_to_yojson_l = to_wrap_l message_entity_list_to_yojson
let link_preview_options_to_yojson_l = to_wrap_l link_preview_options_to_yojson
let reply_parameters_to_yojson_l = to_wrap_l reply_parameters_to_yojson
let reply_markup_type_to_yojson_l = to_wrap_l reply_markup_type_to_yojson
let true_to_yojson_l = to_wrap_l true_to_yojson
let input_file_or_string_type_to_yojson_l = to_wrap_l input_file_or_string_type_to_yojson
let input_file_or_string_type_list_to_yojson_l = to_wrap_l input_file_or_string_type_list_to_yojson
let input_poll_option_list_to_yojson_l = to_wrap_l input_poll_option_list_to_yojson
let poll_type_to_yojson_l = to_wrap_l poll_type_to_yojson
let chat_action_to_yojson_l = to_wrap_l chat_action_to_yojson
let reaction_type_list_to_yojson_l = to_wrap_l reaction_type_list_to_yojson
let int64_to_yojson_l = to_wrap_l int64_to_yojson
let chat_permissions_to_yojson_l = to_wrap_l chat_permissions_to_yojson

let bool_option_to_yojson_l = lift_opt bool_to_yojson_l
let int_option_to_yojson_l = lift_opt int_to_yojson_l
let float_option_to_yojson_l = lift_opt float_to_yojson_l
let string_option_to_yojson_l = lift_opt string_to_yojson_l
let formatting_option_option_to_yojson_l = lift_opt formatting_option_to_yojson_l
let message_entity_list_option_to_yojson_l = lift_opt message_entity_list_to_yojson_l
let link_preview_options_option_t_to_yojson = lift_opt link_preview_options_to_yojson_l
let reply_parameters_option_t_to_yojson_l = lift_opt reply_parameters_to_yojson_l
let reply_markup_type_option_to_yojson_l = lift_opt reply_markup_type_to_yojson_l
let true_option_to_yojson_l = lift_opt true_to_yojson_l
let input_file_or_string_type_option_to_yojson_l = lift_opt input_file_or_string_type_to_yojson_l
let input_file_or_string_type_list_option_to_yojson_l = lift_opt input_file_or_string_type_list_to_yojson_l
let input_poll_option_list_option_to_yojson_l = lift_opt input_poll_option_list_to_yojson_l
let poll_type_option_to_yojson_l = lift_opt poll_type_to_yojson_l
let chat_action_option_to_yojson_l = lift_opt chat_action_to_yojson_l
let reaction_type_list_option_to_yojson_l = lift_opt reaction_type_list_to_yojson_l
let int64_option_to_yojson_l = lift_opt int64_to_yojson_l
let chat_permissions_option_to_yojson_l = lift_opt chat_permissions_to_yojson_l

let business_connection_id_option_to_yojson_l bci = string_option_to_yojson_l "business_connection_id" bci
let chat_id_to_yojson_l c_id = target_chat_to_yojson_l "chat_id" c_id
let from_chat_id_to_yojson_l c_id = target_chat_to_yojson_l "from_chat_id" c_id
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
let reply_parameters_option_to_yojson_l rp = reply_parameters_option_t_to_yojson_l "reply_parameters" rp
let reply_markup_option_to_yojson_l rm = reply_markup_type_option_to_yojson_l "reply_markup" rm
let video_start_timestamp_option_to_yojson_l fm = int_option_to_yojson_l "video_start_timestamp" fm
let message_id_to_yojson_l mi = int_to_yojson_l "message_id" mi
let message_ids_to_yojson_l mi = int_list_to_yojson_l "message_ids" mi
let caption_option_to_yojson_l c = string_option_to_yojson_l "caption" c
let caption_entities_option_to_yojson_l ce = message_entity_list_option_to_yojson_l "caption_entities" ce
let show_caption_above_media_option_to_yojson_l scam = bool_option_to_yojson_l "show_caption_above_media" scam
let remove_caption_option_to_yojson_l rc = true_option_to_yojson_l "remove_caption" rc
let photo_option_to_yojson_l p = input_file_or_string_type_option_to_yojson_l "photo" p
let photo_to_yojson_l p = input_file_or_string_type_to_yojson_l "photo" p
let has_spoiler_option_to_yojson_l hs = bool_option_to_yojson_l "has_spoiler" hs
let audio_to_yojson_l a = input_file_or_string_type_to_yojson_l "audio" a
let duration_option_to_yojson_l d = int_option_to_yojson_l "duration" d
let performer_option_to_yojson_l p = string_option_to_yojson_l "performer" p
let title_option_to_yojson_l t = string_option_to_yojson_l "title" t
let thumbnail_option_to_yojson_l t = input_file_or_string_type_option_to_yojson_l "thumbnail" t
let document_to_yojson_l d = input_file_or_string_type_to_yojson_l "document" d
let disable_content_type_detection_option_to_yojson_l d = bool_option_to_yojson_l "disable_content_type_detection" d
let video_to_yojson_l v = input_file_or_string_type_to_yojson_l "video" v
let animation_to_yojson_l a = input_file_or_string_type_to_yojson_l "animation" a
let width_option_to_yojson_l w = int_option_to_yojson_l "width" w
let height_option_to_yojson_l h = int_option_to_yojson_l "height" h
let cover_option_to_yojson_l c = input_file_or_string_type_option_to_yojson_l "cover" c
let start_timestamp_option_to_yojson_l st = int_option_to_yojson_l "start_timestamp" st
let voice_to_yojson_l v = input_file_or_string_type_to_yojson_l "voice" v
let video_note_to_yojson_l v = input_file_or_string_type_to_yojson_l "video_note" v
let length_option_to_yojson_l l = int_option_to_yojson_l "length" l
let star_count_to_yojson_l sc = int_to_yojson_l "star_count" sc
let media_to_yojson_l m = input_file_or_string_type_list_to_yojson_l "media" m
let payload_option_to_yojson_l p = string_option_to_yojson_l "payload" p
let latitude_to_yojson x = float_to_yojson_l "latitude" x
let longtitude_to_yojson x = float_to_yojson_l "longtitude" x
let horizontal_accuracy_to_yojson x = float_option_to_yojson_l "horizontal_accuracy" x
let live_period_to_yojson x = int_option_to_yojson_l "live_period" x
let heading_to_yojson x = int_option_to_yojson_l "heading" x
let proximity_alert_triggered_to_yojson x = int_option_to_yojson_l "proximity_alert_triggered" x
let title_to_yojson x = string_to_yojson_l "title" x
let address_to_yojson x = string_to_yojson_l "address" x
let foursquare_id_option_to_yojson x = string_option_to_yojson_l "foursquare_id" x
let foursquare_type_option_to_yojson x = string_option_to_yojson_l "foursquare_type" x
let google_place_id_option_to_yojson x = string_option_to_yojson_l "fgoogle_place_id" x
let google_place_type_option_to_yojson x = string_option_to_yojson_l "google_place_type" x
let phone_number_to_yojson x = string_to_yojson_l "phone_number" x
let first_name_to_yojson x = string_to_yojson_l "first_name" x
let last_name_to_yojson x = string_option_to_yojson_l "last_name" x
let vcard_to_yojson x = string_option_to_yojson_l "vcard" x
let question_to_yojson x = string_to_yojson_l "question" x
let question_parse_mode_to_yojson x = formatting_option_option_to_yojson_l "question_parse_mode" x
let question_entities_to_yojson x = message_entity_list_option_to_yojson_l "question_entities" x
let options_to_yojson x = input_poll_option_list_to_yojson_l "options" x
let is_anonymous_to_yojson x = bool_option_to_yojson_l "is_anonymous" x
let poll_type_to_yojson x = poll_type_option_to_yojson_l "_type" x
let allows_multiple_answers_to_yojson x = bool_option_to_yojson_l "allows_multiple_answers" x
let correct_option_id_to_yojson x = int_option_to_yojson_l "correct_option_id" x
let explanation_to_yojson x = string_option_to_yojson_l "explanation" x
let explanation_parse_mode_to_yojson x = formatting_option_option_to_yojson_l "explanation_parse_mode" x
let explanation_entities_to_yojson x = message_entity_list_option_to_yojson_l "explanation_entities" x
let open_period_to_yojson x = int_option_to_yojson_l "open_period" x
let close_date_to_yojson x = int_option_to_yojson_l "close_date" x
let is_closed_to_yojson x = bool_option_to_yojson_l "is_closed" x
let dice_to_yojson x = string_option_to_yojson_l "dice" x
let action_to_yojson x = chat_action_to_yojson_l "action" x
let reaction_to_yojson x = reaction_type_list_option_to_yojson_l "reaction" x
let is_big_to_yojson x = bool_option_to_yojson_l "is_big" x
let user_id_to_yojson x = int64_to_yojson_l "user_id" x
let offset_to_yojson x = int_option_to_yojson_l "offset" x
let limit_to_yojson x = int_option_to_yojson_l "limit" x
let emoji_status_custom_emoji_id_to_yojson x = string_option_to_yojson_l "emoji_status_custom_emoji_id" x
let emoji_status_expiration_date_to_yojson x = int_option_to_yojson_l "emoji_status_expiration_date" x
let file_id_to_yojson x = string_to_yojson_l "file_id" x
let until_date_to_yojson x = int_option_to_yojson_l "until_date" x
let revoke_messages_to_yojson x = bool_option_to_yojson_l "revoke_messages" x
let only_if_banned_to_yojson x = bool_option_to_yojson_l "only_if_banned" x
let permissions_to_yojson x = chat_permissions_to_yojson_l "permissions" x
let use_independent_chat_permissions_to_yojson x = bool_option_to_yojson_l "use_independent_chat_permissions" x

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
  let reply_parameters = reply_parameters_option_to_yojson_l sm.reply_parameters in
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

let forward_message_to_yojson (fm : forward_message) : t =
  let chat_id = chat_id_to_yojson_l fm.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l fm.message_thread_id in
  let from_chat_id = from_chat_id_to_yojson_l fm.from_chat_id in
  let video_start_timestamp = video_start_timestamp_option_to_yojson_l fm.video_start_timestamp in
  let disable_notification = disable_notification_option_to_yojson_l fm.disable_notification in
  let protect_content = protect_content_option_to_yojson_l fm.protect_content in
  let message_id = message_id_to_yojson_l fm.message_id in
  build_assoc
  [ chat_id
  ; message_thread_id
  ; from_chat_id
  ; video_start_timestamp
  ; disable_notification
  ; protect_content
  ; message_id
  ]

let forward_messages_to_yojson (fm : forward_messages) : t =
  let chat_id = chat_id_to_yojson_l fm.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l fm.message_thread_id in
  let from_chat_id = from_chat_id_to_yojson_l fm.from_chat_id in
  let message_ids = message_ids_to_yojson_l fm.message_ids in
  let disable_notification = disable_notification_option_to_yojson_l fm.disable_notification in
  let protect_content = protect_content_option_to_yojson_l fm.protect_content in
  build_assoc
  [ chat_id
  ; message_thread_id
  ; from_chat_id
  ; message_ids
  ; disable_notification
  ; protect_content
  ]

let copy_message_to_yojson (cm : copy_message) : t =
  let chat_id = chat_id_to_yojson_l cm.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l cm.message_thread_id in
  let from_chat_id = from_chat_id_to_yojson_l cm.from_chat_id in
  let message_id = message_id_to_yojson_l cm.message_id in
  let video_start_timestamp = video_start_timestamp_option_to_yojson_l cm.video_start_timestamp in
  let caption = caption_option_to_yojson_l cm.caption in
  let parse_mode = parse_mode_option_to_yojson_l cm.parse_mode in
  let caption_entities = caption_entities_option_to_yojson_l cm.caption_entities in
  let show_caption_above_media = show_caption_above_media_option_to_yojson_l cm.show_caption_above_media in
  let disable_notification = disable_notification_option_to_yojson_l cm.disable_notification in
  let protect_content = protect_content_option_to_yojson_l cm.protect_content in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l cm.allow_paid_broadcast in
  let reply_parameters = reply_parameters_option_to_yojson_l cm.reply_parameters in
  let reply_markup = reply_markup_option_to_yojson_l cm.reply_markup in
  build_assoc
  [ chat_id
  ; message_thread_id
  ; from_chat_id
  ; message_id
  ; video_start_timestamp
  ; caption
  ; parse_mode
  ; caption_entities
  ; show_caption_above_media
  ; disable_notification
  ; protect_content
  ; allow_paid_broadcast
  ; reply_parameters
  ; reply_markup
  ]

let copy_messages_to_yojson (cm : copy_messages) : t =
  let chat_id = chat_id_to_yojson_l cm.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l cm.message_thread_id in
  let from_chat_id = from_chat_id_to_yojson_l cm.from_chat_id in
  let message_ids = message_ids_to_yojson_l cm.message_ids in
  let disable_notification = disable_notification_option_to_yojson_l cm.disable_notification in
  let protect_content = protect_content_option_to_yojson_l cm.protect_content in
  let remove_caption = remove_caption_option_to_yojson_l cm.remove_caption in
  build_assoc
  [ chat_id
  ; message_thread_id
  ; from_chat_id
  ; message_ids
  ; disable_notification
  ; protect_content
  ; remove_caption
  ]

let send_photo_to_yojson (sp : send_photo) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l sp.business_connection_id in
  let chat_id = chat_id_to_yojson_l sp.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l sp.message_thread_id in
  let photo = photo_to_yojson_l sp.photo in
  let caption = caption_option_to_yojson_l sp.caption in
  let parse_mode = parse_mode_option_to_yojson_l sp.parse_mode in
  let caption_entities = caption_entities_option_to_yojson_l sp.caption_entities in
  let show_caption_above_media = show_caption_above_media_option_to_yojson_l sp.show_caption_above_media in
  let has_spoiler = has_spoiler_option_to_yojson_l sp.has_spoiler in
  let disable_notification = disable_notification_option_to_yojson_l sp.disable_notification in
  let protect_content = protect_content_option_to_yojson_l sp.protect_content in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l sp.allow_paid_broadcast in
  let message_effect_id = message_effect_id_option_to_yojson_l sp.message_effect_id in
  let reply_parameters = reply_parameters_option_to_yojson_l sp.reply_parameters in
  let reply_markup = reply_markup_option_to_yojson_l sp.reply_markup in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; message_thread_id
  ; photo
  ; caption
  ; parse_mode
  ; caption_entities
  ; show_caption_above_media
  ; has_spoiler
  ; disable_notification
  ; protect_content
  ; allow_paid_broadcast
  ; message_effect_id
  ; reply_parameters
  ; reply_markup
  ]

let send_audio_to_yojson (sa : send_audio) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l sa.business_connection_id in
  let chat_id = chat_id_to_yojson_l sa.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l sa.message_thread_id in
  let audio = audio_to_yojson_l sa.audio in
  let caption = caption_option_to_yojson_l sa.caption in
  let parse_mode = parse_mode_option_to_yojson_l sa.parse_mode in
  let caption_entities = caption_entities_option_to_yojson_l sa.caption_entities in
  let duration = duration_option_to_yojson_l sa.duration in
  let performer = performer_option_to_yojson_l sa.performer in
  let title = title_option_to_yojson_l sa.title in
  let thumbnail = thumbnail_option_to_yojson_l sa.thumbnail in
  let disable_notification = disable_notification_option_to_yojson_l sa.disable_notification in
  let protect_content = protect_content_option_to_yojson_l sa.protect_content in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l sa.allow_paid_broadcast in
  let message_effect_id = message_effect_id_option_to_yojson_l sa.message_effect_id in
  let reply_parameters = reply_parameters_option_to_yojson_l sa.reply_parameters in
  let reply_markup = reply_markup_option_to_yojson_l sa.reply_markup in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; message_thread_id
  ; audio
  ; caption
  ; parse_mode
  ; caption_entities
  ; duration
  ; performer
  ; title
  ; thumbnail
  ; disable_notification
  ; protect_content
  ; allow_paid_broadcast
  ; message_effect_id
  ; reply_parameters
  ; reply_markup
  ]

let send_document_to_yojson (sd : send_document) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l sd.business_connection_id in
  let chat_id = chat_id_to_yojson_l sd.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l sd.message_thread_id in
  let document = document_to_yojson_l sd.document in
  let thumbnail = thumbnail_option_to_yojson_l sd.thumbnail in
  let caption = caption_option_to_yojson_l sd.caption in
  let disable_content_type_detection = disable_content_type_detection_option_to_yojson_l sd.disable_content_type_detection in
  let disable_notification = disable_notification_option_to_yojson_l sd.disable_notification in
  let protect_content = protect_content_option_to_yojson_l sd.protect_content in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l sd.allow_paid_broadcast in
  let message_effect_id = message_effect_id_option_to_yojson_l sd.message_effect_id in
  let reply_parameters = reply_parameters_option_to_yojson_l sd.reply_parameters in
  let reply_markup = reply_markup_option_to_yojson_l sd.reply_markup in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; message_thread_id
  ; document
  ; thumbnail
  ; caption
  ; disable_content_type_detection
  ; disable_notification
  ; protect_content
  ; allow_paid_broadcast
  ; message_effect_id
  ; reply_parameters
  ; reply_markup
  ]

let send_video_to_yojson (x : send_video) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l x.business_connection_id in
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l x.message_thread_id in
  let video = video_to_yojson_l x.video in
  let duration = duration_option_to_yojson_l x.duration in
  let width = width_option_to_yojson_l x.width in
  let height = height_option_to_yojson_l x.height in
  let thumbnail = thumbnail_option_to_yojson_l x.thumbnail in
  let cover = cover_option_to_yojson_l x.cover in
  let start_timestamp = start_timestamp_option_to_yojson_l x.start_timestamp in
  let caption = caption_option_to_yojson_l x.caption in
  let parse_mode = parse_mode_option_to_yojson_l x.parse_mode in
  let caption_entities = caption_entities_option_to_yojson_l x.caption_entities in
  let show_caption_above_media = show_caption_above_media_option_to_yojson_l x.show_caption_above_media in
  let disable_content_type_detection = disable_content_type_detection_option_to_yojson_l x.disable_content_type_detection in
  let disable_notification = disable_notification_option_to_yojson_l x.disable_notification in
  let protect_content = protect_content_option_to_yojson_l x.protect_content in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l x.allow_paid_broadcast in
  let message_effect_id = message_effect_id_option_to_yojson_l x.message_effect_id in
  let reply_parameters = reply_parameters_option_to_yojson_l x.reply_parameters in 
  let reply_markup = reply_markup_option_to_yojson_l x.reply_markup in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; message_thread_id
  ; video
  ; duration
  ; width
  ; height
  ; thumbnail
  ; cover
  ; start_timestamp
  ; caption
  ; parse_mode
  ; caption_entities
  ; show_caption_above_media
  ; disable_content_type_detection
  ; disable_notification
  ; protect_content
  ; allow_paid_broadcast
  ; message_effect_id
  ; reply_parameters
  ; reply_markup
  ]

let send_animation (x : send_animation) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l x.business_connection_id in
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l x.message_thread_id in
  let animation = animation_to_yojson_l x.animation in
  let duration = duration_option_to_yojson_l x.duration in
  let width = width_option_to_yojson_l x.width in
  let height = height_option_to_yojson_l x.height in
  let thumbnail = thumbnail_option_to_yojson_l x.thumbnail in
  let cover = cover_option_to_yojson_l x.cover in
  let start_timestamp = start_timestamp_option_to_yojson_l x.start_timestamp in
  let caption = caption_option_to_yojson_l x.caption in
  let parse_mode = parse_mode_option_to_yojson_l x.parse_mode in
  let caption_entities = caption_entities_option_to_yojson_l x.caption_entities in
  let show_caption_above_media = show_caption_above_media_option_to_yojson_l x.show_caption_above_media in
  let disable_content_type_detection = disable_content_type_detection_option_to_yojson_l x.disable_content_type_detection in
  let disable_notification = disable_notification_option_to_yojson_l x.disable_notification in
  let protect_content = protect_content_option_to_yojson_l x.protect_content in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l x.allow_paid_broadcast in
  let message_effect_id = message_effect_id_option_to_yojson_l x.message_effect_id in
  let reply_parameters = reply_parameters_option_to_yojson_l x.reply_parameters in 
  let reply_markup = reply_markup_option_to_yojson_l x.reply_markup in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; message_thread_id
  ; animation
  ; duration
  ; width
  ; height
  ; thumbnail
  ; cover
  ; start_timestamp
  ; caption
  ; parse_mode
  ; caption_entities
  ; show_caption_above_media
  ; disable_content_type_detection
  ; disable_notification
  ; protect_content
  ; allow_paid_broadcast
  ; message_effect_id
  ; reply_parameters
  ; reply_markup
  ]

let send_voice (x : send_voice) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l x.business_connection_id in
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l x.message_thread_id in
  let voice = voice_to_yojson_l x.voice in
  let caption = caption_option_to_yojson_l x.caption in
  let parse_mode = parse_mode_option_to_yojson_l x.parse_mode in
  let caption_entities = caption_entities_option_to_yojson_l x.caption_entities in
  let duration = duration_option_to_yojson_l x.duration in
  let disable_notification = disable_notification_option_to_yojson_l x.disable_notification in
  let protect_content = protect_content_option_to_yojson_l x.protect_content in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l x.allow_paid_broadcast in
  let message_effect_id = message_effect_id_option_to_yojson_l x.message_effect_id in
  let reply_parameters = reply_parameters_option_to_yojson_l x.reply_parameters in 
  let reply_markup = reply_markup_option_to_yojson_l x.reply_markup in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; message_thread_id
  ; voice
  ; caption
  ; parse_mode
  ; caption_entities
  ; duration
  ; disable_notification
  ; protect_content
  ; allow_paid_broadcast
  ; message_effect_id
  ; reply_parameters
  ; reply_markup
  ]

let send_video_note_to_yojson (x : send_video_note) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l x.business_connection_id in
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l x.message_thread_id in
  let video_note = video_note_to_yojson_l x.video_note in
  let duration = duration_option_to_yojson_l x.duration in
  let length = length_option_to_yojson_l x.length in
  let thumbnail = thumbnail_option_to_yojson_l x.thumbnail in
  let parse_mode = parse_mode_option_to_yojson_l x.parse_mode in
  let disable_notification = disable_notification_option_to_yojson_l x.disable_notification in
  let protect_content = protect_content_option_to_yojson_l x.protect_content in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l x.allow_paid_broadcast in
  let message_effect_id = message_effect_id_option_to_yojson_l x.message_effect_id in
  let reply_parameters = reply_parameters_option_to_yojson_l x.reply_parameters in 
  let reply_markup = reply_markup_option_to_yojson_l x.reply_markup in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; message_thread_id
  ; video_note
  ; duration
  ; length
  ; thumbnail
  ; parse_mode
  ; disable_notification
  ; protect_content
  ; allow_paid_broadcast
  ; message_effect_id
  ; reply_parameters
  ; reply_markup
  ]

let send_paid_media_to_yojson (x : send_paid_media) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l x.business_connection_id in
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let star_count = star_count_to_yojson_l x.star_count in
  let media = media_to_yojson_l x.media in
  let payload = payload_option_to_yojson_l x.payload in
  let caption = caption_option_to_yojson_l x.caption in
  let parse_mode = parse_mode_option_to_yojson_l x.parse_mode in
  let caption_entities = caption_entities_option_to_yojson_l x.caption_entities in
  let show_caption_above_media = show_caption_above_media_option_to_yojson_l x.show_caption_above_media in
  let disable_notification = disable_notification_option_to_yojson_l x.disable_notification in
  let protect_content = protect_content_option_to_yojson_l x.protect_content in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l x.allow_paid_broadcast in
  let reply_parameters = reply_parameters_option_to_yojson_l x.reply_parameters in 
  let reply_markup = reply_markup_option_to_yojson_l x.reply_markup in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; star_count
  ; media
  ; payload
  ; caption
  ; parse_mode
  ; caption_entities
  ; show_caption_above_media
  ; disable_notification
  ; protect_content
  ; allow_paid_broadcast
  ; reply_parameters
  ; reply_markup
  ]

let send_media_group_to_yojson (x : send_media_group) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l x.business_connection_id in
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l x.message_thread_id in
  let media = media_to_yojson_l x.media in
  let disable_notification = disable_notification_option_to_yojson_l x.disable_notification in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l x.allow_paid_broadcast in
  let message_effect_id = message_effect_id_option_to_yojson_l x.message_effect_id in
  let reply_parameters = reply_parameters_option_to_yojson_l x.reply_parameters in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; message_thread_id
  ; media
  ; disable_notification
  ; allow_paid_broadcast
  ; message_effect_id
  ; reply_parameters
  ]

let send_location_to_yojson (x : send_location) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l x.business_connection_id in
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l x.message_thread_id in
  let latitude = latitude_to_yojson x.latitude in
  let longtitude = longtitude_to_yojson x.longtitude in
  let horizontal_accuracy = horizontal_accuracy_to_yojson x.horizontal_accuracy in
  let live_period = live_period_to_yojson x.live_period in
  let heading = heading_to_yojson x.heading in
  let proximity_alert_triggered = proximity_alert_triggered_to_yojson x.proximity_alert_triggered in
  let disable_notification = disable_notification_option_to_yojson_l x.disable_notification in
  let protect_content = protect_content_option_to_yojson_l x.protect_content in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l x.allow_paid_broadcast in
  let message_effect_id = message_effect_id_option_to_yojson_l x.message_effect_id in
  let reply_parameters = reply_parameters_option_to_yojson_l x.reply_parameters in
  let reply_markup = reply_markup_option_to_yojson_l x.reply_markup in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; message_thread_id
  ; latitude
  ; longtitude
  ; horizontal_accuracy
  ; live_period
  ; heading
  ; proximity_alert_triggered
  ; disable_notification
  ; protect_content
  ; allow_paid_broadcast
  ; message_effect_id
  ; reply_parameters
  ; reply_markup
  ]

let send_venue_to_yojson (x : send_venue) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l x.business_connection_id in
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l x.message_thread_id in
  let latitude = latitude_to_yojson x.latitude in
  let longtitude = longtitude_to_yojson x.longtitude in
  let title = title_to_yojson x.title in
  let address = address_to_yojson x.address in
  let foursquare_id = foursquare_id_option_to_yojson x.foursquare_id in
  let foursquare_type = foursquare_type_option_to_yojson x.foursquare_type in
  let google_place_id = google_place_id_option_to_yojson x.google_place_id in
  let google_place_type = google_place_type_option_to_yojson x.google_place_type in
  let disable_notification = disable_notification_option_to_yojson_l x.disable_notification in
  let protect_content = protect_content_option_to_yojson_l x.protect_content in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l x.allow_paid_broadcast in
  let message_effect_id = message_effect_id_option_to_yojson_l x.message_effect_id in
  let reply_parameters = reply_parameters_option_to_yojson_l x.reply_parameters in
  let reply_markup = reply_markup_option_to_yojson_l x.reply_markup in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; message_thread_id
  ; latitude
  ; longtitude
  ; title
  ; address
  ; foursquare_id
  ; foursquare_type
  ; google_place_id
  ; google_place_type
  ; disable_notification
  ; protect_content
  ; allow_paid_broadcast
  ; message_effect_id
  ; reply_parameters
  ; reply_markup
  ]

let send_contact_to_yojson (x : send_contact) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l x.business_connection_id in
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l x.message_thread_id in
  let phone_number = phone_number_to_yojson x.phone_number in
  let first_name = first_name_to_yojson x.first_name in
  let last_name = last_name_to_yojson x.last_name in
  let vcard = vcard_to_yojson x.vcard in
  let disable_notification = disable_notification_option_to_yojson_l x.disable_notification in
  let protect_content = protect_content_option_to_yojson_l x.protect_content in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l x.allow_paid_broadcast in
  let message_effect_id = message_effect_id_option_to_yojson_l x.message_effect_id in
  let reply_parameters = reply_parameters_option_to_yojson_l x.reply_parameters in
  let reply_markup = reply_markup_option_to_yojson_l x.reply_markup in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; message_thread_id
  ; phone_number
  ; first_name
  ; last_name
  ; vcard
  ; disable_notification
  ; protect_content
  ; allow_paid_broadcast
  ; message_effect_id
  ; reply_parameters
  ; reply_markup
  ]

let send_poll_to_yojson (x : send_poll) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l x.business_connection_id in
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l x.message_thread_id in
  let question = question_to_yojson x.question in
  let question_parse_mode = question_parse_mode_to_yojson x.question_parse_mode in
  let question_entities = question_entities_to_yojson x.question_entities in
  let options = options_to_yojson x.options in
  let is_anonymous = is_anonymous_to_yojson x.is_anonymous in
  let _type = poll_type_to_yojson x._type in
  let allows_multiple_answers = allows_multiple_answers_to_yojson x.allows_multiple_answers in
  let correct_option_id = correct_option_id_to_yojson x.correct_option_id in
  let explanation = explanation_to_yojson x.explanation in
  let explanation_parse_mode = explanation_parse_mode_to_yojson x.explanation_parse_mode in
  let explanation_entities = explanation_entities_to_yojson x.explanation_entities in
  let open_period = open_period_to_yojson x.open_period in
  let close_date = close_date_to_yojson x.close_date in
  let is_closed = is_closed_to_yojson x.is_closed in
  let disable_notification = disable_notification_option_to_yojson_l x.disable_notification in
  let protect_content = protect_content_option_to_yojson_l x.protect_content in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l x.allow_paid_broadcast in
  let message_effect_id = message_effect_id_option_to_yojson_l x.message_effect_id in
  let reply_parameters = reply_parameters_option_to_yojson_l x.reply_parameters in
  let reply_markup = reply_markup_option_to_yojson_l x.reply_markup in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; message_thread_id
  ; question
  ; question_parse_mode
  ; question_entities
  ; options
  ; is_anonymous
  ; _type
  ; allows_multiple_answers
  ; correct_option_id
  ; explanation
  ; explanation_parse_mode
  ; explanation_entities
  ; open_period
  ; close_date
  ; is_closed
  ; disable_notification
  ; protect_content
  ; allow_paid_broadcast
  ; message_effect_id
  ; reply_parameters
  ; reply_markup
  ]

let send_dice_to_yojson (x: send_dice) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l x.business_connection_id in
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l x.message_thread_id in
  let dice = dice_to_yojson x.dice in
  let disable_notification = disable_notification_option_to_yojson_l x.disable_notification in
  let protect_content = protect_content_option_to_yojson_l x.protect_content in
  let allow_paid_broadcast = allow_paid_broadcast_option_to_yojson_l x.allow_paid_broadcast in
  let message_effect_id = message_effect_id_option_to_yojson_l x.message_effect_id in
  let reply_parameters = reply_parameters_option_to_yojson_l x.reply_parameters in
  let reply_markup = reply_markup_option_to_yojson_l x.reply_markup in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; message_thread_id
  ; dice
  ; disable_notification
  ; protect_content
  ; allow_paid_broadcast
  ; message_effect_id
  ; reply_parameters
  ; reply_markup
  ]

let send_chat_action_to_yojson (x : send_chat_action) : t =
  let business_connection_id = business_connection_id_option_to_yojson_l x.business_connection_id in
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let message_thread_id = message_thread_id_option_to_yojson_l x.message_thread_id in
  let action = action_to_yojson x.action in
  build_assoc
  [ business_connection_id
  ; chat_id
  ; message_thread_id
  ; action
  ]

let set_message_reaction_to_yojson (x : set_message_reaction) : t =
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let message_id = message_id_to_yojson_l x.message_id in
  let reaction = reaction_to_yojson x.reaction in
  let is_big = is_big_to_yojson x.is_big in
  build_assoc
  [ chat_id
  ; message_id
  ; reaction
  ; is_big
  ]

let get_user_profile_photos_to_yojson (x : get_user_profile_photos) : t =
  let user_id = user_id_to_yojson x.user_id in
  let offset = offset_to_yojson x.offset in
  let limit = limit_to_yojson x.limit in
  build_assoc
  [ user_id
  ; offset
  ; limit 
  ]

let set_user_emoji_status_to_yojson (x : set_user_emoji_status) : t =
  let user_id = user_id_to_yojson x.user_id in
  let emoji_status_custom_emoji_id = emoji_status_custom_emoji_id_to_yojson x.emoji_status_custom_emoji_id in
  let emoji_status_expiration_date = emoji_status_expiration_date_to_yojson x.emoji_status_expiration_date in
  build_assoc
  [ user_id 
  ; emoji_status_custom_emoji_id 
  ; emoji_status_expiration_date
  ]

let get_file_to_yojson (x : get_file) : t =
  let file_id = file_id_to_yojson x.file_id in
  build_assoc [ file_id ]

let ban_chat_member_to_yojson (x : ban_chat_member) : t =
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let user_id = user_id_to_yojson x.user_id in
  let until_date = until_date_to_yojson x.until_date in
  let revoke_messages = revoke_messages_to_yojson x.revoke_messages in
  build_assoc
  [ chat_id
  ; user_id
  ; until_date
  ; revoke_messages
  ]

let unban_chat_member (x : unban_chat_member) : t =
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let user_id = user_id_to_yojson x.user_id in
  let only_if_banned = only_if_banned_to_yojson x.only_if_banned in
  build_assoc
  [ chat_id
  ; user_id
  ; only_if_banned
  ]

let restrict_chat_member_to_yojson (x : restrict_chat_member) : t =
  let chat_id = chat_id_to_yojson_l x.chat_id in
  let user_id = user_id_to_yojson x.user_id in
  let permissions = permissions_to_yojson x.permissions in
  let use_independent_chat_permissions = use_independent_chat_permissions_to_yojson x.use_independent_chat_permissions in
  let until_date = until_date_to_yojson x.until_date in
  build_assoc
  [ chat_id
  ; user_id
  ; permissions
  ; use_independent_chat_permissions
  ; until_date
  ]