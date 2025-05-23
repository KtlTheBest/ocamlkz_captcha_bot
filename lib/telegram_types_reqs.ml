open Telegram_types

type send_message = {
  chat_id: target_chat;
  text: string;
  message_thread_id: int option;
  parse_mode: formatting_option option;
  entities: message_entity list option;
  link_preview_options: link_preview_options option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  message_effect_id: string option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type forward_message = {
  chat_id: target_chat;
  message_thread_id: int option;
  from_chat_id: target_chat;
  video_start_timestamp: int option;
  disable_notification: bool option;
  protect_content: bool option;
  message_id: int;
}
[@@deriving show]

type forward_messages = {
  chat_id: target_chat;
  message_thread_id: int option;
  from_chat_id: target_chat;
  message_ids: int list;
  disable_notification: bool option;
  protect_content: bool option;
}
[@@deriving show]

type copy_message = {
  chat_id: target_chat;
  message_thread_id: int option;
  from_chat_id: target_chat;
  message_id: message_id;
  video_start_timestamp: int option;
  caption: string option;
  parse_mode: formatting_option option;
  caption_entities: message_entity list option;
  show_caption_above_media: bool option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type copy_messages = {
  chat_id: target_chat;
  message_thread_id: int option;
  from_chat_id: target_chat;
  message_ids: int list;
  disable_notification: bool option;
  protect_content: bool option;
  remove_caption: unit option;
}

type send_photo = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_thread_id: int;
  photo: input_file_or_string_type;
  caption: string option;
  parse_mode: formatting_option option;
  caption_entities: message_entity list option;
  show_caption_above_media: bool option;
  has_spoiler: bool option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  message_effect_id: string option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type send_audio = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_thread_id: int;
  audio: input_file_or_string_type;
  caption: string option;
  parse_mode: formatting_option option;
  caption_entities: message_entity list option;
  duration: int option;
  performer: string option;
  title: string option;
  thumbnail: input_file_or_string_type option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  message_effect_id: string option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type send_document = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_thread_id: int;
  document: input_file_or_string_type;
  thumbnail: input_file_or_string_type option;
  caption: string option;
  parse_mode: formatting_option option;
  caption_entities: message_entity list option;
  disable_content_type_detection: bool option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  message_effect_id: string option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type send_video = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_thread_id: int;
  video: input_file_or_string_type;
  duration: int option;
  width: int option;
  height: int option;
  thumbnail: input_file_or_string_type option;
  cover: input_file_or_string_type option;
  start_timestamp: int option;
  caption: string option;
  parse_mode: formatting_option option;
  caption_entities: message_entity list option;
  show_caption_above_media: bool option;
  disable_content_type_detection: bool option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  message_effect_id: string option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type send_animation = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_thread_id: int;
  animation: input_file_or_string_type;
  duration: int option;
  width: int option;
  height: int option;
  thumbnail: input_file_or_string_type option;
  cover: input_file_or_string_type option;
  start_timestamp: int option;
  caption: string option;
  parse_mode: formatting_option option;
  caption_entities: message_entity list option;
  show_caption_above_media: bool option;
  disable_content_type_detection: bool option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  message_effect_id: string option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type send_voice = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_thread_id: int;
  voice: input_file_or_string_type;
  caption: string option;
  parse_mode: formatting_option option;
  caption_entities: message_entity list option;
  duration: int option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  message_effect_id: string option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type send_video_note = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_thread_id: int;
  video_note: input_file_or_string_type;
  duration: int option;
  length: int option;
  thumbnail: input_file_or_string_type option;
  parse_mode: formatting_option option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  message_effect_id: string option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type send_paid_media = {
  business_connection_id: string option;
  chat_id: target_chat;
  star_count: int;
  media: input_file_or_string_type list;
  payload: string option;
  caption: string option;
  parse_mode: formatting_option option;
  caption_entities: message_entity list option;
  show_caption_above_media: bool option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type send_media_group = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_thread_id: int option;
  media: input_file_or_string_type list;
  disable_notification: bool option;
  allow_paid_broadcast: bool option;
  message_effect_id: string option;
  reply_parameters: reply_parameters option;
}
[@@deriving show]

type send_location = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_thread_id: int;
  latitude: float;
  longtitude: float;
  horizontal_accuracy: float option;
  live_period: int option;
  heading: int option;
  proximity_alert_triggered: int option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  message_effect_id: string option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type send_venue = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_thread_id: int;
  latitude: float;
  longtitude: float;
  title: string;
  address: string;
  foursquare_id: string option;
  foursquare_type: string option;
  google_place_id: string option;
  google_place_type: string option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  message_effect_id: string option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type send_contact = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_thread_id: int;
  phone_number: string;
  first_name: string;
  last_name: string option;
  vcard: string option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  message_effect_id: string option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type send_poll = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_thread_id: int;
  question: string;
  question_parse_mode: formatting_option option;
  question_entities: message_entity list option;
  options: input_poll_option list;
  is_anonymous: bool option;
  _type: poll_type option;
  allows_multiple_answers: bool option;
  correct_option_id: int option;
  explanation: string option;
  explanation_parse_mode: formatting_option option;
  explanation_entities: message_entity list;
  open_period: int option;
  close_date: int option;
  is_closed: bool option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  message_effect_id: string option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type send_dice = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_thread_id: int;
  dice: string option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  message_effect_id: string option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type send_chat_action = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_thread_id: int option;
  action: chat_action;
}
[@@deriving show]

type set_message_reaction = {
  chat_id: target_chat;
  message_id: int;
  reaction: reaction_type list option;
  is_big: bool option;
}
[@@deriving show]

type get_user_profile_photos = {
  user_id: int64;
  offset: int option;
  limit: int option;
}
[@@deriving show]

type set_user_emoji_status = {
  user_id: int64;
  emoji_status_custom_emoji_id: string option;
  emoji_status_expiration_date: int option;
}
[@@deriving show]

type get_file = {
  file_id: string;
}
[@@deriving show]

type ban_chat_member = {
  chat_id : target_chat;
  user_id: int64;
  until_date: int option;
  revoke_messages: bool option;
}
[@@deriving show]

type unban_chat_member = {
  chat_id : target_chat;
  user_id: int64;
  only_if_banned: bool option;
}
[@@deriving show]

type restrict_chat_member = {
  chat_id : target_chat;
  user_id: int64;
  permissions: chat_permissions;
  use_independent_chat_permissions: bool option;
  until_date: int option;
}
[@@deriving show]

type promote_chat_member = {
  chat_id : target_chat;
  user_id: int64;
  is_anonymous: bool option;
  can_manage_chat: bool option;
  can_delete_messages: bool option;
  can_manage_video_chats: bool option;
  can_restrict_members: bool option;
  can_promote_users: bool option;
  can_change_info: bool option;
  can_invite_users: bool option;
  can_post_stories: bool option;
  can_edit_stories: bool option;
  can_delete_stories: bool option;
  can_post_messages: bool option;
  can_edit_messages: bool option;
  can_pin_messages: bool option;
  can_manage_topics: bool option;
  can_add_web_page_previews: bool option;
  use_independent_chat_permissions: bool option;
  until_date: int option;
}
[@@deriving show]

type set_chat_administrator_custom_title = {
  chat_id: target_chat;
  user_id: int64;
  custom_title: string;
}
[@@deriving show]

type ban_chat_sender_chat = {
  chat_id: target_chat;
  sender_chat_id: int64;
}
[@@deriving show]

type unban_chat_sender_chat = {
  chat_id: target_chat;
  sender_chat_id: int64;
}
[@@deriving show]

type set_chat_permissions = {
  chat_id: target_chat;
  permissions: chat_permissions;
  use_independent_chat_permissions: bool option;
}
[@@deriving show]

type export_chat_invite_link = {
  chat_id: target_chat;
}
[@@deriving show]

type create_chat_invite_link = {
  chat_id: target_chat;
  name: string option;
  expire_date: int option;
  member_limit: int option;
  create_join_request: bool option;
}
[@@deriving show]

type edit_chat_invite_link = {
  chat_id: target_chat;
  invite_link: string;
  name: string option;
  expire_date: int option;
  member_limit: int option;
  creates_join_request: bool option;
}
[@@deriving show]

type create_chat_subscription_invite_link = {
  chat_id: target_chat;
  name: string option;
  subscription_period: int;
  subscription_price: int;
}
[@@deriving show]

type edit_chat_subscription_invite_link = {
  chat_id: target_chat;
  invite_link: string;
  name: string option;
}
[@@deriving show]

type revoke_chat_invite_link = {
  chat_id: target_chat;
  invite_link: string;
}
[@@deriving show]

type approve_chat_join_request = {
  chat_id: target_chat;
  user_id: int64;
}
[@@deriving show]

type decline_chat_join_request = {
  chat_id: target_chat;
  user_id: int64;
}
[@@deriving show]

type set_chat_photo = {
  chat_id: target_chat;
  photo: input_file_or_string_type;
}
[@@deriving show]

type delete_chat_photo = {
  chat_id: target_chat;
}
[@@deriving show]

type set_chat_title = {
  chat_id: target_chat;
  title: string;
}
[@@deriving show]

type set_chat_description = {
  chat_id: target_chat;
  description: string;
}
[@@deriving show]

type pin_chat_message = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_id: int;
  disable_notification: bool option;
}
[@@deriving show]

type unpin_chat_message = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_id: int;
}
[@@deriving show]

type unpin_all_chat_messages = {
  chat_id: target_chat;
}
[@@deriving show]

type leave_chat = {
  chat_id: target_chat;
}
[@@deriving show]

type get_chat = {
  chat_id: target_chat;
}
[@@deriving show]

type get_chat_administators = {
  chat_id: target_chat;
}
[@@deriving show]

type get_chat_member_count = {
  chat_id: target_chat;
}
[@@deriving show]

type get_chat_member = {
  chat_id: target_chat;
  user_id: int64;
}
[@@deriving show]

type set_chat_sticker_set = {
  chat_id: target_chat;
  sticker_set_name: string;
}
[@@deriving show]

type delete_chat_sticker_set = {
  chat_id: target_chat;
}
[@@deriving show]

type get_forum_topic_icon_stickers = unit
[@@deriving show]

type create_forum_topic = {
  chat_id: target_chat;
  name: string;
  icon_color: int option;
  icon_custom_emoji_id: string option;
}
[@@deriving show]

type edit_forum_topic = {
  chat_id: target_chat;
  message_thread_id: int;
  name: string;
  icon_custom_emoji_id: string option;
}
[@@deriving show]

type close_forum_topic = {
  chat_id: target_chat;
  message_thread_id: int;
}
[@@deriving show]

type reopen_forum_topic = {
  chat_id: target_chat;
  message_thread_id: int;
}
[@@deriving show]

type delete_forum_topic = {
  chat_id: target_chat;
  message_thread_id: int;
}
[@@deriving show]

type unpin_all_forum_topic_messages = {
  chat_id: target_chat;
  message_thread_id: int;
}
[@@deriving show]

type edit_general_forum_topic = {
  chat_id: target_chat;
  name: string;
}
[@@deriving show]

type close_general_forum_topic = {
  chat_id: target_chat;
}
[@@deriving show]

type reopen_general_forum_topic = {
  chat_id: target_chat;
}
[@@deriving show]

type hide_general_forum_topic = {
  chat_id: target_chat;
}
[@@deriving show]

type unhide_general_forum_topic = {
  chat_id: target_chat;
}
[@@deriving show]

type unpin_all_general_forum_topic_messages = {
  chat_id: target_chat;
}
[@@deriving show]

type delete_message = {
  chat_id : target_chat;
  message_id: message_id;
}
[@@deriving show]
