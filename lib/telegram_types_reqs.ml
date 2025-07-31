open Telegram_types

type send_message = {
  business_connection_id: string option;
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
  message_id: message_id;
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
  message_thread_id: int option;
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
  message_thread_id: int option;
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
  message_thread_id: int option;
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
  message_thread_id: int option;
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
  message_thread_id: int option;
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
  message_thread_id: int option;
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
  message_thread_id: int option;
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
  message_thread_id: int option;
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
  message_thread_id: int option;
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
  message_thread_id: int option;
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
  message_thread_id: int option;
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
  explanation_entities: message_entity list option;
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
  message_thread_id: int option;
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

type answer_callback_query = {
  callback_query_id: string;
  text: string option;
  show_alert: bool option;
  url: string option;
  cache_time: int option;
}
[@@deriving show]

type get_user_chat_boosts = {
  chat_id: target_chat;
  user_id: int64;
}
[@@deriving show]

type get_business_connection = {
  business_connection_id: string;
}
[@@deriving show]

type set_my_commands = {
  commands: bot_command list;
  scope: bot_command_scope option;
  language_code: string option;
}
[@@deriving show]

type delete_my_commands = {
  scope: bot_command_scope option;
  language_code: string option;
}
[@@deriving show]

type get_my_commands = {
  scope: bot_command_scope option;
  language_code: string option;
}
[@@deriving show]

type set_my_name = {
  name: string option;
  language_code: string option;
}
[@@deriving show]

type get_my_name = {
  language_code: string option;
}
[@@deriving show]

type set_my_description = {
  description: string option;
  language_code: string option;
}
[@@deriving show]

type get_my_description = {
  language_code: string option;
}
[@@deriving show]

type set_my_short_description = {
  short_description: string option;
  language_code: string option;
}
[@@deriving show]

type get_my_short_description = {
  language_code: string option;
}
[@@deriving show]

type set_chat_menu_button = {
  chat_id: int option;
  menu_button: menu_button option;
}
[@@deriving show]

type get_chat_menu_button = {
  chat_id: int option;
}
[@@deriving show]

type set_my_default_administrator_rights = {
  rights: chat_administrator_rights option;
  for_channels: bool option;
}
[@@deriving show]

type get_my_default_administrator_rights = {
  for_channels: bool option;
}
[@@deriving show]

type edit_message_text = {
  business_connection_id: string option;
  chat_id: target_chat option;
  message_id: int option;
  inline_message_id: string option;
  text: string;
  parse_mode: formatting_option option;
  entities: message_entity list option;
  link_preview_options: link_preview_options option;
  reply_markup: inline_keyboard_markup option;
}
[@@deriving show]

type edit_message_caption = {
  business_connection_id: string option;
  chat_id: target_chat option;
  message_id: int option;
  inline_message_id: string option;
  caption: string option;
  parse_mode: formatting_option option;
  caption_entities: message_entity list option;
  show_caption_above_media: bool option;
  reply_markup: inline_keyboard_markup option;
}
[@@deriving show]

type edit_message_media = {
  business_connection_id: string option;
  chat_id: target_chat option;
  message_id: int option;
  inline_message_id: string option;
  media: input_media;
  reply_markup: inline_keyboard_markup option;
}
[@@deriving show]

type edit_message_live_location = {
  business_connection_id: string option;
  chat_id: target_chat option;
  message_id: int option;
  inline_message_id: int option;
  latitude: float;
  longtitude: float;
  live_period: int option;
  horizontal_accuracy: float option;
  heading: int option;
  proximity_alert_radius: int option;
  reply_markup: inline_keyboard_markup option;
}
[@@deriving show]

type stop_message_live_location = {
  business_connection_id: string option;
  chat_id: target_chat option;
  message_id: int option;
  inline_message_id: int option;
  reply_markup: inline_keyboard_markup option;
}
[@@deriving show]

type edit_message_reply_markup = {
  business_connection_id: string option;
  chat_id: target_chat option;
  message_id: int option;
  inline_message_id: string option;
  reply_markup: inline_keyboard_markup option;
}
[@@deriving show]

type stop_poll = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_id: int;
  reply_markup: inline_keyboard_markup option;
}
[@@deriving show]

type delete_message = {
  chat_id : target_chat;
  message_id: int;
}
[@@deriving show]

type delete_messages = {
  chat_id: target_chat;
  message_ids: int list;
}
[@@deriving show]

type get_available_gifts = GetAvailableGifts
[@@deriving show]

type send_gift = {
  user_id: int64 option;
  chat_id: target_chat option;
  gift_id: string;
  pay_for_upgrade: bool option;
  text: string option;
  text_parse_mode: formatting_option option;
  text_entities: message_entity list option;
}
[@@deriving show]

type gift_premium_subscription = {
  user_id: int64;
  month_count: int;
  star_count: int;
  text: string option;
  text_parse_mode: formatting_option option;
  text_entities: message_entity list option;
}
[@@deriving show]

type verify_user = {
  user_id: int64;
  custom_description: string option;
}
[@@deriving show]

type verify_chat = {
  chat_id: target_chat;
  custom_description: string option;
}
[@@deriving show]

type remove_user_verification = {
  user_id: int64;
}
[@@deriving show]

type remove_chat_verification = {
  chat_id: target_chat;
}
[@@deriving show]

type read_business_message = {
  business_connection_id: string;
  chat_id: int64;
  message_id: int;
}
[@@deriving show]

type delete_business_messages = {
  business_connection_id: string;
  message_ids: int list;
}
[@@deriving show]

type set_business_account_name = {
  business_connection_id: string;
  first_name: string;
  last_name: string option;
}
[@@deriving show]

type set_business_account_username = {
  business_connection_id: string;
  username: string option;
}
[@@deriving show]

type set_business_account_bio = {
  business_connection_id: string;
  bio: string option;
}
[@@deriving show]

type set_business_account_profile_photo = {
  business_connection_id: string;
  photo: input_profile_photo option;
  is_public: bool option;
}
[@@deriving show]

type remove_business_account_profile_photo = {
  business_connection_id: string;
  is_public: bool option;
}
[@@deriving show]

type set_business_account_gift_settings = {
  business_connection_id: string;
  show_gift_button: bool;
  accepted_gift_types: accepted_gift_types;
}
[@@deriving show]

type get_business_account_star_balance = {
  business_connection_id: string;
}
[@@deriving show]

type transfer_business_account_stars = {
  business_connection_id: string;
  star_count: int;
}
[@@deriving show]

type get_business_account_gifts = {
  business_connection_id: string;
  exclude_unsaved: bool option;
  exclude_saved: bool option;
  exclude_unlimited: bool option;
  exclude_limited: bool option;
  exclude_unique: bool option;
  sort_by_price: bool option;
  offset: string option;
  limit: int option;
}
[@@deriving show]

type convert_gift_to_stars = {
  business_connection_id: string;
  owned_gift_id: string;
}
[@@deriving show]

type upgrade_gift = {
  business_connection_id: string;
  owned_gift_id: string;
  keep_original_details: bool option;
  star_count: int option;
}
[@@deriving show]

type transfer_gift = {
  business_connection_id: string;
  owned_gift_id: string;
  new_owner_chat_id: int64;
  star_count: int option;
}
[@@deriving show]

type post_story = {
  business_connection_id: string;
  content: input_story_content;
  active_period: int;
  caption: string option;
  parse_mode: formatting_option option;
  caption_entities: message_entity list;
  areas: story_area list option;
  post_to_chat_page: bool option;
  protect_content: bool option;
}
[@@deriving show]

type edit_story = {
  business_connection_id: string;
  story_id: int;
  content: input_story_content;
  caption: string option;
  parse_mode: formatting_option option;
  caption_entities: message_entity list;
  areas: story_area list option;
}
[@@deriving show]

type delete_story = {
  business_connection_id: string;
  story_id: int;
}
[@@deriving show]

type send_sticker = {
  business_connection_id: string option;
  chat_id: target_chat;
  message_thread_id: int option;
  sticker: input_file_or_string_type;
  emoji: string option;
  disable_notification: bool option;
  protect_content: bool option;
  allow_paid_broadcast: bool option;
  message_effect_id: string option;
  reply_parameters: reply_parameters option;
  reply_markup: reply_markup_type option;
}
[@@deriving show]

type get_sticker_set = {
  name: string
}
[@@deriving show]

type get_custom_emoji_stickers = {
  custom_emoji_ids: string list;
}
[@@deriving show]

type upload_sticker_file = {
  user_id: int64;
  sticker: input_file;
  sticker_format: string;
}
[@@deriving show]

type create_new_sticker_set = {
  user_id: int64;
  name: string;
  title: string;
  stickers: input_sticker list;
  sticker_type: string option;
  needs_repainting: bool option;
}
[@@deriving show]

type add_sticker_to_set = {
  user_id: int64;
  name: string;
  sticker: input_sticker;
}
[@@deriving show]

type set_sticker_position_in_set = {
  sticker: string;
  position: int;
}
[@@deriving show]

type delete_sticker_from_set = {
  sticker: string;
}
[@@deriving show]

type replace_sticker_in_set = {
  user_id: int;
  name: string;
  old_sticker: string;
  sticker: input_sticker;
}
[@@deriving show]

type set_sticker_emoji_list = {
  sticker: string;
  emoji_list: string list;
}
[@@deriving show]

type set_sticker_keywords = {
  sticker: string;
  keywords: string list;
}
[@@deriving show]

type set_sticker_mask_position = {
  sticker: string;
  mask_position: mask_position option;
}
[@@deriving show]

type set_sticker_set_title = {
  name: string;
  title: string;
}
[@@deriving show]

type set_sticker_set_thumbnail = {
  name: string;
  user_id: int;
  thumbnail: input_file_or_string_type option;
  format: string;
}
[@@deriving show]

type set_custom_emoji_sticker_set_thumbnail = {
  name: string;
  custom_emoji_id: string option;
}
[@@deriving show]

type delete_sticker_set = {
  name: string
}
[@@deriving show]