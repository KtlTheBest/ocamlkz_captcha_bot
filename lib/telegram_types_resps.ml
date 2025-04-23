open Telegram_types

type true_res = {
  ok: bool;
  error: string;
  result: unit option;
}
[@@deriving show]

type int_res = {
  ok: bool;
  error: string;
  result: int option;
}
[@@deriving show]

type string_res = {
  ok: bool;
  error: string;
  result: string option;
}
[@@deriving show]

type message_res = {
  ok: bool;
  error: string option;
  result: message option;
}
[@@deriving show]

type message_list_res = {
  ok: bool;
  error: string option;
  result: message list option;
}
[@@deriving show]

type message_id_res = {
  ok: bool;
  error: string option;
  result: message_id option;
}
[@@deriving show]

type message_ids_list_res = {
  ok: bool;
  error: string option;
  result: message_id list option;
}
[@@deriving show]

type user_profile_photos_res = {
  ok: bool;
  error: string option;
  result: user_profile_photos option;
}
[@@deriving show]

type file_res = {
  ok: bool;
  error: string option;
  result: file option;
}
[@@deriving show]

type chat_invite_link_res = {
  ok: bool;
  error: string option;
  result: chat_invite_link option;
}
[@@deriving show]

type chat_full_info_res = {
  ok: bool;
  error: string option;
  result: chat_full_info option;
}
[@@deriving show]

type chat_member_res = {
  ok: bool;
  error: string option;
  result: chat_member option;
}
[@@deriving show]

type chat_member_list_res = {
  ok: bool;
  error: string option;
  result: chat_member list option;
}
[@@deriving show]

type sticker_list_res = {
  ok: bool;
  error: string option;
  result: sticker list option;
}
[@@deriving show]

type send_message = message_res
[@@deriving show]

type forward_message = message_res
[@@deriving show]

type forward_messages = message_ids_list_res
[@@deriving show]

type copy_message = message_id_res
[@@deriving show]

type copy_messages = message_ids_list_res
[@@deriving show]

type send_photo = message_res
[@@deriving show]

type send_audio = message_res
[@@deriving show]

type send_document = message_res
[@@deriving show]

type send_video = message_res
[@@deriving show]

type send_animation = message_res
[@@deriving show]

type send_voice = message_res
[@@deriving show]

type send_video_note = message_res
[@@deriving show]

type send_paid_media = message_res
[@@deriving show]

type send_media_group = message_list_res
[@@deriving show]

type send_location = message_res
[@@deriving show]

type send_venue = message_res
[@@deriving show]

type send_contact = message_res
[@@deriving show]

type send_poll = message_res
[@@deriving show]

type send_dice = message_res
[@@deriving show]

type send_chat_action = true_res
[@@deriving show]

type set_message_reaction = true_res
[@@deriving show]

type get_user_profile_photos = user_profile_photos_res
[@@deriving show]

type set_user_emoji_status = true_res
[@@deriving show]

type get_file = file_res
[@@deriving show]

type ban_chat_member = true_res
[@@deriving show]

type unban_chat_member = true_res
[@@deriving show]

type restrict_chat_member = true_res
[@@deriving show]

type promote_chat_member = true_res
[@@deriving show]

type set_chat_administrator_custom_title = true_res
[@@deriving show]

type ban_chat_sender_chat = true_res
[@@deriving show]

type unban_chat_sender_chat = true_res
[@@deriving show]

type set_chat_permissions = true_res
[@@deriving show]

type export_chat_invite_link = string_res
[@@deriving show]

type create_chat_invite_link = chat_invite_link_res
[@@deriving show]

type edit_chat_invite_link = chat_invite_link_res
[@@deriving show]

type create_chat_subscription_invite_link = chat_invite_link_res
[@@deriving show]

type edit_chat_subscription_invite_link = chat_invite_link_res
[@@deriving show]

type revoke_chat_invite_link = chat_invite_link_res
[@@deriving show]

type approve_chat_join_request = true_res
[@@deriving show]

type decline_chat_join_request = true_res
[@@deriving show]

type set_chat_photo = true_res
[@@deriving show]

type delete_chat_photo = true_res
[@@deriving show]

type set_chat_title = true_res
[@@deriving show]

type set_chat_description = true_res
[@@deriving show]

type pin_chat_message = true_res
[@@deriving show]

type unpin_chat_message = true_res
[@@deriving show]

type unpin_all_chat_messages = true_res
[@@deriving show]

type leave_chat = true_res
[@@deriving show]

type get_chat = chat_full_info_res
[@@deriving show]

type get_chat_administrators = chat_member_list_res
[@@deriving show]

type get_chat_member_count = int_res
[@@deriving show]

type get_chat_member = chat_member_res
[@@deriving show]

type set_chat_sticker_set = true_res
[@@deriving show]

type delete_chat_sticker_set = true_res
[@@deriving show]

type get_forum_topic_icon_stickers = sticker_list_res
[@@deriving show]

type create_forum_topic = true_res
[@@deriving show]

type edit_forum_topic = true_res
[@@deriving show]

type delete_forum_topic = true_res
[@@deriving show]

type unpin_all_forum_topic_messages = true_res
[@@deriving show]

type edit_general_forum_topic = true_res
[@@deriving show]

type close_general_forum_topic = true_res
[@@deriving show]

type hide_general_forum_topic = true_res
[@@deriving show]

type unhide_general_forum_topic = true_res
[@@deriving show]

type unpin_all_general_forum_topic_messages = true_res
[@@deriving show]
