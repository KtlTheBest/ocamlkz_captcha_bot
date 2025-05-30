open Telegram_types

type true_res =
  | True
  | Error of string
[@@deriving show]

type int_res = 
  | Int of int
  | Error of string
[@@deriving show]

type string_res =
  | String of string
  | Error of string
[@@deriving show]

type message_res =
  | Message of string
  | Error of string
[@@deriving show]

type message_list_res = 
  | MessageList of message list
  | Error of string
[@@deriving show]

type message_id_res =
  | MessageID of message_id
  | Error of string
[@@deriving show]

type message_ids_list_res =
  | MessageIDsList of message_id list
  | Error of string
[@@deriving show]

type user_profile_photos_res =
  | UserProfilePhotos of user_profile_photos
  | Error of string
[@@deriving show]

type file_res =
  | File of file
  | Error of string
[@@deriving show]

type chat_invite_link_res =
  | ChatInviteLink of chat_invite_link
  | Error of string
[@@deriving show]

type chat_full_info_res =
  | ChatFullInfo of chat_full_info
  | Error of string
[@@deriving show]

type chat_member_res =
  | ChatMember of chat_member
  | Error of string
[@@deriving show]

type chat_member_list_res =
  | ChatMemberList of chat_member list
  | Error of string
[@@deriving show]

type sticker_list_res =
  | StickerList of sticker list
  | Error of string
[@@deriving show]

type user_chat_boosts_res =
  | UserChatBoosts of user_chat_boosts
  | Error of string
[@@deriving show]
  
type business_connection_res =
  | BusinessConnection of business_connection
  | Error of string
[@@deriving show]

type bot_command_list_res =
  | BotCommandList of bot_command list
  | Error of string
[@@deriving show]

type bot_name_res =
  | BotName of bot_name
  | Error of string
[@@deriving show]

type bot_description_res =
  | BotDescription of bot_description
  | Error of string
[@@deriving show]

type bot_short_description_res =
  | BotShortDescription of bot_short_description
  | Error of string
[@@deriving show]

type menu_button_res =
  | MenuButton of menu_button
  | Error of string
[@@deriving show]

type chat_administrator_rights_res =
  | ChatAdministratorRights of chat_administrator_rights
  | Error of string
[@@deriving show]

type message_or_true_res =
  | Message of message
  | True
  | Error of string
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

type get_user_chat_bosts = user_chat_boosts_res
[@@deriving show]

type answer_callback_query = true_res
[@@deriving show]

type get_business_connection_id = business_connection_res
[@@deriving show]

type set_my_commands = true_res
[@@deriving show]

type delete_my_commands = true_res
[@@deriving show]

type get_my_commands = bot_command_list_res
[@@deriving show]

type set_my_name = true_res
[@@deriving show]

type get_my_name = bot_name_res
[@@deriving show]

type set_my_description = true_res
[@@deriving show]

type get_my_description = bot_description_res
[@@deriving show]

type set_my_short_description = true_res
[@@deriving show]

type get_my_short_description = bot_short_description_res
[@@deriving show]

type set_chat_menu_button = true_res
[@@deriving show]

type get_chat_menu_button = menu_button_res
[@@deriving show]

type set_my_default_administrator_rights = true_res
[@@deriving show]

type get_my_default_administrator_rights = chat_administrator_rights_res
[@@deriving show]

type edit_message_text = message_or_true_res
[@@deriving show]

type edit_message_caption = message_or_true_res
[@@deriving show]

type edit_message_media = message_or_true_res
[@@deriving show]