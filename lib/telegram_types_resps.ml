open Telegram_types

let interpret_err err : string =
  BatOption.get err

type true_res =
  | True
  | Error of string
[@@deriving show]

let true_res_of ok error _ =
  match ok with
  | true -> True
  | false -> Error (interpret_err error)

type int_res = 
  | Int of int
  | Error of string
[@@deriving show]

let int_res_of ok error result =
  match ok with
  | true -> Int (BatOption.get result)
  | false -> Error (interpret_err error)

type string_res =
  | String of string
  | Error of string
[@@deriving show]

let string_res_of ok error result =
  match ok with
  | true -> String (BatOption.get result)
  | false -> Error (interpret_err error)

type message_res =
  | Message of message
  | Error of string
[@@deriving show]

let message_res_of ok error result : message_res =
  match ok with
  | true -> Message (BatOption.get result)
  | false -> Error (interpret_err error)

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

type poll_res = 
  | Poll of poll
  | Error of string
[@@deriving show]

type gifts_res =
  | Gifts of gifts
  | Error of string
[@@deriving show]

type star_amount_res =
  | StarAmount of star_amount
  | Error of string
[@@deriving show]

type owned_gifts_res =
  | OwnedGifts of owned_gifts
  | Error of string
[@@deriving show]

type story_res =
  | Story of story
  | Error of string
[@@deriving show]

type sticker_set_res =
  | StickerSet of sticker_set
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

type edit_message_live_location = message_or_true_res
[@@deriving show]

type stop_message_live_location = message_or_true_res
[@@deriving show]

type edit_message_reply_markup = message_or_true_res
[@@deriving show]

type stop_poll = poll_res
[@@deriving show]

type delete_message = true_res
[@@deriving show]

type delete_messages = true_res
[@@deriving show]

type get_available_gifts = gifts_res
[@@deriving show]

type send_gift = true_res
[@@deriving show]

type gift_premium_subscription = true_res
[@@deriving show]

type verify_user = true_res
[@@deriving show]

type verify_chat = true_res
[@@deriving show]

type remove_user_verification = true_res
[@@deriving show]

type remove_chat_verification = true_res
[@@deriving show]

type read_business_message = true_res
[@@deriving show]

type delete_business_messages = true_res
[@@deriving show]

type set_business_account_name = true_res
[@@deriving show]

type set_business_account_username = true_res
[@@deriving show]

type set_business_account_bio = true_res
[@@deriving show]

type set_business_account_profile_photo = true_res
[@@deriving show]

type remove_business_acccount_profile_photo = true_res
[@@deriving show]

type set_business_account_gift_settings = true_res
[@@deriving show]

type get_business_account_star_balance = star_amount_res
[@@deriving show]

type transfer_business_account_stars = true_res
[@@deriving show]

type get_business_account_gifts = owned_gifts_res
[@@deriving show]

type convert_gift_to_stars = true_res
[@@deriving show]

type upgrade_gift = true_res
[@@deriving show]

type transfer_gift = true_res
[@@deriving show]

type post_story = story_res
[@@deriving show]

type edit_story = story_res
[@@deriving show]

type delete_story = true_res
[@@deriving show]

type send_sticker = message_res
[@@deriving show]

type get_sticker_set = sticker_set_res
[@@deriving show]

type get_custom_emoji_stickers = sticker_list_res
[@@deriving show]

type upload_sticker_file = file_res
[@@deriving show]

type create_new_sticker_set = true_res
[@@deriving show]

type add_sticker_to_set = true_res
[@@deriving show]

type set_sticker_position_in_set = true_res
[@@deriving show]

type delete_sticker_from_set = true_res
[@@deriving show]

type replace_sticker_in_set = true_res
[@@deriving show]

type set_sticker_emoji_list = true_res
[@@deriving show]

type set_sticker_keywords = true_res
[@@deriving show]

type set_sticker_mask_position = true_res
[@@deriving show]

type set_sticker_set_title = true_res
[@@deriving show]

type set_sticker_set_thumbnail = true_res
[@@deriving show]

type set_custom_emoji_sticker_set_thumbnail = true_res
[@@deriving show]

type delete_sticker_set = true_res
[@@deriving show]