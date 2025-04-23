open BatInt64

let to_int64 (j : Yojson.Safe.t) : int64 =
  match j with
  | `Int i -> of_int i
  | `Intlit s -> of_string s
  | _ -> failwith @@ Printf.sprintf "Failed to convert to int64: %s" (Yojson.Safe.show j)

let to_int64_option (j : Yojson.Safe.t) : int64 option =
  match j with
  | `Int i -> Some (of_int i)
  | `Intlit s -> Some (of_string s)
  | _ -> None

type chat_type =
  | Private
  | Group
  | Supergroup
  | Channel
[@@deriving show]

type user = {
  id: int64;
  is_bot: bool;
  first_name: string;
  last_name: string option;
  username: string option;
  is_premium: bool;
  added_to_attachement_menu: bool;
}
[@@deriving show]

type chat = {
  id : int64;
  _type: chat_type;
  title: string option;
  username: string option;
  first_name: string option;
  last_name: string option;
  is_forum: bool;
}
[@@deriving show]

type chat_member_owner = {
  status : string;
  user : user;
  is_anonymous : bool;
  custom_title: string option;
}
[@@deriving show]

type chat_member_administrator = {
  status: string;
  user: user;
  can_be_edited: bool;
  is_anonymous: bool;
  can_manage_chat: bool;
  can_delete_messages: bool;
  can_manage_video_chats: bool;
  can_restrict_messages: bool;
  can_promote_members: bool;
  can_change_info: bool;
  can_invite_users: bool;
  can_post_stories: bool;
  can_edit_stories: bool;
  can_delete_stories: bool;
  can_post_messages: bool;
  can_edit_messages: bool;
  can_pin_messages: bool;
  can_manage_topics: bool;
  custom_title: string option;
}
[@@deriving show]

type chat_member_member = {
  status: string;
  user: user;
  until_date: int option;
}
[@@deriving show]

type chat_member_restricted = {
  status: string;
  user: user;
  is_member: bool;
  can_send_messages: bool;
  can_send_audios: bool;
  can_send_documents: bool;
  can_send_photos: bool;
  can_send_videos: bool;
  can_send_video_notes: bool;
  can_send_voice_notes: bool;
  can_send_polls: bool;
  can_send_other_messages: bool;
  can_add_web_page_previews: bool;
  can_change_info: bool;
  can_invite_users: bool;
  can_pin_messages: bool;
  can_manage_topics: bool;
  until_date: int option;
}
[@@deriving show]

type chat_member_left = {
  status: string;
  user: user;
}
[@@deriving show]

type chat_member_banned = {
  status: string;
  user: user;
  until_date: int option;
}
[@@deriving show]

type chat_member =
  | ChatMemberOwner of chat_member_owner
  | ChatMemberAdministrator of chat_member_administrator
  | ChatMemberMember of chat_member_member
  | ChatMemberRestricted of chat_member_restricted
  | ChatMemberLeft of chat_member_left
  | ChatMemberBanned of chat_member_banned
[@@deriving show]

type chat_member_updated = { 
  chat : chat;
  from : user;
  date : int;
  old_chat_member: chat_member;
  new_chat_member: chat_member;
  via_join_request: bool;
  via_chat_folder_invite_link: bool;
}
[@@deriving show]

type message_origin_user = {
  _type: string;
  date: int;
  sender_user: user;
}
[@@deriving show]

type message_origin_hidden_user = {
  _type: string;
  date: int;
  sender_user_name: string;
}
[@@deriving show]

type message_origin_chat = {
  _type: string;
  date: int;
  sender_chat: chat;
  author_signature: string option;
}
[@@deriving show]

type message_origin_channel = {
  _type: string;
  date: int;
  chat: chat;
  message_id: int;
  author_signature: string option;
}
[@@deriving show]

type message_origin =
  | MessageOriginUser of message_origin_user
  | MessageOriginHiddenUser of message_origin_hidden_user
  | MessageOriginChat of message_origin_chat
  | MessageOriginChannel of message_origin_channel
[@@deriving show]

type link_preview_options = {
  is_disabled: bool;
  url: string option;
  prefer_small_media: bool;
  prefer_large_media: bool;
  show_above_text: bool;
}
[@@deriving show]

type photosize = {
  file_id: string;
  file_unique_id: string;
  width: int;
  height: int;
  file_size: int option;
}
[@@deriving show]

type animation = {
  file_id: string;
  file_unique_id: string;
  width: int;
  height: int;
  duration: int;
  thumbnail: photosize option;
  file_name: string option;
  mime_type: string option;
  file_size: int64 option;
}
[@@deriving show]

type audio = {
  file_id: string;
  file_unique_id: string;
  duration: int;
  performer: string option;
  title: string option;
  file_name: string option;
  mime_type: string option;
  file_size: int64 option;
  thumbnail: photosize option;
}
[@@deriving show]

type document = {
  file_id: string;
  file_unique_id: string;
  thumbnail: photosize option;
  file_name: string option;
  mime_type: string option;
  file_size: int64 option;
}
[@@deriving show]

type sticker_type =
  | Regular
  | Mask
  | CustomEmoji
[@@deriving show]

type file = {
  file_id: string;
  file_unique_id: string;
  file_size: int64 option;
  file_path: string option;
}
[@@deriving show]

type mask_position = {
  point: string;
  x_shift: float;
  y_shift: float;
  scale: float;
}
[@@deriving show]

type sticker = {
  file_id: string;
  file_unique_id: string;
  _type: sticker_type;
  width: int;
  height: int;
  is_animated: bool;
  is_video: bool;
  thumbnail: photosize option;
  emoji: string option;
  set_name: string option;
  premium_animation: file option;
  mask_position: mask_position option;
  custom_emoji_id: string option;
  needs_repainting: bool;
  file_size: int option;
}
[@@deriving show]

type story = {
  chat: chat;
  id: int;
}
[@@deriving show]

type video = {
  file_id: string;
  file_unique_id: string;
  width: int;
  height: int;
  duration: int;
  thumbnail: photosize option;
  file_name: string option;
  mime_type: string option;
  file_size: int64 option;
}
[@@deriving show]

type video_note = {
  file_id: string;
  file_unique_id: string;
  length: int;
  duration: int;
  thumbnail: photosize option;
  file_size: int option;
}
[@@deriving show]

type voice = {
  file_id: string;
  file_unique_id: string;
  duration: int;
  mime_type: string option;
  file_size: int64 option;
}
[@@deriving show]

type contact = {
  phone_number: string;
  first_name: string;
  last_name: string option;
  user_id: int64 option;
  vcard: string option;
}
[@@deriving show]

type dice = {
  emoji: string;
  value: int;
}
[@@deriving show]

type giveaway = {
  chats: chat list;
  winners_selection_date: int;
  winner_count: int;
  only_new_members: bool;
  has_public_winners: bool;
  prize_description: string option;
  country_codes: string list;
  prize_star_count: int option;
  premium_subscription_month_count: int option;
}
[@@deriving show]

type giveaway_winners = {
  chat: chat;
  giveaway_message_id: int;
  winners_selection_date: int;
  winner_count: int;
  winners: user list;
  additional_chat_count: int option;
  prize_star_count: int option;
  premium_subscription_month_count: int option;
  unclaimed_prize_count: int option;
  only_new_members: bool;
  was_refunded: bool;
  prize_description: string option;
}
[@@deriving show]

type location = {
  latitude: float;
  longtitude: float;
  horizontal_accuracy: float option;
  live_period: int option;
  heading: int option;
  proximity_alert_radius: int option;
}
[@@deriving show]

type message_entity_type =
  | Mention
  | Hashtag
  | Cashtag
  | BotCommand
  | Url
  | Email
  | PhoneNumber
  | Bold
  | Italic
  | Underline
  | Strikethrough
  | Spoiler
  | Blockquote
  | ExpandableBlockquote
  | Code
  | Pre
  | TextLink
  | TextMention
  | CustomEmoji
[@@deriving show]

type message_entity = {
  _type: message_entity_type;
  offset: int;
  length: int;
  url: string option;
  user: user option;
  language: string option;
  custom_emoji_id: string option;
}
[@@deriving show]

type poll_option = {
  text: string;
  text_entities: message_entity list;
  voter_count: int;
}
[@@deriving show]

type poll_type =
  | Regular
  | Quiz
[@@deriving show]

type poll = {
  id: string;
  question: string;
  question_entities: message_entity list;
  options: poll_option list;
  total_voter_count: int;
  is_closed: bool;
  is_anonymous: bool;
  _type: poll_type;
  allows_multiple_answers: bool;
  correct_option_id: int option;
  explanation: string option;
  explanation_entities: message_entity list;
  open_period: int option;
  close_date: int option;
}
[@@deriving show]

type external_reply_info = {
  origin: message_origin;
  chat: chat option;
  message_id: int option;
  link_preview_options: link_preview_options option;
  animation: animation option;
  audio: audio option;
  document: document option;
  photo: photosize list;
  sticker: sticker option;
  story: story option;
  video: video option;
  video_note: video_note option;
  voice: voice option;
  has_media_spoiler: bool;
  contact: contact option;
  dice: dice option;
  giveaway: giveaway option;
  giveaway_winners: giveaway_winners option;
  location: location option;
  poll: poll option;
}
[@@deriving show]

type text_quote = {
  text: string;
  entities: message_entity list;
  position: int;
  is_manual: bool;
}
[@@deriving show]

type paid_media_preview = {
  _type: string;
  width: int option;
  height: int option;
  duration: int option;
}
[@@deriving show]

type paid_media_photo = {
  _type: string;
  photo: photosize list;
}
[@@deriving show]

type paid_media_video = {
  _type: string;
  video: video;
}
[@@deriving show]

type paid_media =
  | PaidMediaPreview of paid_media_preview
  | PaidMediaPhoto of paid_media_photo
  | PaidMediaVideo of paid_media_video
[@@deriving show]

type paid_media_info = {
  star_count: int;
  paid_media: paid_media;
}
[@@deriving show]

type game = {
  title : string;
  description: string;
  photo: photosize list;
  text: string option;
  text_entities: message_entity list;
  animation: animation option;
}
[@@deriving show]

type venue = {
  location: location;
  title: string;
  address: string;
  foursquare_id: string option;
  foursquare_type: string option;
  google_place_id: string option;
  google_place_type: string option;
}
[@@deriving show]

type message_auto_delete_timer_changed = {
  message_auto_delete_time: int
}
[@@deriving show]

type inaccessible_message = {
  chat: chat;
  message_id: int;
  date: int;
}
[@@deriving show]

type shared_user = {
  user_id: int64;
  first_name: string option;
  last_name: string option;
  username: string option;
  photo: photosize list;
}
[@@deriving show]

type users_shared = {
  request_id: int;
  users: shared_user list;
}
[@@deriving show]

type chat_shared = {
  request_id: int;
  chat_id: int64;
  title: string option;
  username: string option;
  photo: photosize list;
}
[@@deriving show]

type write_access_allowed = {
  from_request: bool;
  web_app_name: string option;
  from_attachment_menu: bool;
}
[@@deriving show]

type passport_data = PassportData
[@@deriving show]

type proximity_alert_triggered = {
  traveler: user;
  watcher: user;
  distance: int;
}
[@@deriving show]

type chat_boost_added = {
  boost_count: int;
}
[@@deriving show]

type background_fill_solid = {
  _type: string;
  color: int;
}
[@@deriving show]

type background_fill_gradient = {
  _type: string;
  top_color: int;
  bottom_color: int;
  rotation_angle: int;
}
[@@deriving show]

type background_fill_freeform_gradient = {
  _type: string;
  colors: int list;
}
[@@deriving show]

type background_fill =
  | BackgroundFillSolid of background_fill_solid
  | BackgroundFillGradient of background_fill_gradient
  | BackgroundFillFreeformGradient of background_fill_freeform_gradient
[@@deriving show]

type background_type_fill = {
  _type: string;
  fill: background_fill;
  dark_theme_dimming: int;
}
[@@deriving show]

type background_type_wallpaper = {
  _type: string;
  document: document;
  dark_theme_dimming: int;
  is_blurred: bool;
  is_moving: bool;
}
[@@deriving show]

type background_type_pattern = {
  _type: string;
  document: document;
  fill: background_fill;
  intensity: int;
  is_inverted: bool;
  is_moving: bool;
}
[@@deriving show]

type background_type_chat_theme = {
  _type: string;
  theme_name: string;
}
[@@deriving show]

type background_type = 
  | BackgroundTypeFill of background_type_fill
  | BackgroundTypeWallpaper of background_type_wallpaper
  | BackgroundTypePattern of background_type_pattern
  | BackgroundTypeChatTheme of background_type_chat_theme
[@@deriving show]

type chat_background = {
  _type: background_type;
}
[@@deriving show]

type forum_topic_created = {
  name: string;
  icon_color: int;
  icon_custom_emoji_id: string option;
}
[@@deriving show]

type forum_topic_closed = ForumTopicClosed
[@@deriving show]

type forum_topic_edited = {
  name: string option;
  icon_custom_emoji_id: string option;
}
[@@deriving show]

type forum_topic_reopened = ForumTopicReopened
[@@deriving show]

type general_topic_forum_hidden = GeneralTopicForumHidden
[@@deriving show]
type general_topic_forum_unhidden = GeneralTopicForumUnhidden
[@@deriving show]

type giveaway_created = {
  prize_star_count: int option;
}
[@@deriving show]

type video_chat_scheduled = {
  start_date: int;
}
[@@deriving show]

type video_chat_started = VideoChatStarted
[@@deriving show]

type video_chat_ended = {
  duration: int;
}
[@@deriving show]

type video_chat_participants_invited = {
  users: user list;
}
[@@deriving show]

type web_app_data = {
  data: string;
  button_text: string;
}
[@@deriving show]

type web_app_info = { url: string }
[@@deriving show]

type login_url = {
  url: string;
  forward_text: string option;
  bot_username: string option;
  request_write_access: bool option;
}
[@@deriving show]

type switch_inline_query_chosen_chat = {
  query: string option;
  allow_user_chats: bool option;
  allow_bot_chats: bool option;
  allow_group_chats: bool option;
  allow_channel_chats: bool option;
}
[@@deriving show]

type copy_text_button = { text: string }
[@@deriving show]

type inline_keyboard_button = {
  text: string;
  url: string option;
  callback_data: string option;
  web_app: web_app_info option;
  login_url: login_url option;
  switch_inline_query: string option;
  switch_inline_query_current_chat: string option;
  switch_inline_query_chosen_chat: switch_inline_query_chosen_chat option;
  copy_text_button: copy_text_button option;
}
[@@deriving show]

type inline_keyboard_markup = {
  inline_keyboard: inline_keyboard_button list list;
}
[@@deriving show]

type maybe_inaccessible_message =
  | Message of message
  | InaccessibleMessage of inaccessible_message
[@@deriving show]

and giveaway_completed = {
  winner_count: int;
  unclaimed_prize_count: int option;
  giveaway_message: message option;
  is_star_giveaway: bool;
}
[@@deriving show]

and message = {
  message_id: int;
  message_thread_id: int option;
  from: user option;
  sender_chat: chat option;
  sender_boost_count: int option;
  sender_business_bot: user option;
  date: int;
  business_connection_id: string option;
  chat: chat;
  forward_origin: message_origin option;
  is_topic_message: bool;
  is_automatic_forward: bool;
  reply_to_message: message option;
  external_reply: external_reply_info option;
  quote: text_quote option;
  reply_to_story: story option;
  via_bot: user option;
  edit_date: int option;
  has_protected_content: bool;
  is_from_offline: bool;
  media_group_id: string option;
  author_signature: string option;
  text: string option;
  entities: message_entity list;
  link_preview_options: link_preview_options option;
  effect_id: string option;
  animation: animation option;
  audio: audio option;
  document: document option;
  paid_media: paid_media_info option;
  photo: photosize list;
  sticker: sticker option;
  story: story option;
  video: video option;
  video_note: video_note option;
  voice: voice option;
  caption: string option;
  caption_entities: message_entity list;
  show_caption_above_media: bool;
  has_media_spoiler: bool;
  contact: contact option;
  dice: dice option;
  game: game option;
  poll: poll option;
  venue: venue option;
  location: location option;
  new_chat_members: user list;
  left_chat_member: user option;
  new_chat_title: string option;
  new_chat_photo: photosize list;
  delete_chat_photo: bool;
  group_chat_created: bool;
  supergroup_chat_created: bool;
  channel_chat_created: bool;
  message_auto_delete_timer_changed: message_auto_delete_timer_changed option;
  migrate_to_chat_id: int64 option;
  migrate_from_chat_id: int64 option;
  pinned_message: maybe_inaccessible_message option;
  users_shared: users_shared option;
  chat_shared: chat_shared option;
  connected_website: string option;
  write_access_allowed: write_access_allowed option;
  passport_data: passport_data option;
  proximity_alert_triggered: proximity_alert_triggered option;
  boost_added: chat_boost_added option;
  chat_background_set: chat_background option;
  forum_topic_created: forum_topic_created option;
  forum_topic_edited: forum_topic_edited option;
  forum_topic_closed: forum_topic_closed option;
  forum_topic_reopened: forum_topic_reopened option;
  general_topic_forum_hidden: general_topic_forum_hidden option;
  general_topic_forum_unhidden: general_topic_forum_unhidden option;
  giveaway_created: giveaway_created option;
  giveaway: giveaway option;
  giveaway_winners: giveaway_winners option;
  giveaway_completed: giveaway_completed option;
  video_chat_scheduled: video_chat_scheduled option;
  video_chat_ended: video_chat_ended option;
  video_chat_participants_invited: video_chat_participants_invited option;
  web_app_data: web_app_data option;
  reply_markup: inline_keyboard_markup option;
}
[@@deriving show]

type message_refined =
  | UnknownMessage
  | TextMessage
  | EditedTextMessage
  | Sticker
  | Audio
  | Video
  | Document
  | VideoNote
[@@deriving show]

type refined_update = 
  | PrivateChatMessage
  | GroupChatMessage
  | ChannelPostMessage
[@@deriving show]

type business_connection = {
  id: string;
  user: user;
  user_chat_id: int64;
  date: int;
  can_reply: bool;
  is_enabled: bool;
}
[@@deriving show]

type business_messages_deleted = {
  business_connection_id: string;
  chat: chat;
  message_ids: int list;
}
[@@deriving show]

type reaction_type_emoji = {
  _type: string;
  emoji: string;
}
[@@deriving show]

type reaction_type_custom_emoji = {
  _type: string;
  custom_emoji_id: string;
}
[@@deriving show]

type reaction_type_paid = {
  _type: string;
}
[@@deriving show]

type reaction_type =
  | ReactionTypeEmoji of reaction_type_emoji
  | ReactionTypeCustomEmoji of reaction_type_custom_emoji
  | ReactionTypePaid of reaction_type_paid
[@@deriving show]

type message_reaction_updated = {
  chat: chat;
  message_id: int;
  user: user option;
  actor_chat: chat option;
  date: int;
  old_reaction: reaction_type list;
  new_reaction: reaction_type list;
}
[@@deriving show]

type reaction_count = {
  _type: reaction_type;
  total_count: int;
}
[@@deriving show]

type message_reaction_count_updated = {
  chat: chat;
  message_id: int;
  date: int;
  reactions: reaction_count list;
}
[@@deriving show]

type inline_query = {
  id: string;
  from: user;
  query: string;
  offset: string;
  chat_type: string option;
  location: location option;
}
[@@deriving show]

type chosen_inline_result = {
  result_id: string;
  from: user;
  location: location option;
  inline_message_id: string option;
  query: string;
}
[@@deriving show]

type callback_query = {
  id: string;
  from: user;
  message: maybe_inaccessible_message option;
  inline_message_id: string option;
  chat_instance: string option;
  data: string option;
  game_short_name: string option;
}
[@@deriving show]

type shipping_address = {
  country: string;
  state: string;
  city: string;
  street_line1: string;
  street_line2: string;
  post_code: string;
}
[@@deriving show]

type shipping_query = {
  id: string;
  from: user;
  invoice_payload: string;
  shipping_address: shipping_address;
}
[@@deriving show]

type order_info = {
  name: string option;
  phone_number: string option;
  email: string option;
  shipping_address: shipping_address option;
}
[@@deriving show]

type pre_checkout_query = {
  id: string;
  from: user;
  currency: string;
  total_amount: int;
  invoice_payload: string;
  shipping_option_id: string option;
  order_info: order_info option;
}
[@@deriving show]

type paid_media_purchased = {
  from: user;
  paid_media_payload: string;
}
[@@deriving show]

type poll_answer = {
  poll_id: string;
  voter_chat: chat option;
  user: user option;
  option_ids: int list;
}
[@@deriving show]

type chat_invite_link = {
  invite_link: string;
  creator: user;
  creates_join_request: bool;
  is_primary: bool;
  is_revoked: bool;
  name: string option;
  expire_date: int option;
  member_limit: int option;
  pending_join_request_count: int option;
  subscription_period: int option;
  subscription_price: int option;
}
[@@deriving show]

type chat_join_request = {
  chat: chat;
  from: user;
  user_chat_id: int64;
  date: int;
  bio: string option;
  invite_link: chat_invite_link option;
}
[@@deriving show]

type chat_boost_source_premium = {
  source: string;
  user: user;
}
[@@deriving show]

type chat_boost_source_gift_code = {
  source: string;
  user: user;
}
[@@deriving show]

type chat_boost_source_giveaway = {
  source: string;
  giveaway_message_id: int;
  user: user option;
  prize_star_count: int option;
  is_unclaimed: bool;
}
[@@deriving show]

type chat_boost_source = 
  | ChatBoostSourcePremium of chat_boost_source_premium
  | ChatBoostSourceGiftCode of chat_boost_source_gift_code
  | ChatBoostSourceGiveaway of chat_boost_source_giveaway
[@@deriving show]

type chat_boost = {
  boost_id: string;
  add_date: int;
  expiration_date: int;
  source: chat_boost_source;
}
[@@deriving show]

type chat_boost_updated = {
  chat: chat;
  boost: chat_boost;
}
[@@deriving show]

type update = {
  update_id: int;
  message: message option;
  edited_message: message option;
  channel_post: message option;
  edited_channel_post: message option;
  business_connection: business_connection option;
  business_message: message option;
  editted_business_message: message option;
  deleted_business_messages: business_messages_deleted option;
  message_reaction: message_reaction_updated option;
  message_reaction_count: message_reaction_count_updated option;
  inline_query: inline_query option;
  chosen_inline_result: chosen_inline_result option;
  callback_query: callback_query option;
  shipping_query: shipping_query option;
  pre_checkout_query: pre_checkout_query option;
  purchased_paid_media: paid_media_purchased option;
  poll: poll option;
  poll_answer: poll_answer option;
  my_chat_member: chat_member_updated option;
  chat_member: chat_member_updated option;
  chat_join_request: chat_join_request option;
  chat_boost: chat_boost_updated option;
  removed_chat_boost: chat_boost_updated option;
}
[@@deriving show]

(*
let to_user_option j : user option =
  let open Yojson.Safe.Util in
  let open BatOption in
  let to_bool_option' x =
    x |> to_bool_option |> default false
  in
  match j with
  | `Null -> None
  | _ ->
  let id = j |> member "id" |> to_int in
  let is_bot = j |> member "is_bot" |> to_bool_option' in
  let first_name = j |> member "first_name" |> to_string in
  let last_name = j |> member "last_name" |> to_string_option in
  let username = j |> member "username" |> to_string_option in
  let is_premium = j |> member "is_premium" |> to_bool_option' in
  let added_to_attachement_menu = j |> member "added_to_attachement_menu" |> to_bool_option' in
  Some({ id; is_bot; first_name; last_name; username; is_premium; added_to_attachement_menu})
*)

type target_chat =
  | Chat of int64
  | Channel of string
[@@deriving show]

type formatting_option =
  | NoFormat
  | MarkDown
  | MarkDownV2
  | Html
[@@deriving show]

type reply_parameters = {
  message_id: int;
  chat_id: target_chat option;
  allow_sending_without_reply: bool option;
  quote: string option;
  quote_parse_mode: formatting_option option;
  quote_entities: message_entity list option;
  quote_position: int option;
}
[@@deriving show]

type keyboard_button_request_users = {
  request_id: int;
  user_is_bot: bool option;
  user_is_premium: bool option;
  max_quantity: int option;
  request_name: bool option;
  request_username: bool option;
  request_photo: bool option;
}
[@@deriving show]

type chat_administrator_rights = {
  is_anonymous: bool;
  can_manage_chat: bool;
  can_delete_messages: bool;
  can_manage_video_chats: bool;
  can_restrict_members: bool;
  can_promote_members: bool;
  can_change_info: bool;
  can_invite_users: bool;
  can_post_stories: bool;
  can_edit_stories: bool;
  can_delete_stories: bool;
  can_post_messages: bool option;
  can_edit_messages: bool option;
  can_pin_messages: bool option;
  can_manage_topics: bool option;
}
[@@deriving show]

type keyboard_button_request_chat = {
  request_id: int;
  chat_is_channel: bool;
  chat_is_forum: bool option;
  chat_has_username: bool option;
  chat_is_created: bool option;
  user_administrator_rights: chat_administrator_rights option;
  bot_administrator_rights: chat_administrator_rights option;
  bot_is_member: bool option;
  request_title: bool option;
  request_username: bool option;
  request_photo: bool option;
}
[@@deriving show]

type keyboard_button_poll_type = {
  _type: string option;
}
[@@deriving show]

type keyboard_button = {
  text: string;
  request_users: keyboard_button_request_users option;
  request_chat: keyboard_button_request_chat option;
  request_contact: bool option;
  request_location: bool option;
  request_poll: keyboard_button_poll_type option;
  web_app: web_app_info option;
}
[@@deriving show]

type reply_keyboard_markup = {
  keyboard: keyboard_button list list;
  is_persistent: bool option;
  resize_keyboard: bool option;
  one_time_keyboard: bool option;
  input_field_placeholder: string option;
  selective: bool option;
}
[@@deriving show]

type reply_keyboard_remove = {
  remove_keyboard: bool;
  selective: bool option;
}
[@@deriving show]

type force_reply = {
  force_reply: bool;
  input_field_placeholder: string option;
  selective: bool option;
}
[@@deriving show]

type reply_markup_type =
  | InlineKeyboard of inline_keyboard_markup
  | ReplyKeyboard of reply_keyboard_markup
  | ReplyKeyboardRemove of reply_keyboard_remove
  | ForceReply of force_reply
[@@deriving show]

type message_id = int64
[@@deriving show]

type input_file_or_string_type =
  | String of string
  | InputFile of string
[@@deriving show]

type input_poll_option = {
  text: string;
  text_parse_mode: formatting_option option;
  text_entities: message_entity list option;
}
[@@deriving show]

type chat_action =
  | Typing
  | UploadPhoto
  | RecordVideo
  | UploadVideo
  | RecordVoice
  | UploadVoice
  | UploadDocument
  | ChooseSticker
  | FindLocation
  | RecordVideoNote
  | UploadVideoNote
[@@deriving show]

type user_profile_photos = {
  total_count: int;
  photos: photosize list list;
}
[@@deriving show]

type chat_permissions = {
  can_send_messages: bool option;
  can_send_audios: bool option;
  can_send_documents: bool option;
  can_send_photos: bool option;
  can_send_videos: bool option;
  can_send_video_notes: bool option;
  can_send_voice_notes: bool option;
  can_send_polls: bool option;
  can_send_other_messages: bool option;
  can_add_web_page_previews: bool option;
  can_change_info: bool option;
  can_invite_users: bool option;
  can_pin_messages: bool option;
  can_manage_topics: bool option;
}
[@@deriving show]

type chat_photo = {
  small_file_id: string;
  small_file_unique_id: string;
  big_file_id: string;
  big_file_unique_id: string;
}
[@@deriving show]

type birthdate = {
  day: int;
  month: int;
  year: int;
}
[@@deriving show]

type business_intro = {
  title: string option;
  message: string option;
  sticker: sticker option;
}
[@@deriving show]

type business_location = {
  address: string;
  location: location option;
}
[@@deriving show]

type business_opening_hours_interval = {
  opening_minute: int;
  closing_minute: int;
}
[@@deriving show]

type business_opening_hours = {
  time_zone_string: string;
  opening_hours: business_opening_hours_interval list;
}
[@@deriving show]

type accepted_gift_types = {
  unlimited_gifts: bool;
  limited_gifts: bool;
  unique_gifts: bool;
  premium_subscription: bool;
}
[@@deriving show]

type chat_location = {
  location: location;
  address: string;
}
[@@deriving show]

type chat_full_info = {
  id: int64;
  _type: chat_type;
  title: string option;
  username: string option;
  first_name: string option;
  last_name: string option;
  is_forum: bool;
  accent_color_id: int;
  max_reaction_count: int;
  photo: chat_photo option;
  active_usernames: string list;
  birthdate: birthdate option;
  business_intro: business_intro option;
  business_location: business_location option;
  business_opening_hours: business_opening_hours option;
  personal_chat: chat option;
  available_reactions: reaction_type list;
  background_custom_emoji_id: string option;
  profile_accent_color_id: int option;
  profile_background_custom_emoji_id: string option;
  emoji_status_custom_emoji_id: string option;
  emoji_status_expiration_date: string option;
  bio: string option;
  has_private_forwards: bool;
  has_restricted_voice_and_video_messages: bool;
  join_to_send_messages: bool;
  join_by_request: bool;
  description: string option;
  invite_link: string option;
  pinned_message: message option;
  permissions: chat_permissions;
  accepted_gift_types: accepted_gift_types option;
  can_send_paid_media: bool;
  slow_mode_delay: int option;
  unrestrict_boost_count: int option;
  message_auto_delete_timer: int option;
  has_aggressive_anti_spam_enabled: bool;
  has_hidden_members: bool;
  has_protected_content: bool;
  has_visible_history: bool;
  sticker_set_name: string option;
  can_set_sticker_set: bool;
  custom_emoji_sticker_set_name: string option;
  linked_chat_id: int option;
  location: chat_location option;
}
[@@deriving show]