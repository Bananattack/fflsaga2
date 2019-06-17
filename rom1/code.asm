SECTION "ROM1_5472", ROMX[$5471], BANK[1]
main_menu::
; ...
SECTION "ROM1_54B2", ROMX[$54B2], BANK[1]
main_menu_confirm::
; ...
SECTION "ROM1_54B6", ROMX[$54B6], BANK[1]
main_menu_open_abil::
; ...
SECTION "ROM1_54B9", ROMX[$54B9], BANK[1]
main_menu_open_item::
; ...
SECTION "ROM1_54BD", ROMX[$54BD], BANK[1]
main_menu_open_equip::
; ...
SECTION "ROM1_54C1", ROMX[$54C1], BANK[1]
main_menu_open_magi::
; ...
SECTION "ROM1_54C5", ROMX[$54C5], BANK[1]
main_menu_open_memo::
; ...
SECTION "ROM1_54C8", ROMX[$54C8], BANK[1]
main_menu_open_save::
; ...

SECTION "ROM1_5DF8", ROMX[$5DF8], BANK[1]
open_menu_and_clear::
; ...

SECTION "ROM1_5E0D", ROMX[$5E0D], BANK[1]
open_menu_and_show::
; ...

SECTION "ROM1_5E31", ROMX[$5E31], BANK[1]
prepare_menu::
; ...

SECTION "ROM1_5EB1", ROMX[$5EB1], BANK[1]
exit_menu_wait_input_release::
; ...
; fallthrough
exit_menu_wait::

SECTION "ROM1_5F0E", ROMX[$5F0E], BANK[1]
restore_palette::
; ...

SECTION "ROM1_5F22", ROMX[$5F22], BANK[1]
clear_palette::
; ...

SECTION "ROM1_5F44", ROMX[$5F44], BANK[1]
show_window::
; ...

SECTION "ROM1_5F51", ROMX[$5F51], BANK[1]
party_order_menu::
; ...

SECTION "ROM1_60E8", ROMX[$60E8], BANK[1]
character_creator_secondary::
; ...

; Character creator: [$C709] = party index
; Returns: carry = 1 if character class confirmed, 0 if user canceled
SECTION "ROM1_611C", ROMX[$611C], BANK[1]
character_creator_choose_class::
; ...

; Character creator: [$C709] = party index
; Returns: carry = 1 if character name confirmed, 0 if user canceled
SECTION "ROM1_6157", ROMX[$6157], BANK[1]
character_choose_name::
; ...

SECTION "ROM1_6838", ROMX[$6838], BANK[1]
title_screen::
; ...

SECTION "ROM1_68C1", ROMX[$68C1], BANK[1]
title_new_game::
; ...

SECTION "ROM1_68EF", ROMX[$68EF], BANK[1]
title_new_game_scroller_loop::
; ...

SECTION "ROM1_6962", ROMX[$6962], BANK[1]
character_creator_primary::
; ...

SECTION "ROM1_697B", ROMX[$697B], BANK[1]
title_load_game::
; ...

SECTION "ROM1_668C", ROMX[$668C], BANK[1]
select_game_slot::
; ...