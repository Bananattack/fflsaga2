; The OAM buffers used by the map code, alternates between the two buffers on a timer, to animate the sprites.
SECTION "WRAM0_C000", WRAM0[$C000]
map_oam_buffer0:: DS 160
SECTION "WRAM0_C100", WRAM0[$C100]
map_oam_buffer1:: DS 160

ITEM_STRUCT: MACRO
\1_type:: DS 1
\1_uses:: DS 1
ENDM

PLAYER_EQUIP_COUNT EQU 8
PLAYER_STRUCT: MACRO
\1_name:: DS 4
\1_sprite_id:: DS 1
\1_class:: DS 1
\1_max_hp:: DS 2
\1_hp:: DS 2
\1_str:: DS 1
\1_agl:: DS 1
\1_mana:: DS 1
\1_def:: DS 1
\1_equip_0:: ITEM_STRUCT \1_equip_0
\1_equip_1:: ITEM_STRUCT \1_equip_1
\1_equip_2:: ITEM_STRUCT \1_equip_2
\1_equip_3:: ITEM_STRUCT \1_equip_3
\1_equip_4:: ITEM_STRUCT \1_equip_4
\1_equip_5:: ITEM_STRUCT \1_equip_5
\1_equip_6:: ITEM_STRUCT \1_equip_6
\1_equip_7:: ITEM_STRUCT \1_equip_7
\1_padding:: DS 1
ENDM

SECTION "WRAM0_C200", WRAM0[$C200]
party_members::
party_member_0:: PLAYER_STRUCT party_member_0
SECTION "WRAM0_C220", WRAM0[$C220]
party_member_1:: PLAYER_STRUCT party_member_1
SECTION "WRAM0_C240", WRAM0[$C240]
party_member_2:: PLAYER_STRUCT party_member_2
SECTION "WRAM0_C260", WRAM0[$C260]
party_member_3:: PLAYER_STRUCT party_member_3
SECTION "WRAM0_C280", WRAM0[$C280]
party_member_4:: PLAYER_STRUCT party_member_4

INVENTORY_ITEM_COUNT EQU 16

SECTION "WRAM0_C2D9", WRAM0[$C2D9]
item_slots::
item_slot_0:: ITEM_STRUCT item_slot_0
item_slot_1:: ITEM_STRUCT item_slot_1
item_slot_2:: ITEM_STRUCT item_slot_2
item_slot_3:: ITEM_STRUCT item_slot_3
item_slot_4:: ITEM_STRUCT item_slot_4
item_slot_5:: ITEM_STRUCT item_slot_5
item_slot_6:: ITEM_STRUCT item_slot_6
item_slot_7:: ITEM_STRUCT item_slot_7
item_slot_8:: ITEM_STRUCT item_slot_8
item_slot_9:: ITEM_STRUCT item_slot_9
item_slot_A:: ITEM_STRUCT item_slot_A
item_slot_B:: ITEM_STRUCT item_slot_B
item_slot_C:: ITEM_STRUCT item_slot_C
item_slot_D:: ITEM_STRUCT item_slot_D
item_slot_E:: ITEM_STRUCT item_slot_E
item_slot_F:: ITEM_STRUCT item_slot_F

; A routine that does:
;
; ld sp, $NNNN
; ret
;
; The immediate value gets replaced at runtime.
SECTION "WRAM0_C0E0", WRAM0[$C0E0]
ld_sp_ret_dispatcher:: DS 4

; A routine that does:
;
; ld a, $NN
; jp $NNNN
;
; The immediate values get replaced at runtime.
SECTION "WRAM0_C0E4", WRAM0[$C0E4]
ld_a_jmp_dispatcher:: DS 5

; A routine that does:
;
; bit N, register (CB-prefixed opcode)
; ret
;
; The bitwise opcode after the CB-prefix gets replaced at runtime.
SECTION "WRAM0_C0E9", WRAM0[$C0E9]
bitwise_op_dispatcher:: DS 3

; A buffer used to store tiles that will be copied to the background tilemap VRAM.
SECTION "WRAM0_C400", WRAM0[$C400]
map_tilemap_buffer:: DS 44

; The direction the player is facing.
SECTION "WRAM0_C436", WRAM0[$C436]
map_player_direction:: DS 1
; Whether or not the player is idling (0 = moving, 1 = idle)
SECTION "WRAM0_C438", WRAM0[$C438]
map_player_idle:: DS 1
; X position of the player.
SECTION "WRAM0_C42C", WRAM0[$C42C]
map_player_x:: DS 1
; Y position of the player.
SECTION "WRAM0_C42D", WRAM0[$C42D]
map_player_y:: DS 1

; The direction that the player is moving + 1 (0 = not moving).
SECTION "WRAM0_C43D", WRAM0[$C43D]
map_player_move_direction:: DS 1

; VRAM address of the top-left tile corner, changes as the cameras scrolls.
SECTION "WRAM0_C449", WRAM0[$C449]
map_scroll_dest_address_l:: DS 1
SECTION "WRAM0_C44A", WRAM0[$C44A]
map_scroll_dest_address_h:: DS 1

; The currently loaded tileset.
SECTION "WRAM0_C450", WRAM0[$C450]
map_tileset_index:: DS 1

; The previously loaded value of the stat dipatcher code.
; Backed up here during certain temporary operations that override the stat dispatcher.
SECTION "WRAM0_C473", WRAM0[$C473]
stat_dispatcher_old:: DS 3

; The previously loaded value of the stat dipatcher code.
; Backed up here during certain temporary operations that override the stat dispatcher.
SECTION "WRAM0_C7D3", WRAM0[$C7D3]
vram_transfer_stat_dispatcher_old:: DS 3

; The animation timer used to cycle which OAM buffer is used for drawing.
SECTION "WRAM0_C4FF", WRAM0[$C4FF]
map_oam_animation_timer:: DS 1

; The metatile index associated with a specific door.
SECTION "WRAM0_C500", WRAM0[$C500]
map_door_tiles:: DS 32

; The types of every metatile in the currently loaded tileset.
SECTION "WRAM0_C520", WRAM0[$C520]
map_tileset_types:: DS 32

; Background palette.
SECTION "WRAM0_C700", WRAM0[$C700]
dmg_palette_bg:: DS 1
; Object palette 1. Main palette for used for sprites.
SECTION "WRAM0_C701", WRAM0[$C701]
dmg_palette_obj0:: DS 1
; Object palette 2. (Used for stone status effect?)
SECTION "WRAM0_C702", WRAM0[$C702]
dmg_palette_obj1:: DS 1

; A routine that does:
;
; jp $NNNN
;
; The immediate value gets replaced at runtime.
; Called by the vblank interrupt entry point.
SECTION "WRAM0_C703", WRAM0[$C703]
vblank_dispatcher:: DS 3

; A routine that does:
;
; jp $NNNN
;
; The immediate value gets replaced at runtime.
; Called by the stat interrupt entry point.
SECTION "WRAM0_C706", WRAM0[$C706]
stat_dispatcher:: DS 3

; Whether or not clipping should be performed on sprites behind the textbox.
SECTION "WRAM0_C7DE", WRAM0[$C7DE]
map_oam_clip_enable:: DS 1

; Decides the source address for the map OAM data. Affects which buffer is copied.
SECTION "WRAM0_C7DF", WRAM0[$C7DF]
map_oam_table_address:: DS 1

; Delay until the next joypad button repeat.
SECTION "WRAM0_C774", WRAM0[$C774]
joy_repeat_delay:: DS 1

; Which buttons are affected by the joypad button repeat.
SECTION "WRAM0_C775", WRAM0[$C775]
joy_repeat_mask:: DS 1

; The OAM buffer used during menus, and also for textbox-clipping during text events.
SECTION "WRAM0_CC00", WRAM0[$CC00]
menu_oam_buffer:: DS 160

; Map data is unpacked to RAM as a raw 64x64 grid of metatiles.
; $8x = indicates a character is in this cell
; $4x = indicates a door (lower bits are a door index?)
; $2x = ???
; $00 - $1F = metatile index
SECTION "WRAM1_D000", WRAMX[$D000], BANK[1]
map_metatilemap:: DS 64 * 64
