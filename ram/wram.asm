; The OAM buffers used by the map code, alternates between the two buffers on a timer, to animate the sprites.
SECTION "WRAM0_C000", WRAM0[$C000]
map_oam_buffer0:: DS 160
SECTION "WRAM0_C100", WRAM0[$C100]
map_oam_buffer1:: DS 160

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
; bit N, register
; ret
;
; The bit test opcode gets replaced at runtime.
SECTION "WRAM0_C0E9", WRAM0[$C0E9]
bit_test_dispatcher:: DS 3

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
stat_dispatcher_previous:: DS 3

; The previously loaded value of the stat dipatcher code.
; Backed up here during certain temporary operations that override the stat dispatcher.
SECTION "WRAM0_C7D3", WRAM0[$C7D3]
vram_transfer_stat_dispatcher_previous:: DS 3

; The animation timer used to cycle which OAM buffer is used for drawing.
SECTION "WRAM0_C4FF", WRAM0[$C4FF]
map_oam_animation_timer:: DS 1

; The types of every metatile in the currently loaded tileset.
SECTION "WRAM0_C520", WRAM0[$C520]
map_metatile_types:: DS 32

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