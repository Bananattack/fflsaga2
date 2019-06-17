SECTION "ROM0_0200", ROM0[$0200]
reset::
; ...

; Arguments: a = new bank
; Result: a = old bank
SECTION "ROM0_04B1", ROM0[$04B1]
bank_switch::
; ...

SECTION "ROM0_0469", ROM0[$0469]
joy_update::
; ...


SECTION "ROM0_04BF", ROM0[$04BF]
enter_menu_from_map::
; ...

; [map_oam_table_address] = map oam source
SECTION "ROM0_06BA", ROM0[$068A]
transfer_map_oam::
; ...

; Prepares the map oam table for use before it is copied.
;
; Arguments:
; - [map_oam_animation_timer] = 8-bit timer
; - [map_oam_clip_enable] = whether clipping is enabled or not.
;
; Result:
; - [map_oam_animation_timer] is incremented
; - [map_oam_table_address] gets updated to the effective table address.
;   This normally alternates between $C0 and $C1 every $10 frames.
;   If clipping is enabled, the table address is $CC instead.
; - In clipping mode, the all sprites behind the textbox are clipped,
;   and the oam table at $CC00 is updated with the result.
; - If menu active and clipping mode, the clipping routine will let
;   the very last sprite be used as a cursor without clipping.
SECTION "ROM0_06B0", ROM0[$06B0]
prepare_map_oam::
; ...

SECTION "ROM0_0F86", ROM0[$0F86]
character_creator_event::
; ...

SECTION "ROM0_16D9", ROM0[$16D9]
default_stat_handler::
; ...

SECTION "ROM0_16DF", ROM0[$16DF]
default_vblank_handler::
; ...

SECTION "ROM0_1997", ROM0[$1997]
load_game::
; ...

SECTION "ROM0_1B4B", ROM0[$1B4B]
clear_map_oam_buffers::
; ...

; Arguments:
; a = tileset index
;     (tileset pointer address = ROM7:($7000 + a * 32)
; Result:
; [map_metatile_types] table is loaded with the tile types for each metatile in the tileset.
SECTION "ROM0_1BB9", ROM0[$1B4B]
load_tileset_types::
; ...

; Arguments:
; a = tileset index
;     (tileset tile data address = ROM7:($7800 + a * 32)
; Result:
; VRAM at $8800 is loaded is loaded with the tile graphics for each metatile in the tileset.
SECTION "ROM0_1F55", ROM0[$1F55]
load_tileset_graphics::
; ...

; Resets the scroll destination address to the top-left, and loads the screen.
; [map_scroll_dest_address_h], [map_scroll_dest_address_l] = $9800
; Afterwards, the first screen of the map is loaded.
SECTION "ROM_20C8", ROM0[$20C8]
map_tilemap_reset_camera_load_screen::
; ...

; Loads a screen of tiles at the given scroll destination address in VRAM,
; Using the current camera position 
SECTION "ROM_20DB", ROM0[$20DB]
map_tilemap_load_screen::
; ...

; Arguments:
; c = tilemap x
; b = tilemap y
; Result:
; [map_tilemap_buffer] contains unpacked column data
SECTION "ROM_2104", ROM0[$2104]
map_tilemap_prepare_row::
; ...

; Arguments:
; c = tilemap x
; b = tilemap y
; Result:
; [map_tilemap_buffer] contains unpacked column data
SECTION "ROM_212C", ROM0[$212C]
map_tilemap_prepare_column::
; ...

; Arguments:
; a = original tile index
; Result:
; a = remapped tile index
SECTION "ROM_21A0", ROM0[$21A0]
map_remap_metatile_index::
; ...

; Arguments:
; hl = tilemap VRAM dest (top-left corner)
; [map_tilemap_buffer] = $2C (44) bytes of tile row data
; (top-left, top-right, bottom-left, bottom-right, ...)
SECTION "ROM_2155", ROM0[$2155]
map_tilemap_copy_row::
; ...

; Arguments:
; hl = tilemap VRAM dest (top-left corner)
; [map_tilemap_buffer] = $24 (36) bytes of tile column data
; (top-left, top-right, bottom-left, bottom-right, ...)
SECTION "ROM_2178", ROM0[$2178]
map_tilemap_copy_column::
; ...

; Calculates a metatile map address and reads the tile at that location.
;
; Arguments:
; c = tilemap x
; b = tilemap y
;
; Result:
; bc = metatile map address = D000 + y * 64 + x
; a = metatile at metatile map address = [bc]
SECTION "ROM_2636", ROM0[$2636]
map_read_metatile::
; ...

SECTION "ROM0_28A8", ROM0[$28A8]
exit_character_creator_to_map_inner::
; ...

; c = pixel x
; b = pixel y
; de = metasprite data table seems to be around ROM1:$4000 or so? format unknown.
SECTION "ROM_2A5D", ROM0[$2A5D]
metasprite_draw::
; ...

SECTION "ROM0_2B8A", ROM0[$2B8A]
vertical_scale_out::
; ...

SECTION "ROM0_2C95", ROM0[$2C95]
stat_vertical_scale::
; ...

SECTION "ROM0_2CBE", ROM0[$2CBE]
diamond_wipe_out::
; ...

SECTION "ROM0_2D00", ROM0[$2D00]
cross_wipe_out::
; ...

SECTION "ROM0_2D41", ROM0[$2D41]
cross_wipe_in::
; ...

; Arguments:
; bc = new handler
;
; Result:
; [$C706] aka [stat_dispatcher] set new handler, [$C473] has old handler
SECTION "ROM0_2E9C", ROM0[$2E9C]
set_stat_handler::
; ...

SECTION "ROM0_2E9C", ROM0[$2E9C]
set_stat_to_vertical_scale:
; ...

; Result:
; [$C706] set to old handler in [$C473]
SECTION "ROM0_2EEC", ROM0[$2EEC]
restore_stat_handler::
; ...

SECTION "ROM0_2DDC", ROM0[$2DDC]
start_window_split::
; ...

SECTION "ROM0_2F03", ROM0[$2F03]
stop_window_split::
; ...

SECTION "ROM0_2F16", ROM0[$2F16]
stat_diamond_wipe::
; ...

SECTION "ROM0_2F43", ROM0[$2F43]
stat_cross_wipe::
; ...

SECTION "ROM0_3039", ROM0[$3039]
map_handle_buttons::
; ...

SECTION "ROM0_31C5", ROM0[$31C5]
map_pressed_select::
; ...
SECTION "ROM0_31E1", ROM0[$31E1]
map_pressed_start::
; ...
SECTION "ROM0_322B", ROM0[$322B]
map_pressed_b::
; ...
SECTION "ROM0_304E", ROM0[$304E]
map_pressed_a::
; ...

SECTION "ROM0_$338A", ROM0[$338A]
load_npc_graphics::
; ...

; Fades the screen out to white.
SECTION "ROM0_3E58", ROM0[$3E5B]
fade_out::
; ...

; Fades the screen in from white.
SECTION "ROM0_3EA2", ROM0[$3EA2]
fade_in::
; ...