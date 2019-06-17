; A table representing the type of each tile in the tileset.
; (32 bytes per tileset.)
;
; Examples:
; $01 = floor
; $03 = wall
; $04 = bridge
; $11 = feet under background (used by top of beds)
; $12 = water
; $21 = head under background
; $31 = full under background (used by bottom of beds)
; $C0 = door exit
; $C1 = stairs up exit?
; $C2 = stairs down exit?
; $C3 = door exit 2?
;
; This might be a bitfield of different tile properties.
; It's not entirely sure what the format is.
SECTION "ROM7_7000", ROMX[$7800], BANK[$7]
tileset_tile_type_data::

; A table representing the tile index of each tile in the tileset.
; (32 bytes per tileset.)
; The tile address of each tile is ROM2:($4000 + tile_index * 64)
SECTION "ROM7_7800", ROMX[$7800], BANK[$7]
tileset_tile_index_data::