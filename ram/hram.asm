; Performs an OAM DMA transfer.
;
; Arguments:
; a = high byte of address for the source OAM table to transfer. 256-byte aligned, 160 bytes long.
SECTION "HRAM_FF80", HRAM[$FF80]
oam_dma_transfer_routine:: DS 8

; Current rom bank selected (last value written to $2100)
SECTION "HRAM_FF88", HRAM[$FF88]
current_rom_bank:: DS 1

; Current buttons being held.
SECTION "HRAM_FF89", HRAM[$FF89]
joy_held_mask:: DS 1

; Current buttons that were just pressed.
; Repeated buttons will also show up here on an repeating interval.
SECTION "HRAM_FF8A", HRAM[$FF8A]
joy_pressed_mask:: DS 1

; (unsure if this right)
; 0 = no menu (all sprites can be clipped)
; 1 = textbox menu (last 4 hardware sprites reserved for cursor, rest can be clipped)
; 2 = menu loading
; 3 = menu active
SECTION "HRAM_FF96", HRAM[$FF96]
menu_mode:: DS 1

; Pointer to the currently executing script data.
SECTION "HRAM_FFA2", HRAM[$FFA2]
script_pointer_l:: DS 1
script_pointer_h:: DS 1
