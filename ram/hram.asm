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