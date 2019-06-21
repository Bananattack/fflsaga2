SECTION "ROM0_0150", ROM0[$0150]
u8_mul_entry:: jp u8_mul
u8_div_entry:: jp u8_div
u16_sub_entry:: jp u16_sub
u16_cmp_entry:: jp u16_cmp

; ...

SECTION "ROM0_0162", ROM0[$0162]
u24_add_entry:: jp u24_add
u24_sub_entry:: jp u24_sub
u24_cmp_entry:: jp u24_cmp

; ...

SECTION "ROM0_016E", ROM0[$016E]
stat_handler_common_entry:: jp stat_handler_common
joy_update_entry:: jp joy_update

; ...

SECTION "ROM0_0177", ROM0[$0177]
vram_transfer_start_entry:: jp vram_transfer_start
vram_transfer_end_entry:: jp vram_transfer_end
enter_menu_from_map_entry:: jp enter_menu_from_map

; ...

SECTION "ROM0_01AA", ROM0[$01AA]
bank_switch_entry:: jp bank_switch

; ...

SECTION "ROM0_01C8", ROM0[$01C8]
disable_save_ram_entry:: jp disable_save_ram
enable_save_ram_entry:: jp enable_save_ram

; ...

SECTION "ROM0_01EC", ROM0[$01EC]
transfer_map_oam_entry:: jp transfer_map_oam
; ...