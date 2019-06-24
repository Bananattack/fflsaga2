SECTION "ROM0_0150", ROM0[$0150]
u8_mul_u8_entry:: jp u8_mul_u8
u8_div_u8_entry:: jp u8_div_u8
u16_sub_entry:: jp u16_sub
u16_cmp_entry:: jp u16_cmp
u16_mul_u16_entry:: jp u16_mul_u16

; ...

SECTION "ROM0_0162", ROM0[$0162]
u24_add_entry:: jp u24_add
u24_sub_entry:: jp u24_sub
u24_cmp_entry:: jp u24_cmp

; ...


SECTION "ROM0_016B", ROM0[$016B]
routine_043E_entry:: jp routine_043E
stat_handler_common_entry:: jp stat_handler_common
joy_update_entry:: jp joy_update

; ...

SECTION "ROM0_0177", ROM0[$0177]
vram_transfer_start_entry:: jp vram_transfer_start
vram_transfer_end_entry:: jp vram_transfer_end
banked_call_entry:: jp banked_call
u24_mul_u8_entry:: jp u24_mul_u8
u24_div_u8_entry:: jp u24_div_u8

; ...

SECTION "ROM0_0192", ROM0[$0192]
routine_063E_entry:: jp routine_063E

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