SECTION "ROM0_0150", ROM0[$0150]
u8_mul_u8_entry:: jp u8_mul_u8
SECTION "ROM0_0153", ROM0[$0153]
u8_div_u8_entry:: jp u8_div_u8
SECTION "ROM0_0156", ROM0[$0156]
u16_sub_entry:: jp u16_sub
SECTION "ROM0_0159", ROM0[$0159]
u16_cmp_entry:: jp u16_cmp
SECTION "ROM0_015C", ROM0[$015C]
u16_mul_u16_entry:: jp u16_mul_u16
SECTION "ROM0_015F", ROM0[$015F]
routine_033F_entry:: jp routine_033F
SECTION "ROM0_0162", ROM0[$0162]
u24_add_entry:: jp u24_add
SECTION "ROM0_0165", ROM0[$0165]
u24_sub_entry:: jp u24_sub
SECTION "ROM0_0168", ROM0[$0168]
u24_cmp_entry:: jp u24_cmp
SECTION "ROM0_016B", ROM0[$016B]
routine_043E_entry:: jp routine_043E
SECTION "ROM0_016E", ROM0[$016E]
stat_handler_common_entry:: jp stat_handler_common
SECTION "ROM0_0171", ROM0[$0171]
joy_update_entry:: jp joy_update
SECTION "ROM0_0174", ROM0[$0174]
routine_049D_entry:: jp routine_049D
SECTION "ROM0_0177", ROM0[$0177]
vram_transfer_start_entry:: jp vram_transfer_start
SECTION "ROM0_017A", ROM0[$017A]
vram_transfer_end_entry:: jp vram_transfer_end
SECTION "ROM0_017D", ROM0[$017D]
banked_call_entry:: jp banked_call
SECTION "ROM0_0180", ROM0[$0180]
u24_mul_u8_entry:: jp u24_mul_u8
SECTION "ROM0_0183", ROM0[$0183]
u24_div_u8_entry:: jp u24_div_u8

; ...

SECTION "ROM0_018C", ROM0[$018C]
routine_0615_entry:: jp routine_0615
SECTION "ROM0_018F", ROM0[$018F]
routine_0621_entry:: jp routine_0621
SECTION "ROM0_0192", ROM0[$0192]
routine_063E_entry:: jp routine_063E
SECTION "ROM0_0195", ROM0[$0195]
routine_064A_entry:: jp routine_064A
SECTION "ROM0_0198", ROM0[$0198]
routine_0608_entry:: jp routine_0608
SECTION "ROM0_019B", ROM0[$019B]
routine_05D9_entry:: jp routine_05D9
SECTION "ROM0_019E", ROM0[$019E]
routine_055D_entry:: jp routine_055D
SECTION "ROM0_01A1", ROM0[$01A1]
routine_1884_entry:: jp routine_1884
SECTION "ROM0_01A4", ROM0[$01A4]
routine_188B_entry:: jp routine_188B
SECTION "ROM0_01A7", ROM0[$01A7]
routine_0494_entry:: jp routine_0494
SECTION "ROM0_01AA", ROM0[$01AA]
bank_switch_entry:: jp bank_switch

; ...

SECTION "ROM0_01C8", ROM0[$01C8]
disable_save_ram_entry:: jp disable_save_ram
SECTION "ROM0_01CB", ROM0[$01CB]
enable_save_ram_entry:: jp enable_save_ram

; ...

SECTION "ROM0_01E9", ROM0[$01E9]
routine_14AC_entry:: jp routine_14AC
SECTION "ROM0_01EC", ROM0[$01EC]
transfer_map_oam_entry:: jp transfer_map_oam
; ...