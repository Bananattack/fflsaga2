SECTION "ROM0_0200", ROM0[$0200]
reset::
; ...

SECTION "ROM0_02F0", ROM0[$02F0]
routine_02F0::
    push af
    push bc
    ld b, $08
    xor a
    ld c, a
.loop
    rr h
    jr nc, .skip
    add l
.skip    
    rra  
    rr c
    dec b
    jr nz, .loop

    ld h, a
    ld l, c
    pop bc
    pop af
    ret

SECTION "ROM0_306", ROM0[$0306]
routine_0306::
    push af
    push bc
    ld a, l
    cpl  
    ld c, a
    inc c
    xor a
    ld b, $08
.loop
    sla h
    rla  
    add c
    jr c, .skip
    add l
    inc h
.skip
    dec b
    jr nz, .loop

    ld l, a
    ld a, h
    cpl  
    ld h, a
    pop bc
    pop af
    ret  

SECTION "ROM0_0321", ROM0[$0321]
routine_0321::
    push af
    push bc
    ld c, l
    ld b, h
    ld hl, $0000
    ld a, $10
.loop
    rr d
    rr e
    jr nc, .skip
    add hl, bc
.skip
    rr h
    rr l
    dec a
    jr nz, .loop
    rr d
    rr e
    pop bc
    pop af
    ret  

SECTION "ROM0_033F", ROM0[$033F]
routine_033F::
    di   
    push af
    push bc
    ld c, l
    ld b, h
    ld hl, .done
    push hl
    ld [ld_sp_ret_dispatcher + 1], sp
    ld a, e
    cpl  
    ld l, a
    ld a, d
    cpl  
    ld h, a
    inc hl
    ld sp, hl
    ld hl, $0000
    ld a, $10
.loop
    sla c
    rl b
    rl l
    rl h
    add hl, sp
    jr c, .skip
    add hl, de
    inc c
.skip
    dec a
    jr nz, .loop
    jp ld_sp_ret_dispatcher

.done
    push hl
    ld a, c
    cpl  
    ld l, a
    ld a, b
    cpl  
    ld h, a
    pop de
    pop bc
    pop af
    reti 

SECTION "ROM0_0376", ROM0[$0376]
routine_0376::
    ldh [$FF90], a
    push de
    ld a, l
    sub e
    ld l, a
    ld a, h
    sbc d
    ld h, a
    jr c, .else
    or l
    jr .done
.else
    or l
    scf  
.done
    pop de
    ldh a, [$FF90]
    ret  

SECTION "ROM0_038A", ROM0[$038A]
routine_038A::
    push hl
    call routine_0376
    pop hl
    ret 


SECTION "ROM0_0390", ROM0[$0390]
routine_0390::
    ldh [$FF90], a
    push de
    push hl
    ld a, [de]
    add [hl]
    ld [de], a
    inc de
    inc hl
    ld a, [de]
    adc [hl]
    ld [de], a
    inc de
    inc hl
    ld a, [de]
    adc [hl]
    ld [de], a
    pop hl
    pop de
    ldh a,[$FF90]
    ret

SECTION "ROM0_03A6", ROM0[$03A6]
routine_03A6::
    ldh [$FF90],a
    push bc
    push de
    push hl
    ld a,[de]
    sub [hl]
    ld [de],a
    ld c,a
    inc de
    inc hl
    ld a,[de]
    sbc [hl]
    ld [de],a
    ld b,a
    inc de
    inc hl
    ld a,[de]
    sbc [hl]
    ld [de],a
    jr label_03CD

SECTION "ROM0_03BC", ROM0[$03BC]
routine_03BC::
    ldh [$FF90], a
    push bc
    push de
    push hl
    ld a, [de]
    sub [hl]
    ld c, a
    inc de
    inc hl
    ld a, [de]
    sbc [hl]
    ld b, a
    inc de
    inc hl
    ld a, [de]
    sbc [hl]
    ; fallthrough

label_03CD::
    jr c, .else
    or c
    or b
    jr .done
.else
    or c
    or b
    scf
.done
    pop hl
    pop de
    pop bc
    ldh a, [$FF90]
    ret

SECTION "ROM0_03DC", ROM0[$03DC]
routine_03DC::
    push af
    push bc
    push de
    push hl
    push de
    ld l, e
    ld h, d
    ld e, [hl]
    inc hl
    ld d, [hl]
    inc hl
    ld l, [hl]
    ld h, a
    ld b, $18
    xor a
.loop
    rr l
    rr d
    rr e
    jr nc, .skip
    add h
.skip
    rra  
    dec b
    jr nz, .loop

    rr l
    rr d
    rr e
    ld c,l
    pop hl
    ld [hl], e
    inc hl
    ld [hl], d
    inc hl
    ld [hl], c
    inc hl
    ld [hl], a
    jp pop_hl_de_bc_af

SECTION "ROM0_040B", ROM0[$040B]
routine_040B::
    push bc
    push de
    push hl
    ld l, e
    ld h, d
    ld c, a
    cpl  
    ld b, a
    inc b
    push hl
    ld e, [hl]
    inc hl
    ld d, [hl]
    inc hl
    ld a, [hl]
    ld h, c
    ld c, a
    xor a
    ld l, $18
.loop
    sla e
    rl d
    rl c
    rla  
    add b
    jr c, .skip
    add h
    inc e
.skip
    dec l
    jr nz, .loop

    pop hl
    ld b, a
    ld a, e
    cpl  
    ldi [hl], a
    ld a, d
    cpl  
    ldi [hl], a
    ld a, c
    cpl  
    ld [hl], a
    ld a, b
    pop hl
    pop de
    pop bc
    ret

SECTION "ROM0_043E", ROM0[$043E]
routine_043E::
    push de
    push hl
    ld hl, $C0A0
    rst rst_hl_plus_a
    inc [hl]
    ld l, [hl]
    ld h, $40
    ld a, $0F
    rst rst_bank_switch
    ld h, [hl]
    rst rst_bank_switch
    ld a, e
    cp a, $FF
    jr z, .done
    ld a, d
    and a
    jr z, .done
    cp e
    jr z, .done
    sub e
    ld l,a
    cp a, $FF
    ld a,h
    jr z, .skip
    inc l
    call routine_0306
    ld a,l
.skip
    add e
.done
    pop hl
    pop de
    ret  

SECTION "ROM0_0469", ROM0[$0469]
joy_update::
    ldh a, [joy_held_mask]
    ld c, a
    ld a, [joy_repeat_mask]
    cp c
    jr nz, .changed_buttons
    ld a, [joy_repeat_delay]
    dec a
    jr z, .repeat_activated
.repeat_cooldown
    ld [joy_repeat_delay], a
    xor a
    ldh [joy_pressed_mask], a
    ret
.repeat_activated
    ld a, $05
    ld [joy_repeat_delay], a
    ld a, c
    ldh [joy_pressed_mask], a
    ret
.changed_buttons
    ld a, $1E
    ld [joy_repeat_delay], a
    ld a,c
    ldh [joy_pressed_mask], a
    ld [joy_repeat_mask], a
    ret  

SECTION "ROM0_0494", ROM0[$0494]
routine_0494::
    call joy_update
    call routine_068F
    ldh a, [joy_pressed_mask]
    ret

SECTION "ROM0_049D", ROM0[$049D]
routine_049D::
    push af
.loop
    rst rst_wait_vblank
    ldh a, [$FF89]
    and a
    jr nz, .loop
    pop af
    ret  

SECTION "ROM0_04A6", ROM0[$04A6]
routine_04A6::
    push af
.loop
    call routine_068F
    ldh a, [joy_held_mask]
    and a
    jr nz, .loop
    pop af
    ret

; Arguments:
; - a = new bank
;
; Result:
; - MBC ROM bank is switched to the new bank.
; - [current_rom_bank] is also updated to the new bank.
; - a = old bank.
SECTION "ROM0_04B1", ROM0[$04B1]
bank_switch::
    push bc
    ld   c, a
    ldh  a, [current_rom_bank]
    ld   b, a
    ld   a, c
    ldh  [current_rom_bank], a
    ld   [$2100], a
    ld   a, b
    pop  bc
    ret  

SECTION "ROM0_04BF", ROM0[$04BF]
enter_menu_from_map::
    push af
    push hl
    push de
    ld hl, sp + 6
    ld a, [hl]
    ld e, a
    add a, $03
    ldi [hl], a
    ld d, [hl]
    jr nc, .skip
    inc [hl]
.skip
    ld l, e
    ld h, d
    ldi a, [hl]
    ld [ld_a_jmp_dispatcher + 3], a
    ldi a, [hl]
    ld [ld_a_jmp_dispatcher + 4], a
    ld a, [hl]
    rst rst_bank_switch
    ld e, a
    ld hl, sp + 5
    ld a, [hl]
    di   
    ld [hl], e
    ld [ld_a_jmp_dispatcher + 1], a
    pop de
    pop hl
    pop af
    dec sp
    ei   
    call ld_a_jmp_dispatcher
    push af
    push hl
    ld hl, sp + 4
    ld a, [hl]
    rst rst_bank_switch
    pop hl
    pop af
    inc sp
    ret

; Disables save RAM access, and enables interrupts afterwards.
SECTION "ROM0_04F4", ROM0[$04F4]
disable_save_ram::
    push af
    xor a
    ld [$0000], a
    pop af
    reti 

; Enables save RAM access, and disables interrupts beforehand.
SECTION "ROM0_04FB", ROM0[$04FB]
enable_save_ram::
    di   
    push af
    ld a, $0A
    ld [$0000], a
    pop af
    ret  

; Arguments: 
; - a = operand
; - c = bit index (modulo 8)
;
; Result:
; - a is trashed.
; - flags are affected in same way as a `bit n, a` instruction,
;   but the bit index is determined by c.
SECTION "ROM0_0504", ROM0[$0504]
test_bit::
    push bc
    ld b, a
    ld a, c
    ld c, $47
    jr launch_bit_op_dispatcher

; Arguments: 
; a = operand
; c = bit index (modulo 8)
;
; Result:
; - a and flags are affected in same way as a `set n, a` instruction,
;   but the bit index is determined by c.
SECTION "ROM0_050B", ROM0[$050B]
set_bit::
    push bc
    ld b, a
    ld a, c
    ld c, $C7
    jr launch_bit_op_dispatcher

; Arguments: 
; a = operand
; c = bit index (modulo 8)
;
; Result:
; - a and flags are affected in same way as a `res n, a` instruction,
;   but the bit index is determined by c.
SECTION "ROM0_0512", ROM0[$0512]
reset_bit::
    push bc
    ld b, a
    ld a, c
    ld c, $87
    ; fallthrough

; Arguments:
; a = bit index
; c = base opcode
;
; Result:
; - runs the opcode sequence `$CB, (a << 3 | c), $C9`, and the results reflect this.
; - pops bc and returns.
SECTION "ROM0_0517", ROM0[$0517]
launch_bit_op_dispatcher::
    and a, $07
    rlca 
    rlca 
    rlca 
    or c
    ld [bitwise_op_dispatcher + 1], a
    ld a, b
    call bitwise_op_dispatcher
    pop bc
    ret 

; Multiplies both d and e by 8, independently.
SECTION "ROM0_0526", ROM0[$0526]
d_times_8_e_times_8::
    push af

    ; d = d * 8
    ld a, d
    add a
    add a
    add a
    ld d, a

    ; e = e * 8
    ld a, e
    add a
    add a
    add a
    ld e, a

    pop af
    ret

SECTION "ROM0_0533", ROM0[$0533]
routine_0533::
    xor a
    ldh [$FF9B], a
    xor a
    ld [$C7CA], a
    ld [$C7CF], a
    dec a
    ld b, $80
    ld hl, $C380
    jp memset8
    ld hl, $FF97
    ld a, $FF
    ldi [hl], a
    ldi [hl], a
    ldi [hl], a
    ld [hl], a
    ret

SECTION "ROM0_0550", ROM0[$0550]
routine_0550::
    ld hl, $7F00
    ld de, $8700
    ld b, $00
    ld a, $03
    jp banked_vramcpy8
    ldh [$FF90], a
    ld a, $0D
    rst rst_bank_switch
    push af
    ldh a, [$FF90]
    ld l, a
    ld h, $0A
    call routine_02F0
    ld de, $6F80
    add hl, de
    push hl
    ld a, [$C709]
    ld hl, $C204
    call routine_05D9
    ld e, l
    ld d, h
    ldh a, [$FF90]
    ld [de], a
    inc de
    pop hl
    ldi a, [hl]
    inc hl
    ldh [$FF92], a
    swap a
    and a, $0F
    ld [de], a
    ldh [$FF91], a
    inc de
    xor a
    ld [de], a
    inc de
    ld c, [hl]
    inc hl
    ld b, [hl]
    inc hl
    ld a, c
    ld [de], a
    inc de
    ld a, b
    ld [de], a
    inc de
    ld a, c
    ld [de], a
    inc de
    ld a, b
    ld [de], a
    inc de
    ld b, $04
    call memcpy8
    ldh a, [$FF92]
    and a, $07
    inc a
    ld b, a
    ldh [$FF90], a
    ldi a, [hl]
    ld h, [hl]
    ld l, a
    push de
.loop
    ldi a, [hl]
    ld [de], a
    inc de
    inc de
    dec b
    jr nz, .loop

    pop de
    ld a, $0C
    rst rst_bank_switch
    ldh a, [$FF90]
    ld b, a
.loop2
    ld a, [de]
    inc de
    ld hl, $7E80
    rst rst_hl_plus_a
    ldh a, [$FF91]
    cp a, $03
    ld a, [hl]
    jr nz, .skip
    cp a, $FE
    jr z, .skip
    srl a
.skip
    ld [de], a
    inc de
    dec b
    jr nz, .loop2

    pop af
    rst rst_bank_switch
    ret

SECTION "ROM0_05D9", ROM0[$05D9]
routine_05D9::
    push bc
    ld b, a
    ldh a, [$FF8B]
    and a
    jr z, .else
    ld a, b
    add h
    ld h, a
    jr .done
.else
    ld a, b
    call routine_05EF
    call a_times_32
    rst rst_hl_plus_a
.done
    pop bc
    ret  

SECTION "ROM0_05EF", ROM0[$05EF]
routine_05EF::
    push bc
    cp a, $04
    jr c, .else
    ld a, $04
    jr .done
.else
    ld b, a
    inc b
    ld a, [$C2A0]
    rlca
    rlca
.loop
    rrca 
    rrca 
    dec b
    jr nz, .loop
    and a, $03
.done
    pop bc
    ret

SECTION "ROM0_0608", ROM0[$0608]
routine_0608::
    ldh  [$FF90], a
    push de
    ld e, $00
    call routine_063E
    and a
    pop de
    ldh a, [$FF90]
    ret  

SECTION "ROM0_0615", ROM0[$0615]
routine_0615::
    push bc
    push hl
    call routine_062E
    ld a, [hl]
    call test_bit
    pop hl
    pop bc
    ret

SECTION "ROM0_0621", ROM0[$0621]
routine_0621::
    push bc
    push hl
    call routine_062E
    ld a, [hl]
    call set_bit
    ld [hl], a
    pop hl
    pop bc
    ret

SECTION "ROM0_062E", ROM0[$062E]
routine_062E::
    ld c, a
    srl a
    srl a
    srl a
    ld hl, $C306
    rst rst_hl_plus_a
    ld a, c
    and a, $07
    ld c, a
    ret

SECTION "ROM0_063E", ROM0[$063E]
routine_063E::
    push hl
    call routine_0661
    jr c, $0646
    swap a
.skip
    and a,$0F
    pop hl
    ret

SECTION "ROM0_064A", ROM0[$064A]
routine_064A::
    push de
    push hl
    and a, $0F
    ld d, a
    call routine_0661
    jr c, .else
    and a, $0F
    swap d
    jr .done
.else
    and a, $F0
.done
    or d
    ld [hl],a
    pop hl
    pop de
    ret

SECTION "ROM0_0661", ROM0[$0661]
routine_0661::
    ld a, e
    and a, $1F
    srl a
    push af
    ld hl, $C2F6
    rst rst_hl_plus_a
    pop af
    ld a, [hl]
    ret

SECTION "ROM0_066E", ROM0[$066E]
routine_066E::
    push bc
    push hl
    call routine_0679
    call test_bit
    pop hl
    pop bc
    ret  

SECTION "ROM0_0679", ROM0[$0679]
routine_0679::
    ld a, d
    add a
    ld hl, $C31D
    rst rst_hl_plus_a
    bit 3, e
    jr z, .skip
    inc hl
.skip
    ld a, e
    and a, $07
    ld c, a
    ld a, [hl]
    ret

; Arguments:
; [map_oam_table_address] = map oam source
SECTION "ROM0_068A", ROM0[$068A]
transfer_map_oam::
    ld a, [map_oam_table_address]
    rst rst_oam_dma_transfer
    ret

SECTION "ROM0_068F", ROM0[$068F]
routine_068F::
    push af
    push bc
    rst rst_wait_vblank
    ld c, $CC
    ldh a, [$FF8B]
    and a
    jr nz, .skip
    ld a, [$C764]
    and a
    jr nz, .skip
    ldh a, [menu_mode]
    rrca 
    jr c, .skip
    rrca 
    jr c, .skip
    ld a, [map_oam_table_address]
    ld c, a
.skip
    ld a, c
    rst rst_oam_dma_transfer
    pop bc
    pop af
    ret

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
    push bc
    push de
    push hl
    ld hl, map_oam_animation_timer
    inc [hl]
    ld a, [hl]
    ld hl, $C000
    and a, $10
    swap a
    or h
    ld h, a
    ld [map_oam_table_address], a
    ld a, [map_oam_clip_enable]
    and a
    jr z, .done

    ld b, $28
    ldh a, [menu_mode]
    and a
    jr z, .skip
    ld b, $24
.skip
    ld c, $5A
    ld de, $CC00
.loop
    ldi a, [hl]
    cp c
    jr c, .skip2
    xor a
.skip2
    ld [de], a
    inc e
    ldi a, [hl]
    ld [de], a
    inc e
    ldi a, [hl]
    ld [de], a
    inc e
    ldi a, [hl]
    ld [de], a
    inc e
    dec b
    jr nz, .loop
    ld a, $CC
    ld [map_oam_table_address],a
.done
    pop hl
    pop de
    pop bc
    ret  

SECTION "ROM0_06F4", ROM0[$06F4]
routine_06F4::
    xor a
    ldh [$FF8B],a
    ld [$C764],a
    ldh [$FF96],a
    ldh [$FFA5],a
    jp routine_0550

SECTION "ROM0_0701", ROM0[$0701]
routine_0701::   
    push de
    call routine_07B5
    ld a, [de]
    inc de
    call routine_07BE
    pop de
    ret  

SECTION "ROM0_070C", ROM0[$070C]
routine_070C::
    xor a
    ld [$C77B], a
    rst $30
    cp a, $9E
    jr nc, routine_073B

    cp a, $4E
    jr nc, .else
    ld hl, $13BE
    add a
    rst rst_hl_plus_a
    ld e, [hl]
    inc hl
    ld d, [hl]
    push de
    pop hl
    jp hl
.else
    ld hl, $6560
    sub a, $4E
    add a
    rst rst_hl_plus_a
    ld a, $0F
    call banked_load
    inc hl
    push hl
    call routine_073B
    pop hl
    ld a, $0F
    call banked_load
    ; fallthrough

SECTION "ROM0_073B", ROM0[$073B]
routine_073B::
    call routine_075E
    call routine_0755
    call routine_07AA
    ldh a, [$FFA0]
    and a
    ret z
    cp a, $04
    ret z
    ld hl, $C77F
    dec [hl]
    cp a, $05
    ret z
    jp routine_08D6

SECTION "ROM0_0755", ROM0[$0755]
routine_0755::
    ld hl, $C7A1
    inc [hl]
    call routine_079F
    ldi [hl], a
    ret

SECTION "ROM0_075E", ROM0[$075E]
routine_075E::
    push af
    ldh a, [$FFA0]
    and a
    jr z, .skip3
    cp a, $04
    jr z, .skip3
    cp a, $05
    jr z, .skip
    call $0E30
.skip
    push bc
    push de
    push hl
    ld hl, $C77F
    ld a, [hl]
    and a
    jr nz, .skip2
    dec hl
    ldi a, [hl]
    ld [hl], a
    call $0DED
.skip2
    pop hl
    pop de
    pop bc
    ldh a,[$FFA0]
    cp a,$05
    jr z, .skip3
    call routine_068F
.skip3
    pop af
    ret

SECTION "ROM0_078D", ROM0[$078D]
routine_078D::
    ld a, [$C783]
    ld l, a
    ld a, [$C784]
    ld h, a
    ret

SECTION "ROM0_0796", ROM0[$0796]
routine_0796::
    ld a, l
    ld [$C783], a
    ld a, h
    ld [$C784], a
    ret

SECTION "ROM0_079F", ROM0[$079F]
routine_079F::
    push af
    ld a, [$C781]
    ld l, a
    ld a, [$C782]
    ld h, a
    pop af
    ret  

SECTION "ROM0_07AA", ROM0[$07AA]
routine_07AA::
    push af
    ld a, l
    ld [$C781], a
    ld a, h
    ld [$C782], a
    pop af
    ret

SECTION "ROM0_07B5", ROM0[$07B5]
routine_07B5::
    push hl
    ld hl, $FFA2
    ld e, [hl]
    inc hl
    ld d, [hl]
    pop hl
    ret  

SECTION "ROM0_07BE", ROM0[$07BE]
routine_07BE::
    push hl
    ld hl, $FFA2
    ld [hl], e
    inc hl
    ld [hl], d
    pop hl
    ret

SECTION "ROM0_07C7", ROM0[$07C7]
routine_07C7::
    ld l, e
    ldh a, [$FFA0]
    and a
    jr z, .then
    dec a
    jr z, .then
    dec a
    jr z, .else
    dec a
    jr nz, .then
    ld a,[$D90C]
    ld l,a
.then
    ld h, $00
    jr .done
.else
    ld h, d
    dec h
.done
    add hl, hl
    add hl, bc
    ldi a, [hl]
    ldh [$FFA2], a
    ld a, [hl]
    ldh [$FFA3], a
    ret  

SECTION "ROM0_07E9", ROM0[$07E9]
routine_07E9::
    push af
    ldh a, [$FFA0]
    and a
    jr z, .then
    dec a
    jr z, .then
    dec a
    jr z, .else
.then
    ld a, $0B
    jr .done
.else
    ld a, $0A
.done
    rst $28
    ldh [$FFA1],a
    pop af
    ret  

SECTION "ROM0_0800", ROM0[$0800]
routine_0800::
    push af
    push bc
    push de
    push hl
    ldh a, [$FF8B]
    and a
    ld a, $03
    ld bc, $6B80
    jr nz, routine_084E
    dec a
    ld bc, $4000
    inc d
    dec d
    jr nz, routine_084E
    ld bc, $4000
    ld a, e
    cp a, $70
    ld a, $01
    jr nz, routine_084E
    ld a, $04
    ldh [$FFA0], a
    ld a, $14
    ld [$C77E], a
    call routine_07E9
    call routine_07C7
    ld hl, $C800
    ld bc, $0300
    ld a, $FF
    call memset16
    ld hl, $C814
    call routine_0796
    call routine_07AA
.loop
    call routine_070C
    jr .loop
    ldh a,[$FFA1]
    rst $28
    jp $000B

SECTION "ROM0_084E", ROM0[$084E]
routine_084E::
    push af
    push bc
    push de
    ld hl, $C779
    ld e, [hl]
    inc hl
    ld d, [hl]
    push hl
    ld hl, $FFA0
    ld b, $04
    call memcpy8
    pop hl
    ld [hl], d
    dec hl
    ld [hl], e
    ld hl, $C77D
    ld a, [hl]
    ld [$C77C] ,a
    inc [hl]
    pop de
    pop bc
    pop af
    ldh [$FFA0], a
    call routine_07E9
    call routine_07C7
    ld a, $20
    ld [$C77E], a
.loop
    call routine_070C
    jr .loop

; ...

; ...

SECTION "ROM0_08D6", ROM0[$08D6]
routine_08D6::

; ... 

SECTION "ROM0_0F86", ROM0[$0F86]
character_creator_event::
; ...

; Result:
; - [vram_transfer_stat_dispatcher_old] is set to the previous stat handler
; - [stat_dispatcher] is set to the new handler
SECTION "ROM0_1674", ROM0[$1674]
vram_transfer_start::
    rst rst_wait_vblank
    di   
    push af
    push de
    push hl
    ld hl, stat_dispatcher
    ld de, vram_transfer_stat_dispatcher_old
    ld a, [hl]
    ld [de], a
    inc de
    ld a, $C3
    ldi [hl], a
    ld a, [hl]
    ld [de], a
    inc de
    ld a, vram_transfer_stat_handler & $FF
    ldi [hl], a
    ld a, [hl]
    ld [de], a
    ld a, vram_transfer_stat_handler >> 8
    jr done_vram_transfer_stat_dispatcher_change

; Result:
; - [stat_dispatcher] is set to [vram_transfer_stat_dispatcher_old]
SECTION "ROM0_1691", ROM0[$1691]
vram_transfer_end::
    rst rst_wait_vblank
    di   
    push af
    push de
    push hl
    ld hl, stat_dispatcher
    ld de, vram_transfer_stat_dispatcher_old
    ld a, [de]
    inc de
    ldi [hl], a
    ld a, [de]
    inc de
    ldi [hl], a
    ld a, [de]
    ; fallthrough

SECTION "ROM0_16A3", ROM0[$16A3]
done_vram_transfer_stat_dispatcher_change::
    ld [hl], a
    pop hl
    pop de
    pop af
    reti 

SECTION "ROM0_16A8", ROM0[$16A8]
vram_transfer_stat_handler::
    push af
    ldh a, [$FF44]
    and a
    call z, vram_transfer_new_frame_handler
    inc a
    ldh [$FF45], a
    ldh a, [$FF41]
    ldh [$FF41], a
.wait_hblank
    ldh a, [$FF41]
    and a, $03
    jr nz, .wait_hblank
    pop af
    reti

SECTION "ROM0_16BE", ROM0[$16BE]
vram_transfer_new_frame_handler::
    call stat_handler_common
    call wait_lcd_busy
.wait_hblank
    ldh a, [$FF41]
    and a, $03
    jr nz, .wait_hblank
    call wait_lcd_busy
    ldh a, [$FF44]
    ret 

; Waits until the stat register is in mode 3 (busy)
SECTION "ROM0_16D0", ROM0[$16D0]
wait_lcd_busy:
.loop
    ldh a, [$FF41]
    and a, $03
    cp a, $03
    jr nz, .loop
    ret  

SECTION "ROM0_16D9", ROM0[$16D9]
default_stat_handler::
    call stat_handler_common
    push af
    jr done_irq_handler

SECTION "ROM0_16DF", ROM0[$16DF]
default_vblank_handler::
    push af
    ldh a, [$FFA5]
    and a
    jr z, done_irq_handler

    ld a, $01
    ld [$2100], a
    call $502D

    ldh a, [current_rom_bank]
    ld [$2100], a
    ; fall through

SECTION "ROM0_16F2", ROM0[$16F2]
done_irq_handler::
    xor a
    ldh [$FF45], a
    ldh [$FF0F], a
    pop af
    reti 

SECTION "ROM0_16F9", ROM0[$16F9]
stat_handler_common:
    push af
    push bc
    push de
    push hl
    ld hl, $FF9C
    ld a, [hl]
    and a
    jr nz, .done
    inc [hl]

    ld a, $0E
    call bank_switch
    push af
    call $4000
    ldh a, [$FFA5]
    and a
    jr z, .skip
    ld a, $01
    call bank_switch
    call $5033
.skip
    pop af
    call bank_switch
.shift_joypad_buffer
    ld hl, $C770
    ld de, $C771
    ld b, $03
    push hl
    call memcpy8
    pop hl
.read_joypad_buttons
    ld c, $00
    ld a, $20
    ld [$ff00+c], a
    call poll_joypad_short
    swap a
.read_joypad_directions
    ld b, a
    ld a, $10
    ld [$ff00+c], a
    call poll_joypad_long
SECTION "ROM0_173D", ROM0[$173D]
; ...
SECTION "ROM0_1751", ROM0[$1751]
.handle_reset_combo
; ...
SECTION "ROM0_175C", ROM0[$175C]
    ld hl, $FF9C
    dec [hl]
.done
    jp pop_hl_de_bc_af

; Repeatedly polls the joypad port.
; Discards the first few reads so that the result has time to stabilize.
;
; Arguments:
; c = joypad register address ($00)
;
; Results:
; a = result of reading the joypad port.
SECTION "ROM0_1763", ROM0[$1763]
poll_joypad_long::
    ld a, [$ff00+c]
    ld a, [$ff00+c]
    ld a, [$ff00+c]
    ld a, [$ff00+c]
    ; fallthrough
poll_joypad_short::
    ld a, [$ff00+c]
    ld a, [$ff00+c]
    or a, $F0
    ret

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
; - hl = tilemap VRAM dest (top-left corner)
; - [map_tilemap_buffer] = $2C (44) bytes of tile row data
;   (top-left, top-right, bottom-left, bottom-right, ...)
SECTION "ROM_2155", ROM0[$2155]
map_tilemap_copy_row::
; ...

; Arguments:
; - hl = tilemap VRAM dest (top-left corner)
; - [map_tilemap_buffer] = $24 (36) bytes of tile column data
;   (Byte order:top-left, top-right, bottom-left, bottom-right, ...)
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
; - bc = new handler
;
; Result:
; - [stat_dispatcher_old] is set to the previous stat handler
; - [stat_dispatcher] is set to the new handler
SECTION "ROM0_2E9C", ROM0[$2E9C]
set_stat_handler::
; ...

SECTION "ROM0_2E9C", ROM0[$2E9C]
set_stat_to_vertical_scale:
; ...

; Result:
; - [stat_dispatcher] is set to default_stat_handler
SECTION "ROM0_2ECE", ROM0[$2ECE]
set_default_stat_handler::
; ...

; Result:
; - [stat_dispatcher] is set to [stat_dispatcher_old]
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