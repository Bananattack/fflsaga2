SECTION "ROM0_0200", ROM0[$0200]
reset::
    di   
    ld sp, $CF00
    ld a, $80
    ldh [$FF40], a
    xor a
    ldh [$FF0F], a
    ldh [$FFFF], a
    ldh [$FF41], a
    ldh [$FF47], a
    ldh [$FF48], a
    ldh [$FF49], a
    ldh [$FF43], a
    ldh [$FF42], a
    ld b, a
    ld a, $1B

    ld hl, $C776
    push hl
    cp [hl]
    inc hl
    jr nz, .skip
    cpl  
    cp [hl]
    jr nz, .skip
    inc b
.skip
    push bc
    ld hl,$C000
    ld b,$A0
    call memclear8
    ld hl, $C100
    ld bc, $0D00
    call memclear16
    ld h, $CF
    ld b, $11
    call memclear16
    ld hl, $FF80
    ld b, $7F
    call memclear8
    pop bc
    pop hl

    ld a, $1B
    ldi [hl], a
    cpl  
    ldi [hl], a
    ld [hl], b
    inc b
    dec b
    jr nz, .skip2
    ld b, $40
    ld hl, $C0A0
    ldh a, [$FF04]
.loop
    ldi [hl],a
    inc a
    dec b
    jr nz, .loop
.skip2

    ld hl, $C779
    ld [hl], $A0
    inc hl
    ld [hl], $CC
    ld hl, oam_dma_transfer_code
    ld de, oam_dma_transfer_routine
    ld b, $08
    call memcpy8
    ld hl, ld_sp_ret_dispatcher_code
    ld de, ld_sp_ret_dispatcher
    ld b, $0C
    call memcpy8

    ld a, $0E
    rst rst_bank_switch
    call $4003
    di   
    xor a
    ldh [$FF45], a
    ldh [$FF0F], a
    ld a, $03
    ldh [$FFFF], a
    ld a, $40
    ldh [$FF41], a
    ; vblank_dispatcher: `jp default_vblank_handler`
    ld hl, vblank_dispatcher
    ld a, $C3
    ldi [hl], a
    ld a, default_vblank_handler & $FF
    ldi [hl], a
    ld a, default_vblank_handler >> 8
    ldi [hl], a
    ; stat_dispatcher: `jp default_stat_handler` 
    ld hl, stat_dispatcher
    ld a, $C3
    ldi [hl], a
    ld a, default_stat_handler & $FF
    ldi [hl], a
    ld a, default_stat_handler >> 8
    ld [hl], a
    call routine_0550
    ld hl, $4800
    ld bc, $0800
    ld a, $04
    call banked_vramcpy16

    call enable_save_ram
    ld hl, $A781
    ldi a, [hl]
    cp a, $1B
    jr nz, .then
    ld a, [hl]
    cp a, $E4
    jr z, .done
.then
    ld a, $01
    ld [$A780], a
.done
    call disable_save_ram

    ld hl, $C200
    ld c, $04
.loop2
    ld b, $04
    ld a, $FF
    call memset8
    ld  a, $1C
    rst rst_hl_plus_a
    dec c
    jr nz, .loop2

    ld a, $01
    rst rst_bank_switch
    call $500F
    jp nc, routine_1900
    jp routine_1903

; Arguments:
; h = left-hand side
; l = right-hand side
;
; Result:
; hl = 16-bit product.
SECTION "ROM0_02F0", ROM0[$02F0]
u8_mul_u8::
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

; Arguments:
; h = left-hand side (numerator)
; l = right-hand side (denominator)
;
; Result:
; h = quotient
; l = remainder 
SECTION "ROM0_306", ROM0[$0306]
u8_div_u8::
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

; Arguments:
; de = left-hand side
; hl = right-hand side
;
; Result:
; de = low bits of 32-bit product
; hl = high bits of 32-bit product
SECTION "ROM0_0321", ROM0[$0321]
u16_mul_u16::
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

; Arguments:
; - hl = dest
; - de = source
;
; Result:
; - de = de - hl;
; - zero and carry flags are updated to the subtraction result
SECTION "ROM0_0376", ROM0[$0376]
u16_sub::
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

; Arguments:
; - hl = dest
; - de = source
;
; Result:
; - zero and carry flags are updated to the comparison result
SECTION "ROM0_038A", ROM0[$038A]
u16_cmp::
    push hl
    call u16_sub
    pop hl
    ret

; Arguments:
; - de = dest pointer
; - hl = source pointer
;
; Result:
; - Performs a 24-bit addition.
; - [de] = [de] + [hl]
; - carry flag indicates whether the addition generated a carry
; - zero flag indicates whether the addition resulted in the highest byte becoming zero.
SECTION "ROM0_0390", ROM0[$0390]
u24_add::
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
    ldh a, [$FF90]
    ret

; Arguments:
; - de = dest pointer
; - hl = source pointer
;
; Result:
; - Performs a 24-bit subtraction.
; - [de] = [de] - [hl]
; - zero flag and carry flags are updated to the subtraction result.
; - (unlike the u24_add routine, this computes the full zero flag)
SECTION "ROM0_03A6", ROM0[$03A6]
u24_sub::
    ldh [$FF90], a
    push bc
    push de
    push hl
    ld a, [de]
    sub [hl]
    ld [de], a
    ld c, a
    inc de
    inc hl
    ld a, [de]
    sbc [hl]
    ld [de], a
    ld b, a
    inc de
    inc hl
    ld a, [de]
    sbc [hl]
    ld [de], a
    jr _u24_subtract_end

; Arguments:
; - de = dest pointer
; - hl = source pointer
;
; Result:
; - Performs a 24-bit comparison.
; - Compare [de] to [hl]
; - zero flag and carry flags are updated to the comparison result.
; - (unlike the u24_add routine, this computes the full zero flag)
SECTION "ROM0_03BC", ROM0[$03BC]
u24_cmp::
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

; Jumped to by u24_sub and u24_cmp.
; Corrects the flags, pops register and returns.
SECTION "ROM0_03CD", ROM0[$03CD]
_u24_subtract_end::
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

; Arguments:
; - de = dest pointer (passes in 24-bit multiplicand, receives 32-bit product)
; - a = 8-bit multiplier
; 
; Result:
; - Performs a 24-bit x 8-bit -> 32-bit multiplication.
; - [de] = 32-bit product
SECTION "ROM0_03DC", ROM0[$03DC]
u24_mul_u8::
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
    ld c, l
    pop hl
    ld [hl], e
    inc hl
    ld [hl], d
    inc hl
    ld [hl], c
    inc hl
    ld [hl], a
    jp pop_hl_de_bc_af

; Arguments:
; - de = dest pointer (passes in 24-bit numerator, receives 24-bit quotient)
; - a = 8-bit denominator
; 
; Result:
; - Performs a 24-bit / 8-bit division
; - [de] = 24-bit quotient
; - a = 8-bit remainder
SECTION "ROM0_040B", ROM0[$040B]
u24_div_u8::
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
    ld l, a
    cp a, $FF
    ld a, h
    jr z, .skip
    inc l
    call u8_div_u8
    ld a, l
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
    ld a, c
    ldh [joy_pressed_mask], a
    ld [joy_repeat_mask], a
    ret

SECTION "ROM0_0494", ROM0[$0494]
routine_0494::
    call joy_update
    call transfer_current_oam
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
wait_button_release::
    push af
.loop
    call transfer_current_oam
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
    ld c, a
    ldh a, [current_rom_bank]
    ld b, a
    ld a, c
    ldh [current_rom_bank], a
    ld [$2100], a
    ld a, b
    pop bc
    ret


; Calls a subroutine in another bank.
;
; The 3 bytes after the pc on the stack are the
; (low, high, bank) of the subroutine to call.
;
; After the call, the previous bank is restored.
; The register pairs are passed through to the subroutine,
; and the result of subroutine call is left in-tact.
;
; Usage:
; call banked_call
; DW subroutine_address
; DB subroutine_bank
;
; Arguments:
; - af, hl, bc, de are passed to the subroutine.
;
; Result:
; - af, hl, bc, de contain the result of calling the subroutine.
SECTION "ROM0_04BF", ROM0[$04BF]
banked_call::
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

SECTION "ROM0_0546", ROM0[$0546]
routine_0546::
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

SECTION "ROM0_055D", ROM0[$055D]
routine_055D::
    ldh [$FF90], a
    ld a, $0D
    rst rst_bank_switch
    push af
    ldh a, [$FF90]
    ld l, a
    ld h, $0A
    call u8_mul_u8
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
    ldh [$FF90], a
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
    jr c, .skip
    swap a
.skip
    and a, $0F
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
    ld [hl], a
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
transfer_current_oam::
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
    ld [map_oam_table_address], a
.done
    pop hl
    pop de
    pop bc
    ret

SECTION "ROM0_06F4", ROM0[$06F4]
routine_06F4::
    xor a
    ldh [$FF8B], a
    ld [$C764], a
    ldh [$FF96], a
    ldh [$FFA5], a
    jp routine_0550

; Updates the accumulator to the value pointed to by [script_pointer_h][script_pointer_l]
; Advances [script_pointer_h][script_pointer_l] by one byte.
SECTION "ROM0_0701", ROM0[$0701]
script_read_byte::
    push de
    call script_load_pointer
    ld a, [de]
    inc de
    call script_save_pointer
    pop de
    ret

SECTION "ROM0_070C", ROM0[$070C]
script_execute_step::
    xor a
    ld [$C77B], a
    rst rst_script_read_byte
    ; fallthrough

SECTION "ROM0_0711", ROM0[$0711]
script_handle_opcode::
    cp a, $9E
    jr nc, routine_073B
    cp a, $4E
    jr nc, script_handle_other_opcode
    ; fallthrough

script_handle_instruction_opcode::
    ld hl, script_instruction_jump_table
    add a
    rst rst_hl_plus_a
    ld e, [hl]
    inc hl
    ld d, [hl]
    push de
    pop hl
    jp hl

script_handle_other_opcode::    
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
    call routine_0E30
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
    call routine_0DED
.skip2
    pop hl
    pop de
    pop bc
    ldh a, [$FFA0]
    cp a, $05
    jr z, .skip3
    call transfer_current_oam
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

; Result:
; de is updated with the current 16-bit value in [script_pointer_h][script_pointer_l]
SECTION "ROM0_07B5", ROM0[$07B5]
script_load_pointer::
    push hl
    ld hl, script_pointer_l
    ld e, [hl]
    inc hl
    ld d, [hl]
    pop hl
    ret

; Result:
; [script_pointer_h][script_pointer_l] is updated with the current 16-bit value in de.
SECTION "ROM0_07BE", ROM0[$07BE]
script_save_pointer::
    push hl
    ld hl, script_pointer_l
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
    ld a, [$D90C]
    ld l, a
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
    ldh [script_pointer_l], a
    ld a, [hl]
    ldh [script_pointer_h], a
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
    rst rst_bank_switch
    ldh [$FFA1], a
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
    call script_execute_step
    jr .loop
    ldh a, [$FFA1]
    rst rst_bank_switch
    jp pop_hl_de_bc_af

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
    ld [$C77C], a
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
    call script_execute_step
    jr .loop

SECTION "ROM0_0881", ROM0[$0881]
routine_0881::
    ldh a, [$FFA1]
    rst rst_bank_switch
    ld hl, $C779
    ld e, [hl]
    inc hl
    ld d, [hl]
    push hl
    ld hl, $FFA3
    ld b, $04
    call routine_08A3
    pop hl
    ld [hl],d
    dec hl
    ld [hl], e
    ld hl, $C77D
    dec [hl]
    ld a, [hl]
    dec a
    ld [$C77C], a
    jp pop_hl_de_bc_af

SECTION "ROM0_08A3", ROM0[$08A3]
routine_08A3::
.loop
    dec de
    ld a, [de]
    ldd [hl], a
    dec b
    jr nz, .loop
    ret

routine_08AA::
    ld hl, $9C40
    ld de, $9C20
    ld b, $E0
    ldh a, [$FFA0]
    cp a, $03
    jr z, .skip
    ld b, $A0
.skip
    call vram_transfer_start
    call memcpy8
    ld b, $12
    ld hl, $9D01
    ldh a, [$FFA0]
    cp a, $03
    jr z, .skip2
    ld hl, $9CC1
.skip2
    ld a, $FF
    call memset8
    call vram_transfer_end
    ; fallthrough

SECTION "ROM0_08D6", ROM0[$08D6]
routine_08D6::
    ld c, $89
    ld a, [$C765]
    ld e, a
    rrca 
    jr c, .skip
    ld a, [$ff00+c]
    rrca 
    ret c
.skip
    ld a, [$C31B]
    add a
    and a
    ret z
    ld b, a
.loop
    ld a, [$ff00+c]
    bit 0, e
    jr nz, .skip2
    bit 0, a
    ret nz
.skip2
    bit 1,e
    jr nz, .skip3
    bit 1, a
    jr z, .skip3
    inc b
.skip3
    call transfer_current_oam
    dec b
    jr nz, .loop
    ret

SECTION "ROM0_0901", ROM0[$0901]
routine_0901::
    ld [$C7D2],a
    call routine_0916
    ret

SECTION "ROM0_0908", ROM0[$0908]
routine_0908::
    push af
    push bc
    push de
    push hl
    xor a
    ld [$C7D2], a
    call routine_0916
    jp pop_hl_de_bc_af

SECTION "ROM0_0916", ROM0[$0916]
routine_0916::
    xor a
    ldh [$FFA0], a
    ld hl, $C799
    ldi [hl], a
    ld [hl], a
    call routine_07E9
    ld bc, $7400
    call routine_07C7
    call script_load_pointer
    ld hl, $C799
    ld bc, $C79B
    ld a, $02
.loop
    ldh [$FF90], a
    ld a, [de]
    rla  
    rl [hl]
    inc hl
    ld a, [de]
    inc de
    and a, $1F
    ld [bc], a
    inc bc
    ldh a, [$FF90]
    dec a
    jr nz, .loop

    ld l, $02
.loop2
    ld a, [de]
    inc de
    ld [bc], a
    inc bc
    dec l
    jr nz, .loop2

    call script_save_pointer
    ld hl, $C800
    ld bc, $0300
    ld a, [$C799]
    and a
    jr z, .skip
    ld h, $D0
    ld b, $04
.skip

    ld a, $FF
    call memset16
    ld hl, data_0B09
    ldh a, [$FF8B]
    and a
    jr nz, .done
    ld a, [$C764]
    and a
    jr z, .else
    ld hl, data_0B0B
    jr .done
.else
    ld hl, data_0B0D
.done

    ld de, $C7A4
    ld b, $02
    call memcpy8
    ld hl, $C79B
    ld de, $C7A1
    ld a, [$C7D2]
    bit 1, a
    jr z, .else2
    ldi a, [hl]
    ld [de], a
    inc de
    ld [de], a
    inc de
    ldi a, [hl]
    ld [de], a
    jr .done2
.else2
    ldi a, [hl]
    inc a
    ld [de], a
    inc de
    ld [de], a
    inc de
    ld a, [hl]
    inc a
    ld [de], a
.done2

    ld a, [$C7A4]
    ld c, a
    ld a, [hl]
    sub c
    jr nc, .skip2
    xor a
.skip2
    
    ld [hl], a
    ; 09AC
    ld hl,  $C79D
    ld b,  [hl]
    inc hl
    ld c, [hl]
    ld a, [$C7D2]
    bit 1, a
    jr nz, .skip3
    call routine_0A2B
.skip3

    ld a, [$C79D]
    ld b, a
    ld a, [$C799]
    and a
    jr z, .skip4
    ld b, $22
.skip4
    ld a, [$C79A]
    and a
    jr z, .else3
    ld a, l
    ld [$C7C8], a
    ld a, h
    ld [$C7C9], a
    dec b
    dec b
    ld a, b
    rst rst_hl_plus_a
    jr .done3
.else3
    call routine_0AE2
.done3

    ld a, b
    ld [$C77E], a
    call routine_0796
    call routine_07AA
    ld hl, $C79B
    ld de, $C79F
    ld c, [hl]
    inc hl
    ld b, [hl]
    inc hl
    ldi a, [hl]
    add c
    dec a
    ld [de], a
    inc de
    ld a, [hl]
    add b
    dec a
    ld b, a
    ld a, [$C7A4]
    ld c, a
    add b
    ld [de], a
    ld hl, $C79B
    ld de, $C7D0
    ldi a, [hl]
    ld [de], a
    inc de
    ld a, [hl]
    add c
    ld [de], a
    ld a, [$C79A]
    and a
    jr z, .skip5
    ld a, [$C7CA]
.skip5

    ld [$C7CF], a
.loop3
    call script_execute_step
    jr .loop3    

SECTION "ROM0_0A1F", ROM0[$0A1F]
routine_0A1F::
    di   
    call routine_176C
    ei   
    call routine_0A5C
    ldh a, [$FFA1]
    rst rst_bank_switch
    ret

SECTION "ROM0_0A2B", ROM0[$0A2B]
routine_0A2B::
    dec b
    dec b
    dec c
    dec c
    ld e,b
    ld hl, $C800
    ld a, [$C799]
    and a
    jr z, .skip
    ld hl,$D000
.skip

    ld a, $F7
    call routine_0A52
.loop
    ldi [hl], a
    push af
    ld a, $FF
    ld b, e
    call memset8
    pop af
    inc a
    ldi [hl], a
    dec a
    dec c
    jr nz, .loop

    inc a
    inc a
    ; fallthrough

SECTION "ROM0_0A52", ROM0[$0A52]
routine_0A52::
    ldi [hl], a
    ld b, e
    inc a
    call memset8
    inc a
    ldi [hl], a
    inc a
    ret  

SECTION "ROM0_0A5C", ROM0[$0A5C]
routine_0A5C::
    ld a, [$C7D2]
    rrca 
    ret c
    ld hl, $C79B
    ld c, [hl]
    ld b, $9C
    inc hl
    ld l, [hl]
    ld h, $00
    call hl_times_32_plus_bc
    ld e, l
    ld d, h
    ld a, [$C79A]
    and a
    jr z, .done
    push de
    ld a,[$C79D]
    inc a
    ld e, a
    ld d, $C8
    ld hl, $C7C8
    ld c, [hl]
    inc hl
    ld b, [hl]
    ld a, [$C77E]
    ld l, a
    ld a, [$C798]
    ld h, a
    call u8_mul_u8
    add hl, bc
    ld a, [$C799]
    and a
    jr z, .skip
    ld d, $D0
    ld a, [$C797]
    rst rst_hl_plus_a
.skip
    ld a, [$C79D]
    sub a, $02
    ld b, a
    ld a, [$C79E]
    sub a, $02
    ld c, a
.outer_loop
    push bc
    push hl
.inner_loop
    ldi a, [hl]
    ld [de], a
    inc de
    dec b
    jr nz, .inner_loop
    inc de
    inc de
    pop hl
    pop bc
    ld a,[$C77E]
    rst rst_hl_plus_a
    dec c
    jr nz, .outer_loop
    pop de
.done
    ld hl, $C79D
    ld b, [hl]
    inc hl
    ld c, [hl]
    ; fallthrough
SECTION "ROM0_0AC2", ROM0[$0AC2]
routine_0AC2::
    ld hl, $C800
    ld a, [$C799]
    and a
    jr z, .skip
    ld hl, $D000
.skip
    call routine_0AF3
    call transfer_current_oam
    ld a, [$C7A5]
    ldh [$FF4A], a
    ld a, $07
    ldh [$FF4B], a
    ld a, $E3
    ldh [$FF40], a
    ret

SECTION "ROM0_0AE2", ROM0[$0AE2]
routine_0AE2::
    ld a, [$C7D2]
    bit 1, a
    ld a, [$C79D]
    jr nz, .skip
    add a
    inc a
.skip
    ld hl, $C800
    rst rst_hl_plus_a
    ret

SECTION "ROM0_0AF3", ROM0[$0AF3]
routine_0AF3::
    call vram_transfer_start
.loop
    push bc
    call memcpy8
    pop bc
    ld a, $20
    sub b
    add e
    ld e, a
    jr nc, .skip
    inc d
.skip
    dec c
    jr nz, .loop
    jp vram_transfer_end

SECTION "ROM0_0B09", ROM0[$0B09]
data_0B09::
    DW $4008
SECTION "ROM0_0B0B", ROM0[$0B0B]
data_0B0B::
    DW $500A
SECTION "ROM0_0B0D", ROM0[$0B0D]
data_0B0D::
    DW $0000

SECTION "ROM0_0B0F", ROM0[$0B0F]
routine_0B0F::
    ldh a, [$FFA0]
    and a
    jr z, .skip
    cp a, $03
    jr nc, .skip
    ld a, [$C77C]
    and a
    jr nz, .skip
    ld a, [$C764]
    and a
    jr z, .skip
    call routine_0CA9
    call wait_button_release
    call routine_0E5C
.skip
    pop hl
    inc hl
    inc hl
    jp hl

SECTION "ROM0_0B31", ROM0[$0B31]
ret_::
    ret  

SECTION "ROM0_0B32", ROM0[$0B32]
routine_0B32::
    call routine_0B46
    cp a, $0F
    jr z, routine_0B43
    inc a
    jr routine_0B43

SECTION "ROM0_0B3C", ROM0[$0B3C]
routine_0B3C::    
    call routine_0B46
    and a
    jr z, routine_0B43
    dec a
    ; fallthrough

SECTION "ROM0_0B43", ROM0[$0B43]
routine_0B43::
    jp routine_064A

SECTION "ROM0_0B46", ROM0[$0B46]
routine_0B46::
    rst rst_script_read_byte
    ld e, a
    jp routine_063E

SECTION "ROM0_0B4B", ROM0[$0B4B]
routine_0B4B::    
    rst rst_script_read_byte
    ld e, a
    rst rst_script_read_byte
    jr routine_0B43

SECTION "ROM0_0B50", ROM0[$0B50]
routine_0B50::
    call routine_0B66
    call routine_0679
    call set_bit
    jr routine_0B64

SECTION "ROM0_0B5B", ROM0[$0B5B]
routine_0B5B::
    call routine_0B66
    call routine_0679
    call reset_bit
    ; fallthrough

SECTION "ROM0_0B64", ROM0[$0B64]
routine_0B64::
    ld [hl], a
    ret  

SECTION "ROM0_0B66", ROM0[$0B66]
routine_0B66::
    rst rst_script_read_byte
    ld e, a
    and a, $F0
    swap a
    ld d, a
    ld a, e
    and a, $0F
    ld e, a
    ret

SECTION "ROM0_0B72", ROM0[$0B72]
routine_0B72::
    rst rst_script_read_byte
    ld b, $04
    ld hl, $C20F
    ld de, $0010
.loop
    ld c, $08
    call routine_1603
    ret nc
    add hl, de
    dec b
    jr nz, .loop
    call routine_0608
    jr z, .skip
    ld c, $08
    call routine_1603
    ret nc
.skip
    ld hl, $C2B9
    ld c, $10
    call routine_1603
    ret nc
    jr routine_0C05

SECTION "ROM0_0B9B", ROM0[$0B9B]
routine_0B9B::
    ld bc, $0000
.loop
    ld a, c
    ld hl, $C206
    call routine_05D9
    bit 4, [hl]
    jr z, .skip
    inc b
.skip
    inc c
    ld a,c
    cp a,$04
    jr c, .loop

    ld a, b
    cp a, $04
    ret z
    jr routine_0C05

SECTION "ROM0_0BB6", ROM0[$0BB6]
routine_0BB6::
    rst rst_script_read_byte
    ld e, a
    rst rst_script_read_byte
    ld c, a
    and a, $0F
    ld b, a
    ld a, c
    and a, $F0
    swap a
    ld c, a
    call routine_063E
    inc b
    cp c
    jr c, routine_0C05
    cp b
    jr nc, routine_0C05
    ret  

SECTION "ROM0_0BCE", ROM0[$0BCE]
routine_0BCE::
    rst rst_script_read_byte
    ld b, a
    inc b
    ld a, [$C2D9]
    cp b
    ret c
    jr routine_0C05

SECTION "ROM0_0BD8", ROM0[$0BD8]
routine_0BD8::
    rst rst_script_read_byte
    ld hl, $C2DA
    add a
    rst rst_hl_plus_a
    ld a, [hl]
    and a, $0F
    ret nz
    jr routine_0C05

SECTION "ROM0_0BE4", ROM0[$0BE4]
routine_0BE4::
    ld de, $0003
    ld a, [$C763]
    and a
    ret z
    jp routine_0800

SECTION "ROM0_0BEF", ROM0[$0BEF]
routine_0BEF::
    rst rst_script_read_byte
    ld c, a
    ldh a, [$FFB0]
    cp c
    ret z
    jr routine_0C05

SECTION "ROM0_0BF7", ROM0[$0BF7]
routine_0BF7::
    rst rst_script_read_byte
    ld c, a
    ld a, [$C2A1]
    cp c
    ret nc
    jr routine_0C05

SECTION "ROM0_0C00", ROM0[$0C00]
routine_0C00::
    call routine_1477
    and a
    ret z
    ; fallthrough

SECTION "ROM0_0C05", ROM0[$0C05]
routine_0C05::
    call script_load_pointer
    inc de
    inc de
    inc de
    inc de
    jp script_save_pointer

SECTION "ROM0_0C0F", ROM0[$0C0F]
routine_0C0F::
    call banked_call
        DW $5036 ; subroutine address
        DB $01 ; subroutine bank
    call routine_0CA9
    jp reset

SECTION "ROM0_0C1B", ROM0[$0C1B]
routine_0C1B::
    ld a, $FF
    ld [$C354], a
    ret

SECTION "ROM0_0C21", ROM0[$0C21]
routine_0C21::
    call routine_0B50
    jp routine_11A1

SECTION "ROM0_0C27", ROM0[$0C27]
routine_0C27::
    rst rst_script_read_byte
    and a
    jr z, .skipC57
    call enable_save_ram
    dec a
    jr z, .skipC5E
    dec a
    jr z, .skipC63
    dec a
    jr z, .skipC6E
    dec a
    jr z, .skipC7A
    ld bc, $0006
    call routine_0C8C
    xor a
    call routine_05D9
    ld a, [hl]
    call routine_1648
    and a
    jr nz, .skipC86
    ld bc, $0009
    call routine_0C8C
    xor a
    call routine_1204
    jr .skipC89

.skipC57
    ld a, [$C7DD]
    inc a
    jp routine_123F

.skipC5E
    ld bc, $011C
    jr .skipC66

.skipC63
    ld bc, $00D9
.skipC66
    call routine_0C8C
    call routine_12E6
    jr .skipC89

.skipC6E
    ld bc, $0000
    call routine_0C8C
    xor a
    call routine_1027
    jr .skipC89

.skipC7A
    ld bc, $0007
    call routine_0C8C
    xor  a
    call routine_1204
    jr .skipC89

.skipC86
    call routine_11F2    
.skipC89
    jp disable_save_ram

SECTION "ROM0_0C8C", ROM0[$0C8C]
routine_0C8C::
    ld a, [$C7DD]
    call a_times_16
    ld l, a
    ld h, $15
    call u8_mul_u8
    add hl, bc
    ld bc, $A000
    add hl, bc
    ret

SECTION "ROM0_0C9E", ROM0[$0C9E]
routine_0C9E::
    rst rst_script_read_byte
    ld  c, a
    rst rst_script_read_byte
    ld b, a
    call banked_call
        DW $502A ; subroutine address
        DB $01 ; subroutine bank
    ret

SECTION "ROM0_0CA9", ROM0[$0CA9]
routine_0CA9::
    call wait_button_release
.loop
    call transfer_current_oam
    call joy_update
    ldh a, [$FF8A]
    and a
    jr z, .loop
    jp wait_button_release

SECTION "ROM0_0CBA", ROM0[$0CBA]
routine_0CBA::    
    call routine_1909
    jp transfer_current_oam

SECTION "ROM0_0CC0", ROM0[$0CC0]
routine_0CC0::
    rst rst_script_read_byte
    ld b, a
.loop2
    call transfer_current_oam
    call transfer_current_oam
    dec b
    jr nz, .loop2
    ret

SECTION "ROM0_0CCC", ROM0[$0CCC]
routine_0CCC::
    rst rst_script_read_byte
    jp routine_073B

SECTION "ROM0_0CD0", ROM0[$0CD0]
routine_0CD0::
    ld de, $D500
    call routine_0CEF
    ld b, $01
    call routine_1549
    rst rst_script_read_byte
    push af
    ld de, $C7D6
    ld a, [de]
    ld hl, $D400
    rst rst_hl_plus_a
    inc a
    ld [de], a
    pop af
    ld [hl], a
    jp script_handle_opcode

SECTION "ROM0_0CEC", ROM0[$0CEC]
routine_0CEC::
    ld de, $C380
    ; fallthrough
SECTION "ROM0_0CEF", ROM0[$0CEF]
routine_0CEF::
    ld hl, $C7CA
    ld a, [hl]
    inc [hl]
    ld l, a
    ld h, $00
    add hl, hl
    add hl, de
    ld de, $C7A3
    ld a, [de]
    inc a
    ldi [hl], a
    dec de
    dec de
    ld a, [de]
    dec a
    ld [hl], a
    ret  

SECTION "ROM0_0D05", ROM0[$0D05]
routine_0D05::
    rst rst_script_read_byte
    cp a, $05
    jr c, .labelD1B
    cp a, $FF
    jr z, .labelD18
    sub a, $05
    ld hl, $C73D
    rst rst_hl_plus_a
    ld e, $C0
    jr .labelD37
.labelD18
    ld a, [$C709]
.labelD1B
    cp a, $04
    jr nz, .labelD24
    call routine_0608
    jr z, .labelD8A
.labelD24
    push af
    call routine_0D91
    ld hl, $C204
    ldh a,[$FF8B]
    and a
    jr z, .labelD33
    ld hl, $D00A
.labelD33
    pop af
    call routine_05D9
.labelD37
    ld c, [hl]
    ld hl, $C796
    ld a, [hl]
    ld b, a
    inc a
    cp a, $08
    jr c, .labelD43
    xor a
.labelD43
    ld [hl], a
    ld hl, $C7A6
    ld a, b
    add a
    add a
    rst rst_hl_plus_a
    ld a, b
    or  e
    ldi [hl], a
    push hl
    ld a, c
    ld hl, $6B70
    rst rst_hl_plus_a
    ld a, $0D
    call banked_load
    ld hl, $4300
    rst rst_hl_plus_a
    ld a, $01
    call banked_load
    pop hl
    cp a, $01
    jr nz, .labelD69
    or a, $04
.labelD69
    ldi [hl], a
    push hl
    ld hl, $C7A3
    ld d, [hl]
    dec hl
    dec hl
    ld  e, [hl]
    inc [hl]
    inc [hl]
    pop hl
    call d_times_8_e_times_8
    ld [hl], d
    inc hl
    ld [hl], e
    ld l, b
    ld h, $00
    ld de, $8000
    call hl_times_128_plus_de
    ld e, l
    ld d, h
    ld a, c
    call routine_160C
.labelD8A
    call routine_079F
    inc  hl
    inc  hl
    jr routine_0DEA

SECTION "ROM0_0D91", ROM0[$0D91]
routine_0D91::
    call routine_139A
    call routine_05D9
    ld a, [hl]
    call routine_1648
    ld hl, $4240
    rst rst_hl_plus_a
    ld a, $0F
    call banked_load
    ld e, a
    ret

SECTION "ROM0_0DA6", ROM0[$0DA6]
routine_0DA6::
    ld hl, $C7A1
    inc [hl]
    call routine_079F
    inc hl
    jr routine_0DEA

SECTION "ROM0_0DB0", ROM0[$0DB0]
routine_0DB0::
    ld hl, $C7A1
    dec [hl]
    call routine_079F
    dec hl
    jr routine_0DEA

SECTION "ROM0_0DBA", ROM0[$0DBA]
routine_0DBA::
    ld hl, $C7A3
    dec [hl]
    ld a, [$C77E]
    cpl  
    inc a
    ld b, $FF
    jr routine_0DD0

SECTION "ROM0_0DC7", ROM0[$0DC7]
routine_0DC7::
    ld hl, $C7A3
    inc [hl]
    ld a, [$C77E]
    ld b, $00
    ; fallthrough

SECTION "ROM0_0DD0", ROM0[$0DD0]
routine_0DD0::
    ld c, a
    call routine_078D
    add hl, bc
    call routine_0796
    call routine_079F
    add hl, bc
    jr routine_0DEA

SECTION "ROM0_0DDE", ROM0[$0DDE]
routine_0DDE::
    rst rst_script_read_byte
    ld c, a
    ld hl, $C7A1
    add [hl]
    ld [hl], a
    ld a, c
    call routine_079F
    rst rst_hl_plus_a

SECTION "ROM0_0DEA", ROM0[$0DEA]
routine_0DEA::
    jp routine_07AA

SECTION "ROM0_0DED", ROM0[$0DED]
routine_0DED::
    call routine_0DF0
    ; fallthrough
routine_0DF0::
    ld hl, $C780
    ldd a, [hl]
    ld [hl], a
    ldh a, [$FFA0]
    and a
    jr z, .done
    cp a, $04
    jr nc, .done
    call routine_0E30
    ld de, $9D01
    cp a, $03
    jr z, .skip
    ld de, $9CC1
.skip
    call routine_078D
    call u16_cmp
    jr nz, .done
    call routine_08AA
    call routine_078D
    jr routine_0DEA
.done
    call routine_078D
    ld a,[$C77E]
    rst rst_hl_plus_a
    call routine_07AA
    call routine_0796
    ld hl, $C7A3
    inc [hl]
    dec hl
    ldd a, [hl]
    ld [hl], a
    ret

SECTION "ROM0_0E30", ROM0[$0E30]
routine_0E30::
    push af
    push bc
    push de
    push hl
    ld hl, $C764
    ld a, [hl]
    and a
    jr nz, .done
    inc a
    ld [hl], a
    ld hl, $C7DE
    ld [hl], a
    ldh a, [$FF8B]
    and a
    jr z, .skip
    ld [hl], $00
    ld hl, $C7A6
    ld b, $20
    call memclear8
    call routine_0546
.skip
    call transfer_current_oam
    call routine_0E72
.done
    jp pop_hl_de_bc_af

SECTION "ROM0_0E5C", ROM0[$0E5C]
routine_0E5C::
    ld hl,$C764
    ld a,[hl]
    and a
    ret z
    xor a
    ld [hl],a
    ld [$C7DE],a
    rst rst_wait_vblank
    call transfer_map_oam
    ldh a,[$FF40]
    and a,$C3
    ldh [$FF40],a
    ret  

SECTION "ROM0_0E72", ROM0[$0E72]
routine_0E72::
    call routine_0E9C
    ld hl, $9C41
    call routine_07AA
    call routine_0796
    ld e, $50
    ld bc, $1408
    ldh a, [$FF8B]
    and a
    jr z, .skip
    ld e, $40
    ld bc, $140A
.skip
    ld hl, $C7A5
    ld [hl], e
    push bc
    call routine_0A2B
    pop bc
    ld de, $9C00
    jp routine_0AC2

SECTION "ROM0_0E9C", ROM0[$0E9C]
routine_0E9C::
    ld a, $12
    ld hl, $C77F
    ldi [hl], a
    ld [hl], a
    ret

SECTION "ROM0_0EA4", ROM0[$0EA4]
routine_0EA4::
    ld de, $7AEB
    jr routine_0EBB

SECTION "ROM0_0EA9", ROM0[$0EA9]
routine_0EA9::
    ld de, $7AE8
    jr routine_0EBB

SECTION "ROM0_0EAE", ROM0[$0EAE]
routine_0EAE::
    ld de, $7AE5
    jr routine_0EBB

SECTION "ROM0_0EB3", ROM0[$0EB3]
routine_0EB3::
    ld de, $7AE2
    jr routine_0EBB

SECTION "ROM0_0EB8", ROM0[$0EB8]
routine_0EB8::
    ld de, $7AD5
    ; fallthrough

SECTION "ROM0_0EBB", ROM0[$0EBB]
routine_0EBB::
    ld a,$0F
    rst rst_bank_switch
    push af
    call routine_155A
    pop af
    rst rst_bank_switch
    ret

SECTION "ROM0_0EC5", ROM0[$0EC5]
routine_0EC5::
    call routine_0E5C
    ldh a,[$FF88]
    push af
    rst rst_script_read_byte
    ld [$C7F3], a
    cp a, $FF
    jr nz, .else
    call routine_191E
    jr .done
.else
    ld c, a
    call routine_190F
.done
    pop af
    rst rst_bank_switch
.loop
    xor a
    ld [$C764], a
    ld [$C763], a
    ld [$C7DE], a
    cpl  
    ldh [$FF8B], a
    call routine_14E3
    call banked_call
        DW $4000 ; subroutine address
        DB $0D ; subroutine bank
    call routine_151C
    ld a, [$C763]
    and a
    ret z
    ld e, $0B
    call routine_063E
    and a
    jp nz, reset
    ld de, $0003
    rst rst_call_800
    call routine_1477
    and a
    jp nz, reset
    ld hl, $C2A1
    ld a, [hl]
    inc a
    jr z, .loop
    ld [hl], a
    jr .loop

SECTION "ROM0_0F1A", ROM0[$0F1A]
routine_0F1A::
    ld a, $01
    rst rst_bank_switch
    push af
    call $5021
    ld a, $E4
    ld [$C2A0], a
    call $5024
    call routine_1921
    pop af
    rst rst_bank_switch
    ret  

SECTION "ROM0_0F2F", ROM0[$0F2F]
routine_0F2F::
    ld a, $04
    ld [$C709], a
    ld hl, $C340
    add a
    rst rst_hl_plus_a
    xor a
    ldi [hl], a
    ld [hl], a
    ld e, $00
    call routine_063E
    push af
    add a, $E0
    call routine_055D
    pop af
    add a
    add a
    ld hl, $6610
    rst rst_hl_plus_a
    ld de, $C280
    ld a, $0F
    ld b, $04
    jp banked_memcpy8

SECTION "ROM0_0F58", ROM0[$0F58]
routine_0F58::
    call banked_call
        DW $501E ; subroutine address
        DB $01 ; subroutine bank
    ret

routine_0F5F::
    rst rst_script_read_byte
    ld c, a
    rst rst_script_read_byte
    ld b, a
    call banked_call
        DW $5027 ; subroutine address
        DB $01 ; subroutine bank
    ret

routine_0F6A::
    ld hl, $C70A
    rst rst_script_read_byte
    ldi [hl], a
    rst rst_script_read_byte
    ldi [hl], a
    ld [hl], $00
    ld de, $C2A2
    ld hl, $C70A
    call u24_cmp
    jp nc, u24_sub
    ld l, e
    ld h, d
    xor a
    ldi [hl], a
    ldi [hl], a
    ld [hl], a
    ret  

SECTION "ROM0_0F86", ROM0[$0F86]
character_creator_event::
    call routine_0E5C
    call routine_14D5
    call banked_call
        DW $5006 ; subroutine address
        DB $01 ; subroutine bank
    ldh a, [$FF88]
    push af
    call routine_1915
    pop af
    rst rst_bank_switch
    jp routine_150A

SECTION "ROM0_0F9D", ROM0[$0F9D]
routine_0F9D::
    rst rst_script_read_byte
    ld e, a
    push de
    call routine_14CA
    pop de
    rst rst_call_908
.loop
    call routine_18BC
    cp a, $FF
    jr z, .loop
    jr routine_0FC9

SECTION "ROM0_0FAE", ROM0[$0FAE]
routine_0FAE::
    call routine_14CA
    ld e, $45
    rst rst_call_908
    call routine_18BC
    cp a, $FF
    jr z, routine_0FC9
    ld hl, $7AEE
    rst rst_hl_plus_a
    ld a, $0F
    call banked_load
    ldh [$FFB0], a
    ld [$C31A], a
SECTION "ROM0_0FC9", ROM0[$0FC9]
routine_0FC9::
    jp routine_14FF

SECTION "ROM0_0FCC", ROM0[$0FCC]
routine_0FCC::
    inc a
    ld [$C77B],a
    rst rst_script_read_byte
    jp script_handle_instruction_opcode

SECTION "ROM0_0FD4", ROM0[$0FD4]
routine_0FD4::
    ld e, $18
    call routine_063E
    ld c, a
    rst rst_script_read_byte
    cp c
    jr z, .skip
    jp nc, routine_11BA
.skip
    ld l, a
    ld h, $00
    ld de, $76C0
    call hl_times_16_plus_de
    ld b,$10
    call routine_153E
    jp routine_0DED

SECTION "ROM0_0FF2", ROM0[$0FF2]
routine_0FF2::
    ld hl, $C200
    ldh a, [$FF8B]
    and a
    jr z, .skip
    ld hl, $D002
.skip
    rst rst_script_read_byte
    cp a, $05
    jr c, routine_1027
    cp a, $0A
    jr c, routine_101C
    cp a, $0D
    jr nc, routine_1024
    sub a, $0A
    add a
    ld hl, $D906
    rst rst_hl_plus_a
    ld a, [hl]
    ld hl, $D002
    call routine_05D9
    ld b, $08
    jr routine_102C

SECTION "ROM0_101C", ROM0[$101C]
routine_101C::
    sub a, $05
    call a_times_32
    rst rst_hl_plus_a
    jr routine_102A

SECTION "ROM0_1024", ROM0[$1024]
routine_1024::
    ld a, [$C709]
    ; fallthrough
SECTION "ROM0_1027", ROM0[$1027]
routine_1027::
    call routine_05D9
    ; fallthrough
SECTION "ROM0_102A", ROM0[$102A]
routine_102A::
    ld b, $04
    ; fallthrough
SECTION "ROM0_102C", ROM0[$102C]
routine_102C::
    call routine_1598
    jp routine_1557

SECTION "ROM0_1032", ROM0[$1032]
routine_1032::
    rst rst_script_read_byte
    cp a, $05
    jr c, routine_105C
    cp a, $0D
    jr c, routine_1051
    cp a, $10
    jr c, routine_106A
    cp a, $10
    jr z, routine_1059
    cp a, $14
    jr c, routine_1073
    rst rst_script_read_byte
    ld l, a
    ld b, $08
    ld de, $6EC0
    jp routine_1539

SECTION "ROM0_1051", ROM0[$1051]
routine_1051::
    sub a, $05
    ld hl, $C73D
    rst rst_hl_plus_a
    jr routine_1062

SECTION "ROM0_1059", ROM0[$1059]
routine_1059::
    ld a, [$C709]
    ; fallthrough
SECTION "ROM0_105C", ROM0[$105C]
routine_105C::
    ld hl, $C204
    call routine_05D9
    ; fallthrough
SECTION "ROM0_1062", ROM0[$1062]
routine_1062::
    ld b, $08
    ld de, $6EC0
    jp routine_1538

SECTION "ROM0_106A", ROM0[$106A]
routine_106A::
    sub a, $0D
    add a
    ld  hl, $D906
    rst rst_hl_plus_a
    jr routine_1062

SECTION "ROM0_1073", ROM0[$1073]
routine_1073::
    sub a, $11
    ld hl, $D500
    add h
    ld h, a
    ld b, $08
    ld a, [hl]
    and a
    jp z, routine_1549
    ld a, $0A
    rst rst_hl_plus_a
    jr routine_1062

SECTION "ROM0_1086", ROM0[$1086]
routine_1086::
    rst rst_script_read_byte
    cp a, $10
    jr c, routine_1098
    cp a, $30
    jr nc, routine_109E
    sub a, $20
    ld hl, $C71D
    add a
    rst rst_hl_plus_a
    jr routine_10A4

SECTION "ROM0_1098", ROM0[$1098]
routine_1098::
    ld hl, $C7E0
    rst rst_hl_plus_a
    jr routine_10A4

SECTION "ROM0_109E", ROM0[$109E]
routine_109E::
    ld a, [$C709]
    call routine_10B3
    ; fallthrough
SECTION "ROM0_10A4", ROM0[$10A4]
routine_10A4::
    ld b, $08
    ld a, [hl]
    cp a, $FF
    jp z, routine_1549
    ld l, a
    ld de, $6E40
    jp routine_1539

SECTION "ROM0_10B3", ROM0[$10B3]
routine_10B3::
    call a_times_32
    ld hl, $C21F
    rst rst_hl_plus_a
    ret

SECTION "ROM0_10BB", ROM0[$10BB]
routine_10BB::
    ld hl, $C20F
    ld de, $0020
    rst rst_script_read_byte
    cp a, $10
    jp c, routine_1141
    cp a, $20
    jp c, routine_113A
    cp a, $28
    jr c, routine_1110
    cp a, $30
    jr c, routine_110D
    cp a, $38
    jr c, routine_110A
    cp a, $40
    jr c, routine_1107
    cp a, $48
    jr c, routine_1104
    cp a, $51
    jr c, routine_1115
    cp a, $63
    jr nc, routine_10FA
    sub a, $60
    ld hl, $D906
    add a
    rst rst_hl_plus_a
    ldi a, [hl]
    ld h, [hl]
    ld l, a
    ld b, $08
    ld de, $6640
    jp routine_153B

SECTION "ROM0_10FA", ROM0[$10FA]
routine_10FA::
    rst rst_script_read_byte
    ld b, $08
    ld l, a
    ld de, $6640
    jp routine_1539

SECTION "ROM0_1104", ROM0[$1104]
routine_1104::
    sub a, $08
    add hl, de
    ; fallthrough
SECTION "ROM0_1107", ROM0[$1107]
routine_1107::
    sub a, $08
    add hl, de
    ; fallthrough
SECTION "ROM0_110A", ROM0[$110A]
routine_110A::
    sub a, $08
    add hl, de
    ; fallthrough
SECTION "ROM0_110D", ROM0[$110D]
routine_110D::
    sub a, $08
    add hl, de
    ; fallthrough
SECTION "ROM0_1110", ROM0[$1110]
routine_1110::
    sub a, $20
    add hl, de
    jr routine_1144

SECTION "ROM0_1115", ROM0[$1115]
routine_1115::
    sub a, $48
    ld  b, a
    ldh a, [$FF8B]
    and a
    jr z, routine_112E
    call routine_1622
    ld b, $08
    jp nc, routine_1549
    ldi a, [hl]
    ld h, [hl]
    ld l, a
    ld de, $6640
    jp routine_153B

routine_112E::
    ld a, [$C709]
    ld hl ,$C20F
    call routine_05D9
    ld a, b
    jr routine_1144

SECTION "ROM0_113A", ROM0[$113A]
routine_113A::
    sub a, $10
    ld hl, $C2B9
    jr routine_1144

SECTION "ROM0_1141", ROM0[$1141]
routine_1141::
    ld hl, $C71D
    ; fallthrough

SECTION "ROM0_1144", ROM0[$1144]
routine_1144::
    add a
    ld de, $6640
    rst rst_hl_plus_a
    ld b, $08
    ld a, [hl]
    inc a
    jp z, routine_1549
    jp routine_1538

SECTION "ROM0_1153", ROM0[$1153]
routine_1153::
    rst rst_script_read_byte
    cp a, $10
    jr c, .skip
    ld a, [$C7D9]
.skip

    ld d, a
    ld e, $FF
.loop
    inc e
    ld a, e
    cp a, $10
    jr nc, routine_11BA
    call routine_066E
    jr z, .loop

    ld hl, $C7DA
    ld a, [hl]
    inc [hl]
    ld hl, $C71D
    rst rst_hl_plus_a
    ld [hl],d
    push de
    ld b, $01
    call routine_1549
    pop  de
    ld hl, $4250
    ld a, d
    call routine_11AB
    jp routine_0DED

SECTION "ROM0_1184", ROM0[$1184]
routine_1184::
    ld a, [$C7D9]
    ld d, a
    ld a, [$C7F2]
    ld e, a
    call routine_066E
    ret z
    call routine_0E9C
    ld hl, $FFA0
    ld a, [hl]
    push af
    ld [hl], $05
    call routine_11A1
    pop af
    ldh [$FFA0], a
    ret

SECTION "ROM0_11A1", ROM0[$11A1]
routine_11A1::
    ld l, d
    ld h, $00
    ld bc, $4270
    call hl_times_32_plus_bc
    ld a, e
    ; fallthrough

SECTION "ROM0_11AB", ROM0[$11AB]
routine_11AB::    
    add a
    rst rst_hl_plus_a
    ld a, $0F
    rst rst_bank_switch
    push af
    ld e, [hl]
    inc hl
    ld d, [hl]
    call routine_155A
    pop af
    rst rst_bank_switch
    ret

SECTION "ROM0_11BA", ROM0[$11BA]
routine_11BA::
    ld de, $C380
    ld hl, $C7CA
    dec [hl]
    ld l, [hl]
    ld h, $00
    add hl, hl
    add hl, de
    ld a, $FF
    ldi [hl], a
    ld [hl], a
    ret  

SECTION "ROM0_11CB", ROM0[$11CB]
routine_11CB::
    ld hl, $C207
    ldh a, [$FF8B]
    and a
    jr z, .skip
    ld hl, $D041
.skip
    jr routine_1201

SECTION "ROM0_11D8", ROM0[$11D8]
routine_11D8::    
    call routine_139A
    call routine_13B6
    ld c, a
    call routine_05D9
    ld a, [hl]
    call routine_1648
    and a
    jr nz, routine_11F2
    call routine_0DA6
    call routine_13A8
    ld a, c
    jr routine_1204

SECTION "ROM0_11F2", ROM0[$11F2]
routine_11F2::
    dec a
    add a
    add a
    ld hl, $7840
    rst rst_hl_plus_a
    ld b, $04
    jp routine_153E

SECTION "ROM0_11FE", ROM0[$11FE]
routine_11FE::
    call routine_13A8
    ; fallthrough
routine_1201::
    call routine_13B6
    ; fallthrough    
routine_1204::
    call routine_05D9
    ldi a, [hl]
    ld h, [hl]
    ld l, a
    ld de, $03E8
    call u16_cmp
    jr c, .skip
    ld hl, $03E7
.skip
    call routine_15B5
    ld hl, $C787
    jp routine_157A

SECTION "ROM0_121E", ROM0[$121E]
routine_121E::
    rst rst_script_read_byte
    cp a, $10
    jr c, .else
    ld a, [$C709]
    call routine_10B3
    jr .done
.else
    ld hl, $C7E0
    rst rst_hl_plus_a
.done
    ld b, $01
    ld a, [hl]
    cp a, $FF
    jp z, routine_1549
    add a
    ld hl, $C2DA
    rst rst_hl_plus_a
    ld a, [hl]
    and a, $0F
    ; fallthrough
SECTION "ROM0_123F", ROM0[$123F]
routine_123F::
    ld l, a
    ld h, $00
    call routine_15B5
    ld hl, $C789
    jp routine_157A

SECTION "ROM0_124B", ROM0[$124B]
routine_124B::
    ld hl, $0000
    ld de, $0020
    rst rst_script_read_byte
    cp a, $10
    jr c, routine_12AB
    cp a, $20
    jr c, routine_12A4
    cp a, $28
    jr c, routine_129C
    cp a, $30
    jr c, routine_1299
    cp a, $38
    jr c, routine_1296
    cp a, $40
    jr c, routine_1293
    cp a, $48
    jr c, routine_1290
    sub a, $48
    ld b, a
    ldh a, [$FF8B]
    and a
    ld a, b
    jr z, routine_1283
    call routine_1622
    ld b,$02
    jp nc, routine_1549
    inc hl
    inc hl
    jr routine_12B7

SECTION "ROM0_1283", ROM0[$1283]
routine_1283::
    ld hl, $C20F
    add a
    rst  rst_hl_plus_a
    ld  a, [$C709]
    call routine_05D9
    jr routine_12B0
SECTION "ROM0_1290", ROM0[$1290]
routine_1290::
    sub a, $08
    add hl, de
    ; fallthrough
SECTION "ROM0_1293", ROM0[$1293]
routine_1293::
    sub a, $08
    add hl, de
    ; fallthrough
SECTION "ROM0_1296", ROM0[$1296]
routine_1296::
    sub a, $08
    add hl, de
    ; fallthrough
SECTION "ROM0_1299", ROM0[$1299]
routine_1299::
    sub a, $08
    add hl, de
    ; fallthrough
SECTION "ROM0_129C", ROM0[$129C]
routine_129C::
    ld de, $C20F
    sub a, $20
    add hl, de
    jr routine_12AE

SECTION "ROM0_12A4", ROM0[$12A4]
routine_12A4::
    sub a, $10
    ld hl, $C2B9
    jr routine_12AE

SECTION "ROM0_12AB", ROM0[$12AB]
routine_12AB::
    ld hl, $C71D
    ; fallthrough
SECTION "ROM0_12AE", ROM0[$12AE]
routine_12AE::
    add a
    rst rst_hl_plus_a
    ; fallthrough
SECTION "ROM0_12B0", ROM0[$12B0]
routine_12B0::
    ld b, $02
    ldi a, [hl]
    inc a
    jp z, routine_1549
    ; fallthrough
SECTION "ROM0_12B7", ROM0[$12B7]
routine_12B7::
    ld a, [hl]
    cp a, $FE
    jr nz, routine_12E6
    ld de, $7AD2
    jp routine_0EBB

SECTION "ROM0_12C2", ROM0[$12C2]
routine_12C2::
    ld hl, $C20B
    jr routine_12D4
SECTION "ROM0_12C7", ROM0[$12C7]
routine_12C7::
    ld hl, $C20E
    jr routine_12D4
SECTION "ROM0_12CC", ROM0[$12CC]
routine_12CC::
    ld hl,$C20C
    jr routine_12D4

SECTION "ROM0_12D1", ROM0[$12D1]
routine_12D1::
    ld hl, $C20D
SECTION "ROM0_12D4", ROM0[$12D4]
routine_12D4::
    call routine_13B6
    call routine_05D9
    jr routine_12E6

SECTION "ROM0_12DC", ROM0[$12DC]
routine_12DC::
    ld hl, $C2D9
    jr routine_12E6

SECTION "ROM0_12E1", ROM0[$12E1]
routine_12E1::
    rst rst_script_read_byte
    ld hl, $C70D
    rst rst_hl_plus_a
    ; fallthrough
SECTION "ROM0_12E6", ROM0[$12E6]
routine_12E6::
    ld a, [hl]
    cp a, $64
    jr c, .skip
    ld a, $63
.skip
    ld l, a
    ld h, $00
    call routine_15B5
    ld hl, $C788
    jp routine_157A

SECTION "ROM0_12F9", ROM0[$12F9]
routine_12F9::
    call routine_1313
    ld hl, $C785
    rst rst_script_read_byte
    add a, $02
    ld b, a
    ld a, $08
    sub b
    rst rst_hl_plus_a
    jp routine_157A


SECTION "ROM0_130A", ROM0[$130A]
routine_130A::
    call routine_1313
    ld hl, $C785
    jp routine_157A

SECTION "ROM0_1313", ROM0[$1313]
routine_1313::
    ld hl, $C2A2
    rst rst_script_read_byte
    and a
    jr z, .skip
    dec a
    ld c, a
    add a
    add c
    ld hl, $C745
    rst rst_hl_plus_a
.skip

    ldi a, [hl]
    and [hl]
    inc hl
    and [hl]
    dec hl
    dec hl
    inc a
    jr nz, routine_1334
    dec a
    ld b, $08
    call memset8
    xor a
    ld [hl], a
    ret  

SECTION "ROM0_1334", ROM0[$1334]
routine_1334::
    ld de, $1469
    call u24_cmp
    jr nc, .skip
    ld hl, table_1469
.skip
    ld de, $C70A
    ld b, $03
    push de
    call memcpy8
    pop de
    ld bc, $C785
    ld hl, table_145A
    ld a, $05
.outer_loop
    ldh [$FF92],a
    xor a
.inner_loop
    call u24_sub
    inc a
    jr nc, .inner_loop
    dec a
    call u24_add
    ld [bc], a
    inc bc
    inc hl
    inc hl
    inc hl
    ldh a, [$FF92]
    dec a
    jr nz, .outer_loop
    ld a, [de]
    ld [bc], a
    ld b, $06
    call routine_15BA
    ld [hl], $C0
    inc hl
    ld [hl], $C9
    inc hl
    ld [hl], $00
    ret  

routine_1378::
    ld hl, $D900
    rst rst_script_read_byte
    add a
    rst rst_hl_plus_a
    ldi a, [hl]
    ld  h, [hl]
    ld l, a
    call routine_15B5
    ld hl, $C785
    jp routine_157A

routine_138A::
    rst rst_script_read_byte
    ld b, $02
    ld hl, $D500
    add h
    ld h, a
    ldi a, [hl]
    and a
    jp z, routine_1549
    jp routine_12E6

routine_139A::
    push af
    ld hl, $C206
    ldh a,[$FF8B]
    and a
    jr z, .skip
    ld hl, $D040
.skip
    pop  af
    ret

routine_13A8::
    push af
    ld hl,$C209
    ldh a,[$FF8B]
    and a
    jr z, .skip
    ld hl, $D00C
.skip
    pop af
    ret

routine_13B6::
    rst rst_script_read_byte
    cp a, $05
    ret c
    ld a, [$C709]
    ret

SECTION "ROM0_13BE", ROM0[$13BE]
script_instruction_jump_table::
    DW routine_0B0F
    DW routine_0DA6
    DW routine_0DB0
    DW routine_0DBA
    DW routine_0DC7
    DW routine_0DF0
    DW routine_0DED
    DW routine_0CCC
    DW routine_0D05
    DW routine_0EC5
    DW routine_0EB8
    DW routine_0CA9
    DW routine_0E30
    DW routine_0E5C
    DW routine_0B0F
    DW routine_0E72
    DW routine_0CBA
    DW routine_0CC0
    DW routine_0B32
    DW routine_0B3C
    DW routine_0B4B
    DW routine_0BB6
    DW routine_0C00
    DW routine_0B72
    DW routine_0B50
    DW routine_1490
    DW routine_0BCE
    DW routine_0BD8
    DW routine_1086
    DW routine_121E
    DW routine_130A
    DW routine_0FF2
    DW routine_0FCC
    DW routine_1032
    DW routine_10BB
    DW routine_124B
    DW routine_11CB
    DW routine_11FE
    DW routine_12E1
    DW routine_12C2
    DW routine_12C7
    DW routine_12CC
    DW routine_12D1
    DW routine_124B
    DW routine_12DC
    DW routine_0FD4
    DW routine_0CEC
    DW routine_0EB3
    DW routine_1378
    DW ret_
    DW routine_0F9D
    DW routine_0F6A
    DW character_creator_event
    DW routine_12F9
    DW routine_0DDE
    DW routine_0BE4
    DW routine_0F2F
    DW routine_0CD0
    DW routine_138A
    DW routine_11D8
    DW routine_0EAE
    DW routine_0EA9
    DW routine_0F58
    DW routine_0F5F
    DW routine_0B5B
    DW routine_1153
    DW routine_1184
    DW routine_0B9B
    DW routine_0BEF
    DW routine_0BF7
    DW routine_0F1A
    DW routine_0FAE
    DW routine_0C9E
    DW routine_0C27
    DW routine_146C
    DW routine_0C21
    DW routine_0C1B
    DW routine_0C0F

SECTION "ROM0_145A", ROM0[$145A]
table_145A::
    DB $A0, $86, $01, $10, $27, $00, $E8, $03, $00, $64, $00, $00, $0A, $00, $00

SECTION "ROM0_1469", ROM0[$1469]
table_1469::
    DB $3F, $42, $0F

SECTION "ROM0_146C", ROM0[$146C]
routine_146C::
    ld a, $03
    ld [$C765], a
    ld a, $04
    ld [$C31B], a
    ret

SECTION "ROM0_1477", ROM0[$1477]
routine_1477::
    call routine_14CA
    call wait_button_release
    ld e, $12
    rst rst_call_908
.loop
    call routine_18BC
    cp a, $FF
    jr z, .loop
    call wait_button_release
    push af
    call routine_14FF
    pop af
    ret  

SECTION "ROM0_1490", ROM0[$1490]
routine_1490::
    ld hl, $FFA2
    ldi a, [hl]
    ld h, [hl]
    ld l, a
    rst rst_script_read_byte
    ld d, a
    rst rst_script_read_byte
    ld e, a
    ld a, d
    cp a, $04
    jp c, routine_0800
    cp a, $04
    jr z, routine_14AC
    call routine_1906
    ld e, l
    ld d, h
    jp script_save_pointer

SECTION "ROM0_14AC", ROM0[$14AC]
routine_14AC::
    push af
    push bc
    push de
    push hl
    push de
    call routine_0E5C
    call routine_14D5
    pop de
    ld a, $01
    rst rst_bank_switch
    push af
    call $5009
    call routine_191B
    pop af
    rst rst_bank_switch
    call routine_150A
    jp pop_hl_de_bc_af

SECTION "ROM0_14CA", ROM0[$14CA]
routine_14CA::
    ld b, $00
    ld hl, $9C00
    ld de, $9D00
    call vramcpy8
    ; fallthrough
SECTION "ROM0_14D5", ROM0[$14D5]
routine_14D5::
    xor a
    ld [$C798],a
    call routine_0546
    call routine_0533
    ld a, $02
    ldh [$FF96], a
    ; fallthrough
SECTION "ROM0_14E3", ROM0[$14E3]
routine_14E3::
    ld hl, $C779
    ld e, [hl]
    inc hl
    ld d, [hl]
    push hl
    ld hl, $FFA0
    ld b,$04
    call memcpy8
    ld hl, $C77E
    ld b, $07
    call memcpy8
    pop hl
    ld [hl], d
    dec hl
    ld  [hl], e
    ret

SECTION "ROM0_14FF", ROM0[$14FF]
routine_14FF::
    ld b, $00
    ld hl, $9D00
    ld de, $9C00
    call vramcpy8
    ; fallthrough
SECTION "ROM0_150A", ROM0[$150A]
routine_150A::
    xor a
    ldh [$FF96], a
    call routine_0546
    ld hl, $CC80
    ld b, $20
    call memclear8
    rst rst_wait_vblank
    ld a, $CC
    rst rst_oam_dma_transfer
    ; fallthrough
SECTION "ROM0_151C", ROM0[$151C]
routine_151C::
    ld hl, $C779
    ld e, [hl]
    inc hl
    ld d, [hl]
    push hl
    ld hl, $C784
    ld b, $07
    call routine_08A3
    ld hl, $FFA3
    ld b, $04
    call routine_08A3
    pop hl
    ld [hl], d
    dec hl
    ld [hl], e
    ret  

SECTION "ROM0_1538", ROM0[$1538]
routine_1538::
    ld l, [hl]
    ; fallthrough
SECTION "ROM0_1539", ROM0[$1539]
routine_1539::
    ld h, $00
    ; fallthrough
SECTION "ROM0_153B", ROM0[$153B]
routine_153B::    
    call hl_times_8_plus_de
    ; fallthrough
SECTION "ROM0_153E", ROM0[$153E]
routine_153E::    
    ld a, $0F
    rst rst_bank_switch
    push af
    call routine_1598
    pop af
    rst rst_bank_switch
    jr routine_1557

SECTION "ROM0_1549", ROM0[$1549]
routine_1549::
    ld a, [$C77B]
    and a
    ret nz
    dec a
    ld hl,$C785
    call memset8
    xor a
    ld [hl], a
    ; fallthrough
SECTION "ROM0_1557", ROM0[$1557]
routine_1557::
    ld de, $C785
    ; fallthrough
SECTION "ROM0_155A", ROM0[$155A]
routine_155A::
    ld hl, $FFA2
    ld c, [hl]
    inc hl
    ld b, [hl]
    ld [hl], d
    dec hl
    ld [hl], e
    push bc
    push hl
    ld hl, $C77C
    ld a, [hl]
    ld [hl], $01
    push af
.loop
    call script_execute_step
    jr .loop

routine_1571::
    pop af
    ld [$C77C], a
    pop hl
    pop de
    ; fallthrough
routine_1577::
    jp script_save_pointer

routine_157A::
    ld de, $C785
    ld a, [$C77B]
    and a
    jr nz, .else
.loop
    ldi a,[hl]
    ld [de],a
    inc de
    and a
    jr nz, .loop
    jr .done
.else
.loop2
    ldi a,[hl]
    cp a, $FF
    jr z, .loop2
    ld [de], a
    inc de
    and a
    jr nz, .loop2
.done
    jp routine_1557

SECTION "ROM0_1598", ROM0[$1598]
routine_1598::
    ld de, $C785
    ld a, [$C77B]
    and a
    jr nz, routine_15A6
    call memcpy8
    jr routine_15B2

SECTION "ROM0_15A6", ROM0[$15A6]
routine_15A6::
    ld c, e
.loop
    ldi a, [hl]
    ld [de], a
    inc de
    inc a
    jr z, .skip
    ld c, e
.skip
    dec b
    jr nz, .loop
    ld e, c
    ; fallthrough
SECTION "ROM0_15B2", ROM0[$15B2]
routine_15B2::
    xor a
    ld [de], a
    ret

SECTION "ROM0_15B5", ROM0[$15B5]
routine_15B5::
    call routine_15D9
    ld b, $05
    ; fallthrough
SECTION "ROM0_15BA", ROM0[$15BA]
routine_15BA::
    ld hl, $C785
    ld c, $00
.loop
    ld a, [hl]
    and a
    jr nz, .else
    inc c
    dec c
    jr nz, .else
    dec b
    jr nz, .skip
    ld a, $B1
.skip
    inc b
    dec a
    jr .done
.else
    inc c
    add a, $B0
.done
    ldi [hl], a
    dec b
    jr nz, .loop
    ld [hl], b
    ret  

SECTION "ROM0_15D9", ROM0[$15D9]
routine_15D9::
    ld bc, $C785
    ld de, $2710
    call routine_15F7
    ld de, $03E8
    call routine_15F7
    ld de, $0064
    call routine_15F7
    ld de, $000A
    call routine_15F7
    ld a, l
    ld [bc], a
    ret

SECTION "ROM0_15F7", ROM0[$15F7]
routine_15F7::
    xor a
.loop
    call u16_sub
    inc a
    jr nc, .loop
    dec a
    add hl, de
    ld [bc], a
    inc bc
    ret

SECTION "ROM0_1603", ROM0[$1603]
routine_1603::
.loop
    cp [hl]
    ret z
    inc hl
    inc hl
    dec c
    jr nz, .loop
    scf  
    ret  

SECTION "ROM0_160C", ROM0[$160C]
routine_160C::
    ld hl, $6B70
    rst rst_hl_plus_a
    ld a, $0D
    call banked_load
    ld h, $40
    add h
    ld h, a
    ld l, $00
    ld a, $03
    ld b, $80
    jp banked_vramcpy8

SECTION "ROM0_1622", ROM0[$1622]
routine_1622::
    ld hl,$D012
    ld a,[$C709]
    call routine_05D9
    ld a, b
    add a
    add b
    rst rst_hl_plus_a
    ld a, [hl]
    cp a, $FF
    ret z
    push hl
    inc hl
    ld h, [hl]
    ld l, a
    ld de, $6F80
    call hl_times_8_plus_de
    ld a, $0C
    call banked_load
    pop hl
    and a, $01
    ret z
    scf  
    ret

SECTION "ROM0_1648", ROM0[$1648]
routine_1648::
    push bc
    push de
    push hl
    ld b, a
    ld a, $0F
    rst rst_bank_switch
    push af
    ld hl, $4238
    ld d, $08
.loop
    ld c, [hl]
    inc hl
    ld e, c
    inc e
    ld a, b
    call test_bit
    jr nz, .found
    dec d
    jr nz, .loop
.not_found
    ld e, $00
.found
    pop af
    rst rst_bank_switch
    ld a, e
    pop hl
    pop de
    pop bc
    ret

SECTION "ROM0_166B", ROM0[$166B]
routine_166B::
    push af
.loop
    ldh a,[$FF44]
    cp a, $90
    jr nz, .loop
    pop af
    ret  

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
    and b
    cpl  
    ldi [hl], a
    inc hl
    inc hl
    ldd a, [hl]
    or [hl]
    dec hl
    and [hl]
    dec hl
    and [hl]
    ldh [joy_held_mask], a
    ld a, $30
    ld [$ff00+c], a
.check_soft_reset
    ldh a, [joy_held_mask]
    and a, $0F
    cp a, $0F
    jp z, $0200
.done_check_soft_reset
    ldh a, [$FF96]
    and a
    call nz, routine_176C
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

SECTION "ROM0_176C", ROM0[$176C]
routine_176C::
    ld  a, $0F
    call bank_switch
    push af
    ld hl, $C7C6
    inc [hl]
    ld a, [hl]
    and a, $10
    jr z, .skip1783
    xor a
    ldi [hl], a
    ld a, [hl]
    and a ,$01
    xor a, $01
    ld [hl], a
.skip1783
    ldh a, [$FF96]
    bit 0, a
    jp z, .done182A
    ld hl,$CC00
    ld b, $80
    call memclear8
    ld hl, $C7A6
    ld b, $08
.loop1797
    push bc
    ld a, [hl]
    rla  
    jp nc, .done1821
    push hl
    ldi a, [hl]
    ldh [$FF95], a
    ldi a, [hl]
    ldh [$FF94], a
    ldh a, [$FF95]
    and a, $07
    add a
    add a
    add a
    add a
    ld d, [hl]
    inc hl
    ld e, [hl]
    ld l, a
    ld h, $CC
    ldh a, [$FF95]
    bit 5, a
    jr z, .skip17BE
    ld a, [$C7C7]
    and a
    jr z, .done1820
.skip17BE
    ldh a, [$FF95]
    bit 6, a
    jr z, .skip17CF
    ldh a, [$FF94]
    bit 2, a
    jr nz, .skip17CF
    ld a, [$C7C7]
    add e
    ld e, a
.skip17CF
    push hl
    call routine_1869
    ld hl, $4200
    ldh a, [$FF95]
    bit 6, a
    jr z, .skip17E5
    ld a, [$C7C7]
    and a
    jr z, .skip17E5
    ld hl, $4218
.skip17E5
    ldh a, [$FF95]
    bit 4, a
    jr z, .skip17EE
    ld a, $0C
    rst rst_hl_plus_a
.skip17EE
    ldh a, [$FF94]
    and a, $03
    add a
    add a
    rst rst_hl_plus_a
    ld e, l
    ld d, h
    pop hl
    ldh a, [$FF95]
    and a, $07
    add a
    add a
    add a
    ldh [$FF94], a
    ldh a, [$FF95]
    and a, $08
    rlca 
    ldh [$FF93],a
    ld b, $04
.loop180A
    inc hl
    inc hl
    ldh a, [$FF94]
    ld c, a
    ld a, [de]
    and a, $0F
    add c
    ldi [hl], a
    ldh a, [$FF93]
    ld c, a
    ld a, [de]
    inc de
    and a, $F0
    or c
    ldi [hl], a
    dec b
    jr nz, .loop180A
.done1820
    pop hl
.done1821
    inc hl
    inc hl
    inc hl
    inc hl
    pop bc
    dec b
    jp nz, .loop1797
.done182A
    ld hl, $CC90
    xor a
    ldh [$FF95], a
    ld c, $97
    add a
    add c
    ld c, a
    ld a, [$ff00+c]
    ld d, a
    inc c
    ld a, [$ff00+c]
    ld e, a
    inc a
    jr nz, .else1844
    ld b, $10
    call memclear8
    jr .done185A
.else1844
    call d_times_8_e_times_8
    push hl
    call routine_1869
    pop hl
    ld b, $04
    ld a, $78
.loop1850
    inc hl
    inc hl
    ldi [hl], a
    inc a
    ld [hl], $00
    inc hl
    dec b
    jr nz, .loop1850
.done185A::
    ld hl, $CC80
    ldh a, [$FF95]
    inc a
    cp a, $02
    jr c ,$182E
    pop af
    call bank_switch
    ret

routine_1869::
    push af
    push bc
    ld a, $04
    ld bc, $4230
.loop
    ldh [$FF93], a
    ld a, [bc]
    inc bc
    add d
    ldi [hl], a
    ld a, [bc]
    inc bc
    add e
    ldi [hl], a
    inc hl
    inc hl
    ldh a, [$FF93]
    dec a
    jr nz, .loop
    pop bc
    pop af
    ret

SECTION "ROM0_1884", ROM0[$1884]
routine_1884::
    call banked_call
        DW $5000
        DB $01
    ret

SECTION "ROM0_188B", ROM0[$188B]
routine_188B::
    call banked_call
        DW $5003
        DB $01
    ret

SECTION "ROM0_1892", ROM0[$1892]
routine_1892::
    call banked_call
        DW $5015
        DB $01
    ret

SECTION "ROM0_1899", ROM0[$1899]
routine_1899::
    call banked_call
        DW $5018
        DB $01
    ret

SECTION "ROM0_18A0", ROM0[$18A0]
routine_18A0::
    call banked_call
        DW $501B
        DB $01
    ret

SECTION "ROM0_18A7", ROM0[$18A7]
routine_18A7::
    call banked_call
        DW $6080
        DB $0F
    ret

SECTION "ROM0_18AE", ROM0[$18AE]
routine_18AE::
    call banked_call
        DW $5000
        DB $0D
    ret

SECTION "ROM0_18B5", ROM0[$18B5]
routine_18B5::
    call banked_call
        DW $6083
        DB $0F
    ret 

SECTION "ROM0_18BC", ROM0[$18BC]
routine_18BC::
    xor a
    call routine_18C3
    ldh a, [$FF8C]
    ret

routine_18C3::
    push af
    push bc
    push de
    push hl
    ld b, a
    ld a, [$C79A]
    add a
    or b
    ld [$C7CD], a
    ld a, $01
    rst rst_bank_switch
    push af
    call $500C
    pop af
    rst rst_bank_switch
    jp pop_hl_de_bc_af

routine_18DC::
    push af
    push bc
    push de
    push hl
    ld b, a
    ld a, [$C79A]
    add a
    or b
    ld [$C7CD], a
    ld a, $01
    rst rst_bank_switch
    push af
    call $500C
    pop af
    rst rst_bank_switch
    jp pop_hl_de_bc_af

SECTION "ROM0_18F5", ROM0[$18F5]
    nop  
    nop  
    nop  
    nop  
    nop  
    nop  
    nop  
    nop  
    nop  
    nop  
    nop

SECTION "ROM0_1900", ROM0[$1900]
routine_1900: jp routine_19D6
routine_1903: jp load_game
routine_1906: jp $388C
routine_1909: jp $333C
routine_190C: jp $3232
routine_190F: jp $2B14
routine_1912: jp routine_194B
routine_1915: jp routine_1B5F
routine_1918: jp routine_1B99
routine_191B: jp $2FB1
routine_191E: jp $2ADF
routine_1921: jp routine_1F1A
    nop  
    nop  
    nop  
    nop

SECTION "ROM0_1928", ROM0[$1928]
table_1928::
    DB $00, $FC, $00, $02, $01, $00, $FF, $00, $02, $FF, $FF, $01, $01, $00, $FF, $04, $08, $0A, $0C, $0C, $0E, $0C, $0A, $08, $04, $FC, $F8, $F4, $F2, $F0, $F2, $F4, $F6, $FA, $80

SECTION "ROM0_194B", ROM0[$194B]
routine_194B::
    ld a, [$C44B]
    ld [$C2A5], a
    ld a, [$C44C]
    ld [$C2A6], a
    ld a, [$C42E]
    swap a
    and a, $0F
    ld c, a
    ld a, [$C42C]
    add c
    ld c, a
    ld a, [$C436]
    rrca 
    rrca 
    and a, $C0
    or c
    ld [$C2A7], a
    ld a, [$C42F]
    swap a
    and a, $0F
    ld c, a
    ld a, [$C42D]
    add c
    ld [$C2A8], a
    ld a, [$C434]
    ld [$C33D], a
    ld a, [$C43A]
    ld [$C33E], a
    ld a, [$C43B]
    ld [$C33F], a
    ld a, [$C460]
    ld [$C305], a
    ret

SECTION "ROM0_1997", ROM0[$1997]
load_game::
    ld a, $69
    ld [$C468], a
    ld a, [$C2A5]
    ld [$C44B], a
    ld a, [$C2A6]
    ld [$C44C], a
    ld a, [$C2A7]
    ld b, a
    and a, $3F
    sub a, $05
    ld [$C42C], a
    ld a, b
    rlca 
    rlca 
    and a, $03
    ld [$C436], a
    ld a, [$C2A8]
    and a, $3F
    sub a, $04
    ld [$C42D], a
    ld a, $40
    ld [$C42F], a
    ld a, $50
    ld [$C42E], a
    ld a, $01
    ld [$C466], a
    jr routine_19F0

SECTION "ROM0_19D6", ROM0[$19D6]
routine_19D6::
    ld a, $69
    ld [$C468], a
    xor a
    ld [$C466], a
    ld a, $FE
    ld [$C442], a
    ld a, $06
    ld [$C443], a
    call routine_1BF0
    ld a,$01
    jr routine_19F1

SECTION "ROM0_19F0", ROM0[$19F0]
routine_19F0::
    xor a
routine_19F1::
    ld [$C45B], a
    ldh [$FF47], a
    ldh [$FF48], a
    ldh [$FF49], a
    ld a, $90
    ldh [$FF4A], a
    call routine_1B37
    call routine_1CAF
    ld a, [$C450]
    call map_load_tileset_graphics
    call $2232
    ld a, [$C450]
    call map_load_tileset_types
    call routine_1A7B
    call routine_1F23
    call routine_1E21
    call $338A
    call routine_1D5C
    call $32D8
    ld a, [$C466]
    or a
    jr z, .skip
    ld a, [$C33D]
    ld [$C434], a
    ld a, [$C33E]
    ld [$C43A], a
    ld a, [$C33F]
    ld [$C43B], a
.skip
    call $20C8
    call $3EA2
    ; fallthrough
SECTION "ROM0_1A43", ROM0[$1A43]
routine_1A43::
.loop
    ld a, [$C43D]
    or a
    jr nz, .skip2
    call $27C9
    ld  a,[$C443]
    cp a, $0E
    jp z, routine_1AD9
    cp a, $05
    jr c, .skip
    cp a, $07
    jp c, routine_1AD9
.skip
    call $3039
    call $26D5
    call $2FBF
.skip2
    call $3249
    call $344B
    call $391D
    jr .loop

SECTION "ROM0_1A71", ROM0[$1A71]
table_1A71::
    DB $00, 00
SECTION "ROM0_1A73", ROM0[$1A73]
table_1A73::
    DB $00, $01, $00, $FF, $FF, $00, $01, $00

SECTION "ROM0_1A7B", ROM0[$1A7B]
routine_1A7B::
    ld a, $00
    ld [$C435], a
    xor a  
    ld_abs [$FFC0], a
    ld [$C45C], a
    ld [$C45D], a
    ld a, $FF
    ld [$C442], a
    ld [$C443], a
    ld a, $C3
    ldh [$FF40], a
    ret  

SECTION "ROM0_1A97", ROM0[$1A97]
routine_1A97::
    push hl
    push af
    call transfer_map_oam_entry
    ld a, [$C305]
    and a, $F0
    cp a, $10
    jr nz, .else
    ld hl, $C441
    inc [hl]
    ld a, [hl]
    and a, $07
    add a
    add a, $27
    ld l, a
    ld h, $19
    ldi a, [hl]
    ldh [$FFC3], a
    ld a, [hl]
    ldh [$FFC4], a
    jr .done
.else
    xor a
    ldh [$FFC3], a
    ldh [$FFC4], a
    ld hl, table_1928
.done
    ldh a, [$FFC2]
    add [hl]
    ldh [$FF43], a
    dec l
    ldh a, [$FFC1]
    add [hl]
    ldh [$FF42], a
    ld a, [$C464]
    inc a
    and a, $1F
    ld [$C464], a
    pop af
    pop hl
    ret

SECTION "ROM0_1AD9", ROM0[$1AD9]
routine_1AD9::
    call routine_1AEC
    ld a,$FF
    ld [$C442],a
    ld [$C443],a
    jp routine_1A43

SECTION "ROM0_1AE7", ROM0[$1AE7]
routine_1AE7::
    call routine_1BDB
    jr routine_1AEF

SECTION "ROM0_1AEC", ROM0[$1AEC]
routine_1AEC::
    call routine_1BF0
    ; fallthrough
SECTION "ROM0_1AEF", ROM0[$1AEF]
routine_1AEF::
    call $3E5B
    ld a, [$C305]
    ld [$C463], a
    and a,$0F
    ld [$C305], a
    call $2ECE
    call routine_1CAF
    call routine_1B37
    ld a,[$C44D]
    or a
    jr nz, .skip
    ld a, [$C450]
    call map_load_tileset_graphics
    call $2232
    ld a, [$C450]
    call map_load_tileset_types
    call routine_1F23
    call $338A
    call routine_1D5C
.skip
    call routine_1E21
    call $32D8
    call $20C8
    call $3E4C
    ld a,[$C463]
    ld [$C305],a
    ret 

SECTION "ROM0_1B37", ROM0[$1B37]
routine_1B37::
    ld hl, $C010
    xor a
    ld b, $90
.loop
    ldi [hl], a
    dec b
    jr nz, .loop

    ld hl, $C110
    ld b, $90
.loop2
    ldi [hl], a
    dec b
    jr nz, .loop2
    ret

SECTION "ROM0_1B4B", ROM0[$1B4B]
clear_map_oam_buffers::
    ld hl, $C000
    xor a
    ld b, $A0
.loop
    ldi [hl], a
    dec b
    jr nz, .loop
    ld hl, $C100
    ld b, $A0
.loop2
    ldi [hl],a
    dec b
    jr nz, .loop2
    ret

SECTION "ROM0_1B5F", ROM0[$1B5F]
routine_1B5F::
    call exit_character_creator_to_map_inner
    ld a, $0D
    rst rst_bank_switch
    ret

SECTION "ROM0_1B66", ROM0[$1B66]
routine_1B66::
    ld a, [$C305]
    ld [$C461], a
    and a, $0F
    ld [$C305], a
    call $2ECE
    rst rst_call_800
    ld a, [$C305]
    and a, $F0
    cp a, $30
    jr z, routine_1B90
    or a
    ret nz
    ld a, [$C305]
    and a, $0F
    ld b, a
    ld a, [$C461]
    and a, $F0
    or b
    ld [$C305], a
    ret  

SECTION "ROM0_1B90", ROM0[$1B90]
routine_1B90::
    ld a, [$C305]
    and a, $0F
    ld [$C305], a
    ret  

SECTION "ROM0_1B99", ROM0[$1B99]
routine_1B99::
    cp a, $04
    jr z, routine_1BB4
    inc a
    push bc
    ld b, a
    ld a, [$C2A0]
    rlca 
    rlca 
.loop
    rrca 
    rrca 
    dec b
    jr nz, .loop
    pop bc
    and a, $03
    swap a
    sla a
    add l
    ld l, a
    ret  

SECTION "ROM0_1BB4", ROM0[$1BB4]
routine_1BB4::
    ld a, l
    add a, $80
    ld l, a
    ret  

; Arguments:
; a = tileset index
;     (tileset pointer address = ROM7:($7000 + a * 32)
; Result:
; [map_tileset_types] table is loaded with the tile types for each metatile in the tileset.
SECTION "ROM0_1BB9", ROM0[$1BB9]
map_load_tileset_types::
    ld c, $00
    srl a
    rr c
    srl a
    rr c
    srl a
    rr c
    ld b,a
    ld hl, $7000
    add hl, bc
    ld a, $07
    rst rst_bank_switch
    ld de, $C520
    ld b, $20
.loop
    ldi a, [hl]
    ld [de], a
    inc e
    dec b
    jr nz, .loop
    ret

routine_1BDB::
    ld a, [$C442]
    ld l, a
    ld a, [$C443]
    and a, $01
    ld h, a
    add hl, hl
    add hl, hl
    ld de, $4000
    ld a, $07
    rst rst_bank_switch
    add  hl, de
    jr routine_1C61

SECTION "ROM0_1BF0", ROM0[$1BF0]
routine_1BF0::
    ld hl, table_1A71
    call $29B3
    call $2636
    ld a, [bc]
    res 7, a
    ld [bc], a
    ld a, [$C442]
    ld l, a
    ld a, [$C443]
    cp a, $0E
    jr nz, routine_1C12
    and a, $01
    ld h, a
    add hl, hl
    add hl, hl
    ld de, $6F80
    jr routine_1C27

SECTION "ROM0_1C12", ROM0[$1C12]
routine_1C12::
    and a, $01
    xor a, $01
    ld h, a
    jr z, routine_1C22
    ld a, l
    inc a
    jr nz, routine_1C22
    ld hl, $C316
    jr routine_1C61

SECTION "ROM0_1C22", ROM0[$1C22]
routine_1C22::
    add hl, hl
    add hl, hl
    ld de, $4000
    ; fallthrough
SECTION "ROM0_1C27", ROM0[$1C27]
routine_1C27::
    ld a,$07
    rst rst_bank_switch
    add hl, de
    ld a, [$C2A5]
    ld [$C316], a
    ld a, [$C2A6]
    ld [$C317], a
    ld a, [$C42C]
    ld e, a
    ld a, [$C42E]
    swap a
    and a, $0F
    add e
    ld e, a
    ld a, [$C436]
    xor a, $01
    rrca 
    rrca 
    and a, $C0
    or e
    ld [$C318], a
    ld a, [$C42D]
    ld e, a
    ld a, [$C42F]
    swap a
    and a, $0F
    add e
    inc a
    ld [$C319], a
    ; fallthrough
SECTION "ROM0_1C61", ROM0[$1C61]
routine_1C61::
    ld e, [hl]
    inc hl
    ld d, [hl]
    inc hl
    ld a, e
    ld [$C2A5], a
    ld [$C44B], a
    ld a, d
    ld [$C2A6], a
    ld [$C44C], a
    ldi a, [hl]
    ld b, a
    and a, $3F
    sub a, $05
    ld [$C42C], a
    ld a, b
    rlca 
    rlca 
    and a, $03
    ld [$C436], a
    ldi a, [hl]
    ld b, a
    and a, $3F
    sub a, $04
    ld [$C42D], a
    ld a, b
    and a, $40
    ld [$C44D], a
    ld a, b
    and a, $80
    ld b, a
    jr z, .skip
    ld a, $16
    ldh [$FFB2], a
.skip
    ld a, [$C319]
    or b
    ld [$C319],a
    ld a,$40
    ld [$C42F],a
    ld a,$50
    ld [$C42E],a
    ret

SECTION "ROM0_1CAF", ROM0[$1CAF]
routine_1CAF::
    ld a, $07
    rst rst_bank_switch
    ld a, [$C44B]
    ld e, a
    ld a, [$C44C]
    ld d, a
    ld a, [de]
    inc de
    ld [$C44E], a
    ld a, [de]
    inc de
    ld [$C44F], a
    ld a, [de]
    inc de
    ld [$C450], a
    ld a, [de]
    inc de
    ld c, a
    and a, $0F
    ld [$C451], a
    ld a, $80
    and c
    ld [$C452], a
    ld a, $40
    and c
    ld [$C453], a
    swap c
    ld a, c
    and a, $03
    ld [$C456], a
    ld a, [$C453]
    or a
    jr z, .skip
    ld a, [de]
    ld [$C454], a
    inc de
    ld a, [de]
    ld [$C455], a
    inc de
.skip
    ld a, [de]
    ld l, a
    inc de
    ld a, e
    ld [$C457], a
    ld a, d
    ld [$C458], a
    ld h, $00
    add hl, hl
    add hl, de
    ld a, l
    ld [$C459], a
    ld a, h
    ld [$C45A], a
    ret

SECTION "ROM0_1D0D", ROM0[$1D0D]
routine_1D0D::
    ld a, [$C45C]
    ld c, a
    ld a, [$C45D]
    or c
    ret nz
    ld a, [$C434]
    ld [$CFE6], a
    ld a, [$C43A]
    ld [$CFE7], a
    ld a, [$C43B]
    ld [$CFE8], a
    ld a, [$C2A5]
    ld [$C45C], a
    ld a, [$C2A6]
    ld [$C45D], a
    ld a, [$C42C]
    ld e, a
    ld a, [$C42E]
    swap a
    and a, $0F
    add e
    ld e, a
    ld a, [$C436]
    rrca 
    rrca 
    and a, $C0
    or e
    ld [$C45E], a
    ld a, [$C42D]
    ld e, a
    ld a, [$C42F]
    swap a
    and a, $0F
    add e
    ld [$C45F], a
    ret

SECTION "ROM0_1D5C", ROM0[$1D5C]
routine_1D5C::
    xor a
    ld [$C446],a
    ld de, $C6F0
    ld hl, $C2A9
    ld c, $04
.outer_loop
    push hl
    push bc
    ld a,[$C44B]
    cp [hl]
    jp nz, .next
    inc hl
    ld a,[$C44C]
    xor [hl]
    and a, $3F
    jp nz, .next
    ldi a, [hl]
    and a, $C0
    rlca
    rlca
    inc a
    ld c, $80
.rotate_left_loop
    rlc c
    dec a
    jr nz, .rotate_left_loop
    ld a, e
    or a, $0C
    ld e, a
    ld a, c
    ld [de], a
    ld a, e
    and a, $F0
    ld e, a
    pop bc
    push bc
    ld a, $05
    sub c
    ld c, a
    push de
    ld e, $1F
    call routine_063E_entry
    pop de
    cp c
    ld b, $80
    jr z, .skip
    ld b, $00
.skip
    ldi a, [hl]
    ld c, a
    and a, $3F
    or a, $40
    or b
    ld [de], a
    inc e
    xor a
    ld [de], a
    inc e
    ld a, b
    or a
    ldi a, [hl]
    ld b, a
    jr nz, .skip2
    push bc
    ld a, c
    and a, $3F
    ld c, a
    ld a, b
    and a, $3F
    ld b, a
    call $2636
    set 7, a
    ld [bc], a
    pop bc
.skip2
    ld a, b
    and a, $3F
    ld [de], a
    inc e
    xor a
    ld [de], a
    inc e
    ld [de], a
    ld a, b
    and a, $C0
    rrca 
    rrca 
    ld b, a
    ld a, e
    and a, $F0
    or a, $0C
    ld e, a
    ld a, [de]
    or b
    ld [de], a
    ld a, e
    and a, $F0
    or a, $05
    ld e, a
    ld a, b
    or a
    jr z, .skip3
    ld b, $01
.skip3
    ld a, c
    and a, $C0
    ld c, a
    rrca 
    rrca 
    or c
    or b
    ld [de], a
    inc e
    xor a
    ld [de], a
    inc e
    inc e
    pop bc
    push bc
    ld a, $10
    ld [de], a
    inc e
    ld a, $04
    sub c
    or a, $08
    ld [de], a
    inc e
    ld a,$F0
    ld [de], a
    inc e
    xor a
    ld [de], a
    inc e
    inc e
    ld [de], a
.next
    pop bc
    pop hl
    inc l
    inc l
    inc l
    inc l
    ld a, e
    and a, $F0
    sub a, $10
    ld e, a
    dec c
    jp nz, .outer_loop
    ret

SECTION "ROM0_1E21", ROM0[$1E21]
routine_1E21::
    push de
    ld e, $1F
    call routine_063E_entry
    pop de
    or a
    jr z, .else
    ld b, a
    ld a, $05
    sub b
    ld b, $80
.loop
    rlc b
    dec a
    jr nz, .loop
    ld a, b
    ld [$C43E], a
    ld [$C43F], a
    ld a, $01
    ld [$C430], a
    ld a, $01
    ld [$C431], a
    ld a, $80
    ld [$C432], a
    ld a, $40
    ld [$C433], a
    jr .done
.else
    ld a, $01
    ld [$C43E], a
    ld [$C43F], a
    xor a
    ld [$C430], a
    call routine_1EC2
.done
    xor  a
    ld [$C437], a
    ld [$C43D], a
    ld [$C438], a
    ld hl, $1A71
    call $29B3
    call $2636
    ld a, [bc]
    set 7, a
    ld [bc], a
    call $2946
    ld c, a
    and a, $20
    ld [$C43C], a
    ld a, c
    and a, $1F
    add a, $20
    ld c, a
    ld b, $C5
    ld a, [bc]
    ld e, a
    cp a, $C0
    jr nc, routine_1EB4
    bit 7, a
    jr nz, routine_1EA1
    bit 2, a
    jr nz, routine_1EA9
    and a, $03
    jr z, .skip
    xor a, $03
.skip
    ld [$C434], a
    ; fallthrough
SECTION "ROM0_1EA1", ROM0[$1EA1]
routine_1EA1::
    ld a, e
    and a, $30
    ld [$C43B], a
    jr routine_1EBB

SECTION "ROM0_1EA9", ROM0[$1EA9]
routine_1EA9::
    ld a, [$C434]
    and a, $01
    jr z, routine_1EA1
    ld e, $30
    jr routine_1EA1

SECTION "ROM0_1EB4", ROM0[$1EB4]
routine_1EB4::
    xor a
    ld [$C43B], a
    ld [$C434], a
    ; fallthrough
SECTION "ROM0_1EBB", ROM0[$1EBB]
routine_1EBB::
    ld a, [$C436]
    call $29E2
    ret 

SECTION "ROM0_1EC2", ROM0[$1EC2]
routine_1EC2::
    ld e, $1F
    call routine_063E_entry
    or a
    jr nz, routine_1F0A
    xor a
.loop
    push af
    ld hl,$C206
    call $1B99
    pop af
    bit 4, [hl]
    jr z, .done_loop
    inc a
    jr .loop
.done_loop
    ld a, l
    sub a, $02
    ld l, a
    ld a, [hl]
    ld b, $00
    ld c, a
    ld hl, $6B70
    add hl, bc
    ld a, $0D
    rst rst_bank_switch
    ld a, [hl]
    add a, $00
    ld l, a
    ld h, $43
    ld a, $01
    rst rst_bank_switch
    ld c, $00
    ld a, [hl]
    ld [$C431], a
    srl a
    rr c
    ld b, a
    ld hl, $4000
    add hl, bc
    ld a, l
    ld [$C432], a
    ld a, h
    ld [$C433], a
    ret  

SECTION "ROM0_1F0A", ROM0[$1F0A]
routine_1F0A::
    ld a, $01
    ld [$C431], a
    ld a, $80
    ld [$C432], a
    ld a, $40
    ld [$C433], a
    ret

SECTION "ROM0_1F1A", ROM0[$1F1A]
routine_1F1A::
    call $1F23
    call $1EC2
    jp routine_1EBB

SECTION "ROM0_1F23", ROM0[$1F23]
routine_1F23::
    xor a
.loop
    push af
    ld hl, $C206
    call routine_1B99
    pop af
    bit 4, [hl]
    jr z, .done_loop
    inc a
    jr .loop
.done_loop
    ld a, l
    sub a, $02
    ld l, a
    ld a, [hl]
    ld b, $00
    ld c, a
    ld hl, $6B70
    add hl, bc
    ld a, $0D
    rst rst_bank_switch
    ld a, [hl]
    add a, $40
    ld h, a
    ld l, $00
    ld a, $03
    rst rst_bank_switch
    ld de, $8000
    ld bc, $0100
    call vramcpy16
    ret 

; Arguments:
; a = tileset index
;     (tileset tile data address = ROM7:($7800 + a * 32)
; Result:
; VRAM at $8800 is loaded is loaded with the tile graphics for each metatile in the tileset.
SECTION "ROM0_1F55", ROM0[$1F55]
map_load_tileset_graphics::
    ld c, $00
    srl a
    rr c
    srl a
    rr c
    srl a
    rr c
    ld b, a
    ld hl, $7800
    add hl, bc
    ld a, $02
    rst rst_bank_switch
.wait_mid_vblank
    ldh a, [$FF44]
    cp a, 150
    jr nz, .wait_mid_vblank

    ld a, $43
    ldh [$FF40], a
    ld de, $9000
    ld b, $20
.loop
    push bc
    ld a, $07
    call banked_load
    inc hl
    ld b,a
    ld c, $00
    srl b
    rr c
    srl b
    rr c
    push hl
    ld hl, $4000
    add hl, bc
    ld b, $40
    call memcpy8
    pop hl
    pop bc
    dec b
    jr nz, .loop

    ld a, $C3
    ldh [$FF40], a
    xor a
    ld [$C439], a
    ret

SECTION "ROM_1FA4", ROM0[$1FA4]
routine_1FA4::
    call $2F7F
    rst rst_wait_vblank
    call routine_1A97
    call $21EA
    ld c, $C1
    ld a, [$C43E]
    ld b, a
    ld a, [$C43D]
    dec a
    jr z, .label1FD4
    dec a
    jr z, .label1FD0
    dec a
    jr z, .label1FC8
    inc c
    ld a, [$ff00+c]
    add b
    ld [$ff00+c], a
    add a, $08
    jr .label1FD9
.label1FC8
    inc c
    ld a, [$ff00+c]
    sub b
    ld [$ff00+c], a
    add a, $08
    jr .label1FD9
.label1FD0
    ld a, [$ff00+c]
    sub b
    jr .label1FD8
.label1FD4
    ld a, [$ff00+c]
    add b
    jr .label1FD8
.label1FD8
    ld [$ff00+c], a
.label1FD9
    and a,$0F
    ret nz
    ld [$C43D], a
    ld a, [$C43F]
    ld [$C43E], a
    xor a
    ld [$C437], a
    ret  

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