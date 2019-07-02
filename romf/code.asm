; ...

SECTION "ROMF_6098", ROMX[$6098], BANK[$F]
battle_intro::
    rst rst_wait_vblank
    xor a
    ldh [$FF47], a
    ldh [$FF48], a
    ldh [$FF49], a
    ld a, $C0
    ldh [$FF40], a
    ld hl, $D920
    ld bc, $01E0
    call memclear16
    ldh [$FF96], a
    dec a
    ld hl, $9800
    ld bc, $0800
    call vramset16
    ld hl, $CC00
    ld b, $A0
    call memclear8
    rst rst_wait_vblank
    ld a, $CC
    rst rst_oam_dma_transfer
    ld hl, $4700
    ld de, $9700
    ld bc ,$0100
    ld a, $04
    call banked_vramcpy16
    ld hl, $D90D
    xor a
    ldi [hl], a
    ldi [hl], a
    ld [hl], a
    ld a, [$C7F3]
    and a
    jr z, .else
.then    
    ld b, $07
    ld hl, $62A6
.loop
    cp [hl]
    jr z, .found
    inc hl
    dec b
    jr nz, .loop
.not_found
    xor a
    jr .done
.found
    ld a, $01
    jr .done
.else
    ld a, $02
.done
    ld [$D98B], a
    call banked_call_entry
        DW $5012 ; subroutine address
        DB $0D ; subroutine bank
    call $6257
    call $627F
    call $6146
    rst rst_wait_vblank
    ld a, $40
    ldh [$FF4A], a
    ld a, $07
    ldh [$FF4B], a
    xor a
    ldh [$FF42], a
    ldh [$FF43], a
    ld a, $C3
    ldh [$FF40], a
    ld a, [$D98B]
    and a
    jr z, .if_eq_0
    dec a
    jr z, .if_eq_1
    call $61E3
    jr .done_check
.if_eq_1
    call $61EC
    jr .done_check
.if_eq_0
    call $61ED
.done_check
    rst rst_wait_vblank
    ld a, $D2
    ldh [$FF47], a
    ld [$C700], a
    ld a, $D2
    ldh [$FF48], a
    ld [$C701], a
    ld a, $81
    ldh [$FF49], a
    ld [$C702], a
    ret

; ...