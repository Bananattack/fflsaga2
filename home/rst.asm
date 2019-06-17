rst_hl_plus_a EQU $00
rst_call_908 EQU $08
rst_next_frame EQU $10
rst_oam_dma_transfer EQU $18
rst_call_800 EQU $20
rst_bank_switch EQU $28
rst_call_701 EQU $30
rst_infinite_loop EQU $38

; Increments hl by a.
;
; Arguments:
; hl = 16-bit value to increment
; a = 8-bit value to add
;
; Result:
; hl = hl + a.
SECTION "ROM0_00", ROM0[$00]
rst_hl_plus_a_::
    push bc
    ld b, $00
    ld c, a
    add hl, bc
    pop bc
    ret 
    nop  

; Calls a routine at hex address $908.
; Purpose unknown.
SECTION "ROM0_08", ROM0[$08]
rst_call_908_::
    jp $0908

; Pops hl, de, bc, then af off the stack, in that order.
; Then it returns (popping the PC off the stack).
SECTION "ROM0_0B", ROM0[$0B]
pop_hl_de_bc_af::
    pop hl
    pop de
    pop bc
    pop af
    ret  

; Waits for the vblank
SECTION "ROM0_10", ROM0[$10]
rst_next_frame_::
    jp $00D9

; Returns from interrupt.
SECTION "ROM0_11", ROM0[$11]
reti_::
    reti

; Clears the d register, then falls through code that calls the dma routine. Purpose unknown.
SECTION "ROM0_14", ROM0[$14]
clear_d_oam_transfer::
    ld d, $00
    nop
    nop
    ; fallthrough
; Performs an OAM DMA transfer.
;
; Arguments:
; a = high byte of address for the source OAM table to transfer. 256-byte aligned, 160 bytes long.
SECTION "ROM0_18", ROM0[$18]
rst_oam_dma_transfer_::
    jp oam_dma_transfer_routine
    nop
    nop
    nop
    nop
    nop

; Calls a routine at hex address $800. Purpose unknown.
SECTION "ROM0_20", ROM0[$20]
rst_call_800_::
    jp $0800
    nop
    nop
    nop
    nop
    nop

; Disables interrupts, calls the bank switch routine, restores interrupts after.
SECTION "ROM0_28", ROM0[$28]
rst_bank_switch_::
    di
    call $04B1
    reti
    nop
    nop
    nop

; Calls a routine at hex address $701. Purpose unknown.
SECTION "ROM0_30", ROM0[$30]
rst_call_701_::
    jp $0701
    nop
    nop
    nop
    nop
    nop

; Loops infinitely, effectively crashing the program until a reset is performed.
SECTION "ROM0_38", ROM0[$38]
rst_infinite_loop_::
    jp $0038
    nop
    nop
    nop
    nop
    nop

; Fixed vblank interrupt routine.
; Calls a dispatcher in RAM that gets remapped at runtime.
SECTION "ROM0_40", ROM0[$40]
vblank_entry::
    jp vblank_dispatcher

; Fixed stat interrupt routine.
; Calls a dispatcher in RAM that gets remapped at runtime.
SECTION "ROM0_48", ROM0[$48]
stat_entry::
    jp stat_dispatcher

; (Timer, serial and joypad interrupts are not used.)

; Multiplies a by 64, via repeated self-addition.
;
; Arguments:
; a = original value
;
; Result:
; a = a * 64
SECTION "ROM0_004B", ROM0[$004B]
a_times_64::
    add a
    add a
    add a
    add a
    add a
    add a
    ret  

; Multiplies hl by 128, via repeated self-addition.
;
; Arguments:
; hl = original value
;
; Result:
; hl = hl * 128
SECTION "ROM0_0052", ROM0[$0052]
hl_times_128::
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    ret

; Multiplies hl by 128 via repeated self-addition, then increments hl by bc.
;
; Arguments:
; hl = original value
; bc = increment amount
;
; Result:
; hl = hl * 128 + bc
SECTION "ROM0_005A", ROM0[$005A]
hl_times_128_plus_bc::
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, bc
    ret

; Multiplies hl by 128 via repeated self-addition, then increments hl by de.
;
; Arguments:
; hl = original value
; de = increment amount
;
; Result:
; hl = hl * 128 + de
SECTION "ROM0_0063", ROM0[$0063]
hl_times_128_plus_de::
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, de
    ret

; Clears b bytes at [hl] with the value 0.
;
; Arguments:
; hl = dest
; b = count (0 = 256 bytes)
;
; Result:
; a, b are cleared.
; hl is advanced ahead by b bytes.
SECTION "ROM0_006C", ROM0[$006C]
memclear8::
    xor a
    ; fallthrough
; Sets b bytes at [hl] with the value a.
;
; Arguments:
; hl = dest
; a = value
; b = count (0 = 256 bytes)
;
; Result:
; b is cleared.
; hl is advanced ahead by b bytes.
SECTION "ROM0_006D", ROM0[$006D]
memset8::
.loop
    ldi [hl], a
    dec b
    jr nz, .loop
    ret  

; Clears bc bytes at [hl] with the value 0.
;
; Arguments:
; hl = dest
; bc = count (0 = 65536 bytes)
;
; Result:
; a, bc are cleared.
; hl is advanced ahead by bc bytes.
SECTION "ROM0_0072", ROM0[$0072]
memclear16::
    xor a
    ; fallthrough
; Sets bc bytes at [hl] with the value a.
;
; Arguments:
; hl = dest
; a = value
; bc = count (0 = 65536 bytes)
;
; Result:
; bc is cleared.
; hl is advanced ahead by bc bytes.
SECTION "ROM0_0073", ROM0[$0073]
memset16::
    push af
    push de
    ld e, a
.loop
    ld [hl], e
    inc hl
    dec bc
    ld a, c
    or b
    jr nz, .loop
    pop de
    pop af
    ret

; Copies b bytes from [hl] to [de]
;
; Arguments:
; de = dest
; hl = source
; b = count (0 = 256 bytes)
;
; Result:
; b is cleared.
; hl is advanced ahead by bc bytes.
; de is advanced ahead by bc bytes.
SECTION "ROM0_0080", ROM0[$0080]
memcpy8::
    push af
.loop
    ldi a,[hl]
    ld [de],a
    inc de
    dec b
    jr nz, .loop
    pop af
    ret  

; Copies bc bytes from [hl] to [de]
;
; Arguments:
; de = dest
; hl = source
; bc = count (0 = 65536 bytes)
;
; Result:
; bc is cleared.
; hl is advanced ahead by bc bytes.
; de is advanced ahead by bc bytes.
SECTION "ROM0_0089", ROM0[$0089]
memcpy16::
    push af
.loop
    ldi a, [hl]
    ld [de], a
    inc de
    dec bc
    ld a, c
    or b
    jr nz, .loop
    pop af
    ret

; Purpose unknown.
SECTION "ROM0_0094", ROM0[$0094]
routine_0094::
    call $1674
    call $006D
    jr $00B2
    call $1674
    call $0073
    jr $00B2
    call $1674
    call $0080
    jr $00B2
    call $1674
    call $0089
    jp $1691
    rst  $28
    push af
    call $0080
    jr $00CF
    rst $28
    push af
    call $0089
    jr $00CF
    rst $28
    push af
    call $00A4
    jr $00CF
    rst $28
    push af
    call $00AC
    pop af
    rst $28
    ret 

; Bank-switched load of some ROM address in another bank.
; Restores the previous bank after completion.
;
; Arguments:
; a = source bank
; hl = source pointer
;
; Result:
; c = value read from the banked location
SECTION "ROM0_00D2", ROM0[$00D2]
banked_load::
    push bc
    rst rst_bank_switch
    ld c, [hl]
    rst rst_bank_switch
    ld a, c
    pop bc
    ret  

; Calls a routine that prepares OAM buffer usage.
; Then waits until inside the vblank period.
SECTION "ROM0_00D9", ROM0[$00D9]
next_frame::
    push af
    call $06B0
.wait_vblank
    halt 
    ldh a, [$FF44]
    cp a, 144
    jr c, .wait_vblank
    pop af
    ret  

; Code used to initialize the RAM routine that does:
;
; ld sp, $NNNN
; ret
;
; The immediate value gets replaced at runtime.
SECTION "ROM0_00E6", ROM0[$00E6]
ld_sp_ret_dispatcher_code::
    ld sp, $0000
    ret

; Code used to initialize the RAM routine that does:
;
; ld a, $NN
; jp $NNNN
;
; The immediate values get replaced at runtime.
SECTION "ROM0_00EA", ROM0[$00EA]
ld_a_jmp_dispatcher_code::
    ld a, $00
    jp $0000

; Code used to initialize the RAM routine that does:
;
; bit N, register
; ret
;
; The bit test opcode gets replaced at runtime.
SECTION "ROM0_00EF", ROM0[$00EF]
bit_test_dispatcher_code::
    bit 0, a
    ret

; Code used to initialize the OAM DMA transfer routine. Uploaded to $FF80.
SECTION "ROM0_00F2", ROM0[$00F2]
oam_dma_transfer_code::
    ldh [$FF46], a
    ld a, $28
.loop
    dec a
    jr nz, .loop
    ret
    nop
    nop  
    nop  
    nop  
    nop  
    nop  
    nop

SECTION "ROM0_0100", ROM0[$0100]
reset_entry::
    jp $0200