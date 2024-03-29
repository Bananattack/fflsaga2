; Macros to force a 16-bit ld [nnnn], a
; when RGBDS would otherwise try to replace with ldh [$FF00 + nn], a
ld_abs: MACRO
    IF STRCMP(STRLWR("\2"), "a") == 0
        IF STRCMP(STRSUB("\1", 1, 1), "[") != 0
            FAIL "first operand must be absolute address [$nnnn] starting with ["
        ENDC
        IF STRCMP(STRSUB("\1", STRLEN("\1"), 1), "]") != 0
            FAIL "first operand must be absolute address [$nnnn] ending with ]"
        ENDC

TEMP_OPERAND\@ EQUS STRSUB("\1", 2, STRLEN("\1") - 2)
        DB $EA
        DW TEMP_OPERAND\@
    ELSE
        IF STRCMP(STRLWR("\1"), "a") != 0
            FAIL "ld_abs operand must have accumulator as either source or dest"
        ENDC

        IF STRCMP(STRSUB("\2", 1, 1), "[") != 0
            FAIL "second operand must be absolute address [$nnnn] starting with ["
        ENDC
        IF STRCMP(STRSUB("\2", STRLEN("\2"), 1), "]") != 0
            FAIL "second operand must be absolute address [$nnnn] ending with ]"
        ENDC

TEMP_OPERAND\@ EQUS STRSUB("\2", 2, STRLEN("\2") - 2)
        DB $FA
        DW TEMP_OPERAND\@
    ENDC
ENDM

INCLUDE "ram.asm"
INCLUDE "rom0.asm"
INCLUDE "rom1.asm"
INCLUDE "rom7.asm"
INCLUDE "romf.asm"
