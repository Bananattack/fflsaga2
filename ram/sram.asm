SAVE_STRUCT: MACRO
\1_party_members::
\1_party_member_0:: PLAYER_STRUCT \1_party_member_0
\1_party_member_1:: PLAYER_STRUCT \1_party_member_1
\1_party_member_2:: PLAYER_STRUCT \1_party_member_2
\1_party_member_3:: PLAYER_STRUCT \1_party_member_3
\1_party_member_4:: PLAYER_STRUCT \1_party_member_4

\1_unknown:: DS $19

\1_item_slot_0:: ITEM_STRUCT \1_item_slot_0
\1_item_slot_1:: ITEM_STRUCT \1_item_slot_1
\1_item_slot_2:: ITEM_STRUCT \1_item_slot_2
\1_item_slot_3:: ITEM_STRUCT \1_item_slot_3
\1_item_slot_4:: ITEM_STRUCT \1_item_slot_4
\1_item_slot_5:: ITEM_STRUCT \1_item_slot_5
\1_item_slot_6:: ITEM_STRUCT \1_item_slot_6
\1_item_slot_7:: ITEM_STRUCT \1_item_slot_7
\1_item_slot_8:: ITEM_STRUCT \1_item_slot_8
\1_item_slot_9:: ITEM_STRUCT \1_item_slot_9
\1_item_slot_A:: ITEM_STRUCT \1_item_slot_A
\1_item_slot_B:: ITEM_STRUCT \1_item_slot_B
\1_item_slot_C:: ITEM_STRUCT \1_item_slot_C
\1_item_slot_D:: ITEM_STRUCT \1_item_slot_D
\1_item_slot_E:: ITEM_STRUCT \1_item_slot_E
\1_item_slot_F:: ITEM_STRUCT \1_item_slot_F

\1_unknown2:: DS $A7
ENDM

SECTION "SRAM0_A000", SRAM[$A000], BANK[0]
save_0:: SAVE_STRUCT save_0

SECTION "SRAM0_A180", SRAM[$A180], BANK[0]
save_1:: SAVE_STRUCT save_1

SECTION "SRAM0_A300", SRAM[$A300], BANK[0]
save_2:: SAVE_STRUCT save_2
