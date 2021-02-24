; vim: set ft=asm6502-2 ts=8 et:

;----------------------------------------------------------------------
;			Application includes
;----------------------------------------------------------------------
;.include "overlay.mac"

;======================================================================
;			   Format du fichier
;======================================================================
.if 0

.byte .strlen(SEGID)
.asciiz SEGID
function_table:
        .byte (function_table_end - function_table) >> 1
        .addr fn1
        .addr fn2
        .addr fn3
function_table_end:

ou

.byte .strlen(SEGID)
.asciiz SEGID
function_table:
        .byte (function_table_end - function_table) / 3
        jmp fn1
        jmp fn2
        jmp fn3
function_table_end:

.endif

;======================================================================
;			        Overlay 1
;======================================================================
;OVL_OPTIONS::use_fn_table .set $02

start_overlay "OVERLAY1", "OVERLAY.1"

        ovl_public ovl1
                print hello, NOSAVE
                rts
        ovl_public_end

        ovl_public toto
                lda #$0a
                rts
        ovl_public_end

        hello:
        .asciiz "\x1bAHello from OVL1@OVERLAY.1"

end_overlay
;OVL_OPTIONS::use_fn_table .set $00

;======================================================================
;			        Overlay 2
;======================================================================
start_overlay "OVERLAY2", "OVERLAY.2"

        .proc dummy
                lda #$22
                rts
        .endproc

        .proc ovl2
                print hello, NOSAVE
                rts
        .endproc

        hello:
        .asciiz "\x1bBHello from OVL2@OVERLAY.2"
end_overlay

;======================================================================
;			        Overlay 1 bis
;======================================================================
start_overlay "OVERLAY3", "OVERLAY.1"

        .proc ovl1
                print hello, NOSAVE
                rts
        .endproc

        hello:
        .asciiz "\x1bCHello from OVL1@OVERLAY.1"
end_overlay

;======================================================================
;			        Overlay 4
;======================================================================
start_overlay 4, "OVERLAY.4"

        .proc ovl1
                print hello, NOSAVE
                rts
        .endproc

        hello:
        .asciiz "\x1bDHello from OVL1@OVERLAY.4"
end_overlay

;OVL_OPTIONS::use_fn_table .set $00

