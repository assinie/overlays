; vim: set ft=asm6502-2 ts=8 et:

.feature labels_without_colons
.feature string_escapes
.feature loose_char_term
.feature loose_string_term
;.feature dollar_in_identifiers

;----------------------------------------------------------------------
;			cc65 includes
;----------------------------------------------------------------------
.include "telestrat.inc"
.include "fcntl.inc"

;----------------------------------------------------------------------
;			Orix Kernel includes
;----------------------------------------------------------------------
.include "kernel/src/include/kernel.inc"
;.include "kernel/src/include/memory.inc"
;.include "kernel/src/include/process.inc"
.include "kernel/src/include/ch376.inc"
;.include "kernel/src/orix.inc"


;----------------------------------------------------------------------
;			Orix Shell includes
;----------------------------------------------------------------------
; Pour userzp: include les 2 fichiers...
;.include "shell/src/include/bash.inc"
;.include "shell/src/include/orix.inc"


;----------------------------------------------------------------------
;			Orix SDK includes
;----------------------------------------------------------------------
.include "macros/SDK.mac"
.include "include/SDK.inc"
.include "macros/types.mac"
.include "include/errors.inc"
.include "macros/M65C02.mac"

;----------------------------------------------------------------------
;			Application includes
;----------------------------------------------------------------------
.include "overlay.mac"

;----------------------------------------------------------------------
;				Imports
;----------------------------------------------------------------------
;.autoimport +

;.import Keyword_ptr
;.import KBDBUF
; From debug

.import PrintHexByte
.import PrintRegs

; From sopt
.import spar1, sopt1, incr
.import calposp
.import inbuf
.importzp cbp, opt
spar := spar1
sopt := sopt1

; From stop-or-cont
.import StopOrCont

; From ermes
.import ermes

; From WaitResponse
;.import WaitResponse

; From main
.import __BSS_LOAD__, __BSS_SIZE__, __RAMEND__

.import __OVERLAY_START__, __OVERLAY_SIZE__

;----------------------------------------------------------------------
;				Exports
;----------------------------------------------------------------------
.export _main
;.export _argc
;.export _argv

; Pour ermes
.export crlf1, out1, seter1
.export prfild, prnamd

.export drive
.exportzp xtrk, psec

;----------------------------------------------------------------------
;			Librairies
;----------------------------------------------------------------------
; Doit êrte chargé en premier si on utilise des .scope/.endscope pour
; les overlays (à cause de macros)
.include "overlays/overlays.s"

;----------------------------------------------------------------------
; Defines / Constants
;----------------------------------------------------------------------
	max_path := KERNEL_MAX_PATH_LENGTH
        overlay_ptr := address

XGETCWD = $48
XPUTCWD = $49

;----------------------------------------------------------------------
;				Page Zéro
;----------------------------------------------------------------------
.zeropage
	unsigned short address		; hexdump / fgetline

        unsigned char xtrk		; Pour ermes
        unsigned char psec

;	unsigned short offset		; Offset pour les liens

        unsigned char aovl
        unsigned char xovl
        unsigned char yovl
;----------------------------------------------------------------------
;				Variables
;----------------------------------------------------------------------
.segment "DATA"
	unsigned char drive		; Pour la lecture  des fichiers
        unsigned char dskname[max_path]
	unsigned short fp

;----------------------------------------------------------------------
;				Variables
;----------------------------------------------------------------------
.segment "BSS"
        unsigned char cwd[max_path]

;----------------------------------------------------------------------
; Variables et buffers
;----------------------------------------------------------------------
.segment "CODE"

;----------------------------------------------------------------------
;			Segments vides
;----------------------------------------------------------------------
.segment "STARTUP"
.segment "INIT"
.segment "ONCE"

;----------------------------------------------------------------------
;				Programme
;----------------------------------------------------------------------
.segment "CODE"

.macro test fn
        .ifdef fn
                .assert (fn >= __OVERLAY_START__) .and (fn < __OVERLAY_START__+__OVERLAY_SIZE__), error, "KO"

        .else
                .out "ERREUR"
        .endif
.endmacro
.asciiz "TEST"
test OVL1::ovl1

.if 0
        fn1 = 12
        .out "\nOVL1:12"
        _jsr_ovl OVL1:12

        .out "\nOVL1:fn1"
        _jsr_ovl OVL1:fn1

        ;fn2=OVL1::ovl1
        .out "\nOVL1:fn2"
        _jsr_ovl OVL1:fn2

        .out "\nOVL1::12"
        _jsr_ovl OVL1::12

        .out "\nOVL1::fn1"
        _jsr_ovl OVL1::fn1

        .out "\nOVL1::ovl1"
        _jsr_ovl OVL1::ovl1

        .out "\nOVL1::ovl2"
        _jsr_ovl OVL1::ovl2

        .out "\nOVL1->12"
        _jsr_ovl OVL1->12

        .out "\nOVL1->fn1"
        _jsr_ovl OVL1->fn1

        .out "\nOVL1->fn2"
        _jsr_ovl OVL1->fn2

        .out "\nOVL-L"
        _jsr_ovl OVL-L

        .out "\n"
.endif

.proc _main

        ; Adresse de la ligne de commande
        ldy #<(BUFEDT+.strlen("OVERLAY"))
        lda #>(BUFEDT+.strlen("OVERLAY"))
        sty cbp
        sta cbp+1

        ; Saute au premier paramètre
        ldy #$00
        jsr calposp

;        lda #<OVL1_NAME
;        ldy #>OVL1_NAME
;        jsr ovl_load
;        bcs error
;
;        jsr ovl1

        ; la ligne suivante est nécessaire si les overlays sont déclarés dans
        ; un .scope /.endscope et qu'on souhaite utilser le syntaxe ovl:fn ou ovl->fn
        ovl1_1 := OVL1::ovl1
        jsr_ovl OVL1:ovl1_1
        ;jsr PrintRegs
        jsr crlf1

        jsr_ovl OVL1->ovl1_1
        ;jsr PrintRegs
        jsr crlf1

        ; équivalent avec un segment
        jsr_ovl OVL1::ovl1
        ;jsr PrintRegs
        jsr crlf1


;        lda #<OVL2_NAME
;        ldy #>OVL2_NAME
;        jsr ovl_load
;        bcs error
;
;        jsr ovl2
;        jsr_ovl OVL2->ovl2
        jsr_ovl OVL2::ovl2
        ;jsr PrintRegs
        jsr crlf1

        clc

  error:
	jsr ermes
	BRK_KERNEL XCRLF
	rts
.endproc



;===========================================================================
;                       Gestion des overlays
; à mettre dans une librairie
;===========================================================================

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;       Modifiées:
;               -
;       Utilisées:
;               -
; Sous-routines:
;       -
;----------------------------------------------------------------------
.proc ovl_saveregs
        sta aovl
        stx xovl
        sty yovl
        rts
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;       Modifiées:
;               -
;       Utilisées:
;               -
; Sous-routines:
;       -
;----------------------------------------------------------------------
.proc ovl_restoreregs
        ldy yovl
        ldx xovl
        lda aovl
        rts
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;       Modifiées:
;               -
;       Utilisées:
;               -
; Sous-routines:
;       -
;----------------------------------------------------------------------
.proc ovl_load
        sta overlay_ptr
        sty overlay_ptr+1

        jsr ovl_checkname
        bcs reload
        rts

  reload:
        jsr crlf1
        print loading_msg, NOSAVE
        print (overlay_ptr), NOSAVE
        jsr crlf1

        jsr opendir_overlay
	fopen (overlay_ptr), O_RDONLY
        jsr closedir_overlay
	sta fp
	sty fp+1

	ora fp+1
	beq errFopen

	; Lecture du magic word
;	fread Keywords_table, (file_header-2), fp

	; Vérification
;	ldy file_header-1
;  loop:
;	lda Keywords_table,y
;	cmp file_header,y
;	bne errFormat
;	dey
;	bpl loop

	; Lecture du nombre de tokens de la table suivi de la taille de la table
;	fread Keywords_table, #$03, fp

	; Lecture du module
	fread __OVERLAY_START__, __OVERLAY_SIZE__, fp

	fclose (fp)
;	clc
        jsr ovl_checkname
        bcs errFormat2
	rts

  errFormat:
	fclose (fp)
  errFormat2:
	lda #e29
	.byte $2c

  errFopen:
	lda #e13
	sec
  error:
	rts
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;       Modifiées:
;               -
;       Utilisées:
;               -
; Sous-routines:
;       -
;----------------------------------------------------------------------
.proc ovl_checkname
        ldy #$ff
  loop:
        iny
        ; beq error
        lda (overlay_ptr),y
        ; pour les messages d'erreurs
        sta dskname,y

        cmp __OVERLAY_START__,y
        bne reload
        cmp #$00
        bne loop
        clc
        rts

  reload:
        sec
        rts
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;       Modifiées:
;               -
;       Utilisées:
;               -
; Sous-routines:
;       -
;----------------------------------------------------------------------
.proc opendir_overlay
        ; Sauvegarde le chemin actuel
        BRK_KERNEL XGETCWD
        sta RESB
        sty RESB+1

        ldy #$ff
  loop:
        iny
        lda (RESB),y
        sta cwd,y
        bne loop

        lda #<overlay_path
        ldy #>overlay_path
        BRK_KERNEL XPUTCWD

        rts
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;       Modifiées:
;               -
;       Utilisées:
;               -
; Sous-routines:
;       -
;----------------------------------------------------------------------
.proc closedir_overlay
        pha
        phy

        lda #<cwd
        ldy #>cwd
        BRK_KERNEL XPUTCWD

        ply
        pla
        rts
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;       cbp: Pointeur sur la ligne de commande
;
; Sortie:
;       A  : Modifié
;       X,Y: Modifiés
;       C=0: Ok
;       C=1: Erreur (A=code erreur)
;
; Variables:
;	Modifiées:
;		fp
;               Keywords_table
;	Utilisées:
;		file_header
;
; Sous-routines:
;	getfname
;	fopen
;	fread
;	fclose
;----------------------------------------------------------------------
.if 0
.proc ovl_load

	jsr getfname
	bcs error

        jsr opendir_overlay
	fopen dskname, O_RDONLY
        jsr closedir_overlay
	sta fp
	sty fp+1

	ora fp+1
	beq errFopen

	; Lecture du magic word
;	fread Keywords_table, (file_header-2), fp

	; Vérification
;	ldy file_header-1
;  loop:
;	lda Keywords_table,y
;	cmp file_header,y
;	bne errFormat
;	dey
;	bpl loop

	; Lecture du nombre de tokens de la table suivi de la taille de la table
;	fread Keywords_table, #$03, fp

	; Lecture de la table
	; TODO: Ajouter un test sur la taille de la table
	fread __OVERLAY_START__, __OVERLAY_SIZE__, fp

  end:
	fclose (fp)
	clc
	rts

  errFormat:
	fclose (fp)
	lda #e29
	.byte $2c

  errFopen:
	lda #e13
	sec
  error:
	rts
.endproc

;.segment "RODATA"
;	.word $0004
;file_header:
;	.byte "OVL",$80
;.segment "CODE"
.endif


;===========================================================================
;               Gestion des erreurs
;===========================================================================
.segment "CODE"

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
crlf1:
        BRK_KERNEL XCRLF
        rts

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
out1:
        BRK_KERNEL XWR0
        rts

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
.proc prfild
        print dskname, NOSAVE
        rts
.endproc

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
.proc prnamd
        print dskname, NOSAVE
        rts
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;       Modifiées:
;               -
;       Utilisées:
;               -
; Sous-routines:
;       -
;----------------------------------------------------------------------
seter:
seter1:
        rts

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;       Modifiées:
;               -
;       Utilisées:
;               -
; Sous-routines:
;       -
;----------------------------------------------------------------------
svzp:
        rts


;**********************************************************************
; Fin du programme
;**********************************************************************

;----------------------------------------------------------------------
;                               DATAS
;----------------------------------------------------------------------
.segment "RODATA"


        loading_msg:
                .asciiz "Auto-loading "

        overlay_path:
                .asciiz "/USR/SHARE/OVERLAY"

        OVL1_NAME:
                .asciiz "OVERLAY.1"
        OVL2_NAME:
                .asciiz "OVERLAY.2"

; Nom des fichiers overlay
;        OVERLAYS_FNAMES:
;                .word OVL1_NAME
;                .word OVL2_NAME

; Identification des overlays
;        OVERLAYS_NAMES:
;                .word OVL1_NAME
;                .word OVL2_NAME


