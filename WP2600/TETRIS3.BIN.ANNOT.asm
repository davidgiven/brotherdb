;
;This builds with the z88dk assembler:
;z80asm -b  --cpu=z180 TETRIS3.BIN.ANNOT.asm
; -b generates a binary. 
; --cpu = z180 allows using of HD64180 instructions.


;This was initially generated with 
; z80dasm 1.1.3
; command line: z80dasm -a -l -t -b TETRIS3.BIN_0008_5008.BIN.blockfile -g 0x5008 -s TETRIS3.BIN_0008_5008.BIN.symout TETRIS3.BIN_0008_5008.BIN
; the .BIN binary header of 8 bytes was cut off with dd so disassembly could
; start at the first byte.

; BROTHER Wordprocessor 

	org	05000h

;HEADER:
  defb 0x83, 0xc1, 0x01, 0x01
  defb 0x00, 0x00, 'B', 'R'

	nop			;5008	00 	. 
	nop			;5009	00 	. 
	nop			;500a	00 	. 
	jp APL_BEGIN		;500b	c3 84 5a 	. . Z 

REVstr:
	defb	"REV 1.8D",0
KEYS_LOCATION_DATA:  ;There are 18 double-entries here.
; The key-check code below knows only about the keys listed.
  defb	1,80h,6,20h,6,10h,4,80h
	defb	7,4,7,40h,7,80h,3,80h
	defb	7,2,5,80h,2,8,4,4
	defb	4,2,8,8,8,2,4,20h
	defb	3,20h,2,4

; VARIABLES
FIELD_BYTES equ 324  
GAME_FIELD equ 07e68h 
  ; This is the first variable after the end of the program binary.
  ; It should start an array that is 26 rows by 12 wide.
  ; The active game field is 10 wide but the array is 12 wide for
  ; the boundaries to the left and right.  
  ; There is a boundary row at the bottom, for 27 rows total.
  ; It should be 324 bytes and end at 7FACh (for the TETRIS3 binary).
  ; Each row is 12 wide with 1 boundary square (starts at 0), 
  ; 10 game squares, and one more boundary square at 11.
BOTTOM_ROW equ GAME_FIELD+312
GAMEFIELD_2 equ 07fach
  
CURSHAPE   equ 080f0h ; 13 occurrences
CURORIENT  equ 080f1h ; 3 occurrences?
CURBRICK_X  equ 080f2h ; 2 occur. byte  (also accessed as word as CURBRICKXYLH)
CURBRICK_Y  equ 080f3h ; 7 occur.
CURBRICKXYLH  equ 080f2h ; 5 occur as word
; This could be confusing.  First byte is X, 2nd is Y.
; But if loaded into HL, H = Y and L = X.

NEXTSHAPE  equ 080f4h ; 4 occur.
NEXTORIENT equ 080f5h ; 2 occur.
NEXTBRICK_ON equ 080f6h  ;4 occur. byte  

; These several variables are cleared near comment "CLEAR SCORE VARIABLES" 
LINECOUNT_L  equ 080f7h ; Lines count (word) (affects speed?)
LINECOUNT_H  equ 080f8h ;
GAMESCORE1 equ 080f9h  ; Current score!  Word.  8 occur.
GAMESCORE2 equ 080fah  ;1 occurrence
GAMESCORE3 equ 080fbh  ; Third byte of score  5 occur.
; End of variables that get cleared...

STARTHEIGHT equ 080fch  ; 5 occur
LEVELNUMBER equ 080fdh  ; Level number 7 occur
STARTLEVEL equ 080feh  ; 80fe is put into 80fd on game start
                            ; (after ask_level)  6 occur
RANDSD  equ 080ffh  ;Random number generator store  3 occur

KEY_COORDS equ 08101h
KEYSCAN1 equ 08103h ; KEYSCAN1 and KEYSCAN2RND are used for random number generation.
KEYSCAN2RND  equ 08104h
KEYSCANROW equ 08105h ; X or Y, idunno
KEYSCANTABLE  equ 08106h ;Start of keyboard table?
KEYTAB21 equ KEYSCANTABLE+27 ; 
KEYSDETECTED equ KEYSCANTABLE+36 ; CLEARED in CLEAR_KEYBOARD_TABLE (rename)
GAMELOOP_ROWVARIABLE_unknown equ 08133h
TEXTBUF7 equ 08134h ; 8-byte Text buffer for score text conversion
TEXTBUF5 equ 08136h ; 5-digits only
TEXTBUF4 equ 08137h ;4-digit
; End of textbuf is $813B.
SOUNDON  equ 0813dh  ; Sound on/off?
FIELD_NOREDRAW equ 0813eh
BUFFERPOINTER1 equ 0813fh  ; Buffer pointer used for image data manipulations and transfer
 ; This 2-byte word is a pointer to a buffer requested from the system.  
 ; We don't decide where it is; the system does.  This is probably because of
 ; the bank switching (MMU operations) that need to be done to access the
 ; display memory.
 
DEMO_VAR1 equ 08141h ; Swoosh var 1 (tbd)
DEMO_VAR2 equ 08143h ; Swoosh var 2 (tbd)
 
BUFFERPOINTER2 equ 08145h  ; buffer pointer store used in Double_pixels_in_C
PALACEARTVAR equ 08147h
LINE_MSB equ 08158h
ORPHAN_VAR equ 08159h ; orphanfunc variable  (tbd)

DEMOWORKSPACE1 equ 0815bh ; Swoosh 3d image manipulation area 1.  4k.
; There are 96 bytes ($60) between these two, don't know why?  space2-space1=$1060
DEMOWORKSPACE2 equ 091bbh ; Swoosh 3d image manipulation area 2.  4k.

; DISPLAY CONSTANTS:
; For locations supplied in DE; 
; the screen bitmap is 26 * 12 scanlines, by 90 columns of 8-bit bytes.  
; That 
; A "text row" would amount to 90*12 bytes.
SCRNCOLS equ 90
SCANLINE equ 90
SCRNROWS equ 26
FONTHEIGHT equ 12
TXTROW equ SCRNCOLS*FONTHEIGHT
SCRNHEIGHT equ 320
  
; KEYBOARD CONSTANTS for TETRIS-ONLY SCANNING ROUTINE

KEY_UP       equ 000h
KEY_DOWN     equ 001h
KEY_LEFT     equ 002h
KEY_RIGHT    equ 003h
KEY_RET      equ 004h
KEY_SPACE    equ 005h
KEY_SHIFT    equ 006h
KEY_CAPSLOCK equ 007h
KEY_CANCEL   equ 008h
KEY_MENU     equ 009h
KEY_H        equ 00ah
KEY_I        equ 00bh
KEY_J        equ 00ch
KEY_K        equ 00dh
KEY_L        equ 00eh
KEY_N        equ 00fh
KEY_S        equ 010h
  
  
CHECK_FOR_KEY:
	push bc			;503b	c5 	. 
	push hl			;503c	e5 	. 
	ld c,a			;503d	4f 	O 
	ld b,002h		;503e	06 02 	. . 
  mlt bc      ;	defb 0edh,04ch	;5040	ed 4c	. L
	ld hl,(KEY_COORDS)		;5042	2a 01 81 	* . . 
	add hl,bc			;5045	09 	. 
	ld c,(hl)			;5046	4e 	N 
	inc hl			;5047	23 	# 
	ld a,(hl)			;5048	7e 	~ 
	ld b,000h		;5049	06 00 	. . 
	ld hl,KEYSDETECTED		;504b	21 2a 81 	! * . 
	add hl,bc			;504e	09 	. 
	and (hl)			;504f	a6 	. 
	pop hl			;5050	e1 	. 
	pop bc			;5051	c1 	. 
	ret			;5052	c9 	. 
	
ALTERNATE_CHECK_KEY_in_A:
	push bc			;5053	c5 	. 
	push hl			;5054	e5 	. 
	ld c,a			;5055	4f 	O 
	ld b,002h		;5056	06 02 	. . 
  mlt bc      ;	defb 0edh,04ch	;5058	ed 4c	. L
	ld hl,(KEY_COORDS)		;505a	2a 01 81 	* . . 
	add hl,bc			;505d	09 	. 
	ld c,(hl)			;505e	4e 	N 
	inc hl			;505f	23 	# 
	ld a,(hl)			;5060	7e 	~ 
	ld b,000h		;5061	06 00 	. . 
	ld hl,KEYSDETECTED		;5063	21 2a 81 	! * . 
	add hl,bc			;5066	09 	. 
	cpl			;5067	2f 	/      ;  COMPLEMENT A
	and (hl)			;5068	a6 	. 
	ld (hl),a			;5069	77 	w 
	pop hl			;506a	e1 	. 
	pop bc			;506b	c1 	. 
	ret			;506c	c9 	. 

; CLEAR KEYBOARD TABLE:
; This routine is used several times
; and clears what is apparently a keyboard-scanning table.	
CLEAR_KEYBOARD_TABLE:
	push af			;506d	f5 	. 
	ld a,000h		;506e	3e 00 	> . 
	ld (KEYSDETECTED+0),a		;5070	32 2a 81 	2 * . 
	ld (KEYSDETECTED+1),a		;5073	32 2b 81 	2 + . 
	ld (KEYSDETECTED+2),a		;5076	32 2c 81 	2 , . 
	ld (KEYSDETECTED+3),a		;5079	32 2d 81 	2 - . 
	ld (KEYSDETECTED+4),a		;507c	32 2e 81 	2 . . 
	ld (KEYSDETECTED+5),a		;507f	32 2f 81 	2 / . 
	ld (KEYSDETECTED+6),a		;5082	32 30 81 	2 0 . 
	ld (KEYSDETECTED+7),a		;5085	32 31 81 	2 1 . 
	ld (KEYSDETECTED+8),a		;5088	32 32 81 	2 2 . 
	pop af			;508b	f1 	. 
	ret			;508c	c9 	. 
	
SCAN_KEYBOARD:
	push af			;508d	f5 	. 
	push bc			;508e	c5 	. 
	push de			;508f	d5 	. 
	push ix		;5090	dd e5 	. .                    
	ld a,(KEYSCANROW)		;5092	3a 05 81 	: . .      ; Get current row from previous keyboard scan
	ld e,a			;5095	5f 	_                        ; E = A (save it)
	ld d,000h		;5096	16 00 	. .                  ; D = 0
	ld ix,KEYSCANTABLE		;5098	dd 21 06 81 	. ! . .  ; THIS IS A TABLE WITH AS MANY AS $24+8=44 ENTRIES.
	add ix,de		;509c	dd 19 	. .                  ; OFFSET IX by the current scanning row.
	in a,(0b8h)		;509e	db b8 	. .                ; KEYBOARD Input = KI7..KI0

	cpl			;50a0	2f 	/                            ; A = COMPLEMENT A.  

	                                               ; It's probably 1's when a key is pressed,
	                                               ; but I haven't confirmed this.

	                                               ; This next part all looks like a DEBOUNCE scheme.
	ld (ix+000h),a		;50a1	dd 77 00 	. w .        ; SAVE the keyscan row data to IX[0]
	and (ix+009h)		;50a4	dd a6 09 	. . .          ; AND it with IX[9]  
	and (ix+012h)		;50a7	dd a6 12 	. . .          ; AND it with IX[18] 
	or (ix+01bh)		;50aa	dd b6 1b 	. . .          ; OR  it with IX[27] 
	ld b,a			;50ad	47 	G                        ; B = A
	ld a,(ix+000h)		;50ae	dd 7e 00 	. ~ .        ; A = IX[0]
	or (ix+009h)		;50b1	dd b6 09 	. . .          ; OR it with IX[9]
	or (ix+012h)		;50b4	dd b6 12 	. . .          ; OR it with IX[18]
	and b			;50b7	a0 	.                          ; AND it with the previous product/sum
	ld b,a			;50b8	47 	G                        ; B = A 
	ld a,(ix+01bh)		;50b9	dd 7e 1b 	. ~ .        ; A = IX[27]
	cpl			;50bc	2f 	/                            ; A = COMPLEMENT A
	and b			;50bd	a0 	.                          ; A = A AND B
	or (ix+024h)		;50be	dd b6 24 	. . $          ; A = A OR IX[36]

	ld (ix+024h),a		;50c1	dd 77 24 	. w $        ; IX[36] = A

	                                               ; This next part is saying IX[18] = IX[9], 
	                                               ;    IX[9]=IX[0], which is like a delay line .
	                                               ; This leaves IX[0] open for the next cycle.
	ld a,(ix+009h)		;50c4	dd 7e 09 	. ~ .        ; A = IX[9]
	ld (ix+012h),a		;50c7	dd 77 12 	. w .        ; IX[18] = A
	ld a,(ix+000h)		;50ca	dd 7e 00 	. ~ .        ; A = IX[0]
	ld (ix+009h),a		;50cd	dd 77 09 	. w .        ; IX[9] = A

	ld (ix+01bh),b		;50d0	dd 70 1b 	. p .        ; IX[27] = B
	
	ld de,(KEYSCAN1)		;50d3	ed 5b 03 81 	. [ . . ; DE = KEYSCAN1
	inc de			;50d7	13 	.               ; This incrementing is a part of RANDOM NUMBER generation.
	ld (KEYSCAN1),de		;50d8	ed 53 03 81 	. S . . ; Stow KEYSCAN1++.
	ld a,(KEYSCANROW)		;50dc	3a 05 81 	: . .       ; A = KEYSCANROW
	inc a			;50df	3c 	<                           ; KEYSCANROW++. Scan next row.
	cp 009h		;50e0	fe 09 	. .                     ; set flags on A-9
	jr c,sk_nextrow		;50e2	38 01 	8 .             ; if A<=9 then don't zero.
	xor a			;50e4	af 	.                           ; A = 0, if A=10.
sk_nextrow:
	ld (KEYSCANROW),a		;50e5	32 05 81 	2 . .       ; Stow KEYSCANROW
	out (0b8h),a		;50e8	d3 b8 	. .               ; KEYBOARD Output = KX3..KX0
	pop ix		;50ea	dd e1 	. . 
	pop de			;50ec	d1 	. 
	pop bc			;50ed	c1 	. 
	pop af			;50ee	f1 	. 
	ret			;50ef	c9 	. 
	
;DELAY LOOPS:
; The first delay loop doesn't check for specific keys or interpret them.  
; It just scans.
; 
; The 2nd delay loop checks for S or MENU.  S changes sound, MENU pauses the game.
; CANCEL also exits the game during this delay loop.
;
; ARGUMENTS: BC is the number of units to delay.
; I don't know as yet if the timing of this is controlled, 
; such as by a system timer.  I think it's just a cpu loop.
DELAY_BC_and_KEYSCAN_NO_EXIT:
	call SCAN_KEYBOARD		;50f0	cd 8d 50 	. . P 
	dec bc			;50f3	0b 	. 
	ld a,b			;50f4	78 	x 
	or c			;50f5	b1 	. 
	jr nz,DELAY_BC_and_KEYSCAN_NO_EXIT		;50f6	20 f8 	  . ; LOOP until BC = 0
	ret			;50f8	c9 	. 

DELAY_BC_and_KEYSCAN_Pausable:
	call SCAN_KEYBOARD		;50f9	cd 8d 50 	. . P 
	call INTERPRET_KEY		;50fc	cd ae 59 	. . Y 
	dec bc			;50ff	0b 	. 
	ld a,b			;5100	78 	x 
	or c			;5101	b1 	. 
	jr nz,DELAY_BC_and_KEYSCAN_Pausable	;5102	20 f5 	  . ; LOOP until BC = 0
	ret			;5104	c9 	. 
	
	
; Looks like DE supplies a pointer to four variables, 
; which may be in an array.  Seems like this
; is a four-byte note element.  
; If the 2nd item (b) is zero, then end the little tune.
; Otherwise, keep incrementing and play another note.
PLAY_MELODY:
	push af			;5105	f5 	. 
	push bc			;5106	c5 	. 
	push de			;5107	d5 	. 
	push hl			;5108	e5 	. 
pm_NextNote:
	ex de,hl			;5109	eb 	. 
	ld c,(hl)			;510a	4e 	N 
	inc hl			;510b	23 	# 
	ld b,(hl)			;510c	46 	F 
	inc hl			;510d	23 	# 
	ld e,(hl)			;510e	5e 	^ 
	inc hl			;510f	23 	# 
	ld d,(hl)			;5110	56 	V 
	inc hl			;5111	23 	# 
	ld a,b			;5112	78 	x 
	or c			;5113	b1 	. 
	jr z,pm_EndMelody		;5114	28 06 	( . 
	call MAKE_SOUND		;5116	cd 21 51 	. ! Q 
	ex de,hl			;5119	eb 	. 
	jr pm_NextNote		;511a	18 ed 	. . 
pm_EndMelody:
	pop hl			;511c	e1 	. 
	pop de			;511d	d1 	. 
	pop bc			;511e	c1 	. 
	pop af			;511f	f1 	. 
	ret			;5120	c9 	. 
	
	
; THIS IS THE SOUND GENERATION ROUTINE.
;Looks like it accepts "period" in bc
MAKE_SOUND:
	push hl			;5121	e5 	. 
	ld hl,00000h		;5122	21 00 00 	! . . 
mksnd_loop:
	add hl,de			;5125	19 	. 
	ld a,h			;5126	7c 	| 
	rlca			;5127	07 	. 
	ld a,000h		;5128	3e 00 	> . 
	sbc a,000h		;512a	de 00 	. .     ; CARRY, so subtract either 1 or zero.
	and 000h		;512c	e6 00 	. .       ;
	or 000h		;512e	f6 00 	. .         ;I don't know how this works.
	out (0f0h),a		;5130	d3 f0 	. .   ; TOGGLE BUZZER
	dec bc			;5132	0b 	.             ;Countdown bc
	ld a,b			;5133	78 	x 
	or c			;5134	b1 	.    ; these two lines were to check for bc not zero
	jr nz,mksnd_loop		;5135	20 ee 	  . 
	ld a,000h		;5137	3e 00 	> . 
	out (0f0h),a		;5139	d3 f0 	. .   ; TOGGLE BUZZER
	pop hl			;513b	e1 	. 
	ret			;513c	c9 	. 


Sound_Status_Indicate:
	push af			;513d	f5 	. 
	push de			;513e	d5 	. 
	ld a,002h		;513f	3e 02 	> . 
	ld (0512dh),a		;5141	32 2d 51 	2 - Q 
	ld a,(SOUNDON)		;5144	3a 3d 81 	: = .   
	cpl			;5147	2f 	/                 ;INVERT A.    ; COMPLEMENT
	or 0fdh		;5148	f6 fd 	. . 
	ld (0512fh),a		;514a	32 2f 51 	2 / Q 
	ld a,0ffh		;514d	3e ff 	> . 
	ld (05138h),a		;514f	32 38 51 	2 8 Q 
	ld (05124h),a		;5152	32 24 51 	2 $ Q 
	ld a,(SOUNDON)		;5155	3a 3d 81 	: = .   ;SOUNDON is sound on/off, I think.
	or a			;5158	b7 	.                  ;Test it
	ld a,020h		;5159	3e 20 	>            ;This is a space.
	jr z,ssi_NoSound		;515b	28 02 	( .    ;Depending on whether we want sound...
	ld a,05ch		;515d	3e 5c 	> \          ;This is the musical note character!
ssi_NoSound:           
	ld de,25*TXTROW+1  ;515f	11 79 69 	. y i  ;Lower left corner, for musical note.
	call DisplayChar		;5162	cd ee 51 	. . Q  ;Put a musical note or space in corner
	pop de			;5165	d1 	. 
	pop af			;5166	f1 	. 
	ret			;5167	c9 	. 
	
	
; Generate a random number 
; Supply A as non-inclusive upper range.  (for 0-3, supply A=4)
; Return A as random number.
GetRandomNum:
	push bc			;5168	c5 	. 
	push de			;5169	d5 	. 
	push hl			;516a	e5 	. 
	push af			;516b	f5 	. 
	ld bc,(RANDSD)		;516c	ed 4b ff 80 	. K . . 
	ld hl,03d09h		;5170	21 09 3d 	! . = 
	ld d,b			;5173	50 	P 
	ld e,l			;5174	5d 	] 
  mlt de      ;	defb 0edh,05ch	;5175	ed 5c	. \
	ld a,e			;5177	7b 	{ 
	ld d,c			;5178	51 	Q 
	ld e,h			;5179	5c 	\ 
  mlt de      ;	defb 0edh,05ch	;517a	ed 5c	. \
	add a,e			;517c	83 	. 
	ld d,c			;517d	51 	Q 
	ld e,l			;517e	5d 	] 
  mlt de      ;	defb 0edh,05ch	;517f	ed 5c	. \
	add a,d			;5181	82 	. 
	ld d,a			;5182	57 	W 
	inc de			;5183	13 	. 
	ld (RANDSD),de		;5184	ed 53 ff 80 	. S . . 
	pop af			;5188	f1 	. 
	ld l,e			;5189	6b 	k 
	ld h,a			;518a	67 	g     ; Get input scale so we can scale random number
	ld e,a			;518b	5f 	_     ; Get input scale so we can scale random number
  mlt hl
  mlt de      ;	defb 0edh,05ch	;518e	ed 5c	. \
	ld l,h			;5190	6c 	l 
	ld h,000h		;5191	26 00 	& . 
	add hl,de			;5193	19 	. 
	ld a,h			;5194	7c 	| 
	pop hl			;5195	e1 	. 
	pop de			;5196	d1 	. 
	pop bc			;5197	c1 	. 
	ret			;5198	c9 	. 


StrangeBase11_Math_whatisit:
	push hl			;5199	e5 	. 
	ld a,000h		;519a	3e 00 	> . 
	ld h,010h		;519c	26 10 	& . 
sb11_notzero:
	sla c		;519e	cb 21 	. ! 
	rl b		;51a0	cb 10 	. . 
	rl a		;51a2	cb 17 	. . 
	sub 00bh		;51a4	d6 0b 	. . 
	inc c			;51a6	0c 	. 
	jr nc,sb11_nocarry		;51a7	30 03 	0 . 
	add a,00bh		;51a9	c6 0b 	. . 
	dec c			;51ab	0d 	. 
sb11_nocarry:
	dec h			;51ac	25 	% 
	jr nz,sb11_notzero		;51ad	20 ef 	  . 
	pop hl			;51af	e1 	. 
	ret			;51b0	c9 	. 

;Convert number to decimal text? I didn't figure out how this works, 
;but it's only used in Int24ToText.
GetDecimalDigit: 
	push hl			;51b1	e5 	. 
	ld a,000h		;51b2	3e 00 	> . 
	ld h,018h		;51b4	26 18 	& . 
gdd_notzero:
	sla d		;51b6	cb 22 	. " ;Multiply by two
	rl c		;51b8	cb 11 	. . ;with carry, by 
	rl b		;51ba	cb 10 	. . ;shifting left.
	rl a		;51bc	cb 17 	. . ;
	sub 00ah		;51be	d6 0a 	. .   ; Subtract 10.
	inc d			;51c0	14 	.           ; D++
	jr nc,gdd_nocarry		;51c1	30 03 	0 . 
	add a,00ah		;51c3	c6 0a 	. . 
	dec d			;51c5	15 	. 
gdd_nocarry:
	dec h			;51c6	25 	% 
	jr nz,gdd_notzero		;51c7	20 ed 	  . 
	pop hl			;51c9	e1 	. 
	ret			;51ca	c9 	. 


; The 24-bit number given in abc will be converted to text.
; The output buffer is at TEXTBUF7 (at the moment)
; Remarkably, 9999999 is very close to consuing 24 bits of binary.
Int24ToText:
	push bc			;51cb	c5 	. 
	push de			;51cc	d5 	. 
	push hl			;51cd	e5 	. 
	ld d,c			;51ce	51 	Q 
	ld c,b			;51cf	48 	H 
	ld b,a			;51d0	47 	G 
	ld hl,TEXTBUF7		;51d1	21 34 81 	! 4 . 
	ld a,008h		;51d4	3e 08 	> .     ; 8 digits, fill 7 with spaces.
ds3_morespaces:
	ld (hl),020h		;51d6	36 20 	6   ; Put a space 
	inc hl			;51d8	23 	#           ; Shift over
	dec a			;51d9	3d 	=             ; max 8 digits
	jr nz,ds3_morespaces  ;51da	20 fa . ; next digit.  Fill 8 digits with spaces. 
	ld (hl),000h		;51dc	36 00 	6 . ;Terminate string with zero.
ds3_moredigits:
	call GetDecimalDigit		;51de	cd b1 51 	. . Q   ;Convert number to decimal?
	add a,030h		;51e1	c6 30 	. 0     ;      ; add $30 to get ASCII digit
	dec hl			;51e3	2b 	+                   ; Work backwards from least significant digit.
	ld (hl),a			;51e4	77 	w                 ; Store the digit.
	ld a,b			;51e5	78 	x                   ; Check to see if we have >0
	or c			;51e6	b1 	.                     ; ...
	or d			;51e7	b2 	.                     ; ...
	jr nz,ds3_moredigits		;51e8	20 f4   .   ; and if not zero, more digits.
	pop hl			;51ea	e1 	. 
	pop de			;51eb	d1 	. 
	pop bc			;51ec	c1 	. 
	ret			;51ed	c9 	. 

;Put a single character to the screen.
; DE = location on screen AS BYTE OFFSET IN BITMAP 1080*row + column
; A = character
DisplayChar:
	push af			;51ee	f5 	. 
	push bc			;51ef	c5 	. 
	push de			;51f0	d5 	. 
	push hl			;51f1	e5 	. 
	ld c,a			;51f2	4f 	O                     ; A is the character, now C is holding it
	ld b,00ch		;51f3	06 0c 	. .               ; B = 12
  mlt bc    ;	defb 0edh,04ch	;51f5	ed 4c	. L ; Multiply character code by 12 to get font data offset
	ld hl,font1_start		;51f7	21 bc 65 	! . e   ; Start at our font
	add hl,bc			;51fa	09 	.                   ; Add offset to starting point
	push hl			;51fb	e5 	.                     ; Save offset
	ex de,hl			;51fc	eb 	.                   ; DE = character bitmap pointer, CHARACTER ADDRESS
	ld a,SCRNCOLS		;51fd	3e 5a 	> Z               ; A = 90
	call GET_COL_inA_from_SCRNINDEX_trashHL_F		;51ff	cd c8 58 	. . X  ; RETURN A = "X" and HL = "Y"
	ld b,h			;5202	44 	D               ; THIS SETS BD TO SCREEN MEMORY LOCATION
	ld d,l			;5203	55 	U               ; 
	ld e,a			;5204	5f 	_               ; THIS SETS E to X
	ld c,001h		;5205	0e 01 	. .         ; C = 1, SCREEN COPY 1 BYTE 
	pop hl			;5207	e1 	.               ; HL is CHARACTER IN MEMORY
	ld a,00ch		;5208	3e 0c 	> .         ; 12 ROWS / 12 BYTES.
dc_nextrow:
	push af			;520a	f5 	. 
	push hl			;520b	e5 	.               ; Save character pointer
	ld a,(hl)			;520c	7e 	~             ; GET BYTE OF CHARACTER IMAGE (SOURCE)
	ld hl,(BUFFERPOINTER1)		;520d	2a 3f 81 	* ? . 
	ld (hl),a			;5210	77 	w             ; STORE BYTE INTO TRANSFER BUFFER
	ld a,028h		;5211	3e 28 	> (         ; SCREEN COPY C BYTES
	rst 10h			;5213	d7 	.               ; DO IT: BD = target location, HL=source, C=count
	di			;5214	f3 	.                   ; DISABLE INTERRUPTS 
	inc d			;5215	14 	.       ;         ;
	jr nz,dc_nocarry		;5216	20 01 	  . ;
	inc b			;5218	04 	.                 ;
dc_nocarry:
	pop hl			;5219	e1 	. 
	inc hl			;521a	23 	# 
	pop af			;521b	f1 	.               ;
	dec a			;521c	3d 	=                 ; COUNT DOWN 12 LINES
	jr nz,dc_nextrow		;521d	20 eb 	  . ;
	pop hl			;521f	e1 	. 
	pop de			;5220	d1 	. 
	pop bc			;5221	c1 	. 
	pop af			;5222	f1 	. 
	ret			;5223	c9 	. 
	
	
; DISPLAY STRING.
; String pointer in HL.
; Location in DE.  (Not sure how this is defined.)
; Seems like location is +$438 or 1080 per row/line.  90 char wide screen.
; Increment by 1 to get next column (next character horizontally).

DisplayString:  
	push de			;5224	d5 	. 
	push hl			;5225	e5 	. 
ds_nextchar:
	ld a,(hl)		;5226	7e 	~    ;Load another character
	or a			  ;5227	b7 	.    ;Test
	jr z,ds_end	;5228	28 07 ( .;If zero, end of string, return. 
	call DisplayChar	;522a	cd ee 51 	. . Q ;Put a single character up.  Char in A.
	inc hl			;522d	23 	#    ;Increment pointer to character
	inc de			;522e	13 	.    ;Increment screen location
	jr ds_nextchar	;522f	18 f5 	. . 
ds_end:
	pop hl			;5231	e1 	. 
	pop de			;5232	d1 	. 
	ret			    ;5233	c9 	. 
	
DrawNextBoxAndFieldBorder:
	ld de, 5*TXTROW+53		;5234	11 4d 15 	. M . ; Screen location in DE
	ld hl,NEXTBOXstr		  ;5237	21 44 7e 	! D ~ ; NEXT string
	call DisplayString		;523a	cd 24 52 	. $ R ;
	ld de, 6*TXTROW+53		;523d	11 85 19 	. . . ; Screen location in DE
	ld hl,NEXTBOXSIDELINESstr ;5240	21 4f 7e 	! O ~ ; This will be used 4 times
	call DisplayString		;5243	cd 24 52 	. $ R ; once,
	ld de, 7*TXTROW+53		;5246	11 bd 1d 	. . . ; down one line
	call DisplayString		;5249	cd 24 52 	. $ R ; 2nd,
	ld de, 8*TXTROW+53		;524c	11 f5 21 	. . ! ; down one line
	call DisplayString		;524f	cd 24 52 	. $ R ; 3rd,
	ld de, 9*TXTROW+53		;5252	11 2d 26 	. - & ; down one line
	call DisplayString		;5255	cd 24 52 	. $ R ; 4th.
	ld de, 10*TXTROW+53		;5258	11 65 2a 	. e * ; Bottom line location
	ld hl,NEXTBOXBOTTOMLINEstr	;525b	21 5a 7e 	! Z ~ ;  Bottom line. 
	call DisplayString		;525e	cd 24 52 	. $ R ; 
	ld c,007h		;5261	0e 07 	. .              ; C = 7
	ld d,000h		;5263	16 00 	. .              ; D = 0
	ld b,000h		;5265	06 00 	. .              ; B = 0
	ld a,01ah		;5267	3e 1a 	> .              ; A = 26 (ROWS)
dnt_nextborderrow:
	push af			;5269	f5 	.                    ; Save A
	ld e,017h		;526a	1e 17 	. .              ; E = 23.  Left border position
	call DrawBorder1char ;526c	cd 3a 53 	. : S; This puts left border around playfield
	ld e,02ch		;526f	1e 2c 	. ,              ; E = 44   Right border position
 	call DrawBorder1char ;5271	cd 3a 53 	. : S; This puts right border around playfield
  
	ld a,00ch		;5274	3e 0c 	> .              ; A = 12
	add a,d			;5276	82 	.                    ; A = D + A
	ld d,a			;5277	57 	W                    ; BD = BD + A
	jr nc,dnt_nocarry		;5278	30 01 	0 .          ; Carry 
	inc b			;527a	04 	.                      ; BD = BD + A
dnt_nocarry:
	pop af			;527b	f1 	. 
	dec a			;527c	3d 	=                      ; COUNT 26 ROWS
	jr nz,dnt_nextborderrow		;527d	20 ea 	  . 
	ret			;527f	c9 	. 

InitGameScreenScoresAndWhatelse:
	call ShowScore		;5280	cd 8d 52 	. . R 
	call ShowLevel		;5283	cd b9 52 	. . R 
	call ShowLinesCount		;5286	cd ce 52 	. . R 
	call ShowHighScore_And_whatelse		;5289	cd ea 52 	. . R 
	ret			;528c	c9 	. 
ShowScore:
	ld de,5*TXTROW+5		;528d	11 1d 15 	. . . ; Location on Screen.
	ld hl,SCOREstr		  ;5290	21 be 7d 	! . } 
	call DisplayString		;5293	cd 24 52 	. $ R 
UpdateScore:
	ld bc,(GAMESCORE1)		;5296	ed 4b f9 80 	. K . .  ; bc=SCORE
	ld a,(GAMESCORE3)		;529a	3a fb 80 	: . .  
	call Int24ToText		;529d	cd cb 51 	. . Q 
	ld de,5*TXTROW+13 ;52a0	11 25 15 	. % . ; Location on Screen.
	ld hl,TEXTBUF5		;52a3	21 36 81 	! 6 . ; Offset 2 bytes, start at 5-digit score
	ld a,020h		;52a6	3e 20 	>           ; This following bit is to left-justify.
	cp (hl)			;52a8	be 	.           
	jr z,sc_justifd		;52a9	28 09 	( .   ; If this is a space, finished.
	dec hl			;52ab	2b 	+               ; If not a space, decrement hl
	cp (hl)			;52ac	be 	.               
	jr z,sc_justifd		;52ad	28 05 	( .   ; If this is a space, finished.
	dec hl			;52af	2b 	+               ; If not a space, decrement hl
	cp (hl)			;52b0	be 	. 
	jr z,sc_justifd		;52b1	28 01 	( .   ; If this is a space, finished.
	dec hl			;52b3	2b 	+               ; If not a space, decrement hl
sc_justifd:
	inc hl			;52b4	23 	# 
	call DisplayString		;52b5	cd 24 52 	. $ R 
	ret			;52b8	c9 	. 

ShowLevel:
	ld de,9*TXTROW+5	;52b9	11 fd 25 	. . % 
	ld hl,LEVELstr		;52bc	21 c7 7d 	! . } 
	call DisplayString		;52bf	cd 24 52 	. $ R 
UpdateLevel:
	ld a,(LEVELNUMBER)		;52c2	3a fd 80 	: . . 
	add a,030h		;52c5	c6 30 	. 0 
	ld de,9*TXTROW+17		;52c7	11 09 26 	. . & 
	call DisplayChar		;52ca	cd ee 51 	. . Q 
	ret			;52cd	c9 	. 
	
ShowLinesCount:
	ld de,7*TXTROW+5		;52ce	11 8d 1d 	. . .   ; Location on Screen.
	ld hl,LINESstr		;52d1	21 d0 7d 	! . } 
	call DisplayString		;52d4	cd 24 52 	. $ R 
UpdateLinesCount:
	ld bc,(LINECOUNT_L)		;52d7	ed 4b f7 80 	. K . . 
	ld a,000h		;52db	3e 00 	> . 
	call Int24ToText		;52dd	cd cb 51 	. . Q 
	ld de,7*TXTROW+13		;52e0	11 95 1d 	. . .   ; Location on Screen.
	ld hl,TEXTBUF4	  	;52e3	21 37 81 	! 7 .   ; Offset in buffer for fewer digits.
	call DisplayString		;52e6	cd 24 52 	. $ R 
	ret			;52e9	c9 	. 

ShowHighScore_And_whatelse:
	ld de,3*TXTROW+2		;52ea	11 aa 0c 	. . . ; Location on Screen.
	ld hl,HISCOREstr		;52ed	21 b2 7d 	! . } 
	call DisplayString		;52f0	cd 24 52 	. $ R 
UpdateHighScore:
	ld bc,(HIGHSCORE1)		;52f3	ed 4b 65 7e 	. K e ~ 
	ld a,(HIGHSCORE2)		;52f7	3a 67 7e 	: g ~ 
	call Int24ToText		;52fa	cd cb 51 	. . Q 
	ld de,3*TXTROW+13		;52fd	11 b5 0c 	. . . ; Location on Screen.
	ld hl,TEXTBUF5		;5300	21 36 81 	! 6 . ; Offset 2 bytes, start at 5-digit score
	ld a,020h		;5303	3e 20 	>   ; Left justify the high score.
	cp (hl)			;5305	be 	. 
	jr z,shs_justifd		;5306	28 09 	( . 
	dec hl			;5308	2b 	+ 
	cp (hl)			;5309	be 	. 
	jr z,shs_justifd		;530a	28 05 	( . 
	dec hl			;530c	2b 	+ 
	cp (hl)			;530d	be 	. 
	jr z,shs_justifd		;530e	28 01 	( . 
	dec hl			;5310	2b 	+ 
shs_justifd:
	inc hl			;5311	23 	# 
	call DisplayString		;5312	cd 24 52 	. $ R 
	ret			;5315	c9 	. 
	
; Add 1 to the score, done once for each row fallen when dropping hard.
; That's a game element; you get more points for your better aim.
; These are long procedures because the score has 3 bytes.
GAMESCORE_Add_One:
	push hl			;5316	e5 	. 
	ld hl,GAMESCORE1		;5317	21 f9 80 	! . . 
	inc (hl)			;531a	34 	4          ;  ADD ONE TO SCORE 
	jr nz,ao_nocarry		;531b	20 06 	  .  ; CARRY
	inc hl			;531d	23 	# 
	inc (hl)			;531e	34 	4 
	jr nz,ao_nocarry		;531f	20 02 	  .  ; CARRY
	inc hl			;5321	23 	# 
	inc (hl)			;5322	34 	4 
ao_nocarry:
	pop hl			;5323	e1 	. 
	ret			;5324	c9 	. 
	
; Subtract 1 from score, to counteract adding one in some circumstance
; These are long procedures because the score has 3 bytes.
GAMESCORE_Subtract1:
	push af			;5325	f5 	. 
	push hl			;5326	e5 	. 
	ld a,0ffh		;5327	3e ff 	> . 
	ld hl,GAMESCORE1		;5329	21 f9 80 	! . .  ; THE SCORE. 
	dec (hl)			;532c	35 	5                  ; --GAMESCORE1;
	cp (hl)			;532d	be 	.                    ; Do 255-(--GAMESCORE1) for flags
	jr nz,sub1_noborrow		;532e	20 07 	  .    ; Borrow
	inc hl			;5330	23 	#                    ;
	dec (hl)			;5331	35 	5                  ;
	cp (hl)			;5332	be 	.                    ;
	jr nz,sub1_noborrow		;5333	20 02 	  .    ;
	inc hl			;5335	23 	#                    ;
	dec (hl)			;5336	35 	5                  ;
sub1_noborrow:
	pop hl			;5337	e1 	. 
	pop af			;5338	f1 	. 
	ret			;5339	c9 	. 

; DRAW BORDER.  
; PARAMETER E = $17 or $2C as above
; D = 0.
; C = character = 7
; BDE:  BD = Y, E = X
; This works with the font.
DrawBorder1char:
	push de			;533a	d5 	. 
	push hl			;533b	e5 	. 
	push bc			;533c	c5 	. 
	ld b,00ch		;533d	06 0c 	. .                  ; B = 12
  mlt bc      ;	defb 0edh,04ch	;533f	ed 4c	. L  ; BC = C * 12.  C = character.
	ld hl,font1_start		;5341	21 bc 65 	! . e      ;
	add hl,bc			;5344	09 	.                      ;
	pop bc			;5345	c1 	.                        ; Save BC
	push bc			;5346	c5 	.                        ;
	ld c,001h		;5347	0e 01 	. .                  ; C = 1
	ld a,00ch		;5349	3e 0c 	> .                  ; A = 12
drb_next:
	push af			;534b	f5 	.                        ;
	push hl			;534c	e5 	.                        ;
	ld a,(hl)			;534d	7e 	~                      ;
	ld hl,(BUFFERPOINTER1)		;534e	2a 3f 81 	* ? .;
	ld (hl),a			;5351	77 	w                      ; 
	ld a,028h		;5352	3e 28 	> (                  ; Buffer -> SCREEN COPY RST 10 function
	rst 10h			;5354	d7 	.                        ;
	di			;5355	f3 	.                            ; DISABLE INTERRUPTS 
	inc d			;5356	14 	.                          ; D++ is lsb of BD, the Y value.
	jr nz,drb_nocarry		;5357	20 01 	  .          ;
	inc b			;5359	04 	.                          ;
drb_nocarry:
	pop hl			;535a	e1 	.                        ;
	inc hl			;535b	23 	#                        ;
	pop af			;535c	f1 	.                        ;
	dec a			;535d	3d 	=                          ; Next scanline up.
	jr nz,drb_next		;535e	20 eb 	  .            ; Repeat until zero.  12 scan lines.
	pop bc			;5360	c1 	.                        ;
	pop hl			;5361	e1 	.                        ;
	pop de			;5362	d1 	.                        ;
	ret			;5363	c9 	.                            ;
	
; ARGUMENTS:
; Location?
; B is square index / shape number
Display_SQUARE:
	push bc			;5364	c5 	. 
	push de			;5365	d5 	. 
	push hl			;5366	e5 	. 
	ld c,b			;5367	48 	H 
	ld h,000h		;5368	26 00 	& . 
	ld l,d			;536a	6a 	j 
	ld a,00ch		;536b	3e 0c 	> .       ;
	call Multiply_HL_by_A_to_BCHL	    ;536d	cd ee 58 	. . X ;
	ld d,l			;5370	55 	U             ;
	ld b,h			;5371	44 	D             ;
	ld l,016h		;5372	2e 16 	. .       ; L = 22
	ld a,e			;5374	7b 	{ 
	add a,a			;5375	87 	. 
	add a,l			;5376	85 	. 
	ld e,a			;5377	5f 	_ 
	ld hl,blanksquare_image		;5378	21 bc 6b 	! . k  ; Point hl to an empty square (zeroes)
	ld a,c			;537b	79 	y 
	cp 020h		;537c	fe 20 	.                        ; Are we erasing this square?  
	jr z,dispsq_nolookup		;537e	28 0b 	( .        ; If so, skip looking up the image.
	push bc			;5380	c5 	. 
	ld b,a			;5381	47 	G 
	ld c,018h		;5382	0e 18 	. .                    ; MULTIPLY A->B by 24.  Each square uses 24 bytes.
  mlt bc      ;	defb 0edh,04ch	;5384	ed 4c	. L
	ld hl,SQUARE_PATTERNS	;5386	21 dc 6b 	! . k 
	add hl,bc			;5389	09 	.     ; 
	pop bc			;538a	c1 	. 
dispsq_nolookup:
	ld a,00ch		;538b	3e 0c 	> .       ; A = 12. 
dispsq_nextline:
	push af			;538d	f5 	. 
	push de			;538e	d5 	. 
	ld a,(hl)			;538f	7e 	~ 
	ld de,(BUFFERPOINTER1)		;5390	ed 5b 3f 81 	. [ ? . 
	ld (de),a			;5394	12 	. 
	inc hl			;5395	23 	# 
	inc de			;5396	13 	. 
	ld a,(hl)			;5397	7e 	~ 
	ld (de),a			;5398	12 	. 
	pop de			;5399	d1 	. 
	inc hl			;539a	23 	# 
	push hl			;539b	e5 	. 
	ld c,002h		;539c	0e 02 	. .       ; TRANSFER TWO BYTES TO VIDEO MEMORY
	ld a,028h		;539e	3e 28 	> (             ; SCREEN COPY
	ld hl,(BUFFERPOINTER1)		;53a0	2a 3f 81 	* ? . 
	rst 10h			;53a3	d7 	. 
	di			;53a4	f3 	.                     ; DISABLE INTERRUPTS 
	pop hl			;53a5	e1 	. 
	inc d			;53a6	14 	. 
	jr nz,dispsq_nocarry		;53a7	20 01 	  . 
	inc b			;53a9	04 	. 
dispsq_nocarry:
	pop af			;53aa	f1 	. 
	dec a			;53ab	3d 	= 
	jr nz,dispsq_nextline		;53ac	20 df 	  .   ; Box has 12 lines (of 16 pixels)
	pop hl			;53ae	e1 	. 
	pop de			;53af	d1 	. 
	pop bc			;53b0	c1 	. 
	ret			;53b1	c9 	. 
	

; RENDER BRICK
; If B=$20, draw spaces.
; Otherwise supply B=shape number 0-6

DRAW_BRICK:  
	push af			;53b2	f5 	. 
	push bc			;53b3	c5 	. 
	push de			;53b4	d5 	. 
	push hl			;53b5	e5 	. 
	ld c,000h		;53b6	0e 00 	. . 
nextsquare1:
	call GetBrickSubsquarePosition		;53b8	cd e7 53 	. . S 
	call Display_SQUARE		;53bb	cd 64 53 	. d S 
	inc c			;53be	0c 	.           ;Next square of this brick
	ld a,c			;53bf	79 	y 
	cp 004h		;53c0	fe 04 	. .     ;Stop after 4th square of this brick
	jp c,nextsquare1		            ;53c2	da b8 53 	. . S 
	pop hl			;53c5	e1 	. 
	pop de			;53c6	d1 	. 
	pop bc			;53c7	c1 	. 
	pop af			;53c8	f1 	. 
	ret			;53c9	c9 	. 
	
; Check Brick Collision
; Tests each square of the current brick, to see
; if it hits any other bricks or the walls.
; RETURN boolean variable A.  Zero if no collision.  >0 if collision.
CheckBrickCollision:   
	push bc			;53ca	c5 	. 
	push de			;53cb	d5 	. 
	push hl			;53cc	e5 	. 
	ld b,000h		;53cd	06 00 	. . 
	ld c,000h		;53cf	0e 00 	. . 
nextsquare2:
	call GetBrickSubsquarePosition		;53d1	cd e7 53 	. . S 
	call SQUAREYX_DE_MEMLOC_to_HL		;53d4	cd e5 5d 	. . ] 
	ld a,(hl)			;53d7	7e 	~       ; This square is pointed to by HL
	or b			;53d8	b0 	. 
	ld b,a			;53d9	47 	G         ; B is a running or-tally of brick presence (bit 7)
	inc c			;53da	0c 	.           ; NEXT Subsquare
	ld a,c			;53db	79 	y         ; Into A To compare...
	cp 004h		;53dc	fe 04 	. .     ; There are 4 subsquares for each brick
	jp c,nextsquare2	  ;53de	da d1 53 	. . S 
	ld a,b			;53e1	78 	x         ; A = B
	or a			;53e2	b7 	.           ; Set flags
	pop hl			;53e3	e1 	. 
	pop de			;53e4	d1 	. 
	pop bc			;53e5	c1 	. 
	ret			;53e6	c9 	. 
	
; This seems to be where we select the tetromino brick shape from the list
; SUPPLY c as the current subsquare of the given shape
; Return DE as D, E with Y/X (or x/y, I'm not sure)
GetBrickSubsquarePosition:
	push hl			;53e7	e5 	. 
	ld a,(CURSHAPE)		;53e8	3a f0 80 	: . . ; a= current shape 
	add a,a			;53eb	87 	. 
	add a,a			;53ec	87 	. ;Multiply A by 4 (four tetrads)
	ld l,a			;53ed	6f 	o ; L is now the current shape number * 4 
	                        ; (or orientation, idunno).
	ld a,(CURORIENT)		;53ee	3a f1 80 	: . .   ; a is current orientation (or shape idunno) 
	add a,l			;53f1	85 	. ;
	add a,a			;53f2	87 	. 
	add a,a			;53f3	87 	. ;Multiply A by 4
	add a,c			;53f4	81 	. ; Add the current brick index (c out of 4)
	ld l,a			;53f5	6f 	o ; l=a
	ld h,000h		;53f6	26 00 	& . 
	ld de,TETROMINO_SHAPES		;53f8	11 84 6c 	. . l  ; INDEX the selected shape
	add hl,de			;53fb	19 	. 
	ld e,(hl)			;53fc	5e 	^       ; Four squares.  This square's coords, put into e.
	ld a,e			;53fd	7b 	{         ; a=e
	and 00fh		;53fe	e6 0f 	. .
	ld d,a			;5400	57 	W         ; Get the X value into d.   
	srl e		;5401	cb 3b 	. ;
	srl e		;5403	cb 3b 	. ; 
	srl e		;5405	cb 3b 	. ; 
	srl e		;5407	cb 3b 	. ; Shift right to get Y value. in e.
	ld a,(CURBRICK_X)		;5409	3a f2 80 	: . .  ;Current Y (or x?)
	add a,e			;540c	83 	.                ;Add E
	ld e,a			;540d	5f 	_                ;stow in E
	ld a,(CURBRICK_Y)		;540e	3a f3 80 	: . .  ;Current X (or y?)
	add a,d			;5411	82 	.                ;Add D
	ld d,a			;5412	57 	W                ;stow in D
	pop hl			;5413	e1 	. 
	ret			;5414	c9 	.           ;Return D, E with current location of a square.


Screen_Change_Clear:
	push hl			;5415	e5 	. 
	push de			;5416	d5 	. 
	push bc			;5417	c5 	. 
	ld hl,(BUFFERPOINTER1)		;5418	2a 3f 81 	* ? . 
	ld de,(BUFFERPOINTER1)		;541b	ed 5b 3f 81 	. [ ? . 
	inc de			;541f	13 	. 
	ld (hl),000h		;5420	36 00 	6 . 
	ld bc,SCRNCOLS		;5422	01 5a 00 	. Z .  ; CLEAR 90 BYTES POINTED BY (BUFFERPOINTER1)
	ldir		;5425	ed b0 	. .      ; (HL++)->(DE++) BC times.
	ld b,000h		;5427	06 00 	. .             ; Y MSB = 0
	ld d,000h		;5429	16 00 	. .             ; Y LSB = 0
	ld e,000h		;542b	1e 00 	. .             ; X = 0
	ld c,SCRNCOLS		;542d	0e 5a 	. Z         ; COPY ONE SCAN LINE OF ZEROES (90)
	ld hl,SCRNHEIGHT		;542f	21 40 01 	! @ . ; DO IT FOR ALL 320 SCAN LINES
cls_nextline:
	push hl			;5432	e5 	.                   ; Save scanline countdown
	ld hl,(BUFFERPOINTER1) ;5433	2a 3f 81 	* ? . ;
	ld a,028h		;5436	3e 28 	> (             ; SCREEN COPY
	rst 10h			;5438	d7 	.                   ; Do the copy.
	di			;5439	f3 	.                       ; DISABLE INTERRUPTS
	inc d			;543a	14 	.                     ; Next scanline
	jr nz,cls_nocarry		;543b	20 01 	  .     ; Carry?
	inc b			;543d	04 	.                     ; Carry to MSB
cls_nocarry:
	pop hl			;543e	e1 	. 
	dec hl			;543f	2b 	+ 
	ld a,h			;5440	7c 	| 
	or l			;5441	b5 	. 
	jr nz,cls_nextline		;5442	20 ee 	  . 

	ld b,001h		;5444	06 01 	. .           ; What RST function
	ld a,00fh		;5446	3e 0f 	> .           ; is this?  Graphics mode??
	rst 10h			;5448	d7 	.                 ; 
	di			;5449	f3 	.                     ; DISABLE INTERRUPTS 
	pop bc			;544a	c1 	. 
	pop de			;544b	d1 	. 
	pop hl			;544c	e1 	. 
	ret			;544d	c9 	. 
	
; This draws all rows and bricks in the game field.  (Not the borders.)
Draw_Game_Field:
	ld d,001h		;544e	16 01 	. .    ; D = 1
dgf_nextrow:
	ld e,001h		;5450	1e 01 	. .    ; E = 1
dgf_nextsquare:
	push de			;5452	d5 	.          ; Save D, E
	call SQUAREYX_DE_MEMLOC_to_HL		;5453	cd e5 5d 	. . ]   ; GET TOP ROW POINTER
	ld a,(hl)			;5456	7e 	~        ; GET TOP ROW BOX from the game field
	or a			;5457	b7 	.            ; TEST
	jr nz,dgf_occupied ;5458	20 02 	.  ; If occupied, process it.
	ld a,020h		;545a	3e 20 	>      ; ERASE the box if zero
dgf_occupied:
	and 07fh		;545c	e6 7f 	. .    ; Clear high bit
	ld b,a			;545e	47 	G          ; B = A (this is the brick "color", or $20 to clear)
	pop de			;545f	d1 	.          ; Recall D, E
	push de			;5460	d5 	.          ; Save D, E
	call Display_SQUARE		;5461	cd 64 53 	. d S  ; Display the square 
	pop de			;5464	d1 	.          ; Recall D, E
	inc e			;5465	1c 	.            ; X = X + 1
	ld a,e			;5466	7b 	{          ; A = E (work with X)
	cp 00bh		;5467	fe 0b 	. .      ; If X <11 do next box
	jr c,dgf_nextsquare		;5469	38 e7 	8 .  ; Next square on this row.
	inc d			;546b	14 	.            ; D = D + 1.  This is Y.  
	ld a,d			;546c	7a 	z          ; into A for comparison. 
	cp 01ah		;546d	fe 1a 	. .      ; If A <= 26, render the next row.
	jr c,dgf_nextrow		;546f	38 df 	8 .  ; 
	ret			;5471	c9 	. 
	
; Randomize and select the shape and orientation 
; of the next tetromino brick
RandomizeNextBrick:
	ld a,004h		;5472	3e 04 	> . 
	call GetRandomNum		;5474	cd 68 51 	. h Q   ; GENERATE RANDOM 0-3
	ld (NEXTORIENT),a		;5477	32 f5 80 	2 . . 
	ld a,007h		;547a	3e 07 	> .       
	call GetRandomNum		;547c	cd 68 51 	. h Q   ; GENERATE RANDOM 0-6 
	ld (NEXTSHAPE),a		;547f	32 f4 80 	2 . .       ; DETERMINE NEXT BRICK
	ret			;5482	c9 	. 
	
	
DrawNextBrickIfEnabled:
	push bc			;5483	c5 	. 
	ld a,(NEXTSHAPE)		;5484	3a f4 80 	: . .  ;NEXT BRICK SHAPE (also for pattern)
	ld b,a			;5487	47 	G                   
	ld a,(NEXTBRICK_ON)		;5488	3a f6 80 	: . .  
	or a			;548b	b7 	. 
	jr nz,dnb_yes_shownextbrick		;548c	20 02 	  . 
	ld b,020h		;548e	06 20 	.                ;ERASE BRICK if NEXTBRICK_ON is ZERO
dnb_yes_shownextbrick:
	call Display_Next_Brick_Squares		;5490	cd 9d 54 	. . T 
	pop bc			;5493	c1 	. 
	ret			;5494	c9 	. 
	
	
Clear_Next_Brick:
	push bc			;5495	c5 	. 
	ld b,020h		;5496	06 20 	.   
	call Display_Next_Brick_Squares		;5498	cd 9d 54 	. . T 
	pop bc			;549b	c1 	. 
	ret			;549c	c9 	. 
	

Display_Next_Brick_Squares:
	push hl			;549d	e5 	. 
	ld hl,(CURSHAPE)		;549e	2a f0 80 	* . . 
	push hl			;54a1	e5 	. 
	ld hl,(CURBRICKXYLH)		;54a2	2a f2 80 	* . . 
	push hl			;54a5	e5 	. 
	ld l,16 		;54a6	2e 10 	. .   ; X for NEXT BRICK POSITION relative to playfield
	ld h,6   		;54a8	26 06 	& .   ; Y for NEXT BRICK POSITION relative to playfield 
	ld (CURBRICKXYLH),hl		;54aa	22 f2 80 	" . . 
	ld hl,(NEXTSHAPE)		;54ad	2a f4 80 	* . . 
	ld (CURSHAPE),hl		;54b0	22 f0 80 	" . . 
	call DRAW_BRICK		;54b3	cd b2 53 	. . S 
	pop hl			;54b6	e1 	. 
	ld (CURBRICKXYLH),hl		;54b7	22 f2 80 	" . . 
	pop hl			;54ba	e1 	. 
	ld (CURSHAPE),hl		;54bb	22 f0 80 	" . . 
	pop hl			;54be	e1 	. 
	ret			;54bf	c9 	. 


LEVEL_DIFFICULTY_DELAY:
	ld a,00bh		;54c0	3e 0b 	> .                  ; A = 11
	ld hl,LEVELNUMBER		;54c2	21 fd 80 	! . .      ; HL= address of levelnumber
	sub (hl)			;54c5	96 	.                      ; Subtract (LEVELNUMBER) from A
	ld b,a			;54c6	47 	G                        ; B = 11-(LEVELNUMBER)
	ld c,003h		;54c7	0e 03 	. .                  ; C = 3
  mlt bc      ;	defb 0edh,04ch	;54c9	ed 4c	. L  ; BC = 3 * (11-LEVELNUMBER)
	jp DELAY_BC_and_KEYSCAN_Pausable		;54cb	c3 f9 50 	. . P  ; DELAY BASED ON LEVEL NUMBER 


INIT_GAMEFIELD:
	ld hl,GAME_FIELD		;54ce	21 68 7e 	! h ~     ; SETTING UP A MEMORY WIPE
	ld de,GAME_FIELD+1	;54d1	11 69 7e 	. i ~         ; 
	ld a,000h		;54d4	3e 00 	> .                 ; 
	ld (hl),a			;54d6	77 	w                     ; 
	ld bc,00137h		;54d7	01 37 01 	. 7 .         ; CLEAR 312 BYTES TO ZERO (311 + 1)
	ldir		;54da	ed b0 	. .                     ; (HL++)->(DE++) BC times.
	ld a,(STARTHEIGHT)		;54dc	3a fc 80 	: . .   ; 
	cp 005h		;54df	fe 05 	. .                   ; What happens if STARTHEIGHT is 5?
	jr z,START_AT_OPTION_5		;54e1	28 38 	( 8   ; If 
	ld c,a			;54e3	4f 	O                       ; 
	ld b,000h		;54e4	06 00 	. . 
	ld hl,Height_Options_List		;54e6	21 04 7c 	! . | ; POINTER TO LIST: 0 4 7 10 13 
	add hl,bc			;54e9	09 	.            ; Index array 0 4 7 10 13 << with A=STARTHEIGHT
	ld a,(hl)			;54ea	7e 	~                     ; This is the height to start
	or a			;54eb	b7 	.                         ; set flags to test for zero
	jr z,start_height_done		;54ec	28 38 	( 8   ; 0 means nothing special, no handicap
	ld c,a			;54ee	4f 	O                       ; 
	ld b,019h		;54ef	06 19 	. .                 ; ROW B = 25 (Bottom row)
handicap_another_row:
	ld e,b			;54f1	58 	X                       ; E = B
	ld d,00ch		;54f2	16 0c 	. .                 ; D = 12
  mlt de      ;	defb 0edh,05ch	;54f4	ed 5c	. \ ; DE = 12 * ROW
	ld hl,GAME_FIELD		;54f6	21 68 7e 	! h ~     ; 
	add hl,de			;54f9	19 	.                     ; LOCATE ROW IN MEMORY
	ld d,00ah		;54fa	16 0a 	. .                 ; D = 10
handicap_another_square_in_this_row:            ; First box will be 1 not zero...
	inc hl			;54fc	23 	#                       ; increment 
	ld a,00eh		;54fd	3e 0e 	> .                 ; A = 14
	call GetRandomNum		;54ff	cd 68 51 	. h Q     ; Random number 0 thru 13.
	scf			;5502	37 	7                           ; Set Carry flag
	rra			;5503	1f 	.                           ; Divide by 2, with high bit now 1.
	                                              ; Square "color" is 0-6.  
	                                              ;  Carry is now random bit.  
	jr nc,nosquare		;5504	30 01 	0 .           ; It's now 50/50 whether we store this square.
	ld (hl),a			;5506	77 	w                     ; Store the square.
nosquare:                                       ; Or don't.  That was 50/50.
	dec d			;5507	15 	.                         ; Countdown 10 squares in this row.
	jr nz,handicap_another_square_in_this_row		;5508	20 f2 	  .   ; 
	dec b			;550a	05 	.                         ; Go up one row, 
	dec c			;550b	0d 	.                         ; and count down the starting height count.
	jr nz,handicap_another_row		;550c	20 e3  .  ; Handicap rows until done
	ld hl,GAME_FIELD		;550e	21 68 7e 	! h ~ 
	ld de,GAMEFIELD_2		;5511	11 ac 7f 	. . . 
	ld bc,FIELD_BYTES		;5514	01 44 01 	. D .     ; SAVE THIS PLAYFIELD FOR REPEAT GAME option
	ldir		;5517	ed b0 	. .                     ; (HL++)->(DE++) BC times.
	jr start_height_done		;5519	18 0b 	. .     
START_AT_OPTION_5:                              ; 6th OPTION "<<" is REPEAT GAME. 
	ld hl,GAMEFIELD_2		;551b	21 ac 7f 	! . .     ; Restore the gamefield.
	ld de,GAME_FIELD		;551e	11 68 7e 	. h ~ 
	ld bc,FIELD_BYTES		;5521	01 44 01 	. D . 
	ldir		;5524	ed b0 	. .                     ; (HL++)->(DE++) BC times.
start_height_done:
	ld hl,BOTTOM_ROW		;5526	21 a0 7f 	! . .     ; FILL bottom row with 0ffh
	ld b,00ch		;5529	06 0c 	. .                 ; B = 12
	ld a,0ffh		;552b	3e ff 	> .                 ; A = $FF
fr_nextsq:
	ld (hl),a			;552d	77 	w                     ;
	inc hl			;552e	23 	#                       ;
	djnz fr_nextsq		;552f	10 fc 	. .           ; Decrement B and jump if not zero
	ld hl,GAME_FIELD		;5531	21 68 7e 	! h ~     ; FILL left side boundary boxes with FF's
	ld b,01ah		;5534	06 1a 	. .                 ; B  = 26 (rows)
	ld de,0000ch		;5536	11 0c 00 	. . .         ; DE = 12 (columns)
	ld a,0ffh		;5539	3e ff 	> .                 ; A  = $FF
flb_nextrow:
	ld (hl),a			;553b	77 	w                     ; 
	add hl,de			;553c	19 	.                     ;
	djnz flb_nextrow		;553d	10 fc 	. .         ; Decrement B and jump if not zero
	ld hl,GAME_FIELD+11	;553f	21 73 7e 	! s ~     ; FILL RIGHT side boundary boxes with FF's
	ld b,01ah		;5542	06 1a 	. .                 ; B  = 26 (rows)
	ld de,0000ch		;5544	11 0c 00 	. . .         ; DE = 12 (columns)
	ld a,0ffh		;5547	3e ff 	> .                 ; A  = $FF
frb_nextrow:
	ld (hl),a			;5549	77 	w 
	add hl,de			;554a	19 	. 
	djnz frb_nextrow		;554b	10 fc 	. .         ; Decrement B and jump if not zero
	ret			;554d	c9 	. 
	
	
ASK_LEVEL:   ; DISPLAY THE LEVEL-SELECTION GAME-START SCREEN
	push af			;554e	f5 	. 
	push bc			;554f	c5 	. 
	push de			;5550	d5 	. 
	push hl			;5551	e5 	. 
	ld de,0*TXTROW+0      ;5552	11 00 00 	. . .   ; SCREEN LOCATION upper left corner
	ld hl,REVstr		      ;5555	21 0e 50 	! . P   ; REVISION STRING
	call DisplayString    ;5558	cd 24 52 	. $ R   ; PUT STRING
	ld de,80*SCANLINE+35	;555b	11 43 1c 	. C .   ; SCREEN LOCATION middle above centered numbers
	ld hl,LEVEL___HEIGHT  ;555e	21 4e 57 	! N W   ; STRING
	call DisplayString	  ;5561	cd 24 52 	. $ R   ; PUT STRING
	ld hl,SelectLevel		  ;5564	21 67 57 	! g W   ; STRING "Select Level..."
	ld de,256*SCANLINE+10 ;5567	11 0a 5a 	. . Z   ; SCREEN LOCATION near bottom
	call DisplayString		;556a	cd 24 52 	. $ R   ; PUT STRING
	ld a,000h						  ;556d	3e 00 	> .       ; A = 0
menu_nextglyph:
	call Display_BIGFONT_GLYPH		;556f	cd 7d 56 	. } V 
	inc a			;5572	3c 	<                         ; A = A + 1
	cp 010h		;5573	fe 10 	. .                   ; do A-16
	jr c,menu_nextglyph		;5575	38 f8 	8 .       ; Jump if A is less than 16 (A-16 causing carry)
	ld de,119*SCANLINE+29		;5577	11 f3 29 	. . )         ; 
	call Draw_Box_Vertical_Line		;557a	cd ec 56 	. . V       ; 
	ld de,119*SCANLINE+51		;557d	11 09 2a 	. . *         ; 
	call Draw_Box_Vertical_Line		;5580	cd ec 56 	. . V       ; 
	ld de,119*SCANLINE+30		;5583	11 f4 29 	. . )         ; 
	ld b,00fh		;5586	06 0f 	. .                 ; 
	call Draw_Box_Horizontal_Line		;5588	cd 06 57 	. . W       ; 
	ld de,119*SCANLINE+52		;558b	11 0a 2a 	. . *         ; 
	ld b,009h		;558e	06 09 	. .                 ; 
	call Draw_Box_Horizontal_Line		;5590	cd 06 57 	. . W       ; 
	
levelselect_loop:
	ld a,(STARTLEVEL)		;5593	3a fe 80 	: . . 
	or 080h		;5596	f6 80 	. . 
	call Display_BIGFONT_GLYPH		;5598	cd 7d 56 	. } V 
	call CLEAR_KEYBOARD_TABLE		;559b	cd 6d 50 	. m P 
sl_keycheck_checkforanykey:
	call SCAN_KEYBOARD		;559e	cd 8d 50 	. . P 
	call INTERPRET_KEY		;55a1	cd ae 59 	. . Y 
	ld hl,KEYSDETECTED		;55a4	21 2a 81 	! * .    ; HL = KEYSDETECTED, the table of keyscan data
	ld a,(hl)			;55a7	7e 	~                      ; Check row 0
	ld b,008h		;55a8	06 08 	. .                  ; Check 8 more rows, or-ing together
sl_keycheck_nextrow:
	inc hl			;55aa	23 	#                        ; Next scan row
	or (hl)			;55ab	b6 	.                        ; Or the row into A
	djnz sl_keycheck_nextrow		;55ac	10 fc 	. .    ; Decrement B and jump if not zero.  8+1 = 9 rows.
	or a			;55ae	b7 	.                          ; Set flags
	jr z,sl_keycheck_checkforanykey  ;55af	28 ed ( .; No key? check again.
	ld a,(STARTLEVEL)		;55b1	3a fe 80 	: . .      
	call Display_BIGFONT_GLYPH		;55b4	cd 7d 56 	. } V 
	ld a,KEY_LEFT		;55b7	3e 02 	> . 
	call CHECK_FOR_KEY     		;55b9	cd 3b 50 	. ; P    ; LEFT Arrow
	jr nz,sl_declevel		;55bc	20 07 	  . 
	ld a,KEY_J		;55be	3e 0c 	> . 
	call CHECK_FOR_KEY     		;55c0	cd 3b 50 	. ; P    ; J Key
	jr z,sl_check_rightkeys		;55c3	28 08 	( . 
sl_declevel:
	ld hl,STARTLEVEL		;55c5	21 fe 80 	! . . 
	ld a,(hl)			;55c8	7e 	~ 
	or a			;55c9	b7 	. 
	jr z,sl_check_rightkeys		;55ca	28 01 	( . 
	dec (hl)			;55cc	35 	5 
sl_check_rightkeys:
	ld a,KEY_RIGHT		;55cd	3e 03 	> . 
	call CHECK_FOR_KEY     		;55cf	cd 3b 50 	. ; P    ; RIGHT Arrow
	jr nz,sl_inclevel		;55d2	20 07 	  . 
	ld a,KEY_L		;55d4	3e 0e 	> . 
	call CHECK_FOR_KEY     		;55d6	cd 3b 50 	. ; P    ; L Key
	jr z,sl_check_continue		;55d9	28 09 	( . 
sl_inclevel:
	ld hl,STARTLEVEL		;55db	21 fe 80 	! . . 
	ld a,(hl)			;55de	7e 	~ 
	cp 009h		;55df	fe 09 	. . 
	jr nc,sl_check_continue		;55e1	30 01 	0 . 
	inc (hl)			;55e3	34 	4 
sl_check_continue:
	ld a,KEY_SPACE		;55e4	3e 05 	> .                      ; Space Bar
	call CHECK_FOR_KEY     		;55e6	cd 3b 50 	. ; P
	jp nz,sl_setlevelnow		;55e9	c2 ff 55 	. . U 
	ld a,KEY_RET		;55ec	3e 04 	> . 
	call CHECK_FOR_KEY     		;55ee	cd 3b 50 	. ; P     ; Return.
	jp nz,sl_setlevelnow		;55f1	c2 ff 55 	. . U 
	ld a,KEY_SHIFT		;55f4	3e 06 	> . 
	call CHECK_FOR_KEY     		;55f6	cd 3b 50 	. ; P     ; Shift.
	jp nz,sl_setlevelnow		;55f9	c2 ff 55 	. . U 
	jp levelselect_loop		;55fc	c3 93 55 	. . U 
sl_setlevelnow:
	ld a,(STARTLEVEL)		;55ff	3a fe 80 	: . . 
	or 080h		;5602	f6 80 	. . 
	call Display_BIGFONT_GLYPH		;5604	cd 7d 56 	. } V 

heightselect_loop:
	ld a,(STARTHEIGHT)		;5607	3a fc 80 	: . . 
	add a,08ah		;560a	c6 8a 	. . 
	call Display_BIGFONT_GLYPH		;560c	cd 7d 56 	. } V 
	call CLEAR_KEYBOARD_TABLE		;560f	cd 6d 50 	. m P 
sh_keycheck_checkforanykey:
	call SCAN_KEYBOARD		;5612	cd 8d 50 	. . P 
	call INTERPRET_KEY		;5615	cd ae 59 	. . Y 
	ld hl,KEYSDETECTED		;5618	21 2a 81 	! * . 
	ld a,(hl)			;561b	7e 	~ 
	ld b,008h		;561c	06 08 	. . 
sh_keycheck_nextrow:
	inc hl			;561e	23 	# 
	or (hl)			;561f	b6 	. 
	djnz sh_keycheck_nextrow		;5620	10 fc 	. .    ; Decrement B and jump if not zero
	or a			;5622	b7 	. 
	jr z,sh_keycheck_checkforanykey		;5623	28 ed 	( . 
	ld a,(STARTHEIGHT)		;5625	3a fc 80 	: . . 
	add a,00ah		;5628	c6 0a 	. . 
	call Display_BIGFONT_GLYPH		;562a	cd 7d 56 	. } V 
	ld a,KEY_LEFT		;562d	3e 02 	> . 
	call CHECK_FOR_KEY     		;562f	cd 3b 50 	. ; P   ; LEFT Arrow
	jr nz,sh_decheight		;5632	20 07 	  . 
	ld a,KEY_J		;5634	3e 0c 	> . 
	call CHECK_FOR_KEY     		;5636	cd 3b 50 	. ; P   ; J Key
	jr z,sh_checkrightkeys		;5639	28 08 	( . 
sh_decheight:
	ld hl,STARTHEIGHT		;563b	21 fc 80 	! . . 
	ld a,(hl)			;563e	7e 	~ 
	or a			;563f	b7 	. 
	jr z,sh_checkrightkeys		;5640	28 01 	( . 
	dec (hl)			;5642	35 	5 
sh_checkrightkeys:
	ld a,KEY_RIGHT		;5643	3e 03 	> . 
	call CHECK_FOR_KEY     		;5645	cd 3b 50 	. ; P     ; RIGHT Arrow
	jr nz,sh_incheight		;5648	20 07 	  . 
	ld a,KEY_L		;564a	3e 0e 	> . 
	call CHECK_FOR_KEY     		;564c	cd 3b 50 	. ; P     ; L Key
	jr z,sh_check_continue		;564f	28 09 	( . 
sh_incheight:
	ld hl,STARTHEIGHT		;5651	21 fc 80 	! . . 
	ld a,(hl)			;5654	7e 	~ 
	cp 005h		;5655	fe 05 	. . 
	jr nc,sh_check_continue		;5657	30 01 	0 . 
	inc (hl)			;5659	34 	4 
sh_check_continue:
	ld a,KEY_SPACE		;565a	3e 05 	> .                       ; Space Bar
	call CHECK_FOR_KEY     		;565c	cd 3b 50 	. ; P     ; Space
	jp nz,sh_height_is_selected		;565f	c2 75 56 	. u V 
	ld a,KEY_RET		;5662	3e 04 	> . 
	call CHECK_FOR_KEY     		;5664	cd 3b 50 	. ; P     ; Return
	jp nz,sh_height_is_selected		;5667	c2 75 56 	. u V 
	ld a,KEY_SHIFT		;566a	3e 06 	> . 
	call CHECK_FOR_KEY     		;566c	cd 3b 50 	. ; P     ; Shift
	jp nz,sh_height_is_selected		;566f	c2 75 56 	. u V 
	jp heightselect_loop		;5672	c3 07 56 	. . V 
sh_height_is_selected:
	call CLEAR_KEYBOARD_TABLE		;5675	cd 6d 50 	. m P 
	pop hl			;5678	e1 	. 
	pop de			;5679	d1 	. 
	pop bc			;567a	c1 	. 
	pop af			;567b	f1 	. 
	ret			;567c	c9 	. 
	
	
; 
; Looks like A contains the level number or character number 0 thru 14? and
; if high bit 080h of A is set, the number is inverted
Display_BIGFONT_GLYPH:
	push af			;567d	f5 	. 
	push bc			;567e	c5 	. 
	push de			;567f	d5 	. 
	push hl			;5680	e5 	. 
	ld c,a			;5681	4f 	O    ; SAVE ARGUMENT 1 from A to C.  Character number and "invert"
	res 7,c		;5682	cb b9 	. . ; Clear the high bit so we can use it as an index.
	ld b,000h		;5684	06 00 	. . 
	ld hl,LEVEL_CHOICES_COORDS		;5686	21 2e 57 	! . W  ; HL = 4E2A or 2A4E? not sure
	add hl,bc			;5689	09 	. 
	add hl,bc			;568a	09 	.  ; Calculate LEVEL_CHOICES_COORDS + INDEX * 2
	ld e,(hl)			;568b	5e 	^  ; COORDINATE in DE is not straight X/Y, I think it's screen location
	inc hl			;568c	23 	#    
	ld d,(hl)			;568d	56 	V  ; COORDINATE in DE is not straight X/Y, I think it's screen location
	ld b,048h		;568e	06 48 	. H  ; C contains the character number, and 048h = 72, the length of one character
  mlt bc      ;	defb 0edh,04ch	;5690	ed 4c	. L  ; BC = offset of character in BIGFONT_DATA
	ld hl,BIGFONT_DATA		;5692	21 84 77 	! . w 
	add hl,bc			;5695	09 	.                      ; HL=pointer to the specific character
	ld c,a			;5696	4f 	O                        ; Save A so we can check inversion
	ld b, 23		;5697	06 17 	. .                  ; 24 rows of pixels! 3 bytes each row.

; COMMENT: This section could be made much more compact.
; Why not just copy the glyph with ldir, and invert it if necessary in the buffer? 

bf_loopagain:
	ld a,(hl)			;5699	7e 	~                      ;*COPY byte, first byte of character row
	inc hl			;569a	23 	#                        ; increment SOURCE pointer
	bit 7,c		;569b	cb 79 	. y                    ; Test whether we are doing inverse video
	jr z,bf_skipinvert1		;569d	28 01 	( .        ; INVERT OR NOT INVERT?
	cpl			;569f	2f 	/             ; COMPLEMENT.  ; Invert the image of this character.
bf_skipinvert1:
	push hl			;56a0	e5 	.                        ; stow the SOURCE pointer
	ld hl,(BUFFERPOINTER1)		;56a1	2a 3f 81 	* ? .; Get BUFFER/DEST pointer
	ld (hl),a			;56a4	77 	w                      ;*PASTE the byte into the buffer
	pop hl			;56a5	e1 	.                        ; restore the SOURCE pointer
	ld a,(hl)			;56a6	7e 	~                      ;*COPY 2nd byte of this row
	inc hl			;56a7	23 	#                        ; increment SOURCE pointer
	bit 7,c		;56a8	cb 79 	. y                    ; Test whether we are doing inverse video
	jr z,bf_skipinvert2		;56aa	28 01 	( .        ; INVERT OR NOT INVERT?
	cpl			;56ac	2f 	/             ; COMPLEMENT.  ; Invert the image of this character.
bf_skipinvert2:
	push hl			;56ad	e5 	.                        ; stow the SOURCE pointer
	ld hl,(BUFFERPOINTER1)		;56ae	2a 3f 81 	* ? .; Get BUFFER/DEST pointer
	inc hl			;56b1	23 	#                        ; increment BUFFER/DEST pointer once for 2nd byte
	ld (hl),a			;56b2	77 	w                      ;*PASTE the 2nd byte into the buffer
	pop hl			;56b3	e1 	.                        ; restore the SOURCE pointer
	ld a,(hl)			;56b4	7e 	~                      ;*COPY 3rd byte of this row
	inc hl			;56b5	23 	#                        ; increment SOURCE pointer
	bit 7,c		;56b6	cb 79 	. y                    ; Test whether we are doing inverse video
	jr z,bf_skipinvert3		;56b8	28 01 	( .        ; INVERT OR NOT INVERT?
	cpl			;56ba	2f 	/             ; COMPLEMENT.  ; Invert the image of this character.
bf_skipinvert3:
	or 001h		;56bb	f6 01 	. .                    ; ADD the right boundary of the box, 1 pixel
	push hl			;56bd	e5 	.                        ; stow the SOURCE pointer
	ld hl,(BUFFERPOINTER1)		;56be	2a 3f 81 	* ? .; Get BUFFER/DEST pointer
	inc hl			;56c1	23 	# 
	inc hl			;56c2	23 	#                        ; Increment BUFFER/DEST twice for 3rd byte
	ld (hl),a			;56c3	77 	w                      ;*PASTE the 3rd byte into the buffer
	push bc			;56c4	c5 	.                        ; save BC
	ld c,003h		;56c5	0e 03 	. .                  ; 3 bytes, do a copy to video memory?
	call COPY_C_BYTES_TO_VIDEO_MEMORY		;56c7	cd 1b 57 	. . W        ; 
	pop bc			;56ca	c1 	.                        ; restore C (character)
	pop hl			;56cb	e1 	.                        ; restore SOURCE pointer
	ld a,SCRNCOLS		;56cc	3e 5a 	> Z                  ; A=90
                                                 ; Remember D, E contain screen location 
                                                 ; from table LEVEL_CHOICES_COORDS.
                                                 ; COORDINATE in DE is not straight X/Y, 
                                                 ; I think it's screen location
	add a,e			;56ce	83 	.                        ; 
	ld e,a			;56cf	5f 	_                        ; SCREENLOCATION=SCREENLOCATION+90
	ld a,d			;56d0	7a 	z                        ; A=Y
	adc a,000h		;56d1	ce 00 	. .                ; CARRY for high byte.  
	ld d,a			;56d3	57 	W                        ; Y=A.  
	djnz bf_loopagain		;56d4	10 c3 	. .   ; Decrement B and jump if not zero.  NEXT ROW.
	ld a,0ffh		;56d6	3e ff 	> . 
	ld hl,(BUFFERPOINTER1)		;56d8	2a 3f 81 	* ? . 
	ld (hl),a			;56db	77 	w 
	inc hl			;56dc	23 	# 
	ld (hl),a			;56dd	77 	w 
	inc hl			;56de	23 	# 
	ld (hl),a			;56df	77 	w 
	push bc			;56e0	c5 	. 
	ld c,003h		;56e1	0e 03 	. . 
	call COPY_C_BYTES_TO_VIDEO_MEMORY		;56e3	cd 1b 57 	. . W 
	pop bc			;56e6	c1 	. 
	pop hl			;56e7	e1 	. 
	pop de			;56e8	d1 	. 
	pop bc			;56e9	c1 	. 
	pop af			;56ea	f1 	. 
	ret			;56eb	c9 	. 

; This is a specialized vertical line.  It only works for the menu boxes.  
; This draws a 0b00000001 from top to bottom.  Location in DE (screen byte).
Draw_Box_Vertical_Line:
	ld b,031h		;56ec	06 31 	. 1   ; B = 49
drawbox_next:
	ld hl,(BUFFERPOINTER1)		;56ee	2a 3f 81 	* ? . 
	ld a,001h		;56f1	3e 01 	> . 
	ld (hl),a			;56f3	77 	w 
	push bc			;56f4	c5 	. 
	ld c,001h		;56f5	0e 01 	. . 
	call COPY_C_BYTES_TO_VIDEO_MEMORY		;56f7	cd 1b 57 	. . W 
	pop bc			;56fa	c1 	. 
	ld a,e			;56fb	7b 	{ 
	add a,SCRNCOLS		;56fc	c6 5a 	. Z 
	ld e,a			;56fe	5f 	_ 
	ld a,d			;56ff	7a 	z 
	adc a,000h		;5700	ce 00 	. .    ; CARRY for high byte of DE.
	ld d,a			;5702	57 	W 
	djnz drawbox_next		;5703	10 e9 	. .    ; Decrement B and jump if not zero
	ret			;5705	c9 	. 

; ARGUMENTS:
; DE = location, screen byte number.
Draw_Box_Horizontal_Line:
	push de			;5706	d5 	.         ; LOCATION in DE.
	push bc			;5707	c5 	. 
	push hl			;5708	e5 	.         ; 
	ld a,0ffh		;5709	3e ff 	> .   ; A = 255, the byte to transfer.
	ld hl,(BUFFERPOINTER1)		;570b	2a 3f 81 	* ? . ; Get the bufffer pointer.
	ld (hl),a			;570e	77 	w       ; Store the byte into the buffer.
	ld c,001h		;570f	0e 01 	. .   ; Copy 1 byte to video memory
	call COPY_C_BYTES_TO_VIDEO_MEMORY		;5711	cd 1b 57 	. . W  ; Copy it.  Location in DE.
	pop hl			;5714	e1 	. 
	pop bc			;5715	c1 	. 
	pop de			;5716	d1 	. 
	inc de			;5717	13 	.         ; LOCATION: Move to the next byte to the right.
	djnz Draw_Box_Horizontal_Line		;5718	10 ec 	. .    ; Decrement B and jump if not zero
	ret			;571a	c9 	. 


COPY_C_BYTES_TO_VIDEO_MEMORY:
	push de			;571b	d5 	. 
	ex de,hl			;571c	eb 	.             ; HL becomes DE which contains Y, X coords?
	ld a,SCRNCOLS		;571d	3e 5a 	> Z     ; A = 90
	call GET_COL_inA_from_SCRNINDEX_trashHL_F   ;571f	cd c8 58 	. . X ;
	ld b,h			;5722	44 	D               ; B = "Y"
	ld d,l			;5723	55 	U               ; D = "X"
	ld e,a			;5724	5f 	_               ; E = 90
	ld a,028h		;5725	3e 28 	> (         ; SCREEN COPY
	ld hl,(BUFFERPOINTER1)		;5727	2a 3f 81 	* ? . 
	rst 10h			;572a	d7 	. 
	di			;572b	f3 	.                     ; DISABLE INTERRUPTS 
	pop de			;572c	d1 	. 
	ret			;572d	c9 	. 


; BLOCK 'str2' (start 0x572e end 0x57ab)
LEVEL_CHOICES_COORDS:
; This table seems to represent the level and height selection tables, two rows each.	
; It seems to contain the coordinates of the boxes... 64 vs. (?? what was I writing?) 
;LEVEL:
  defb	04eh,02ah,051h,02ah,054h,02ah,057h,02ah,05ah,02ah     ;N*Q*T*W*Z*
	defb	0beh,032h,0c1h,032h,0c4h,032h,0c7h,032h,0cah,032h
;HEIGHT:
  defb  064h,02ah,067h,02ah,06ah,02ah
	defb	0d4h,032h,0d7h,032h,0dah,032h

LEVEL___HEIGHT:
  defb	"LEVEL             HEIGHT",0
SelectLevel:
	defb	"Select LEVEL and HEIGHT by using J or L key.  Press RETURN to start.",0
	
Show_COPYRIGHT:
	ld hl,COPYRIGHT1		;57ac	21 9a 7c 	! . | 
	ld de, 4*TXTROW+20	;57af	11 f4 10 	. . . 
	call DisplayString	;57b2	cd 24 52 	. $ R 
	ld hl,COPYRIGHT2		;57b5	21 cd 7c 	! . | 
	ld de, 6*TXTROW+20	;57b8	11 64 19 	. d . 
	call DisplayString	;57bb	cd 24 52 	. $ R 
	ld hl,COPYRIGHT3		;57be	21 f8 7c 	! . | 
	ld de, 8*TXTROW+20	;57c1	11 d4 21 	. . ! 
	call DisplayString	;57c4	cd 24 52 	. $ R 
	ld hl,COPYRIGHT4		;57c7	21 30 7d 	! 0 } 
	ld de,10*TXTROW+20	;57ca	11 44 2a 	. D * 
	call DisplayString	;57cd	cd 24 52 	. $ R 
	ld hl,COPYRIGHT5		;57d0	21 5c 7d 	! \ } 
	ld de,12*TXTROW+20	;57d3	11 b4 32 	. . 2 
	call DisplayString	;57d6	cd 24 52 	. $ R 
	ld hl,VERSIONstr		;57d9	21 82 7d 	! . } 
	ld de,14*TXTROW+20	;57dc	11 24 3b 	. $ ; 
	call DisplayString	;57df	cd 24 52 	. $ R 
	ld bc,03a98h		    ;57e2	01 98 3a 	. . :        ; DELAY for Copyright Page
copyright_delay_loop:
	call SCAN_KEYBOARD		;57e5	cd 8d 50 	. . P 
	ld a,005h		;57e8	3e 05 	> .                 ; Space Bar to exit copyright
	call CHECK_FOR_KEY     		;57ea	cd 3b 50 	. ; P 
	jr nz,exit_copyright_page		;57ed	20 05 	  . 
	dec bc			;57ef	0b 	. 
	ld a,b			;57f0	78 	x 
	or c			;57f1	b1 	. 
	jr nz,copyright_delay_loop		;57f2	20 f1 	  . 
exit_copyright_page:
	call CLEAR_KEYBOARD_TABLE		;57f4	cd 6d 50 	. m P 
	ret			;57f7	c9 	. 
	
; DRAW THE PALACE, BACKDROP ARTWORK IN THE GAME SCREEN
; This routine must first double the size of the image, part by part.
; It appears to render chunk by chunk, as can be seen when running the program.
; Palace image data is 26 bytes wide, which is 208 pixels.  It will be doubled
; to 416 pixels wide on the screen.
; The data is 102 lines tall, but will be doubled to 204 scanlines.
;	This topmost routine divides the bitmap into 7 sections for rendering.
; Oddly, it's not an even multiple ... the bottom section is short, only six lines.  
; The first 6 chunks are 16 lines tall.
;
; Also, there is a hidden building to the right of the displayed image.

Draw_Palace_Artwork:  
	push af			;57f8	f5 	. 
	push bc			;57f9	c5 	. 
	push de			;57fa	d5 	. 
	push hl			;57fb	e5 	. 
	ld b,007h		;57fc	06 07 	. . 
	ld d,007h		;57fe	16 07 	. . 
	ld e,036h		;5800	1e 36 	. 6          ; This is 18 * 3.  
	ld hl,palace_start		;5802	21 f4 6c 	! . l ; HL=SOURCE POINTER
palace_next_section:
	call Draw_Palace_Artwork_Section		;5805	cd 17 58 	. . X 
	push de			;5808	d5 	.                ; save D, E
	ld de,16*26 ;5809	11 a0 01 	. . .      ; DE=416 (256+160).  This is 16*26 or 16 lines.
	add hl,de	  ;580c	19 	.                ; Add 416 to SOURCE Pointer
	pop de			;580d	d1 	.                ; restore D, E
	inc d		  	;580e	14 	.                ; ...
	inc d		  	;580f	14 	.                ; Increment Y by two ... units?
	djnz palace_next_section		;5810	10 f3 	. .    ; Decrement B and jump if not zero.  B was 7.
	pop hl			;5812	e1 	. 
	pop de			;5813	d1 	. 
	pop bc			;5814	c1 	. 
	pop af			;5815	f1 	. 
	ret			;5816	c9 	. 
	
	
; 
; There are rendered 18 subsections 8x16 pixels.
; 

Draw_Palace_Artwork_Section:   
	push bc			;5817	c5 	. 
	push de			;5818	d5 	. 
	push hl			;5819	e5 	. 
	ld b,18 		;581a	06 12 	. .   ; 18 subsections. 
dpas_repeat:
	call Draw_Palace_Artwork_Subsection		;581c	cd 28 58 	. ( X 
	inc hl			;581f	23 	# 
	inc e			;5820	1c 	.                 ; ...
	inc e			;5821	1c 	.                 ; Increment X by two bytes?
	djnz dpas_repeat		;5822	10 f8 	. .    ; Decrement B and jump if not zero   
	pop hl			;5824	e1 	. 
	pop de			;5825	d1 	. 
	pop bc			;5826	c1 	. 
	ret			;5827	c9 	. 
	
	
Draw_Palace_Artwork_Subsection:
	push bc			;5828	c5 	. 
	push hl			;5829	e5 	. 
	push de			;582a	d5 	. 
	ld de,PALACEARTVAR		;582b	11 47 81 	. G .   
	push de			;582e	d5 	. 
	ld b,16 		;582f	06 10 	. .   ;16 rows (doubled?)
dpsub_repeat:
	ld a,(hl)			;5831	7e 	~ 
	ld (de),a			;5832	12 	. 
	inc de			;5833	13 	.                  ; Increment Y location by one scan line?
	ld a,26 		;5834	3e 1a 	> .            ; A = 26, the size of a line in the bitmap
	call HL_add_A		;5836	cd c3 58 	. . X    ; Index bitmap forward by one line
	djnz dpsub_repeat		;5839	10 f6 	. .    ; Decrement B and jump if not zero.  16 rows.
	pop hl			;583b	e1 	.                  ; 
	pop de			;583c	d1 	.                  ;
	call Display_16_byte_section		;583d	cd 43 58 	. C X  ; 16 BYTES at (HL)
	pop hl			;5840	e1 	. 
	pop bc			;5841	c1 	. 
	ret			;5842	c9 	. 
	
	
; ARGUMENTS:  DE  is Y, X location on screen
;             HL is source pointer
;             SOURCE data is a buffer of 16
Display_16_byte_section:
	push de			;5843	d5 	. 
	push hl			;5844	e5 	. 
	ld b,010h		;5845	06 10 	. .   ; B=16
	ld a,d			;5847	7a 	z         ;   A=D (Y value)
	ld c,a			;5848	4f 	O         ; C=A   (Y value)
  mlt bc      ;	defb 0edh,04ch	;5849	ed 4c	. L  ; now BC = Y * 16.  
	ld d,c			;584b	51 	Q                        ; ultimately BD = Y * 16
	ld a,010h		;584c	3e 10 	> .   ;              ; A=16
d16_nextline:
	push af			;584e	f5 	.                        ; store A
	ld a,(hl)			;584f	7e 	~                      ; Get SOURCE BYTE
	ld c,a			;5850	4f 	O                        ; C = SOURCE BYTE
	call Double_pixels_in_C		;5851	cd 67 58 	. g X; Result is in target of BUFFERPOINTER1 
	call Display_16x2_at_BDE	;5854	cd 99 58 	. . X; Display the two bytes- THIS IS WHERE WE DO IT!
	inc d			;5857	14 	.                          ; Increment Y
	jr nz,d16_nocarry1		;5858	20 01 	  .        ; Carry 
	inc b			;585a	04 	.                          ; Carry.  BD contains Y value 
d16_nocarry1:                                    ;
	inc d			;585b	14 	.                          ; Increment Y again. So, Y=Y+2.
	jr nz,d16_nocarry2		;585c	20 01 	  .        ; Carry
	inc b			;585e	04 	.                          ; Carry.  BD contains Y value, down by 2 pixels.
d16_nocarry2:                                    ;
	inc hl			;585f	23 	#                        ; INCREMENT SOURCE POINTER by one byte
	pop af			;5860	f1 	.                        ; Restore A=16
	dec a			;5861	3d 	=                          ; Count down 16 lines
	jr nz,d16_nextline		;5862	20 ea 	  .        ; 
	pop hl			;5864	e1 	. 
	pop de			;5865	d1 	. 
	ret			;5866	c9 	. 
	
; ARGUMENTS: 
; c = BYTE TO DOUBLE and place into work buffer
; BUFFERPOINTER1 contents point to the work buffer
Double_pixels_in_C:
	push hl			;5867	e5 	. 
	push de			;5868	d5 	. 
	push bc			;5869	c5 	. 
	ld hl,(BUFFERPOINTER1)	;586a	2a 3f 81 	* ? . 
	ld (BUFFERPOINTER2),hl  ;586d	22 45 81 	" E .   ; Store pointer 
	ld e,000h		;5870	1e 00 	. .             ; E = 0
	ld d,001h		;5872	16 01 	. .             ; D = 1
	ld b,008h		;5874	06 08 	. .             ; B = 8  (8 bits?)
double_nextbit:                             ; This will happen 8 times, once for each bit of C.
	rl c		;5876	cb 11 	. .                 ; Rotate C left.  CARRY = MSB of C.
	push bc			;5878	c5 	.                   ; Save B, C
	ld a,002h		;5879	3e 02 	> .             ; A = 2
	ld b,a			;587b	47 	G                   ; B = 2
double_nextpixel:                           ; This will happen twice, two pixels for each bit.
	push af			;587c	f5 	.                   ; Save A
	rl e		;587d	cb 13 	. .                 ; E gets CARRY bit from C. 
	sla d		;587f	cb 22 	. "                 ; D shift left.  CARRY = MSB of D.
	jr nc,dp_not_lastone ;5881	30 0c 	0 .   ; If no carry, move on... remember D starts with 0b00000001.
	ld hl,(BUFFERPOINTER2)  ;5883	2a 45 81 	* E . ; 
	ld (hl),e			;5886	73 	s                 ; Store E into buffer, fully shifted.
	inc hl			;5887	23 	#                   ; Increment buffer pointer
	ld (BUFFERPOINTER2),hl		;5888	22 45 81 	" E . ; Store the incremented buffer pointer.
	ld e,000h		;588b	1e 00 	. .             ; E = 0 to start over
	ld d,001h		;588d	16 01 	. .             ; D = 1 to start over
dp_not_lastone:
	pop af			;588f	f1 	.                   ; Restore A.  
	djnz double_nextpixel		;5890	10 ea 	. .    ; Decrement B and jump if not zero. ; Count B from 2 down to 1.
	pop bc			;5892	c1 	.                   ; Restore B, C.  
	djnz double_nextbit		;5893	10 e1 	. . ; Decrement B and jump if not zero. ; Count other B from 8 down to 1.  
	pop bc			;5895	c1 	. 
	pop de			;5896	d1 	. 
	pop hl			;5897	e1 	. 
	ret			;5898	c9 	. 

; This section copies 16 pixels of expanded data from the buffer
; to the display.  Total operation transfers 4 bytes to bitmap display.
; SCREEN LOCATION in BDE.  BD = Y, E = X
; b is high bit/byte of Y position  (what is Y resolution?)
; Two bytes are in the buffer; these represent represent one byte
; of pixel data expanded horizontally.  Those bytes are doubled vertically
; here when rendered to the screen.
Display_16x2_at_BDE:
	push hl			;5899	e5 	. 
	push de			;589a	d5 	. 
	push bc			;589b	c5 	. 
	ld c,001h		;589c	0e 01 	. .                     ; Copy one byte to display
	ld hl,(BUFFERPOINTER1)		;589e	2a 3f 81 	* ? .   ; from Buffer
	ld a,028h		;58a1	3e 28 	> (                     ; SCREEN COPY
	rst 10h			;58a3	d7 	. 	                        ; Do the copy
	di			;58a4	f3 	.                               ; DISABLE INTERRUPTS
	inc e			;58a5	1c 	.                             ; Increment X
	inc hl			;58a6	23 	#                           ; Increment SOURCE pointer
	ld a,028h		;58a7	3e 28 	> (                     ; SCREEN COPY
	rst 10h			;58a9	d7 	. 	                        ; Do the copy
	di			;58aa	f3 	.                               ; DISABLE INTERRUPTS
	dec e			;58ab	1d 	.                             ; Restore X to original
	inc d			;58ac	14 	.                             ; INCREMENT Y
	jr nz,movedown_nocarry		;58ad	20 01 	  .       ; CARRY D overflow into B 
	inc b			;58af	04 	.                             ; CARRY D overflow into B
movedown_nocarry:                                   ;
	ld c,001h		;58b0	0e 01 	. .                     ; Copy one byte to display
	ld hl,(BUFFERPOINTER1)		;58b2	2a 3f 81 	* ? .   ; from Buffer, down lower!
	ld a,028h		;58b5	3e 28 	> (                     ; SCREEN COPY
	rst 10h			;58b7	d7 	.                           ; Do the copy
	di			;58b8	f3 	.                               ; DISABLE INTERRUPTS
	inc e			;58b9	1c 	.                             ; Increment X
	inc hl			;58ba	23 	#                           ; Increment SOURCE pointer
	ld a,028h		;58bb	3e 28 	> (                     ; SCREEN COPY
	rst 10h			;58bd	d7 	. 	                        ; Do the copy
	di			;58be	f3 	.                               ; DISABLE INTERRUPTS
	pop bc			;58bf	c1 	. 
	pop de			;58c0	d1 	. 
	pop hl			;58c1	e1 	. 
	ret			;58c2	c9 	. 
	

; Math routine, adds A to HL and returns sum in HL (and LSB A).	
HL_add_A:
	add a,l			;58c3	85 	. 
	ld l,a			;58c4	6f 	o 
	ret nc			;58c5	d0 	. 
	inc h			;58c6	24 	$ 
	ret			;58c7	c9 	. 


; THIS IS CALLED WITH
; A = 90 
; HL is always the screen memory location, relative to the bitmap start
; RETURNS COLUMN in A.
; I figured out this procedure by testing in ASM80.com emulation.
; This is doing a sort of modulo arithmetic, or division with remainder.  
; I don't know how it is done.  I guess I would do it differently.
; Seems to trash HL.  Not sure F.
GET_COL_inA_from_SCRNINDEX_trashHL_F:
	push bc			;58c8	c5 	. 
	push de			;58c9	d5 	. 
	ld b,000h		;58ca	06 00 	. . ; B = 0 ; B = 0
	ld c,a			;58cc	4f 	O               ; save A; C = 90 now (in this architecture)
	or a			;58cd	b7 	.                 ; test if A = zero.  It is 90, so ...
	scf			;58ce	37 	7                   ; set carry flag; subtract an extra 1 in subsequent subtract
	jr z,tstsq_exit		;58cf	28 1a 	( .   ; IF A=0, exit.  If this is a null character do nothing.
	ex de,hl			;58d1	eb 	.             ; DE is now the Screen offset
	ld hl,00000h		;58d2	21 00 00 	! . . ; HL = 0
	ld a,010h		;58d5	3e 10 	> .         ; 16 LINES?
tstsq_nextline:
	sla e		;58d7	cb 23 	. #  ; Shift left (multiply e by 2)
	rl d		;58d9	cb 12 	. .  ;rotate left carry into d
	rl l		;58db	cb 15 	. .  ;rotate left carry into l
	rl h		;58dd	cb 14 	. .  ;rotate left carry into h  This has shited 00 00 DD EE left 1 bit.
	   ; SO, now, the location / long number defined by HLDE is HLDE*2
	sbc hl,bc		;58df	ed 42 	. B  ; subtract 90 from HL.  
	                               ; Note it is first like subtracting 91, with carry.
	                               ; At the start, HL is 0 or 1 so this is like -90.
	inc de			;58e1	13 	.        ; Add 1 to HLDE (carry here...)
	jr nc,tstsq_nocarry		;58e2	30 02 	0 . 
	add hl,bc			;58e4	09 	.      ; 
	dec de			;58e5	1b 	. 
tstsq_nocarry:
	dec a			;58e6	3d 	= 
	jr nz,tstsq_nextline		;58e7	20 ee 	  . 
	ld a,l			;58e9	7d 	} 
	ex de,hl			;58ea	eb 	. 
tstsq_exit:
	pop de			;58eb	d1 	. 
	pop bc			;58ec	c1 	. 
	ret			;58ed	c9 	. 
	
Multiply_HL_by_A_to_BCHL:
	push af			;58ee	f5 	. 
	push bc			;58ef	c5 	. 
	ld b,h			;58f0	44 	D                         ; STORE H, set up msb multiply
	ld h,a			;58f1	67 	g                         ; SET UP MULTIPLY
  mlt hl      ;	defb 0edh,06ch	;58f2	ed 6c	. l   ; MULTIPLY HL = L * A
	ld c,a			;58f4	4f 	O                         ; C = A
  mlt bc      ;	defb 0edh,04ch	;58f5	ed 4c	. L   ; BC = H * A
	ld a,c			;58f7	79 	y                         ; A = C
	add a,h			;58f8	84 	.                         ; A += H
	ld h,a			;58f9	67 	g                         ; H = A means H = H+A why?
	pop bc			;58fa	c1 	.                         ; 
	pop af			;58fb	f1 	. 
	ret			;58fc	c9 	. 
	

	
; THE DIFFERENCES BETWEEN SAVESCREEN AND RESTORESCREEN ARE (at least):
;	ld hl,(BUFFERPOINTER1)		;5916	2a 3f 81 	* ? .   ; HL = SCREEN TRANSFER BUFFER
;...
; then this bit above, which seems to pull in 8000 as a buffer location
;	ld de,(BUFFERPOINTER1)		;5925	ed 5b 3f 81 	. [ ? .  ; DE = SCREEN TRANSFER BUFFER
;	ex de,hl			;5929	eb 	.                              ; HL = SCREEN TRANSFER BUFFER; DE = 8000h
;
;ALSO SAVESCREEN does
;	ld a,029h		;591f	3e 29 	> )                     ; THE COMMAND for RST 10h
;	rst 10h			;5921	d7 	.                           ; Do it
;followed by
;	ld a,007h		;592d	3e 07 	> .                          ; 7 = THE COMMAND for RST 18h
;	rst 18h			;592f	df 	.                                ; Do it
;
;whereas RESTORESCREEN does:
;
;	ld a,007h		;5980	3e 07 	> . 
;	rst 18h			;5982	df 	. 
;followed by
;	ld a,028h		;598e	3e 28 	> (            ; SCREEN COPY
;	rst 10h			;5990	d7 	. 
;
; The difference is not as simple as reversing source and destination.
; A different system call is used in SAVE vs. RESTORE, and only the 
; RESTORE version uses the same 028h RST 10h as with the other rendering routines.

PAUSE_SAVESCREEN:
	ld c,078h		;58fd	0e 78 	. x 
	ld b,003h		;58ff	06 03 	. . 
	ld a,003h		;5901	3e 03 	> . 
	rst 30h			;5903	f7 	. 
	di			;5904	f3 	.                      ; DISABLE INTERRUPTS
	ld hl,08000h		;5905	21 00 80 	! . . 
	ld de,00000h		;5908	11 00 00 	. . . 
 	ld bc,SCRNHEIGHT		;590b	01 40 01 	. @ .    ; BC = 320 (Screen bitmap height)
	ld a,000h		;590e	3e 00 	> . 
	ld (LINE_MSB),a		;5910	32 58 81 	2 X . 
ss_nextline:
	push bc			;5913	c5 	. 
	push de			;5914	d5 	. 
	push hl			;5915	e5 	. 
	ld hl,(BUFFERPOINTER1)		;5916	2a 3f 81 	* ? .   ; HL = SCREEN TRANSFER BUFFER
	ld a,(LINE_MSB)		;5919	3a 58 81 	: X .           ; A = LINE_MSB value
	ld b,a			;591c	47 	G                           ; B = LINE_MSB value
	ld c,SCRNCOLS		;591d	0e 5a 	. Z                 ; C = 90
	ld a,029h		;591f	3e 29 	> )                     ; THE COMMAND for RST 10h
	rst 10h			;5921	d7 	.                           ; Do it
	di			;5922	f3 	.                      ; DISABLE INTERRUPTS
	pop hl			;5923	e1 	.                           ; Restore HL=8000h
	push hl			;5924	e5 	. 
	ld de,(BUFFERPOINTER1)		;5925	ed 5b 3f 81 	. [ ? .  ; DE = SCREEN TRANSFER BUFFER
	ex de,hl			;5929	eb 	.                              ; HL = SCREEN TRANSFER BUFFER; DE = 8000h
	ld bc,SCRNCOLS		;592a	01 5a 00 	. Z .                ; BC = 90
	ld a,007h		;592d	3e 07 	> .                          ; 7 = THE COMMAND for RST 18h
	rst 18h			;592f	df 	.                                ; Do it
	di			;5930	f3 	.                      ; DISABLE INTERRUPTS
	pop hl			;5931	e1 	. 
	pop de			;5932	d1 	. 
	ld bc,SCRNCOLS		;5933	01 5a 00 	. Z .      
	add hl,bc			;5936	09 	.                    ; NEXT SCANLINE
	inc d			;5937	14 	.                       ; ROW IS AD 
	jr nz,ss_nocarry		;5938	20 07 	  . 
	ld a,(LINE_MSB)		;593a	3a 58 81 	: X .   ; ROW IS AD
	inc a			;593d	3c 	< 
	ld (LINE_MSB),a		;593e	32 58 81 	2 X .   ; ROW IS AD
ss_nocarry:
	pop bc			;5941	c1 	. 
	dec c			;5942	0d 	. 
	jr nz,ss_nextline		;5943	20 ce 	  . 
	ld a,b			;5945	78 	x 
	cp 000h		;5946	fe 00 	. . 
	jr z,ss_done		;5948	28 03 	( . 
	dec b			;594a	05 	. 
	jr ss_nextline		;594b	18 c6 	. . 
ss_done:
	ret			;594d	c9 	. 

	
PAUSE_RESTORESCREEN:
	ld c,078h		;594e	0e 78 	. x 
	ld b,003h		;5950	06 03 	. . 
	ld a,003h		;5952	3e 03 	> . 
	rst 30h			;5954	f7 	. 
	di			;5955	f3 	.                      ; DISABLE INTERRUPTS
	ld b,000h		;5956	06 00 	. . 
	ld a,002h		;5958	3e 02 	> . 
	rst 10h			;595a	d7 	. 
	di			;595b	f3 	.                      ; DISABLE INTERRUPTS
	ld b,000h		;595c	06 00 	. . 
	ld a,005h		;595e	3e 05 	> . 
	rst 10h			;5960	d7 	. 
	di			;5961	f3 	.                      ; DISABLE INTERRUPTS
	ld a,00fh		;5962	3e 0f 	> . 
	ld b,001h		;5964	06 01 	. . 
	rst 10h			;5966	d7 	. 
	di			;5967	f3 	.                      ; DISABLE INTERRUPTS
	ld hl,08000h		;5968	21 00 80 	! . . 
	ld de,00000h		;596b	11 00 00 	. . . 
	ld bc,SCRNHEIGHT		;596e	01 40 01 	. @ . 
	ld a,000h		;5971	3e 00 	> . 
	ld (LINE_MSB),a		;5973	32 58 81 	2 X . 
sr_nextline:
	push bc			;5976	c5 	. 
	push hl			;5977	e5 	. 
	push de			;5978	d5 	. 
	ld de,(BUFFERPOINTER1)		;5979	ed 5b 3f 81 	. [ ? .         ; DE = SCREEN TRANSFER BUFFER
	ld bc,SCRNCOLS		;597d	01 5a 00 	. Z . 
	ld a,007h		;5980	3e 07 	> . 
	rst 18h			;5982	df 	. 
	di			;5983	f3 	.                     ; DISABLE INTERRUPTS 
	pop de			;5984	d1 	. 
	ld hl,(BUFFERPOINTER1)		;5985	2a 3f 81 	* ? .              ; HL = SCREEN TRANSFER BUFFER
	ld a,(LINE_MSB)		;5988	3a 58 81 	: X .  ; ROW IS AD
	ld b,a			;598b	47 	G 
	ld c,SCRNCOLS		;598c	0e 5a 	. Z 
	ld a,028h		;598e	3e 28 	> (            ; SCREEN COPY
	rst 10h			;5990	d7 	. 
	di			;5991	f3 	.                     ; DISABLE INTERRUPTS 
	inc d			;5992	14 	. 
	jr nz,sr_nocarry		;5993	20 07 	  . 
	ld a,(LINE_MSB)		;5995	3a 58 81 	: X . 
	inc a			;5998	3c 	<                   ; ROW IS AD
	ld (LINE_MSB),a		;5999	32 58 81 	2 X . 
sr_nocarry:
	pop hl			;599c	e1 	. 
	ld bc,SCRNCOLS		;599d	01 5a 00 	. Z . 
	add hl,bc			;59a0	09 	. 
	pop bc			;59a1	c1 	. 
	dec c			;59a2	0d 	. 
	jr nz,sr_nextline		;59a3	20 d1 	  . 
	ld a,b			;59a5	78 	x 
	cp 000h		;59a6	fe 00 	. . 
	jr z,sr_done		;59a8	28 03 	( . 
	dec b			;59aa	05 	. 
	jr sr_nextline		;59ab	18 c9 	. . 
sr_done:
	ret			;59ad	c9 	. 

INTERPRET_KEY:  ; called only after scan keyboard.  Not sure if I named it right.
; MENU (PAUSE) and S key (sound) always work- even during inquiries.
	ld a,010h		;59ae	3e 10 	> .             ; 'S' key
	call CHECK_FOR_KEY     		;59b0	cd 3b 50 .;  
	jr z,ik_checknextkey		;59b3	28 0f 	( . ; If not 's', check next key
	ld a,010h		;59b5	3e 10 	> .             ; 
	call ALTERNATE_CHECK_KEY_in_A		;59b7	cd 53 50 	. S P   ; 
	ld a,(SOUNDON)		;59ba	3a 3d 81 	: = .   ; FLIP SOUND ENABLED/DISABLED
	cpl			;59bd	2f 	/          ;COMPLEMENT  ; FLIP SOUND ENABLED/DISABLED
	ld (SOUNDON),a		;59be	32 3d 81 	2 = .   ; FLIP SOUND ENABLED/DISABLED 
	call Sound_Status_Indicate		;59c1	cd 3d 51 	. = Q 
ik_checknextkey:
	ld a,009h		;59c4	3e 09 	> .     ; MENU KEY FOR GAME PAUSE
	call CHECK_FOR_KEY     		;59c6	cd 3b 50 	. ; P 
	ret z			;59c9	c8 	.             ; RETURN if we are not detecting the menu key.  
	                                  ; Otherwise, begin pause/office work screen!
	push af			;59ca	f5 	. 
	push bc			;59cb	c5 	. 
	push de			;59cc	d5 	. 
	push hl			;59cd	e5 	. 
STILL_PAUSING:
	call PAUSE_SAVESCREEN		;59ce	cd fd 58 	. . X 
	call Screen_Change_Clear		;59d1	cd 15 54 	. . T 
	ld a,00fh		;59d4	3e 0f 	> .                              ;
	ld b,000h		;59d6	06 00 	. .                              ;
	rst 10h			;59d8	d7 	.                                    ;
	di			;59d9	f3 	.                                        ; DISABLE INTERRUPTS
	call Present_Ruler_for_Game_Pause		;59da	cd dc 5f 	. . _  ;
	ld a,00bh		;59dd	3e 0b 	> .                              ;
	ld b,080h		;59df	06 80 	. .                              ; Is this something about the cursor?
	ld d,000h		;59e1	16 00 	. .                              ;
	ld e,051h		;59e3	1e 51 	. Q                              ;
	rst 10h			;59e5	d7 	.                                    ;
	di		      ;59e6	f3 	.                                    ; DISABLE INTERRUPTS
	ld bc,200		;59e7	01 c8 00 	. . . 
	call DELAY_BC_and_KEYSCAN_NO_EXIT		;59ea	cd f0 50 	. . P  ; DELAY 200
	call CLEAR_KEYBOARD_TABLE   ;59ed	cd 6d 50	. m P
  in0 a,(34h)  ;	defb 0edh,038h,034h	;59f0	ed 38 34 	. 8 4  ; INTERRUPTS
	set 1,a		;59f3	cb cf 	. .                                ; Int1 ENABLE
  out0 (34h),a ;	defb 0edh,039h,034h	;59f5	ed 39 34 	. 9 4  ; INTERRUPTS 
	ei			;59f8	fb 	.                                        ;*ENABLE INTERRUPTS
pause_nokey:
	ld a,007h		;59f9	3e 07 	> .                              ;
	rst 8			;59fb	cf 	.                                      ; GET KEY VIA SYSTEM CALL
	jr nc,pause_nokey		;59fc	30 fb 	0 .                      ;
	ld a,d			;59fe	7a 	z                                    ;
	cp 080h		;59ff	fe 80 	. .                                ; CHECK FOR CANCEL BUTTON (80h)
	jp z,EXIT_GAME		;5a01	ca 64 5a 	. d Z                    ;
	cp 09eh		;5a04	fe 9e 	. .                                ; CHECK FOR MENU KEY  (9Eh)
	jr nz,pause_nokey		;5a06	20 f1 	  .                      ; IF ZERO, continue & exit the pause mode
  in0 a,(34h)  ;	defb 0edh,038h,034h	;5a08	ed 38 34 	. 8 4  ; INTERRUPTS
	res 1,a		;5a0b	cb 8f 	. .                                ; Int1 DISABLE
  out0 (34h),a ;	defb 0edh,039h,034h	;5a0d	ed 39 34 	. 9 4  ; INTERRUPTS
	di			;5a10	f3 	.                                        ; DISABLE INTERRUPTS
	ld a,00bh		;5a11	3e 0b 	> .                              ;
	ld b,080h		;5a13	06 80 	. .                              ;
	ld d,00fh		;5a15	16 0f 	. .                              ;
	ld e,000h		;5a17	1e 00 	. .                              ;
	rst 10h			;5a19	d7 	.                                    ; 
	di			;5a1a	f3 	.                                        ; DISABLE INTERRUPTS
	call CLEAR_KEYBOARD_TABLE		;5a1b	cd 6d 50 	. m P 
	call PAUSE_RESTORESCREEN		;5a1e	cd 4e 59 	. N Y 
	ld bc,300		;5a21	01 2c 01 	. , .                          ; BC = 300
	call DELAY_BC_and_KEYSCAN_NO_EXIT		;5a24	cd f0 50 	. . P  ; DELAY 300
	ld a,KEY_MENU		;5a27	3e 09 	> .                          ; MENU KEY TO EXIT PAUSE/WORK SCREEN
	call CHECK_FOR_KEY     		;5a29	cd 3b 50 	. ; P 
	jp nz,STILL_PAUSING		;5a2c	c2 ce 59 	. . Y 
	call CLEAR_KEYBOARD_TABLE		;5a2f	cd 6d 50 	. m P 
	pop hl			;5a32	e1 	. 
	pop de			;5a33	d1 	. 
	pop bc			;5a34	c1 	. 
	pop af			;5a35	f1 	. 
	ret			;5a36	c9 	. 

; I tried to see what the orphan function does,
; but didn't see anything.  I inserted it after
; the "pause" screen above, by replacing ret with NOP.
; There was no apparent effect when going back to the 
; game screen.

; Orphan function?
; This is interesting; it seems to read screen memory, 
; take a complement, and write screen memory.
; I figure it is inverting something, but don't know what.
; I ran it after the game pause, nothing visible.
ORPHAN_FUNCTION:
	ld hl,(ORPHAN_VAR)		;5a37	2a 59 81 	* Y . 
	dec hl			;5a3a	2b 	+ 
	ld (ORPHAN_VAR),hl		;5a3b	22 59 81 	" Y . 
	ld a,l			;5a3e	7d 	} 
	or h			;5a3f	b4 	. 
	ret nz			;5a40	c0 	. ; This may be an easter egg? it only does something after ORPHAN_VAR gets to zero.
	ld hl,009ffh		;5a41	21 ff 09 	! . .     ; $9FF = $A00-1 = 2559.  Ten pages.
	ld (ORPHAN_VAR),hl		;5a44	22 59 81 	" Y .   ; Reset the counter to 2559.
	ld e,00bh		;5a47	1e 0b 	. .                       ; E = 11  ; If you asked me this is Column 11.
	ld d,022h		;5a49	16 22 	. "                       ; D = 34  
	         ; If you asked me, this is screen loc., scan line 34 (3rd row of 2nd 16-line text rows). 
	ld b,00dh		;5a4b	06 0d 	. .                       ; B = 14  ; 14 times.
orphan_countdown:
	push bc			;5a4d	c5 	. 
	ld hl,(BUFFERPOINTER1)		;5a4e	2a 3f 81 	* ? . 
	ld bc,00001h		;5a51	01 01 00 	. . .               ; This would copy 1 byte.
	ld a,029h		;5a54	3e 29 	> )                       ; This RST function copies bytes FROM
	rst 10h			;5a56	d7 	.                             ; the screen TO the buffer, I think. 1 byte.
	di			;5a57	f3 	.                                 ; DISABLE INTERRUPTS
	ld a,(hl)			;5a58	7e 	~                           ; Get 1 byte from the buffer.
	cpl			;5a59	2f 	/                                 ; COMPLEMENT it.
	ld (hl),a			;5a5a	77 	w                           ; Put 1 byte back in the buffer.
	ld a,028h		;5a5b	3e 28 	> (                       ; SCREEN COPY 1 byte, return to screen.
	rst 10h			;5a5d	d7 	.                             ; Do it.
	di			;5a5e	f3 	.                                 ; DISABLE INTERRUPTS
	inc d			;5a5f	14 	.                               ; 
	pop bc			;5a60	c1 	. 
	djnz orphan_countdown		;5a61	10 ea 	. .    ; Decrement B and jump if not zero.  Loop 14 times.
	ret			;5a63	c9 	. 
	
EXIT_GAME:
	call SCAN_KEYBOARD		;5a64	cd 8d 50 	. . P 
	ld hl,KEYTAB21		;5a67	21 21 81 	! ! . 
	ld a,(hl)			;5a6a	7e 	~ 
	ld b,008h		;5a6b	06 08 	. . 
eg_loop:
	inc hl			;5a6d	23 	# 
	or (hl)			;5a6e	b6 	. 
	djnz eg_loop		;5a6f	10 fc 	. .    ; Decrement B and jump if not zero
	or a			;5a71	b7 	. 
	jr nz,EXIT_GAME		;5a72	20 f0 	  . 
	ld a,001h		;5a74	3e 01 	> . 
	rst 8			;5a76	cf 	. 
	di			;5a77	f3 	.                                 ; DISABLE INTERRUPTS
  in0 a,(34h)  ;	defb 0edh,038h,034h	;5a78	ed 38 34 	. 8 4   ; INTERRUPTS
	set 1,a		;5a7b	cb cf 	. .                                 ; Int1 ENABLE
  out0 (34h),a ;	defb 0edh,039h,034h	;5a7d	ed 39 34 	. 9 4   ; INTERRUPTS
	ei			;5a80	fb 	.                                 ; ENABLE INTERRUPTS
	ld a,001h		;5a81	3e 01 	> . 
	rst 30h			;5a83	f7 	.   ;This is the final blow, this RST exits the game.
	
APL_BEGIN:
	ld b,003h		;5a84	06 03 	. . 
	ld a,004h		;5a86	3e 04 	> . 
	rst 30h			;5a88	f7 	. 
	di			;5a89	f3 	.                                 ; DISABLE INTERRUPTS
  in0 a,(34h)  ;	defb 0edh,038h,034h	;5a8a	ed 38 34 	. 8 4   ; INTERRUPTS 
	res 1,a		;5a8d	cb 8f 	. .                                 ; Int1 DISABLE 
  out0 (34h),a ;	defb 0edh,039h,034h	;5a8f	ed 39 34 	. 9 4   ; INTERRUPTS 
	ld hl,GAME_FIELD		;5a92	21 68 7e 	! h ~ ; CLEAR AND INITIALIZE MEMORY Beyond Binary
	ld de,GAME_FIELD+1	;5a95	11 69 7e 	. i ~ 
	ld bc,07372h		;5a98	01 72 73 	. r s 
	ld (hl),000h		;5a9b	36 00 	6 . 
	ldir		;5a9d	ed b0 	. .                 ; (HL++)->(DE++) BC times.
	ld hl,KEYS_LOCATION_DATA		;5a9f	21 17 50 	! . P 
	ld (KEY_COORDS),hl		;5aa2	22 01 81 	" . . 
	ld a,005h		;5aa5	3e 05 	> .                       ; This call gets a pointer
	ld b,000h		;5aa7	06 00 	. .                       ; to a buffer, which can
	rst 30h			;5aa9	f7 	.                             ; be used for copying to screen (??)
	di			;5aaa	f3 	.                                 ; DISABLE INTERRUPTS
	ld (BUFFERPOINTER1),hl		;5aab	22 3f 81 	" ? .     ; INITIALIZE BUFFERPOINTER1
	ld a,0ffh		;5aae	3e ff 	> .                       ; Initialize two boolean
	ld (NEXTBRICK_ON),a		;5ab0	32 f6 80 	2 . .         ; vars to $FF. 
	ld (SOUNDON),a		;5ab3	32 3d 81 	2 = .             ; Sound = on.
	ld a,r		;5ab6	ed 5f 	. _                         ; Unusual register R.
	              ; "The R register is a counter that is updated every instruction"
	ld (KEYSCAN2RND),a		;5ab8	32 04 81 	2 . .   ; Random seed?
	ld b,001h		;5abb	06 01 	. . 
	ld a,00fh		;5abd	3e 0f 	> . 
	rst 10h			;5abf	d7 	. 
	di			;5ac0	f3 	.                                 ; DISABLE INTERRUPTS
	ld bc,00000h		;5ac1	01 00 00 	. . . 
;I found that the next three lines can be omitted for faster startup.  Use 9 NOP's
; to keep all pointer refs original.
	call ThreeDeeLogoSwooshes		;5ac4	cd ff 5d 	. . ] 
	call Screen_Change_Clear		;5ac7	cd 15 54 	. . T 
	call Show_COPYRIGHT		;5aca	cd ac 57 	. . W 
GAME_START_LEVEL_SELECT:
	call Screen_Change_Clear		;5acd	cd 15 54 	. . T 
	call Sound_Status_Indicate		;5ad0	cd 3d 51 	. = Q 
	call ASK_LEVEL		;5ad3	cd 4e 55 	. N U 
	ld hl,(KEYSCAN1)		;5ad6	2a 03 81 	* . . 
	ld a,h			;5ad9	7c 	|     ; SWAP H & L  
	ld h,l			;5ada	65 	e     ; SWAP H & L
	ld l,a			;5adb	6f 	o     ; SWAP H & L
	ld (RANDSD),hl		;5adc	22 ff 80 	" . . 
	ld a,000h		;5adf	3e 00 	> .     ;CLEAR SCORE VARIABLES
	ld (GAMESCORE1),a		;5ae1	32 f9 80 	2 . . 
	ld (GAMESCORE2),a		;5ae4	32 fa 80 	2 . . 
	ld (GAMESCORE3),a		;5ae7	32 fb 80 	2 . . 
	ld (LINECOUNT_L),a		;5aea	32 f7 80 	2 . . 
	ld (LINECOUNT_H),a		;5aed	32 f8 80 	2 . . 
	ld a,(STARTLEVEL)		;5af0	3a fe 80 	: . . 
	ld (LEVELNUMBER),a		;5af3	32 fd 80 	2 . . 
	call Screen_Change_Clear		;5af6	cd 15 54 	. . T 
	call Draw_Palace_Artwork		;5af9	cd f8 57 	. . W 
	call InitGameScreenScoresAndWhatelse		;5afc	cd 80 52 	. . R 
	call Sound_Status_Indicate		;5aff	cd 3d 51 	. = Q 
	call RandomizeNextBrick		;5b02	cd 72 54 	. r T 
	call INIT_GAMEFIELD		;5b05	cd ce 54 	. . T 
	call DrawNextBoxAndFieldBorder		;5b08	cd 34 52 	. 4 R 
	call Draw_Game_Field		;5b0b	cd 4e 54 	. N T 
GAME_LOOP_START:
	call Clear_Next_Brick		;5b0e	cd 95 54 	. . T 
	ld a,(NEXTORIENT)		;5b11	3a f5 80 	: . .  ;NEXT ORIENTATION
	ld (CURORIENT),a		;5b14	32 f1 80 	2 . . 
	ld a,(NEXTSHAPE)		;5b17	3a f4 80 	: . .  ;NEXT SHAPE
	ld (CURSHAPE),a		;5b1a	32 f0 80 	2 . .  ;Current brick shape is the previous "next"
	call RandomizeNextBrick		;5b1d	cd 72 54 	. r T  ; MAKE A NEW NEXT BRICK
	ld a,004h		;5b20	3e 04 	> .         ; STARTING BRICK X POSITION = 4
	ld (CURBRICK_X),a		;5b22	32 f2 80 	2 . . 
	ld a,000h		;5b25	3e 00 	> .         ; STARTING BRICK Y POSITION = 0
	ld (CURBRICK_Y),a		;5b27	32 f3 80 	2 . . 
	call DrawNextBrickIfEnabled		;5b2a	cd 83 54 	. . T 
	ld hl,CURSHAPE		;5b2d	21 f0 80 	! . .   ;SHAPE NUMBER
	ld b,(hl)			;5b30	46 	F 
	call DRAW_BRICK		;5b31	cd b2 53 	. . S 
	call LEVEL_DIFFICULTY_DELAY		;5b34	cd c0 54 	. . T 
	call CLEAR_KEYBOARD_TABLE		;5b37	cd 6d 50 	. m P 
	call CheckBrickCollision  ;5b3a	cd ca 53 	. . S  ; This is where we detect bricks too high end of game
	jp z,gameloop_StartFalling ;5b3d	ca c5 5b 	. . [  ; No collision => go start falling brick loop.
	  ; If there is a collision (A not zero), then we were unable to place the first brick!
	  ; In that case, it's GAME OVER!
; GAME OVER...
	ld de,5*TXTROW+68		;5b40	11 5c 15 	. \ .      ; LOCATION to right of NEXT box
	ld hl,GAMEOVER_string		;5b43	21 d9 7d 	! . } 
	call DisplayString		;5b46	cd 24 52 	. $ R 
	ld de,Melody_GameOver		;5b49	11 d8 64 	. . d 
	call PLAY_MELODY		;5b4c	cd 05 51 	. . Q 
	ld hl,HIGHSCORE1		;5b4f	21 65 7e 	! e ~ 
	ld de,GAMESCORE1		;5b52	11 f9 80 	. . . 
	ld a,(de)			;5b55	1a 	. 
	sub (hl)			;5b56	96 	. 
	inc de			;5b57	13 	. 
	inc hl			;5b58	23 	# 
	ld a,(de)			;5b59	1a 	. 
	sbc a,(hl)			;5b5a	9e 	. 
	inc de			;5b5b	13 	. 
	inc hl			;5b5c	23 	# 
	ld a,(de)			;5b5d	1a 	. 
	sbc a,(hl)			;5b5e	9e 	. 
	jr c,ask_play_again		;5b5f	38 0f 	8 .    ; DETECT high score, move on if not.
	ld hl,(GAMESCORE1)		;5b61	2a f9 80 	* . .    ;No carry, means we beat the high score...
	ld (HIGHSCORE1),hl		;5b64	22 65 7e 	" e ~ ; SET HIGH SCORE
	ld a,(GAMESCORE3)		;5b67	3a fb 80 	: . .     ; What is this?
	ld (HIGHSCORE2),a		;5b6a	32 67 7e 	2 g ~ 
	call UpdateHighScore		;5b6d	cd f3 52 	. . R 
ask_play_again:
	ld de,12*TXTROW+20		;5b70	11 b4 32 	. . 2             ; Location
	ld hl,BlankLine_31ch		;5b73	21 24 7e 	! $ ~ 
	call DisplayString		;5b76	cd 24 52 	. $ R 
	ld de,13*TXTROW+20		;5b79	11 ec 36 	. . 6             ; Location
	ld hl,PlayAgain		;5b7c	21 e3 7d 	! . } 
	call DisplayString		;5b7f	cd 24 52 	. $ R 
	ld de,14*TXTROW+20		;5b82	11 24 3b 	. $ ;             ; Location
	ld hl,BlankLine_31ch		;5b85	21 24 7e 	! $ ~ 
	call DisplayString		;5b88	cd 24 52 	. $ R 
	ld de,15*TXTROW+20		;5b8b	11 5c 3f 	. \ ?             ; Location
	ld hl,PressRETURNorCANCEL		;5b8e	21 03 7e 	! . ~ 
	call DisplayString		;5b91	cd 24 52 	. $ R 
	ld de,16*TXTROW+20		;5b94	11 94 43 	. . C             ; Location
	ld hl,BlankLine_31ch		;5b97	21 24 7e 	! $ ~ 
	call DisplayString		;5b9a	cd 24 52 	. $ R 
gameover_keycheck:
	call CLEAR_KEYBOARD_TABLE		;5b9d	cd 6d 50 	. m P   
gameover_keycheck_waitforkey:
	call SCAN_KEYBOARD		;5ba0	cd 8d 50 	. . P    
	call INTERPRET_KEY		;5ba3	cd ae 59 	. . Y 
	ld hl,KEYSDETECTED		;5ba6	21 2a 81 	! * . 
	ld a,(hl)			;5ba9	7e 	~ 
	ld b,008h		;5baa	06 08 	. . 
gameover_keycheck_nextrow:
	inc hl			;5bac	23 	# 
	or (hl)			;5bad	b6 	. 
	djnz gameover_keycheck_nextrow		;5bae	10 fc 	. .     ; Decrement B and jump if not zero
	or a			;5bb0	b7 	. 
	jr z,gameover_keycheck_waitforkey		;5bb1	28 ed 	( .   ; If no key, keep waiting.
	ld a,KEY_CANCEL		;5bb3	3e 08 	> . 
	call CHECK_FOR_KEY     		;5bb5	cd 3b 50 	. ; P         ; Check for CANCEL ("No play, just exit")
	jp nz,EXIT_GAME		;5bb8	c2 64 5a 	. d Z 
	ld a,KEY_RET		;5bbb	3e 04 	> . 
	call CHECK_FOR_KEY     		;5bbd	cd 3b 50 	. ; P   ; Check for RETURN ("play again")
	jp nz,GAME_START_LEVEL_SELECT		;5bc0	c2 cd 5a 	. . Z 
	jr gameover_keycheck		;5bc3	18 d8 	. . 
gameloop_StartFalling:
	ld a,01ah		;5bc5	3e 1a 	> .                       ; A = 26 --- THIS VALUE LOOKS LOST IN TWO LINES
	ld (GAMELOOP_ROWVARIABLE_unknown),a		;5bc7	32 33 81 	2 3 .             ;

MAIN_GAME_KEYBOARD_ACTION_LOOP:

	ld a,KEY_N		;5bca	3e 0f 	> .                         ;N KEY
	call CHECK_FOR_KEY     		;5bcc	cd 3b 50 	. ; P 
	call nz,FLIP_NEXTBRICK_SHOWN		;5bcf	c4 55 5d 	. U ] 

	ld a,KEY_LEFT		;5bd2	3e 02 	> .                      ; LEFT ARROW
	call CHECK_FOR_KEY     		;5bd4	cd 3b 50 	. ; P 
	call nz,MOVE_BRICK_LEFT		;5bd7	c4 60 5d 	. ` ] 

	ld a,KEY_J    					  ;5bda	3e 0c 	> .        ; J KEY
	call CHECK_FOR_KEY     		;5bdc	cd 3b 50 	. ; P 
	call nz,MOVE_BRICK_LEFT		;5bdf	c4 60 5d 	. ` ] 

	ld a,KEY_DOWN 						;5be2	3e 01 	> .        ; DOWN KEY
	call CHECK_FOR_KEY     		;5be4	cd 3b 50 	. ; P 
	call nz,ROTATE_BRICK		  ;5be7	c4 92 5d 	. . ] 

	ld a,KEY_K     						;5bea	3e 0d 	> .        ; K KEY
	call CHECK_FOR_KEY     		;5bec	cd 3b 50 	. ; P 
	call nz,ROTATE_BRICK	  	;5bef	c4 92 5d 	. . ] 
	
	ld a,KEY_RIGHT						;5bf2	3e 03 	> .        ; RIGHT ARROW
	call CHECK_FOR_KEY     		;5bf4	cd 3b 50 	. ; P 
	call nz,MOVE_BRICK_RIGHT	;5bf7	c4 79 5d 	. y ] 

	ld a,KEY_L								;5bfa	3e 0e 	> .        ; L KEY
	call CHECK_FOR_KEY     		;5bfc	cd 3b 50 	. ; P 
	call nz,MOVE_BRICK_RIGHT	;5bff	c4 79 5d 	. y ] 

	ld a,KEY_UP								;5c02	3e 00 	> .        ; UP ARROW
	call CHECK_FOR_KEY     		;5c04	cd 3b 50 	. ; P 
	call nz,DROP_BRICK_SOFT		;5c07	c4 b1 5d 	. . ] 

	ld a,KEY_I								;5c0a	3e 0b 	> .        ; I KEY
	call CHECK_FOR_KEY     		;5c0c	cd 3b 50 	. ; P 
	call nz,DROP_BRICK_SOFT		;5c0f	c4 b1 5d 	. . ] 

	ld a,KEY_CAPSLOCK					;5c12	3e 07 	> .                ;CAPS LOCK
	call CHECK_FOR_KEY     		;5c14	cd 3b 50 	. ; P 
	call nz,DROP_BRICK_SOFT		;5c17	c4 b1 5d 	. . ] 

	ld a,KEY_H								;5c1a	3e 0a 	> .                ; H KEY
	call CHECK_FOR_KEY     		;5c1c	cd 3b 50 	. ; P 
	call nz,INCREASE_LEVEL		;5c1f	c4 d6 5d 	. . ] 

	ld a,KEY_SPACE						;5c22	3e 05 	> .                ; Space Bar
	call CHECK_FOR_KEY     		;5c24	cd 3b 50 	. ; P 
	jr nz,DROP_BRICK_HARD									;5c27	20 07 	  . 

	ld a,KEY_SHIFT						;5c29	3e 06 	> .               ;SHIFT key
	call CHECK_FOR_KEY     		;5c2b	cd 3b 50 	. ; P 
	jr z,keychecks_done		  	;5c2e	28 22 	( " 

DROP_BRICK_HARD:
	call CLEAR_KEYBOARD_TABLE		;5c30	cd 6d 50 	. m P 
	ld b,020h									;5c33	06 20 	.           ; ERASE BRICK
	call DRAW_BRICK		;5c35	cd b2 53 	. . S 
	ld hl,CURBRICK_Y		;5c38	21 f3 80 	! . . 
dbh_keepdropping:
	inc (hl)			;5c3b	34 	4 
	call GAMESCORE_Add_One		;5c3c	cd 16 53 	. . S ; ADD one point for each row dropped
	call CheckBrickCollision		;5c3f	cd ca 53 	. . S 
	jr z,dbh_keepdropping		;5c42	28 f7 	( .   ;If not yet collided, keep dropping.
	dec (hl)			;5c44	35 	5 
	call GAMESCORE_Subtract1		;5c45	cd 25 53 	. % S 
	ld hl,CURSHAPE		;5c48	21 f0 80 	! . . ;Get current shape
	ld b,(hl)			;5c4b	46 	F             ; B = SHAPE
	call DRAW_BRICK		;5c4c	cd b2 53 	. . S 
	jp gameloop_LANDING		;5c4f	c3 81 5c 	. . \ 

keychecks_done:
	call CLEAR_KEYBOARD_TABLE		;5c52	cd 6d 50 	. m P 
	ld hl,GAMELOOP_ROWVARIABLE_unknown		;5c55	21 33 81 	! 3 . 
	dec (hl)			;5c58	35 	5                        ; GAMELOOP_ROWVARIABLE_unknown--;
	jr nz,GLCOUNTDOWN_NOTZERO		;5c59	20 20          ; if zero, continue here:
	ld (hl), 26		;5c5b	36 1a 	6 .                  ; GAMELOOP_ROWVARIABLE_unknown = 26 (ROW related number, bottom?)

  ; THIS is where we check to see if the brick has landed 
  ; (when falling naturally). 

	ld hl,CURBRICK_Y		;5c5d	21 f3 80 	! . .        ; DROP TEMPORARILY
	inc (hl)			;5c60	34 	4                        ; TO TEST COLLISION
	call CheckBrickCollision		;5c61	cd ca 53 	. . S; Check to see if this brick has landed
	ld hl,CURBRICK_Y		;5c64	21 f3 80 	! . .        ; 
	dec (hl)			;5c67	35 	5                        ; RESTORE BRICK HEIGHT after check
	or a			;5c68	b7 	.                            ; Check zero flag on A
	jr nz,gameloop_LANDING		;5c69	20 16 	  .      ; If zero, continue fall. If A>0, we have landed!
	ld b,020h		;5c6b	06 20 	.                      ; 020h to ERASE BRICK
	call DRAW_BRICK		;5c6d	cd b2 53 	. . S          ; ERASE BRICK previous position
	ld hl,CURBRICK_Y		;5c70	21 f3 80 	! . .        ; GET previous POSITION
	inc (hl)			;5c73	34 	4                        ; Drop it down
	ld hl,CURSHAPE		;5c74	21 f0 80 	! . .          ; Get brick shape
	ld b,(hl)			;5c77	46 	F                        ; B = SHAPE
	call DRAW_BRICK		;5c78	cd b2 53 	. . S          ; Draw the brick in next position, continuing fall.
GLCOUNTDOWN_NOTZERO:                               ;  GAMELOOP_ROWVARIABLE_unknown is zero.
	call LEVEL_DIFFICULTY_DELAY		;5c7b	cd c0 54 	. . T 
	jp MAIN_GAME_KEYBOARD_ACTION_LOOP	;5c7e	c3 ca 5b 	. . [ ; Continue falling & listening to keystrokes
	
  ; Here we deal with the end of a brick's 
  ; travel downward, detected above.  The brick has landed.  
  ; Now we need to:
  ;  - mark the brick present in its resting place, 
  ;  - check for a row completion
  ;   -> Drop superior bricks if there is a row completion
  ;  - play music
  ;  - score
  
gameloop_LANDING:     
	ld a,000h		;5c81	3e 00 	> . 
	ld (FIELD_NOREDRAW),a		;5c83	32 3e 81 	2 > . ; Reset NOREDRAW.  (Turn on Redraw.)
	call LEVEL_DIFFICULTY_DELAY		;5c86	cd c0 54 	. . T 
	ld c,000h		;5c89	0e 00 	. .        ; FIRST square of this brick
nextsquare_of_4:
	call GetBrickSubsquarePosition		;5c8b	cd e7 53 	. . S 
	call SQUAREYX_DE_MEMLOC_to_HL		;5c8e	cd e5 5d 	. . ] 
	ld a,(CURSHAPE)		;5c91	3a f0 80 	: . . 
	or 080h		;5c94	f6 80 	. .          ; Mark high bit, this means a square is present, solidified
	ld (hl),a			;5c96	77 	w            ; MARK A SQUARE PRESENT IN THE PLAYFIELD
	inc c			;5c97	0c 	.                ; Next square
	ld a,c			;5c98	79 	y              ; into A to compare...
	cp 004h		;5c99	fe 04 	. .          ; There are 4 squares in a brick
	jp c,nextsquare_of_4		;5c9b	da 8b 5c 	. . \  ; Repeat until no more squares

; I think this is where we check for a row completion...
	ld b,000h		;5c9e	06 00 	. .        ; 
	push bc			;5ca0	c5 	.              ;
	ld d,000h		;5ca1	16 00 	. .        ; Y = 0 for SQUAREYX_DE_MEMLOC_to_HL
ckrows_nextrow:
	ld c,000h		;5ca3	0e 00 	. .        ; c = 0, boolean sum to watch for empty square
	ld e,001h		;5ca5	1e 01 	. .        ; X = 1 for SQUAREYX_DE_MEMLOC_to_HL
ckrow_nextbox:
	push de			;5ca7	d5 	.              ;
	call SQUAREYX_DE_MEMLOC_to_HL		;5ca8	cd e5 5d 	. . ] 
	pop de			;5cab	d1 	. 
	ld a,(hl)			;5cac	7e 	~            ; READ square status into A
	cpl			;5cad	2f 	/                  ; COMPLEMENT, look for empty squares
	or c			;5cae	b1 	.                ; Empty square inverted will have 1 in bit 7
	ld c,a			;5caf	4f 	O              ; Save in C
	inc e			;5cb0	1c 	.                ; X = X + 1
	ld a,e			;5cb1	7b 	{              ; into A to compare...
	cp 00bh		;5cb2	fe 0b 	. .          ; < 11?
	jr c,ckrow_nextbox		;5cb4	38 f1 8 . ; if less, check another box
	ld a,c			;5cb6	79 	y              ; Otherwise, look at the boolean sum.
	bit 7,a		;5cb7	cb 7f 	. .          ; Check bit 7, indicates if anything this row was empty 
	jp nz,ckrow_incompl		;5cb9	c2 ec 5c 	. . \ ; If not zero, nothing to do, incomplete row.
	push de			;5cbc	d5 	.              ; Otherwise we have a completed row! Work to do...
drop_prev_row:
	push de			;5cbd	d5 	.              ; SAVE D
	call GET_GAMEFIELD_ROW_D_POINTER_in_HL	;5cbe	cd f2 5d 	. . ] ; Get the address of this row
	ld d,h			;5cc1	54 	T              ;
	ld e,l			;5cc2	5d 	]              ; DE = HL, which is the ROW ADDRESS
	ld hl,-12		;5cc3	21 f4 ff 	! . .    ; HL = -12
	add hl,de			;5cc6	19 	.            ; Point up by one row
	ld bc,0000ah		;5cc7	01 0a 00 	. . .; BC = 10
	ldir		;5cca	ed b0 	. .            ; (HL++)->(DE++) BC times.  That moved down the row above this!
	pop de			;5ccc	d1 	.              ; Recall D
	dec d			;5ccd	15 	.                ; Move up one row
	ld a,d			;5cce	7a 	z              ; A = D for test
	or a			;5ccf	b7 	.                ; Check if zero
	jr nz,drop_prev_row		;5cd0	20 eb .  ; if row = 0, we are done.  Otherwise, previous row
	pop de			;5cd2	d1 	.              ; get D 
	ld hl,GAME_FIELD+1 ;5cd3 21 69 7e ! i ~; HL = GAMEFIELD top row
	ld b,00ah		;5cd6	06 0a 	. .        ; B = 10
clr_sq_next:
	ld (hl),000h		;5cd8	36 00 	6 .    ; Zero this row's square...
	inc hl			;5cda	23 	#              ; do it 10 times.
	djnz clr_sq_next		;5cdb	10 fb 	. .; Decrement B and jump if not zero
	pop bc			;5cdd	c1 	.              ; 
	inc b			;5cde	04 	.                ; 
	ld hl,(LINECOUNT_L)		;5cdf	2a f7 80 	* . .    ;
	inc hl			;5ce2	23 	#                        ;
	ld (LINECOUNT_L),hl		;5ce3	22 f7 80 	" . .    ;
	ld a,001h		;5ce6	3e 01 	> .                  ;
	ld (FIELD_NOREDRAW),a		;5ce8	32 3e 81 	2 > .  ;   Stop Redrawing (why?)
	push bc			;5ceb	c5 	.                        ;
ckrow_incompl:
	inc d			;5cec	14 	. 
	ld a,d			;5ced	7a 	z 
	cp 01ah		;5cee	fe 1a 	. . 
	jp c,ckrows_nextrow		;5cf0	da a3 5c 	. . \ 
	pop bc			;5cf3	c1 	. 
	ld a,b			;5cf4	78 	x             ; A = B
	or a			;5cf5	b7 	.               ; Test A
	jr z,done_checking_completion		;5cf6	28 2b 	( +  
	                             ; If zero, no rows cleared.  No scoring, no music, next brick.
	                             
	cp 004h		;5cf8	fe 04 	. .         ; Otherwise, check that it isn't >4!
	jr c,tallied_ok ;5cfa	38 02 	8 .   ;  I'm confused about this logic, sorry... 
	ld b,004h		;5cfc	06 04 	. .       ;  I think this is B = 4 in case B > 4.
tallied_ok:
	ld c,b			;5cfe	48 	H      ; C = B, and is how many rows were cleared!
	dec c			;5cff	0d 	.        ; subtract 1 
	                             ; C now contains the number of rows we have cleared -1
	sla c		;5d00	cb 21 	. !    ; Multiply by four.  
	sla c		;5d02	cb 21 	. !    ; Doing this to index an array of melodies & scores
	ld b,000h		;5d04	06 00 	. .; Now the index is in BC.
	ld hl,Melody_score_table ;5d06	21 ac 65 	! . e ;
	add hl,bc			;5d09	09 	.    ; BC contains the number of cleared rows *4, index the table 
	ld e,(hl)			;5d0a	5e 	^    ; 
	inc hl			;5d0b	23 	#      ;
	ld d,(hl)			;5d0c	56 	V    ;
	inc hl			;5d0d	23 	#      ;  Get four bytes from the table.  DE is the melody pointer.
	ld c,(hl)			;5d0e	4e 	N    ;  BC is scoring.
	inc hl			;5d0f	23 	#      ;
	ld b,(hl)			;5d10	46 	F    ;
	call PLAY_MELODY		;5d11	cd 05 51 	. . Q       ; PLAY MELODY based on cleared rows
	ld hl,(GAMESCORE1)		;5d14	2a f9 80 	* . .     ; SCORE
	add hl,bc			;5d17	09 	.                       ; Add scoring of this # of rows, to the score
	ld (GAMESCORE1),hl		;5d18	22 f9 80 	" . .     ; store the score
	ld a,(GAMESCORE3)		;5d1b	3a fb 80 	: . .       ;
	adc a,000h		;5d1e	ce 00 	. .                 ; Carry.
	ld (GAMESCORE3),a		;5d20	32 fb 80 	2 . .       ;
done_checking_completion:
	call UpdateScore		;5d23	cd 96 52 	. . R 
	call UpdateLinesCount		;5d26	cd d7 52 	. . R 
	ld a,(FIELD_NOREDRAW)		;5d29	3a 3e 81 	: > . 
	cp 001h		;5d2c	fe 01 	. . 
	jr c,game_skip_redraw		;5d2e	38 03 	8 . 
	call Draw_Game_Field		;5d30	cd 4e 54 	. N T 
game_skip_redraw:
	ld bc,(LINECOUNT_L)		;5d33	ed 4b f7 80 	. K . .  ; 
	call StrangeBase11_Math_whatisit	;5d37	cd 99 51 	. . Q ;
	ld a,(LEVELNUMBER)		;5d3a	3a fd 80 	: . .      ;
	ld l,a			;5d3d	6f 	o                          ;
	ld h,000h		;5d3e	26 00 	& .                    ;
	or a			;5d40	b7 	.                            ;
	sbc hl,bc		;5d41	ed 42 	. B                    ;
	jp nc,GAME_LOOP_START		;5d43	d2 0e 5b 	. . [    ;
	cp 009h		;5d46	fe 09 	. .                      ;
	jp nc,GAME_LOOP_START		;5d48	d2 0e 5b 	. . [    ;
	inc a			;5d4b	3c 	<                            ;
	ld (LEVELNUMBER),a		;5d4c	32 fd 80 	2 . .      ;
	call UpdateLevel		;5d4f	cd c2 52 	. . R        ;
	jp GAME_LOOP_START		;5d52	c3 0e 5b 	. . [      ;

FLIP_NEXTBRICK_SHOWN:
	ld a,(NEXTBRICK_ON)		;5d55	3a f6 80 	: . .      ;
	cpl			;5d58	2f 	/     ;INVERT                  ; COMPLEMENT
	ld (NEXTBRICK_ON),a		;5d59	32 f6 80 	2 . . 
	call DrawNextBrickIfEnabled		;5d5c	cd 83 54 	. . T 
	ret			;5d5f	c9 	. 


MOVE_BRICK_LEFT:                                   ;
	ld hl,CURBRICKXYLH		;5d60	21 f2 80 	! . .    ;BRICK X 
	dec (hl)			;5d63	35 	5                ; Move left,
	call CheckBrickCollision		;5d64	cd ca 53 	. . S  ; Check limit left
	inc (hl)			;5d67	34 	4                ;move right.
	or a			;5d68	b7 	. 
	jr nz,mbl_exit		;5d69	20 0d 	  .        ; Don't do anything if we have detected a collision
	ld b,020h		;5d6b	06 20 	.   
	call DRAW_BRICK		;5d6d	cd b2 53 	. . S 
	dec (hl)			;5d70	35 	5     
	ld hl,CURSHAPE		;5d71	21 f0 80 	! . .    ; 
	ld b,(hl)			;5d74	46 	F                ; b = SHAPE
	call DRAW_BRICK		;5d75	cd b2 53 	. . S  ; Draw the brick
mbl_exit:
	ret			;5d78	c9 	. 
	
	
MOVE_BRICK_RIGHT:
	ld hl,CURBRICKXYLH		;5d79	21 f2 80 	! . .    ; BRICK X 
	inc (hl)			;5d7c	34 	4 
	call CheckBrickCollision		;5d7d	cd ca 53 	. . S   ;Check limit right
	dec (hl)			;5d80	35 	5 
	or a			;5d81	b7 	. 
	jr nz,mbr_exit		;5d82	20 0d 	  .        ; Don't do anything if we have detected a collision
	ld b,020h		;5d84	06 20 	.   
	call DRAW_BRICK		;5d86	cd b2 53 	. . S 
	inc (hl)			;5d89	34 	4 
	ld hl,CURSHAPE		;5d8a	21 f0 80 	! . . 
	ld b,(hl)			;5d8d	46 	F 
	call DRAW_BRICK		;5d8e	cd b2 53 	. . S 
mbr_exit:
	ret			;5d91	c9 	. 


ROTATE_BRICK:
	ld hl,CURORIENT		;5d92	21 f1 80 	! . . 
	ld a,(hl)			;5d95	7e 	~     ; GET THE ROTATION VALUE
	ld b,a			;5d96	47 	G       ; SAVE THE ROTATION VALUE
	inc a			;5d97	3c 	<         ; INCREMENT THE ROTATION VALUE
	and 003h		;5d98	e6 03 	. . ; modulo 4
	ld (hl),a			;5d9a	77 	w     ; STORE the new value
	ld c,a			;5d9b	4f 	O       ; Rotation in c?
	call CheckBrickCollision	 ;5d9c	cd ca 53 	. . S ; CHECK FOR COLLISION
	ld (hl),b			;5d9f	70 	p     ; PUT BACK THE OLD ROTATION VALUE?
	or a			;5da0	b7 	.         ; Test 'a' for response
	jr nz,rb_exit		;5da1	20 0d . ; If it's a problem don't rotate, bail.
	ld b,020h		;5da3	06 20 	.   ; SPACES to wipe out the current shape
	call DRAW_BRICK		;5da5	cd b2 53 	. . S  ; DRAW BRICK
	ld (hl),c			;5da8	71 	q     ; KEEP THE NEW ROTATION VALUE
	ld hl,CURSHAPE		;5da9	21 f0 80 	! . . 
	ld b,(hl)			;5dac	46 	F 
	call DRAW_BRICK		;5dad	cd b2 53 	. . S 
rb_exit:
	ret			;5db0	c9 	. 
	
	
DROP_BRICK_SOFT:
	ld hl,CURBRICK_Y		;5db1	21 f3 80 	! . . 
	ld c,(hl)			;5db4	4e 	N                ; SAVE IT: C = CURBRICK_Y
dbh_keep_dropping:
	inc (hl)			;5db5	34 	4                ; DROP THE BRICK ONE LEVEL
	call CheckBrickCollision	;5db6	cd ca 53 	. . S ; CHECK FOR COLLISION
	jr z,dbh_keep_dropping		;5db9	28 fa 	( .; LOWER UNTIL COLLISION
	ld a,(hl)			;5dbb	7e 	~                ; A = CURBRICK_Y; SAVE THE LOWEST POSITION IN A
	ld (hl),c			;5dbc	71 	q                ; REVERT TO STARTING POSITION
	sub 002h		;5dbd	d6 02 	. .            ; A = A - 2.
	cp c			;5dbf	b9 	.                    ; Do A-C to get flags
	jr c,dbh_exit		;5dc0	38 13 	8 .        ; If A<C, exit.  This is if ( ?? didn't get it...) 
	ld c,a			;5dc2	4f 	O 
	ld b,020h		;5dc3	06 20 	.              ; Clear the old brick
	call DRAW_BRICK		;5dc5	cd b2 53 	. . S 
	ld (hl),c			;5dc8	71 	q 
	ld hl,CURSHAPE		;5dc9	21 f0 80 	! . . 
	ld b,(hl)			;5dcc	46 	F 
	call DRAW_BRICK		;5dcd	cd b2 53 	. . S  ;Draw the new brick
	ld hl,GAMELOOP_ROWVARIABLE_unknown		;5dd0	21 33 81 	! 3 . 
	ld (hl),00ah		;5dd3	36 0a 	6 .        ; GAMELOOP_ROWVARIABLE_unknown = 10  (why?)
dbh_exit:
	ret			;5dd5	c9 	. 
	
	
INCREASE_LEVEL:
	ld a,(LEVELNUMBER)		;5dd6	3a fd 80 	: . . 
	cp 009h		;5dd9	fe 09 	. .            ; Compare a with 9
	jr nc,il_bail		;5ddb	30 07 	0 .      ; Exit if can't increase
	inc a			;5ddd	3c 	<                  ; increment if possible
	ld (LEVELNUMBER),a		;5dde	32 fd 80 	2 . . 
	call UpdateLevel		;5de1	cd c2 52 	. . R 
il_bail:
	ret			;5de4	c9 	. 
	
; RETURN square DE's memory Location in HL
SQUAREYX_DE_MEMLOC_to_HL:
	ld l,d			;5de5	6a 	j               ; L = D, which is Y of square
	ld h,00ch		;5de6	26 0c 	& .         ; H = 12.  
  mlt hl ;defb 0edh,06ch;5de8	ed 6c	. l ; Multiply L ("y") by 12 
	ld d,000h		;5dea	16 00 	. .         ; 
	add hl,de			;5dec	19 	.             ; Add the X position.
	ld de,GAME_FIELD ;5ded	11 68 7e 	. h ~ ; Offset to array at 7e68
	add hl,de			;5df0	19 	.   ; Return address of square in HL
	ret			;5df1	c9 	. 


; ARGUMENT:  D = row of game field
GET_GAMEFIELD_ROW_D_POINTER_in_HL:
	ld l,d			;5df2	6a 	j 
	ld h,00ch		;5df3	26 0c 	& .                  ; H = 12
  mlt hl      ;	defb 0edh,06ch	;5df5	ed 6c	. l  ; HL = D * 12
	ld d,000h		;5df7	16 00 	. .                  ; D = 0
	inc hl			;5df9	23 	#                        ; add 1 
	ld de,GAME_FIELD		;5dfa	11 68 7e 	. h ~      ; DE = game field
	add hl,de			;5dfd	19 	.                      ; HL = Pointer to row in game field
	ret			;5dfe	c9 	.
	 

ThreeDeeLogoSwooshes:
	push bc			;5dff	c5 	. 
	push de			;5e00	d5 	. 
	push hl			;5e01	e5 	. 
	push ix		;5e02	dd e5 	. . 
	call Screen_Change_Clear		;5e04	cd 15 54 	. . T 
	call SwooshBufferB_Clear_to_Zero		;5e07	cd 6e 5f 	. n _ 
	ld ix,brotherlogo_3d_table		;5e0a	dd 21 52 60 	. ! R ` 
	ld b,02eh		;5e0e	06 2e 	. .   ; Countdown variable, 46!
BrotherLogo3DSwoosh:
	ld hl,05a00h		;5e10	21 00 5a 	! . Z 
	ld iy,brother_logo		;5e13	fd 21 c2 61 	. ! . a 
	call SwooshLogo3DRenderCode		;5e17	cd 9c 5e 	. . ^ 
	ld a,KEY_SPACE		;5e1a	3e 05 	> .                   ; Space Bar to exit
	call CHECK_FOR_KEY     		;5e1c	cd 3b 50 	. ; P 
	jr nz,exit_swoosh_demo		;5e1f	20 40 	  @ 
	inc ix		;5e21	dd 23 	. # 
	inc ix		;5e23	dd 23 	. # 
	inc ix		;5e25	dd 23 	. # 
	inc ix		;5e27	dd 23 	. # 
	djnz BrotherLogo3DSwoosh		;5e29	10 e5 	. .    ; Decrement B and jump if not zero.  46 times.
	call SwooshBufferCopyA_to_B		;5e2b	cd 82 5f 	. . _ 
	ld ix,tetrislogo_3d_table		;5e2e	dd 21 0a 61 	. ! . a 
	ld b,02eh		;5e32	06 2e 	. .    ; Countdown variable, 46!
TetrisLogo3DSwoosh:
	ld hl,09600h		;5e34	21 00 96 	! . . 
	ld iy,tetris_logo		;5e37	fd 21 42 63 	. ! B c 
	call SwooshLogo3DRenderCode		;5e3b	cd 9c 5e 	. . ^ 
	ld a,KEY_SPACE		;5e3e	3e 05 	> .                  ; Space Bar to exit
	call CHECK_FOR_KEY     		;5e40	cd 3b 50 	. ; P 
	jr nz,exit_swoosh_demo		;5e43	20 1c 	  . 
	inc ix		;5e45	dd 23 	. # 
	inc ix		;5e47	dd 23 	. # 
	inc ix		;5e49	dd 23 	. # 
	inc ix		;5e4b	dd 23 	. # 
	djnz TetrisLogo3DSwoosh		;5e4d	10 e5 	. .    ; Decrement B and jump if not zero.  46 times.
	ld bc,007d0h		;5e4f	01 d0 07 	. . .          ; DELAY LOOP TIMER: Delay 2000
logoshow_delayloop:
	call SCAN_KEYBOARD		;5e52	cd 8d 50 	. . P    
	ld a,KEY_SPACE		;5e55	3e 05 	> .                   ; Space Bar
	call CHECK_FOR_KEY     		;5e57	cd 3b 50 	. ; P 
	jr nz,exit_swoosh_demo		;5e5a	20 05 	  . 
	dec bc			;5e5c	0b 	.                         ; with delay of 2000
	ld a,b			;5e5d	78 	x 
	or c			;5e5e	b1 	. 
	jr nz,logoshow_delayloop		;5e5f	20 f1 	  . 
exit_swoosh_demo:
	call CLEAR_KEYBOARD_TABLE		;5e61	cd 6d 50 	. m P 
	pop ix		;5e64	dd e1 	. . 
	pop hl			;5e66	e1 	. 
	pop de			;5e67	d1 	. 
	pop bc			;5e68	c1 	. 
	ret			;5e69	c9 	. 
	
DE_MULTIPLY_64:
	sla e		;5e6a	cb 23 	. # 
	rl d		;5e6c	cb 12 	. . 
	sla e		;5e6e	cb 23 	. # 
	rl d		;5e70	cb 12 	. . 
	sla e		;5e72	cb 23 	. # 
	rl d		;5e74	cb 12 	. . 
	sla e		;5e76	cb 23 	. # 
	rl d		;5e78	cb 12 	. . 
	sla e		;5e7a	cb 23 	. # 
	rl d		;5e7c	cb 12 	. . 
	sla e		;5e7e	cb 23 	. # 
	rl d		;5e80	cb 12 	. . 
	ret			;5e82	c9 	. 

;MATH:
; PARAMETER in DE.  
; RETURN VALUES:
; DE = DE * 6
; HL = DE * 4
DEx6toDE_and_DEx4toHL:
	sla e		;5e83	cb 23 	. # 
	rl d		;5e85	cb 12 	. . 
	sla e		;5e87	cb 23 	. # 
	rl d		;5e89	cb 12 	. .  ; Shift DE left twice, MULTIPLYING by 4 (no carry out)
	push hl			;5e8b	e5 	. 
	ld h,d			;5e8c	62 	b 
	ld l,e			;5e8d	6b 	k    ; HL = DE
	add hl,de			;5e8e	19 	.  ; Add DE to HL
	add hl,de			;5e8f	19 	.  ; Add DE to HL again.  Now HL = PARAMETER * 6.
	ex de,hl			;5e90	eb 	.  ; Now DE = PARAMETER * 6.
	pop hl			;5e91	e1 	.    ; HL = PARAMETER * 4.
	ret			;5e92	c9 	. 

; If E>128, put 255 n D.  Otherwise put 0 in D.
; Note this isn't quite a two's-complement sign,
; because -128 (10000000) comes out as "positive" (0).
; Also returns C/Carry and N/Negative flags according to the result.
ExtendSign_E_to_DE:
	ld a,e			;5e93	7b 	{ 
	add a,07fh		;5e94	c6 7f 	. .   ; Add 127 to the E value
	ld a,000h		;5e96	3e 00 	> .      ; A=0
	sbc a,000h		;5e98	de 00 	. .    ; CARRY: Subtract the carry?
	ld d,a			;5e9a	57 	W            ; Return this in D??
	ret			;5e9b	c9 	. 

; What follows is the 3d demo rendering code.
; Also it doesn't really look 3d to me; just something that feels 3d.
; More of a combination of skew and 2D rotation.  But I haven't dissected it.

; Swoosh / Demo code - seems like this is the 3d stuff
; CONTAINS SELF-MODIFYING CODE
; Probably a routine to produce one frame of the 3d flying logos.
; It is called 46 times.
; ARGUMENTS:
; ix = brotherlogo_3d_table or tetrislogo_3d_table   ; This presumably defines the 3d motion
;	iy = brother_logo or tetris_logo
; hl = 05a00h or 09600h
;countdown lin
; The logos are bitmaps 128 pixels wide (16 bytes) and 24 rows high.
; (Total 384 bytes each.)

SwooshLogo3DRenderCode:
	push bc			;5e9c	c5 	. 
	push de			;5e9d	d5 	. 
	push hl			;5e9e	e5 	. 
	push ix		;5e9f	dd e5 	. . 
	push iy		;5ea1	fd e5 	. . 
	call SwooshBufferCopyB_to_A		;5ea3	cd 96 5f 	. . _ 
	call SCAN_KEYBOARD		;5ea6	cd 8d 50 	. . P 
	ld e,(ix+000h)		;5ea9	dd 5e 00 	. ^ .                     ; DE = First byte from table entry
	call ExtendSign_E_to_DE		;5eac	cd 93 5e 	. . ^             ; Turn the byte into a 16-bit number
	ld (MODIFYCODEOPER1),de		;5eaf	ed 53 36 5f 	. S 6 _       ; OPER1 below   SELF MODIFYING CODE
	call DE_MULTIPLY_64		;5eb3	cd 6a 5e 	. j ^                 ; DE = DE * 64
	or a			;5eb6	b7 	.                                       ; ? flags update?
	sbc hl,de		;5eb7	ed 52 	. R                               ; HL = HL - DE
	ld e,(ix+001h)		;5eb9	dd 5e 01 	. ^ .                     ; DE = 2nd byte from table entry
	call ExtendSign_E_to_DE		;5ebc	cd 93 5e 	. . ^             ; (make 16-bit)
	ld (MODIFYCODEOPER3),de		;5ebf	ed 53 49 5f 	. S I _       ; OPER3 below   SELF MODIFYING CODE
	call DEx6toDE_and_DEx4toHL		;5ec3	cd 83 5e 	. . ^         ; math
	add hl,de			;5ec6	19 	.                                   ; HL = DE * 10
	ld (DEMO_VAR1),hl		;5ec7	22 41 81 	" A .                   ; DEMO_VAR1 = HL
	ld hl,01000h		;5eca	21 00 10 	! . .                       ; HL = 4096  This is the image buffer size
	ld e,(ix+002h)		;5ecd	dd 5e 02 	. ^ .                     ; DE = 3rd byte from table entry
	call ExtendSign_E_to_DE		;5ed0	cd 93 5e 	. . ^             ; (make 16-bit)
	ld (MODIFYCODEOPER2),de		;5ed3	ed 53 3b 5f 	. S ; _       ; OPER2 below   SELF MODIFYING CODE
	call DE_MULTIPLY_64		;5ed7	cd 6a 5e 	. j ^                 ; DE = DE * 64
	or a			;5eda	b7 	.                                       ; ? flags update?
	sbc hl,de		;5edb	ed 52 	. R                               ; HL = HL - DE
	ld e,(ix+003h)		;5edd	dd 5e 03 	. ^ .                     ; DE = 4th byte from table entry
	call ExtendSign_E_to_DE		;5ee0	cd 93 5e 	. . ^             ; (make 16-bit)
	ld (MODIFYCODEOPER4),de		;5ee3	ed 53 53 5f 	. S S _       ; OPER4 below   SELF MODIFYING CODE
	call DEx6toDE_and_DEx4toHL		;5ee7	cd 83 5e 	. . ^         ; math
	or a			;5eea	b7 	.                                       ; ? flags update?
	sbc hl,de		;5eeb	ed 52 	. R                               ; HL = HL - DE
	ld (DEMO_VAR2),hl		;5eed	22 43 81 	" C .                   ; DEMO_VAR2 = HL
	ld b,018h		;5ef0	06 18 	. .                   ; B = 24. This is the number of rows in each logo
s3d_loop_outer_nextline:
	ld ix,(DEMO_VAR1)		;5ef2	dd 2a 41 81 	. * A .             ; IX = DEMO_VAR1 contents
	ld hl,(DEMO_VAR2)		;5ef6	2a 43 81 	* C .                   ; HL = DEMO_VAR2 contents
	ld c,010h		;5ef9	0e 10 	. .                    ; C = 16. This is the byte width for the logos.
s3d_loop_mid_nextbyte:
	push bc			;5efb	c5 	.                                     ; stow B, C counters
	ld d,(iy+000h)		;5efc	fd 56 00 	. V .                     ; Get byte from LOGO IMAGE
	inc iy		;5eff	fd 23 	. #                                 ; queue next byte
	ld e,008h		;5f01	1e 08 	. .                    ; E = 8.  There are 8 pixels per byte.
s3d_loop_inner_nextpixel:
	bit 7,d		;5f03	cb 7a 	. z                                 ; Get one bit of image data.
	jr z,s3d_done2		;5f05	28 2c 	( ,                         ; If zero, don't need to render it!
	push hl			;5f07	e5 	.                                     ; stow DEMO_VAR2
	srl h		;5f08	cb 3c 	. <                                   ; 
	rr l		;5f0a	cb 1d 	. .                                   ; HL >>= 1 (zero into bit 15)
	ld a,0c0h		;5f0c	3e c0 	> .                               ; A = 192. 
	and l			;5f0e	a5 	.                                       ; A &= L    get top two bits
	push ix		;5f0f	dd e5 	. .                                 ; Stow IX
	pop bc			;5f11	c1 	.                                     ; BC = IX
	push bc			;5f12	c5 	.                                     ; Stow BC/IX
	srl b		;5f13	cb 38 	. 8                                   ; .. 
	srl b		;5f15	cb 38 	. 8                                   ; B >>= 2  (two zeroes into bit 7, 6)
	add a,b			;5f17	80 	.           ; A = top two bits + B (with zero in top two bits) => PPQQQQQQ
	ld l,a			;5f18	6f 	o           ; L = A
	ld a,h			;5f19	7c 	|           ; A = H 
	adc a,000h		;5f1a	ce 00 	. .   ; CARRY for high byte
	ld h,a			;5f1c	67 	g           ; H = A.  H just got a carry bit from previous add.
	ld bc,DEMOWORKSPACE1		;5f1d	01 5b 81 	. [ .    ; BC = DEMOWORKSPACE1
	add hl,bc			;5f20	09 	.                        ; LOCATE DEMOWORKSPACE[HL] HL = HHHHHHHHPPQQQQQQ ? 
	pop bc			;5f21	c1 	.                          ; BC = previous IX (DEMO_VAR1 contents last pushed)
	ld a,b			;5f22	78 	x                          ; A = B
	sla c		;5f23	cb 21 	. !                        ; C <<= 1  (low bit = 0)
	rla			;5f25	17 	.                              ; A <<= 1  (low bit = carry from C's bit 7) so AC<<=1
	and 007h		;5f26	e6 07 	. .                    ; A &= 7 
	ld b,a			;5f28	47 	G                  ; B = A  (Make a number from 0 to 7 for rotation)
	ld a,080h		;5f29	3e 80 	> .            ; A = 0b10000000
	jr z,s3d_noshift		;5f2b	28 03 	( .    ; If this was bit 0, skip the rotate and go to s3d_noshift 
s3d_rotloop:
	rrca			;5f2d	0f 	.                    ; Rotate A with carry, B times.
	djnz s3d_rotloop		;5f2e	10 fd 	. .    ; Decrement B and jump if not zero
s3d_noshift:
	or (hl)			;5f30	b6 	.                  ;
	ld (hl),a			;5f31	77 	w                ;
	pop hl			;5f32	e1 	.                  ;
s3d_done2:
	sla d		;5f33	cb 22 	. "                ; D = D * 2
MODIFYCODEINSTR1:
MODIFYCODEOPER1 equ (MODIFYCODEINSTR1+1)
	ld bc,00000h		;5f35	01 00 00 	. . .       ; THIS INSTRUCTION IS MODIFIED by code above. BC = OPER1
	add ix,bc		;5f38	dd 09 	. .               ; IX = IX + OPER1 
MODIFYCODEINSTR2:
MODIFYCODEOPER2 equ (MODIFYCODEINSTR2+1)
	ld bc,00000h		;5f3a	01 00 00 	. . .       ; THIS INSTRUCTION IS MODIFIED by code above. BC = OPER2
	add hl,bc			;5f3d	09 	.                   ; HL = HL + OPER2
	dec e			;5f3e	1d 	.                                     ; Count down pixels (8 per byte)
	jr nz,s3d_loop_inner_nextpixel		;5f3f	20 c2 	  .       ; Next pixel of this byte
	pop bc			;5f41	c1 	.                                   ; Recall C (and B for later)
	dec c			;5f42	0d 	.                                     ; Count down 16 bytes per scanline
	jr nz,s3d_loop_mid_nextbyte		;5f43	20 b6 	  .           ; Next byte of this scanline
	ld hl,(DEMO_VAR1)		;5f45	2a 41 81 	* A .                 ; HL = DEMO_VAR1
MODIFYCODEINSTR3:
MODIFYCODEOPER3 equ (MODIFYCODEINSTR3+1)
	ld de,00000h		;5f48	11 00 00 	. . .       ; THIS INSTRUCTION IS MODIFIED by code above. DE = OPER3
	add hl,de			;5f4b	19 	.                   ; HL = HL + OPER3
	ld (DEMO_VAR1),hl		;5f4c	22 41 81 	" A .   ; Stow DEMO_VAR1 = HL
	ld hl,(DEMO_VAR2)		;5f4f	2a 43 81 	* C .   ; HL = DEMO_VAR2
MODIFYCODEINSTR4:
MODIFYCODEOPER4 equ (MODIFYCODEINSTR4+1)
	ld de,00000h		;5f52	11 00 00 	. . .       ; THIS INSTRUCTION IS MODIFIED by code above. DE = OPER4
	add hl,de			;5f55	19 	.                   ; HL = HL + OPER4
	ld (DEMO_VAR2),hl		;5f56	22 43 81 	" C .   ; Stow DEMO_VAR1 = HL
	call SCAN_KEYBOARD		;5f59	cd 8d 50 	. . P ;
	dec b			;5f5c	05 	.                       ; Countdown lines to process
	jp nz,s3d_loop_outer_nextline		;5f5d	c2 f2 5e 	. . ^     ; Loop over image, next scanline of 24
	call DisplaySwooshBuffer		;5f60	cd aa 5f 	. . _         ; Present this transformed image
	call SCAN_KEYBOARD		;5f63	cd 8d 50 	. . P               ;
	pop iy		;5f66	fd e1 	. . 
	pop ix		;5f68	dd e1 	. . 
	pop hl			;5f6a	e1 	. 
	pop de			;5f6b	d1 	. 
	pop bc			;5f6c	c1 	. 
	ret			;5f6d	c9 	. 


;Clear 4k of memory at DEMOWORKSPACE2 
SwooshBufferB_Clear_to_Zero:
	push bc			;5f6e	c5 	. 
	push de			;5f6f	d5 	. 
	push hl			;5f70	e5 	. 
	ld hl,DEMOWORKSPACE2		;5f71	21 bb 91 	! . . 
	ld de,DEMOWORKSPACE2+1		;5f74	11 bc 91 	. . . ; Because of the overlap, this clears ~4096 bytes.
	ld bc,00fffh		;5f77	01 ff 0f 	. . . 
	ld (hl),000h		;5f7a	36 00 	6 . 
	ldir		;5f7c	ed b0 	. .                 ; (HL++)->(DE++) BC times.
	pop hl			;5f7e	e1 	. 
	pop de			;5f7f	d1 	. 
	pop bc			;5f80	c1 	. 
	ret			;5f81	c9 	. 
	
	
SwooshBufferCopyA_to_B:
	push bc			;5f82	c5 	. 
	push de			;5f83	d5 	. 
	push hl			;5f84	e5 	. 
	ld hl,DEMOWORKSPACE1		;5f85	21 5b 81 	! [ . 
	ld de,DEMOWORKSPACE2		;5f88	11 bb 91 	. . . 
	ld bc,00fffh		;5f8b	01 ff 0f 	. . . ; Copy 4096 bytes from $815B to $91BB.
	ld (hl),000h		;5f8e	36 00 	6 .     ;  Zero first byte.  Probably a remnant of the zeroing routine.
	ldir		;5f90	ed b0 	. .                 ; (HL++)->(DE++) BC times.
	pop hl			;5f92	e1 	. 
	pop de			;5f93	d1 	. 
	pop bc			;5f94	c1 	. 
	ret			;5f95	c9 	. 
	
	
SwooshBufferCopyB_to_A:
	push bc			;5f96	c5 	. 
	push de			;5f97	d5 	. 
	push hl			;5f98	e5 	. 
	ld hl,DEMOWORKSPACE2		;5f99	21 bb 91 	! . . 
	ld de,DEMOWORKSPACE1		;5f9c	11 5b 81 	. [ . ; Copy 4096 bytes from $91BB back to $815B.
	ld bc,00fffh		;5f9f	01 ff 0f 	. . . ; 
	ld (hl),000h		;5fa2	36 00 	6 .     ;  Zero first byte.  Probably a remnant of the zeroing routine
	ldir		;5fa4	ed b0 	. .                 ; (HL++)->(DE++) BC times.
	pop hl			;5fa6	e1 	. 
	pop de			;5fa7	d1 	. 
	pop bc			;5fa8	c1 	. 
	ret			;5fa9	c9 	. 
	
	
; This presents the manipulated buffer image at DEMOWORKSPACE1 to the display
; ARGUMENTS:
; IMAGE is in DEMOWORKSPACE1.
; BUFFERPOINTER1, we are gonna copy DEMOWORKSPACE1 to there and then present it.
; Other arguments?
;

DisplaySwooshBuffer:
	push bc			;5faa	c5 	. 
	push de			;5fab	d5 	. 
	push hl			;5fac	e5 	. 
	ld hl,DEMOWORKSPACE1		;5fad	21 5b 81 	! [ . 
	ld d,05fh		;5fb0	16 5f 	. _ 
	ld e,00dh		;5fb2	1e 0d 	. . 
	ld b,040h		;5fb4	06 40 	. @ 
dsb_next:
	push bc			;5fb6	c5 	. 
	push hl			;5fb7	e5 	. 
	push de			;5fb8	d5 	. 
	ld de,(BUFFERPOINTER1)		;5fb9	ed 5b 3f 81 	. [ ? . 
	ld bc,00040h		;5fbd	01 40 00 	. @ . 
	ldir		;5fc0	ed b0 	. .                 ; (HL++)->(DE++) BC times.  (64 times)
	pop de			;5fc2	d1 	.                   ; 
	ld hl,(BUFFERPOINTER1)		;5fc3	2a 3f 81 	* ? . 
	ld c,040h		;5fc6	0e 40 	. @             ; 64 bytes
	ld b,000h		;5fc8	06 00 	. . 
	ld a,028h		;5fca	3e 28 	> ( 
	rst 10h			;5fcc	d7 	.             ; SCREEN COPY
	di			;5fcd	f3 	.                     ; DISABLE INTERRUPTS
	inc d			;5fce	14 	. 
	pop hl			;5fcf	e1 	. 
	ld a,040h		;5fd0	3e 40 	> @           ; 64 again?
	call HL_add_A		;5fd2	cd c3 58 	. . X 
	pop bc			;5fd5	c1 	.              ; 
	djnz dsb_next		;5fd6	10 de 	. .    ; Decrement B and jump if not zero
	pop hl			;5fd8	e1 	. 
	pop de			;5fd9	d1 	. 
	pop bc			;5fda	c1 	. 
	ret			;5fdb	c9 	. 
	
	
Present_Ruler_for_Game_Pause:
	push af			;5fdc	f5 	. 
	push bc			;5fdd	c5 	. 
	push de			;5fde	d5 	. 
	push hl			;5fdf	e5 	. 
	ld hl,(BUFFERPOINTER1)		;5fe0	2a 3f 81 	* ? . 
	ld de,RULER2str		;5fe3	11 09 7c 	. . | 
	call StringPrepForScreenMemory		;5fe6	cd 37 60 	. 7 ` 
	ld b,000h		;5fe9	06 00 	. . 
	ld c,a			;5feb	4f 	O 
	ld d,000h		;5fec	16 00 	. .           ; Line 0 
	ld e,000h		;5fee	1e 00 	. .           ; 
	ld a,023h		;5ff0	3e 23 	> #           ; Looks like this function copies to screen memory.
	rst 10h			;5ff2	d7 	.                 ; Do it!
	di			;5ff3	f3 	.                     ; DISABLE INTERRUPTS 
	ld de,RULERstr		;5ff4	11 3c 7c 	. < | 
	call StringPrepForScreenMemory		;5ff7	cd 37 60 	. 7 ` 
	ld b,000h		;5ffa	06 00 	. .  
	ld c,a			;5ffc	4f 	O 
	ld d,001h		;5ffd	16 01 	. .           ; Line 1
	ld e,000h		;5fff	1e 00 	. .            
	ld a,023h		;6001	3e 23 	> #           
	rst 10h			;6003	d7 	. 
	di			;6004	f3 	.                     ; DISABLE INTERRUPTS 
	ld de,RULER3str		;6005	11 98 7c 	. . | 
	call StringPrepForScreenMemory		;6008	cd 37 60 	. 7 ` 
	ld b,000h		;600b	06 00 	. . 
	ld c,a			;600d	4f 	O 
	ld d,002h		;600e	16 02 	. .           ; Line 2
	ld e,000h		;6010	1e 00 	. . 
	ld a,023h		;6012	3e 23 	> # 
	rst 10h			;6014	d7 	. 
	di			;6015	f3 	.                     ; DISABLE INTERRUPTS 
	ld d,002h		;6016	16 02 	. .           ; Line 2
	ld e,00bh		;6018	1e 0b 	. .           ; Column 11
	ld a,001h		;601a	3e 01 	> .           ; CURSOR. DIFFERENT COMMAND from previous.
	rst 10h			;601c	d7 	. 
	di			;601d	f3 	.                     ; DISABLE INTERRUPTS 
	ld b,001h		;601e	06 01 	. . 
	ld a,002h		;6020	3e 02 	> . 
	rst 10h			;6022	d7 	. 
	di			;6023	f3 	.                     ; DISABLE INTERRUPTS 
	ld d,001h		;6024	16 01 	. . 
	ld e,00bh		;6026	1e 0b 	. . 
	ld a,004h		;6028	3e 04 	> . 
	rst 10h			;602a	d7 	. 
	di			;602b	f3 	.                     ; DISABLE INTERRUPTS 
	ld b,001h		;602c	06 01 	. . 
	ld a,005h		;602e	3e 05 	> . 
	rst 10h			;6030	d7 	. 
	di			;6031	f3 	.                      ; DISABLE INTERRUPTS
	pop hl			;6032	e1 	. 
	pop de			;6033	d1 	. 
	pop bc			;6034	c1 	. 
	pop af			;6035	f1 	. 
	ret			;6036	c9 	. 
	
; ARGUMENTS:
; DE = string start
; 
StringPrepForScreenMemory:
	push hl			;6037	e5 	. 
	ld b,000h		;6038	06 00 	. . 
stpr_nextch:
	ld a,(de)			;603a	1a 	.              ; Get character
	cp 000h		;603b	fe 00 	. .            ; Check for zero
	jr z,stpr_finish		;603d	28 0f 	( .  ; End of string, go double A and exit 
	cp 020h		;603f	fe 20 	.              ; Space (standard ASCII, not Brother OS)
	jr nz,stpr_notspc		;6041	20 02 	  .  ; Check for space
	ld a,000h		;6043	3e 00 	> .          ; Change to 0, which is Brother OS space.
stpr_notspc:
	inc b			;6045	04 	.                  ; B++
	ld (hl),000h		;6046	36 00 	6 .      ; PUT a Zero in the buffer for Attributes
	inc hl			;6048	23 	#                ; Next byte
	ld (hl),a			;6049	77 	w              ; PUT the Character in the buffer
	inc hl			;604a	23 	#                ; Next byte of dest
	inc de			;604b	13 	.                ; Next byte of source
	jr stpr_nextch		;604c	18 ec 	. .    ; 
stpr_finish:
	ld a,b			;604e	78 	x 
	add a,a			;604f	87 	. 
	pop hl			;6050	e1 	. 
	ret			;6051	c9 	. 

; BLOCK 'data4' (start 0x6052 end 0x61c2)
brotherlogo_3d_table:
	defb 002h		;6052	02 	. 
	defb 082h		;6053	82 	. 
	defb 0f3h		;6054	f3 	. 
	defb 002h		;6055	02 	. 
	defb 003h		;6056	03 	. 
	defb 082h		;6057	82 	. 
	defb 0f1h		;6058	f1 	. 
	defb 003h		;6059	03 	. 
	defb 004h		;605a	04 	. 
	defb 083h		;605b	83 	. 
	defb 0efh		;605c	ef 	. 
	defb 004h		;605d	04 	. 
	defb 005h		;605e	05 	. 
	defb 084h		;605f	84 	. 
	defb 0ech		;6060	ec 	. 
	defb 005h		;6061	05 	. 
	defb 006h		;6062	06 	. 
	defb 085h		;6063	85 	. 
	defb 0eah		;6064	ea 	. 
	defb 006h		;6065	06 	. 
	defb 008h		;6066	08 	. 
	defb 086h		;6067	86 	. 
	defb 0e8h		;6068	e8 	. 
	defb 008h		;6069	08 	. 
	defb 00ah		;606a	0a 	. 
	defb 088h		;606b	88 	. 
	defb 0e6h		;606c	e6 	. 
	defb 00ah		;606d	0a 	. 
	defb 00bh		;606e	0b 	. 
	defb 089h		;606f	89 	. 
	defb 0e3h		;6070	e3 	. 
	defb 00bh		;6071	0b 	. 
	defb 00dh		;6072	0d 	. 
	defb 08bh		;6073	8b 	. 
	defb 0e1h		;6074	e1 	. 
	defb 00dh		;6075	0d 	. 
	defb 00fh		;6076	0f 	. 
	defb 08ch		;6077	8c 	. 
	defb 0e0h		;6078	e0 	. 
	defb 00fh		;6079	0f 	. 
	defb 011h		;607a	11 	. 
	defb 08eh		;607b	8e 	. 
	defb 0deh		;607c	de 	. 
	defb 011h		;607d	11 	. 
	defb 014h		;607e	14 	. 
	defb 090h		;607f	90 	. 
	defb 0dch		;6080	dc 	. 
	defb 014h		;6081	14 	. 
	defb 016h		;6082	16 	. 
	defb 092h		;6083	92 	. 
	defb 0dbh		;6084	db 	. 
	defb 016h		;6085	16 	. 
	defb 019h		;6086	19 	. 
	defb 094h		;6087	94 	. 
	defb 0d9h		;6088	d9 	. 
	defb 019h		;6089	19 	. 
	defb 01bh		;608a	1b 	. 
	defb 096h		;608b	96 	. 
	defb 0d8h		;608c	d8 	. 
	defb 01bh		;608d	1b 	. 
	defb 01eh		;608e	1e 	. 
	defb 098h		;608f	98 	. 
	defb 0d7h		;6090	d7 	. 
	defb 01eh		;6091	1e 	. 
	defb 021h		;6092	21 	! 
	defb 09bh		;6093	9b 	. 
	defb 0d6h		;6094	d6 	. 
	defb 021h		;6095	21 	! 
	defb 024h		;6096	24 	$ 
	defb 09dh		;6097	9d 	. 
	defb 0d5h		;6098	d5 	. 
	defb 024h		;6099	24 	$ 
	defb 027h		;609a	27 	' 
	defb 0a0h		;609b	a0 	. 
	defb 0d4h		;609c	d4 	. 
	defb 027h		;609d	27 	' 
	defb 02ah		;609e	2a 	* 
	defb 0a3h		;609f	a3 	. 
	defb 0d3h		;60a0	d3 	. 
	defb 02ah		;60a1	2a 	* 
	defb 02dh		;60a2	2d 	- 
	defb 0a5h		;60a3	a5 	. 
	defb 0d3h		;60a4	d3 	. 
	defb 02dh		;60a5	2d 	- 
	defb 031h		;60a6	31 	1 
	defb 0a8h		;60a7	a8 	. 
	defb 0d2h		;60a8	d2 	. 
	defb 031h		;60a9	31 	1 
	defb 034h		;60aa	34 	4 
	defb 0abh		;60ab	ab 	. 
	defb 0d2h		;60ac	d2 	. 
	defb 034h		;60ad	34 	4 
	defb 037h		;60ae	37 	7 
	defb 0aeh		;60af	ae 	. 
	defb 0d2h		;60b0	d2 	. 
	defb 037h		;60b1	37 	7 
	defb 03bh		;60b2	3b 	; 
	defb 0b2h		;60b3	b2 	. 
	defb 0d2h		;60b4	d2 	. 
	defb 03bh		;60b5	3b 	; 
	defb 03eh		;60b6	3e 	> 
	defb 0b5h		;60b7	b5 	. 
	defb 0d3h		;60b8	d3 	. 
	defb 03eh		;60b9	3e 	> 
	defb 042h		;60ba	42 	B 
	defb 0b8h		;60bb	b8 	. 
	defb 0d3h		;60bc	d3 	. 
	defb 042h		;60bd	42 	B 
	defb 045h		;60be	45 	E 
	defb 0bbh		;60bf	bb 	. 
	defb 0d4h		;60c0	d4 	. 
	defb 045h		;60c1	45 	E 
	defb 049h		;60c2	49 	I 
	defb 0bfh		;60c3	bf 	. 
	defb 0d5h		;60c4	d5 	. 
	defb 049h		;60c5	49 	I 
	defb 04ch		;60c6	4c 	L 
	defb 0c2h		;60c7	c2 	. 
	defb 0d6h		;60c8	d6 	. 
	defb 04ch		;60c9	4c 	L 
	defb 050h		;60ca	50 	P 
	defb 0c6h		;60cb	c6 	. 
	defb 0d7h		;60cc	d7 	. 
	defb 050h		;60cd	50 	P 
	defb 053h		;60ce	53 	S 
	defb 0cah		;60cf	ca 	. 
	defb 0d9h		;60d0	d9 	. 
	defb 053h		;60d1	53 	S 
	defb 057h		;60d2	57 	W 
	defb 0cdh		;60d3	cd 	. 
	defb 0dah		;60d4	da 	. 
	defb 057h		;60d5	57 	W 
	defb 05ah		;60d6	5a 	Z 
	defb 0d1h		;60d7	d1 	. 
	defb 0dch		;60d8	dc 	. 
	defb 05ah		;60d9	5a 	Z 
	defb 05eh		;60da	5e 	^ 
	defb 0d5h		;60db	d5 	. 
	defb 0deh		;60dc	de 	. 
	defb 05eh		;60dd	5e 	^ 
	defb 061h		;60de	61 	a 
	defb 0d8h		;60df	d8 	. 
	defb 0e0h		;60e0	e0 	. 
	defb 061h		;60e1	61 	a 
	defb 065h		;60e2	65 	e 
	defb 0dch		;60e3	dc 	. 
	defb 0e3h		;60e4	e3 	. 
	defb 065h		;60e5	65 	e 
	defb 068h		;60e6	68 	h 
	defb 0e0h		;60e7	e0 	. 
	defb 0e5h		;60e8	e5 	. 
	defb 068h		;60e9	68 	h 
	defb 06bh		;60ea	6b 	k 
	defb 0e4h		;60eb	e4 	. 
	defb 0e8h		;60ec	e8 	. 
	defb 06bh		;60ed	6b 	k 
	defb 06fh		;60ee	6f 	o 
	defb 0e8h		;60ef	e8 	. 
	defb 0ebh		;60f0	eb 	. 
	defb 06fh		;60f1	6f 	o 
	defb 072h		;60f2	72 	r 
	defb 0ech		;60f3	ec 	. 
	defb 0eeh		;60f4	ee 	. 
	defb 072h		;60f5	72 	r 
	defb 075h		;60f6	75 	u 
	defb 0f0h		;60f7	f0 	. 
	defb 0f1h		;60f8	f1 	. 
	defb 075h		;60f9	75 	u 
	defb 078h		;60fa	78 	x 
	defb 0f4h		;60fb	f4 	. 
	defb 0f5h		;60fc	f5 	. 
	defb 078h		;60fd	78 	x 
	defb 07bh		;60fe	7b 	{ 
	defb 0f8h		;60ff	f8 	. 
	defb 0f8h		;6100	f8 	. 
	defb 07bh		;6101	7b 	{ 
	defb 07dh		;6102	7d 	} 
	defb 0fch		;6103	fc 	. 
	defb 0fch		;6104	fc 	. 
	defb 07dh		;6105	7d 	} 
	defb 080h		;6106	80 	. 
	defb 000h		;6107	00 	. 
	defb 000h		;6108	00 	. 
	defb 080h		;6109	80 	. 
tetrislogo_3d_table:
	defb 002h		;610a	02 	. 
	defb 0a5h		;610b	a5 	. 
	defb 0f3h		;610c	f3 	. 
	defb 002h		;610d	02 	. 
	defb 003h		;610e	03 	. 
	defb 0b5h		;610f	b5 	. 
	defb 0f1h		;6110	f1 	. 
	defb 003h		;6111	03 	. 
	defb 004h		;6112	04 	. 
	defb 0c6h		;6113	c6 	. 
	defb 0efh		;6114	ef 	. 
	defb 004h		;6115	04 	. 
	defb 005h		;6116	05 	. 
	defb 0d8h		;6117	d8 	. 
	defb 0ech		;6118	ec 	. 
	defb 005h		;6119	05 	. 
	defb 006h		;611a	06 	. 
	defb 0ech		;611b	ec 	. 
	defb 0eah		;611c	ea 	. 
	defb 006h		;611d	06 	. 
	defb 008h		;611e	08 	. 
	defb 000h		;611f	00 	. 
	defb 0e8h		;6120	e8 	. 
	defb 008h		;6121	08 	. 
	defb 00ah		;6122	0a 	. 
	defb 014h		;6123	14 	. 
	defb 0e6h		;6124	e6 	. 
	defb 00ah		;6125	0a 	. 
	defb 00bh		;6126	0b 	. 
	defb 028h		;6127	28 	( 
	defb 0e3h		;6128	e3 	. 
	defb 00bh		;6129	0b 	. 
	defb 00dh		;612a	0d 	. 
	defb 03ah		;612b	3a 	: 
	defb 0e1h		;612c	e1 	. 
	defb 00dh		;612d	0d 	. 
	defb 00fh		;612e	0f 	. 
	defb 04bh		;612f	4b 	K 
	defb 0e0h		;6130	e0 	. 
	defb 00fh		;6131	0f 	. 
	defb 011h		;6132	11 	. 
	defb 05bh		;6133	5b 	[ 
	defb 0deh		;6134	de 	. 
	defb 011h		;6135	11 	. 
	defb 014h		;6136	14 	. 
	defb 068h		;6137	68 	h 
	defb 0dch		;6138	dc 	. 
	defb 014h		;6139	14 	. 
	defb 016h		;613a	16 	. 
	defb 072h		;613b	72 	r 
	defb 0dbh		;613c	db 	. 
	defb 016h		;613d	16 	. 
	defb 019h		;613e	19 	. 
	defb 07ah		;613f	7a 	z 
	defb 0d9h		;6140	d9 	. 
	defb 019h		;6141	19 	. 
	defb 01bh		;6142	1b 	. 
	defb 07eh		;6143	7e 	~ 
	defb 0d8h		;6144	d8 	. 
	defb 01bh		;6145	1b 	. 
	defb 01eh		;6146	1e 	. 
	defb 080h		;6147	80 	. 
	defb 0d7h		;6148	d7 	. 
	defb 01eh		;6149	1e 	. 
	defb 021h		;614a	21 	! 
	defb 07eh		;614b	7e 	~ 
	defb 0d6h		;614c	d6 	. 
	defb 021h		;614d	21 	! 
	defb 024h		;614e	24 	$ 
	defb 07ah		;614f	7a 	z 
	defb 0d5h		;6150	d5 	. 
	defb 024h		;6151	24 	$ 
	defb 027h		;6152	27 	' 
	defb 072h		;6153	72 	r 
	defb 0d4h		;6154	d4 	. 
	defb 027h		;6155	27 	' 
	defb 02ah		;6156	2a 	* 
	defb 068h		;6157	68 	h 
	defb 0d3h		;6158	d3 	. 
	defb 02ah		;6159	2a 	* 
	defb 02dh		;615a	2d 	- 
	defb 05bh		;615b	5b 	[ 
	defb 0d3h		;615c	d3 	. 
	defb 02dh		;615d	2d 	- 
	defb 031h		;615e	31 	1 
	defb 04bh		;615f	4b 	K 
	defb 0d2h		;6160	d2 	. 
	defb 031h		;6161	31 	1 
	defb 034h		;6162	34 	4 
	defb 03ah		;6163	3a 	: 
	defb 0d2h		;6164	d2 	. 
	defb 034h		;6165	34 	4 
	defb 037h		;6166	37 	7 
	defb 028h		;6167	28 	( 
	defb 0d2h		;6168	d2 	. 
	defb 037h		;6169	37 	7 
	defb 03bh		;616a	3b 	; 
	defb 014h		;616b	14 	. 
	defb 0d2h		;616c	d2 	. 
	defb 03bh		;616d	3b 	; 
	defb 03eh		;616e	3e 	> 
	defb 000h		;616f	00 	. 
	defb 0d3h		;6170	d3 	. 
	defb 03eh		;6171	3e 	> 
	defb 042h		;6172	42 	B 
	defb 0ech		;6173	ec 	. 
	defb 0d3h		;6174	d3 	. 
	defb 042h		;6175	42 	B 
	defb 045h		;6176	45 	E 
	defb 0d8h		;6177	d8 	. 
	defb 0d4h		;6178	d4 	. 
	defb 045h		;6179	45 	E 
	defb 049h		;617a	49 	I 
	defb 0c6h		;617b	c6 	. 
	defb 0d5h		;617c	d5 	. 
	defb 049h		;617d	49 	I 
	defb 04ch		;617e	4c 	L 
	defb 0b5h		;617f	b5 	. 
	defb 0d6h		;6180	d6 	. 
	defb 04ch		;6181	4c 	L 
	defb 050h		;6182	50 	P 
	defb 0a5h		;6183	a5 	. 
	defb 0d7h		;6184	d7 	. 
	defb 050h		;6185	50 	P 
	defb 053h		;6186	53 	S 
	defb 098h		;6187	98 	. 
	defb 0d9h		;6188	d9 	. 
	defb 053h		;6189	53 	S 
	defb 057h		;618a	57 	W 
	defb 08eh		;618b	8e 	. 
	defb 0dah		;618c	da 	. 
	defb 057h		;618d	57 	W 
	defb 05ah		;618e	5a 	Z 
	defb 086h		;618f	86 	. 
	defb 0dch		;6190	dc 	. 
	defb 05ah		;6191	5a 	Z 
	defb 05eh		;6192	5e 	^ 
	defb 082h		;6193	82 	. 
	defb 0deh		;6194	de 	. 
	defb 05eh		;6195	5e 	^ 
	defb 061h		;6196	61 	a 
	defb 081h		;6197	81 	. 
	defb 0e0h		;6198	e0 	. 
	defb 061h		;6199	61 	a 
	defb 065h		;619a	65 	e 
	defb 082h		;619b	82 	. 
	defb 0e3h		;619c	e3 	. 
	defb 065h		;619d	65 	e 
	defb 068h		;619e	68 	h 
	defb 086h		;619f	86 	. 
	defb 0e5h		;61a0	e5 	. 
	defb 068h		;61a1	68 	h 
	defb 06bh		;61a2	6b 	k 
	defb 08eh		;61a3	8e 	. 
	defb 0e8h		;61a4	e8 	. 
	defb 06bh		;61a5	6b 	k 
	defb 06fh		;61a6	6f 	o 
	defb 098h		;61a7	98 	. 
	defb 0ebh		;61a8	eb 	. 
	defb 06fh		;61a9	6f 	o 
	defb 072h		;61aa	72 	r 
	defb 0a5h		;61ab	a5 	. 
	defb 0eeh		;61ac	ee 	. 
	defb 072h		;61ad	72 	r 
	defb 075h		;61ae	75 	u 
	defb 0b5h		;61af	b5 	. 
	defb 0f1h		;61b0	f1 	. 
	defb 075h		;61b1	75 	u 
	defb 078h		;61b2	78 	x 
	defb 0c6h		;61b3	c6 	. 
	defb 0f5h		;61b4	f5 	. 
	defb 078h		;61b5	78 	x 
	defb 07bh		;61b6	7b 	{ 
	defb 0d8h		;61b7	d8 	. 
	defb 0f8h		;61b8	f8 	. 
	defb 07bh		;61b9	7b 	{ 
	defb 07dh		;61ba	7d 	} 
	defb 0ech		;61bb	ec 	. 
	defb 0fch		;61bc	fc 	. 
	defb 07dh		;61bd	7d 	} 
	defb 080h		;61be	80 	. 
	defb 000h		;61bf	00 	. 
	defb 000h		;61c0	00 	. 
	defb 080h		;61c1	80 	. 

brother_logo:
	defb 001h		;61c2	01 	. 
	defb 0f0h		;61c3	f0 	. 
	defb 000h		;61c4	00 	. 
	defb 000h		;61c5	00 	. 
	defb 000h		;61c6	00 	. 
	defb 000h		;61c7	00 	. 
	defb 001h		;61c8	01 	. 
	defb 0f0h		;61c9	f0 	. 
	defb 00fh		;61ca	0f 	. 
	defb 0c0h		;61cb	c0 	. 
	defb 000h		;61cc	00 	. 
	defb 000h		;61cd	00 	. 
	defb 000h		;61ce	00 	. 
	defb 000h		;61cf	00 	. 
	defb 000h		;61d0	00 	. 
	defb 000h		;61d1	00 	. 
	defb 001h		;61d2	01 	. 
	defb 0f0h		;61d3	f0 	. 
	defb 000h		;61d4	00 	. 
	defb 000h		;61d5	00 	. 
	defb 000h		;61d6	00 	. 
	defb 000h		;61d7	00 	. 
	defb 001h		;61d8	01 	. 
	defb 0f0h		;61d9	f0 	. 
	defb 00fh		;61da	0f 	. 
	defb 0c0h		;61db	c0 	. 
	defb 000h		;61dc	00 	. 
	defb 000h		;61dd	00 	. 
	defb 000h		;61de	00 	. 
	defb 000h		;61df	00 	. 
	defb 000h		;61e0	00 	. 
	defb 000h		;61e1	00 	. 
	defb 001h		;61e2	01 	. 
	defb 0f0h		;61e3	f0 	. 
	defb 000h		;61e4	00 	. 
	defb 000h		;61e5	00 	. 
	defb 000h		;61e6	00 	. 
	defb 000h		;61e7	00 	. 
	defb 001h		;61e8	01 	. 
	defb 0f0h		;61e9	f0 	. 
	defb 00fh		;61ea	0f 	. 
	defb 0c0h		;61eb	c0 	. 
	defb 000h		;61ec	00 	. 
	defb 000h		;61ed	00 	. 
	defb 000h		;61ee	00 	. 
	defb 000h		;61ef	00 	. 
	defb 000h		;61f0	00 	. 
	defb 000h		;61f1	00 	. 
	defb 001h		;61f2	01 	. 
	defb 0f1h		;61f3	f1 	. 
	defb 0f0h		;61f4	f0 	. 
	defb 000h		;61f5	00 	. 
	defb 000h		;61f6	00 	. 
	defb 000h		;61f7	00 	. 
	defb 001h		;61f8	01 	. 
	defb 0f0h		;61f9	f0 	. 
	defb 00fh		;61fa	0f 	. 
	defb 0c0h		;61fb	c0 	. 
	defb 000h		;61fc	00 	. 
	defb 007h		;61fd	07 	. 
	defb 0e0h		;61fe	e0 	. 
	defb 000h		;61ff	00 	. 
	defb 000h		;6200	00 	. 
	defb 000h		;6201	00 	. 
	defb 001h		;6202	01 	. 
	defb 0f7h		;6203	f7 	. 
	defb 0fch		;6204	fc 	. 
	defb 001h		;6205	01 	. 
	defb 0fch		;6206	fc 	. 
	defb 03fh		;6207	3f 	? 
	defb 001h		;6208	01 	. 
	defb 0ffh		;6209	ff 	. 
	defb 0efh		;620a	ef 	. 
	defb 0dfh		;620b	df 	. 
	defb 0c0h		;620c	c0 	. 
	defb 01fh		;620d	1f 	. 
	defb 0f8h		;620e	f8 	. 
	defb 001h		;620f	01 	. 
	defb 0f8h		;6210	f8 	. 
	defb 000h		;6211	00 	. 
	defb 001h		;6212	01 	. 
	defb 0ffh		;6213	ff 	. 
	defb 0ffh		;6214	ff 	. 
	defb 003h		;6215	03 	. 
	defb 0fdh		;6216	fd 	. 
	defb 0ffh		;6217	ff 	. 
	defb 0e1h		;6218	e1 	. 
	defb 0ffh		;6219	ff 	. 
	defb 0efh		;621a	ef 	. 
	defb 0ffh		;621b	ff 	. 
	defb 0e0h		;621c	e0 	. 
	defb 03fh		;621d	3f 	? 
	defb 0fch		;621e	fc 	. 
	defb 003h		;621f	03 	. 
	defb 0f8h		;6220	f8 	. 
	defb 000h		;6221	00 	. 
	defb 001h		;6222	01 	. 
	defb 0ffh		;6223	ff 	. 
	defb 0ffh		;6224	ff 	. 
	defb 087h		;6225	87 	. 
	defb 0ffh		;6226	ff 	. 
	defb 0ffh		;6227	ff 	. 
	defb 0f1h		;6228	f1 	. 
	defb 0ffh		;6229	ff 	. 
	defb 0efh		;622a	ef 	. 
	defb 0ffh		;622b	ff 	. 
	defb 0f0h		;622c	f0 	. 
	defb 07fh		;622d	7f 	. 
	defb 0feh		;622e	fe 	. 
	defb 007h		;622f	07 	. 
	defb 0f8h		;6230	f8 	. 
	defb 000h		;6231	00 	. 
	defb 001h		;6232	01 	. 
	defb 0ffh		;6233	ff 	. 
	defb 0ffh		;6234	ff 	. 
	defb 0efh		;6235	ef 	. 
	defb 0ffh		;6236	ff 	. 
	defb 0ffh		;6237	ff 	. 
	defb 0f9h		;6238	f9 	. 
	defb 0ffh		;6239	ff 	. 
	defb 0efh		;623a	ef 	. 
	defb 0ffh		;623b	ff 	. 
	defb 0f8h		;623c	f8 	. 
	defb 0fch		;623d	fc 	. 
	defb 03fh		;623e	3f 	? 
	defb 00fh		;623f	0f 	. 
	defb 0f8h		;6240	f8 	. 
	defb 000h		;6241	00 	. 
	defb 001h		;6242	01 	. 
	defb 0feh		;6243	fe 	. 
	defb 00fh		;6244	0f 	. 
	defb 0efh		;6245	ef 	. 
	defb 0efh		;6246	ef 	. 
	defb 0e1h		;6247	e1 	. 
	defb 0fdh		;6248	fd 	. 
	defb 0f0h		;6249	f0 	. 
	defb 00fh		;624a	0f 	. 
	defb 0f1h		;624b	f1 	. 
	defb 0fdh		;624c	fd 	. 
	defb 0f8h		;624d	f8 	. 
	defb 01fh		;624e	1f 	. 
	defb 09fh		;624f	9f 	. 
	defb 0f0h		;6250	f0 	. 
	defb 000h		;6251	00 	. 
	defb 001h		;6252	01 	. 
	defb 0fch		;6253	fc 	. 
	defb 007h		;6254	07 	. 
	defb 0ffh		;6255	ff 	. 
	defb 0dfh		;6256	df 	. 
	defb 0c0h		;6257	c0 	. 
	defb 0ffh		;6258	ff 	. 
	defb 0f0h		;6259	f0 	. 
	defb 00fh		;625a	0f 	. 
	defb 0e0h		;625b	e0 	. 
	defb 0fdh		;625c	fd 	. 
	defb 0f0h		;625d	f0 	. 
	defb 00fh		;625e	0f 	. 
	defb 0bfh		;625f	bf 	. 
	defb 080h		;6260	80 	. 
	defb 000h		;6261	00 	. 
	defb 001h		;6262	01 	. 
	defb 0f8h		;6263	f8 	. 
	defb 003h		;6264	03 	. 
	defb 0ffh		;6265	ff 	. 
	defb 09fh		;6266	9f 	. 
	defb 080h		;6267	80 	. 
	defb 07fh		;6268	7f 	. 
	defb 0f0h		;6269	f0 	. 
	defb 00fh		;626a	0f 	. 
	defb 0c0h		;626b	c0 	. 
	defb 07fh		;626c	7f 	. 
	defb 0e0h		;626d	e0 	. 
	defb 007h		;626e	07 	. 
	defb 0ffh		;626f	ff 	. 
	defb 000h		;6270	00 	. 
	defb 000h		;6271	00 	. 
	defb 001h		;6272	01 	. 
	defb 0f0h		;6273	f0 	. 
	defb 001h		;6274	01 	. 
	defb 0ffh		;6275	ff 	. 
	defb 01fh		;6276	1f 	. 
	defb 080h		;6277	80 	. 
	defb 03fh		;6278	3f 	? 
	defb 0f0h		;6279	f0 	. 
	defb 00fh		;627a	0f 	. 
	defb 0c0h		;627b	c0 	. 
	defb 07fh		;627c	7f 	. 
	defb 0e0h		;627d	e0 	. 
	defb 007h		;627e	07 	. 
	defb 0ffh		;627f	ff 	. 
	defb 000h		;6280	00 	. 
	defb 000h		;6281	00 	. 
	defb 001h		;6282	01 	. 
	defb 0f0h		;6283	f0 	. 
	defb 001h		;6284	01 	. 
	defb 0ffh		;6285	ff 	. 
	defb 01fh		;6286	1f 	. 
	defb 000h		;6287	00 	. 
	defb 03fh		;6288	3f 	? 
	defb 0f0h		;6289	f0 	. 
	defb 00fh		;628a	0f 	. 
	defb 0c0h		;628b	c0 	. 
	defb 07fh		;628c	7f 	. 
	defb 0ffh		;628d	ff 	. 
	defb 0ffh		;628e	ff 	. 
	defb 0feh		;628f	fe 	. 
	defb 000h		;6290	00 	. 
	defb 000h		;6291	00 	. 
	defb 001h		;6292	01 	. 
	defb 0f0h		;6293	f0 	. 
	defb 001h		;6294	01 	. 
	defb 0ffh		;6295	ff 	. 
	defb 01fh		;6296	1f 	. 
	defb 000h		;6297	00 	. 
	defb 03fh		;6298	3f 	? 
	defb 0f0h		;6299	f0 	. 
	defb 00fh		;629a	0f 	. 
	defb 0c0h		;629b	c0 	. 
	defb 07fh		;629c	7f 	. 
	defb 0ffh		;629d	ff 	. 
	defb 0ffh		;629e	ff 	. 
	defb 0feh		;629f	fe 	. 
	defb 000h		;62a0	00 	. 
	defb 000h		;62a1	00 	. 
	defb 001h		;62a2	01 	. 
	defb 0f0h		;62a3	f0 	. 
	defb 001h		;62a4	01 	. 
	defb 0ffh		;62a5	ff 	. 
	defb 01fh		;62a6	1f 	. 
	defb 000h		;62a7	00 	. 
	defb 03fh		;62a8	3f 	? 
	defb 0f0h		;62a9	f0 	. 
	defb 00fh		;62aa	0f 	. 
	defb 0c0h		;62ab	c0 	. 
	defb 07fh		;62ac	7f 	. 
	defb 0ffh		;62ad	ff 	. 
	defb 0ffh		;62ae	ff 	. 
	defb 0feh		;62af	fe 	. 
	defb 000h		;62b0	00 	. 
	defb 000h		;62b1	00 	. 
	defb 001h		;62b2	01 	. 
	defb 0f0h		;62b3	f0 	. 
	defb 001h		;62b4	01 	. 
	defb 0ffh		;62b5	ff 	. 
	defb 01fh		;62b6	1f 	. 
	defb 000h		;62b7	00 	. 
	defb 03fh		;62b8	3f 	? 
	defb 0f0h		;62b9	f0 	. 
	defb 00fh		;62ba	0f 	. 
	defb 0c0h		;62bb	c0 	. 
	defb 07fh		;62bc	7f 	. 
	defb 0e0h		;62bd	e0 	. 
	defb 000h		;62be	00 	. 
	defb 03eh		;62bf	3e 	> 
	defb 000h		;62c0	00 	. 
	defb 000h		;62c1	00 	. 
	defb 001h		;62c2	01 	. 
	defb 0f8h		;62c3	f8 	. 
	defb 003h		;62c4	03 	. 
	defb 0ffh		;62c5	ff 	. 
	defb 01fh		;62c6	1f 	. 
	defb 080h		;62c7	80 	. 
	defb 07fh		;62c8	7f 	. 
	defb 0f0h		;62c9	f0 	. 
	defb 00fh		;62ca	0f 	. 
	defb 0c0h		;62cb	c0 	. 
	defb 07fh		;62cc	7f 	. 
	defb 0e0h		;62cd	e0 	. 
	defb 007h		;62ce	07 	. 
	defb 0beh		;62cf	be 	. 
	defb 000h		;62d0	00 	. 
	defb 000h		;62d1	00 	. 
	defb 000h		;62d2	00 	. 
	defb 0fch		;62d3	fc 	. 
	defb 007h		;62d4	07 	. 
	defb 0ffh		;62d5	ff 	. 
	defb 00fh		;62d6	0f 	. 
	defb 080h		;62d7	80 	. 
	defb 07fh		;62d8	7f 	. 
	defb 0f8h		;62d9	f8 	. 
	defb 01fh		;62da	1f 	. 
	defb 0c0h		;62db	c0 	. 
	defb 07dh		;62dc	7d 	} 
	defb 0f0h		;62dd	f0 	. 
	defb 00fh		;62de	0f 	. 
	defb 0beh		;62df	be 	. 
	defb 000h		;62e0	00 	. 
	defb 000h		;62e1	00 	. 
	defb 000h		;62e2	00 	. 
	defb 0feh		;62e3	fe 	. 
	defb 00fh		;62e4	0f 	. 
	defb 0ffh		;62e5	ff 	. 
	defb 00fh		;62e6	0f 	. 
	defb 0c0h		;62e7	c0 	. 
	defb 0feh		;62e8	fe 	. 
	defb 0fch		;62e9	fc 	. 
	defb 03fh		;62ea	3f 	? 
	defb 0c0h		;62eb	c0 	. 
	defb 07dh		;62ec	7d 	} 
	defb 0f8h		;62ed	f8 	. 
	defb 00fh		;62ee	0f 	. 
	defb 0beh		;62ef	be 	. 
	defb 000h		;62f0	00 	. 
	defb 000h		;62f1	00 	. 
	defb 000h		;62f2	00 	. 
	defb 07fh		;62f3	7f 	. 
	defb 0ffh		;62f4	ff 	. 
	defb 0dfh		;62f5	df 	. 
	defb 007h		;62f6	07 	. 
	defb 0e1h		;62f7	e1 	. 
	defb 0fch		;62f8	fc 	. 
	defb 0ffh		;62f9	ff 	. 
	defb 0ffh		;62fa	ff 	. 
	defb 0c0h		;62fb	c0 	. 
	defb 07ch		;62fc	7c 	| 
	defb 0fch		;62fd	fc 	. 
	defb 01fh		;62fe	1f 	. 
	defb 03eh		;62ff	3e 	> 
	defb 000h		;6300	00 	. 
	defb 000h		;6301	00 	. 
	defb 000h		;6302	00 	. 
	defb 03fh		;6303	3f 	? 
	defb 0ffh		;6304	ff 	. 
	defb 09fh		;6305	9f 	. 
	defb 003h		;6306	03 	. 
	defb 0ffh		;6307	ff 	. 
	defb 0f8h		;6308	f8 	. 
	defb 07fh		;6309	7f 	. 
	defb 0ffh		;630a	ff 	. 
	defb 0c0h		;630b	c0 	. 
	defb 07ch		;630c	7c 	| 
	defb 07fh		;630d	7f 	. 
	defb 0ffh		;630e	ff 	. 
	defb 03eh		;630f	3e 	> 
	defb 000h		;6310	00 	. 
	defb 000h		;6311	00 	. 
	defb 000h		;6312	00 	. 
	defb 01fh		;6313	1f 	. 
	defb 0ffh		;6314	ff 	. 
	defb 01fh		;6315	1f 	. 
	defb 001h		;6316	01 	. 
	defb 0ffh		;6317	ff 	. 
	defb 0f0h		;6318	f0 	. 
	defb 07fh		;6319	7f 	. 
	defb 0ffh		;631a	ff 	. 
	defb 0c0h		;631b	c0 	. 
	defb 07ch		;631c	7c 	| 
	defb 03fh		;631d	3f 	? 
	defb 0feh		;631e	fe 	. 
	defb 03eh		;631f	3e 	> 
	defb 000h		;6320	00 	. 
	defb 000h		;6321	00 	. 
	defb 000h		;6322	00 	. 
	defb 007h		;6323	07 	. 
	defb 0fch		;6324	fc 	. 
	defb 01fh		;6325	1f 	. 
	defb 000h		;6326	00 	. 
	defb 0ffh		;6327	ff 	. 
	defb 0e0h		;6328	e0 	. 
	defb 01fh		;6329	1f 	. 
	defb 0efh		;632a	ef 	. 
	defb 0c0h		;632b	c0 	. 
	defb 07ch		;632c	7c 	| 
	defb 00fh		;632d	0f 	. 
	defb 0f8h		;632e	f8 	. 
	defb 03eh		;632f	3e 	> 
	defb 000h		;6330	00 	. 
	defb 000h		;6331	00 	. 
	defb 000h		;6332	00 	. 
	defb 001h		;6333	01 	. 
	defb 0f0h		;6334	f0 	. 
	defb 000h		;6335	00 	. 
	defb 000h		;6336	00 	. 
	defb 03fh		;6337	3f 	? 
	defb 080h		;6338	80 	. 
	defb 007h		;6339	07 	. 
	defb 0c0h		;633a	c0 	. 
	defb 000h		;633b	00 	. 
	defb 000h		;633c	00 	. 
	defb 007h		;633d	07 	. 
	defb 0f0h		;633e	f0 	. 
	defb 000h		;633f	00 	. 
	defb 000h		;6340	00 	. 
	defb 000h		;6341	00 	. 

tetris_logo:
	defb 000h		;6342	00 	. 
	defb 000h		;6343	00 	. 
	defb 000h		;6344	00 	. 
	defb 000h		;6345	00 	. 
	defb 000h		;6346	00 	. 
	defb 000h		;6347	00 	. 
	defb 000h		;6348	00 	. 
	defb 000h		;6349	00 	. 
	defb 000h		;634a	00 	. 
	defb 000h		;634b	00 	. 
	defb 000h		;634c	00 	. 
	defb 000h		;634d	00 	. 
	defb 000h		;634e	00 	. 
	defb 000h		;634f	00 	. 
	defb 00fh		;6350	0f 	. 
	defb 000h		;6351	00 	. 
	defb 001h		;6352	01 	. 
	defb 0ffh		;6353	ff 	. 
	defb 0ffh		;6354	ff 	. 
	defb 081h		;6355	81 	. 
	defb 0ffh		;6356	ff 	. 
	defb 0ffh		;6357	ff 	. 
	defb 001h		;6358	01 	. 
	defb 0ffh		;6359	ff 	. 
	defb 0ffh		;635a	ff 	. 
	defb 087h		;635b	87 	. 
	defb 0ffh		;635c	ff 	. 
	defb 0e0h		;635d	e0 	. 
	defb 00fh		;635e	0f 	. 
	defb 0f8h		;635f	f8 	. 
	defb 03fh		;6360	3f 	? 
	defb 0c0h		;6361	c0 	. 
	defb 001h		;6362	01 	. 
	defb 0e1h		;6363	e1 	. 
	defb 0c3h		;6364	c3 	. 
	defb 080h		;6365	80 	. 
	defb 07ch		;6366	7c 	| 
	defb 00fh		;6367	0f 	. 
	defb 001h		;6368	01 	. 
	defb 0e1h		;6369	e1 	. 
	defb 0c3h		;636a	c3 	. 
	defb 081h		;636b	81 	. 
	defb 0e0h		;636c	e0 	. 
	defb 070h		;636d	70 	p 
	defb 003h		;636e	03 	. 
	defb 0e0h		;636f	e0 	. 
	defb 070h		;6370	70 	p 
	defb 078h		;6371	78 	x 
	defb 003h		;6372	03 	. 
	defb 081h		;6373	81 	. 
	defb 0c0h		;6374	c0 	. 
	defb 0c0h		;6375	c0 	. 
	defb 038h		;6376	38 	8 
	defb 003h		;6377	03 	. 
	defb 003h		;6378	03 	. 
	defb 081h		;6379	81 	. 
	defb 0c0h		;637a	c0 	. 
	defb 0c0h		;637b	c0 	. 
	defb 0e0h		;637c	e0 	. 
	defb 03ch		;637d	3c 	< 
	defb 001h		;637e	01 	. 
	defb 0c0h		;637f	c0 	. 
	defb 0e0h		;6380	e0 	. 
	defb 038h		;6381	38 	8 
	defb 002h		;6382	02 	. 
	defb 001h		;6383	01 	. 
	defb 0c0h		;6384	c0 	. 
	defb 0c0h		;6385	c0 	. 
	defb 038h		;6386	38 	8 
	defb 001h		;6387	01 	. 
	defb 082h		;6388	82 	. 
	defb 001h		;6389	01 	. 
	defb 0c0h		;638a	c0 	. 
	defb 0c0h		;638b	c0 	. 
	defb 0e0h		;638c	e0 	. 
	defb 03ch		;638d	3c 	< 
	defb 001h		;638e	01 	. 
	defb 0c0h		;638f	c0 	. 
	defb 0c0h		;6390	c0 	. 
	defb 018h		;6391	18 	. 
	defb 006h		;6392	06 	. 
	defb 001h		;6393	01 	. 
	defb 0c0h		;6394	c0 	. 
	defb 060h		;6395	60 	` 
	defb 038h		;6396	38 	8 
	defb 000h		;6397	00 	. 
	defb 086h		;6398	86 	. 
	defb 001h		;6399	01 	. 
	defb 0c0h		;639a	c0 	. 
	defb 060h		;639b	60 	` 
	defb 0e0h		;639c	e0 	. 
	defb 01ch		;639d	1c 	. 
	defb 001h		;639e	01 	. 
	defb 0c0h		;639f	c0 	. 
	defb 0c0h		;63a0	c0 	. 
	defb 008h		;63a1	08 	. 
	defb 004h		;63a2	04 	. 
	defb 001h		;63a3	01 	. 
	defb 0c0h		;63a4	c0 	. 
	defb 020h		;63a5	20 	  
	defb 038h		;63a6	38 	8 
	defb 000h		;63a7	00 	. 
	defb 084h		;63a8	84 	. 
	defb 001h		;63a9	01 	. 
	defb 0c0h		;63aa	c0 	. 
	defb 020h		;63ab	20 	  
	defb 0e0h		;63ac	e0 	. 
	defb 01ch		;63ad	1c 	. 
	defb 001h		;63ae	01 	. 
	defb 0c0h		;63af	c0 	. 
	defb 0c0h		;63b0	c0 	. 
	defb 00ch		;63b1	0c 	. 
	defb 000h		;63b2	00 	. 
	defb 001h		;63b3	01 	. 
	defb 0c0h		;63b4	c0 	. 
	defb 000h		;63b5	00 	. 
	defb 038h		;63b6	38 	8 
	defb 000h		;63b7	00 	. 
	defb 000h		;63b8	00 	. 
	defb 001h		;63b9	01 	. 
	defb 0c0h		;63ba	c0 	. 
	defb 000h		;63bb	00 	. 
	defb 0e0h		;63bc	e0 	. 
	defb 01ch		;63bd	1c 	. 
	defb 001h		;63be	01 	. 
	defb 0c0h		;63bf	c0 	. 
	defb 0e0h		;63c0	e0 	. 
	defb 004h		;63c1	04 	. 
	defb 000h		;63c2	00 	. 
	defb 001h		;63c3	01 	. 
	defb 0c0h		;63c4	c0 	. 
	defb 000h		;63c5	00 	. 
	defb 038h		;63c6	38 	8 
	defb 018h		;63c7	18 	. 
	defb 000h		;63c8	00 	. 
	defb 001h		;63c9	01 	. 
	defb 0c0h		;63ca	c0 	. 
	defb 000h		;63cb	00 	. 
	defb 0e0h		;63cc	e0 	. 
	defb 038h		;63cd	38 	8 
	defb 001h		;63ce	01 	. 
	defb 0c0h		;63cf	c0 	. 
	defb 0e0h		;63d0	e0 	. 
	defb 000h		;63d1	00 	. 
	defb 000h		;63d2	00 	. 
	defb 001h		;63d3	01 	. 
	defb 0c0h		;63d4	c0 	. 
	defb 000h		;63d5	00 	. 
	defb 038h		;63d6	38 	8 
	defb 018h		;63d7	18 	. 
	defb 000h		;63d8	00 	. 
	defb 001h		;63d9	01 	. 
	defb 0c0h		;63da	c0 	. 
	defb 000h		;63db	00 	. 
	defb 0e0h		;63dc	e0 	. 
	defb 038h		;63dd	38 	8 
	defb 001h		;63de	01 	. 
	defb 0c0h		;63df	c0 	. 
	defb 078h		;63e0	78 	x 
	defb 000h		;63e1	00 	. 
	defb 000h		;63e2	00 	. 
	defb 001h		;63e3	01 	. 
	defb 0c0h		;63e4	c0 	. 
	defb 000h		;63e5	00 	. 
	defb 038h		;63e6	38 	8 
	defb 038h		;63e7	38 	8 
	defb 000h		;63e8	00 	. 
	defb 001h		;63e9	01 	. 
	defb 0c0h		;63ea	c0 	. 
	defb 000h		;63eb	00 	. 
	defb 0e0h		;63ec	e0 	. 
	defb 070h		;63ed	70 	p 
	defb 001h		;63ee	01 	. 
	defb 0c0h		;63ef	c0 	. 
	defb 03fh		;63f0	3f 	? 
	defb 000h		;63f1	00 	. 
	defb 000h		;63f2	00 	. 
	defb 001h		;63f3	01 	. 
	defb 0c0h		;63f4	c0 	. 
	defb 000h		;63f5	00 	. 
	defb 03fh		;63f6	3f 	? 
	defb 0f8h		;63f7	f8 	. 
	defb 000h		;63f8	00 	. 
	defb 001h		;63f9	01 	. 
	defb 0c0h		;63fa	c0 	. 
	defb 000h		;63fb	00 	. 
	defb 0ffh		;63fc	ff 	. 
	defb 0c0h		;63fd	c0 	. 
	defb 001h		;63fe	01 	. 
	defb 0c0h		;63ff	c0 	. 
	defb 007h		;6400	07 	. 
	defb 0e0h		;6401	e0 	. 
	defb 000h		;6402	00 	. 
	defb 001h		;6403	01 	. 
	defb 0c0h		;6404	c0 	. 
	defb 000h		;6405	00 	. 
	defb 038h		;6406	38 	8 
	defb 038h		;6407	38 	8 
	defb 000h		;6408	00 	. 
	defb 001h		;6409	01 	. 
	defb 0c0h		;640a	c0 	. 
	defb 000h		;640b	00 	. 
	defb 0ffh		;640c	ff 	. 
	defb 000h		;640d	00 	. 
	defb 001h		;640e	01 	. 
	defb 0c0h		;640f	c0 	. 
	defb 000h		;6410	00 	. 
	defb 038h		;6411	38 	8 
	defb 000h		;6412	00 	. 
	defb 001h		;6413	01 	. 
	defb 0c0h		;6414	c0 	. 
	defb 000h		;6415	00 	. 
	defb 038h		;6416	38 	8 
	defb 018h		;6417	18 	. 
	defb 000h		;6418	00 	. 
	defb 001h		;6419	01 	. 
	defb 0c0h		;641a	c0 	. 
	defb 000h		;641b	00 	. 
	defb 0e7h		;641c	e7 	. 
	defb 080h		;641d	80 	. 
	defb 001h		;641e	01 	. 
	defb 0c0h		;641f	c0 	. 
	defb 000h		;6420	00 	. 
	defb 01ch		;6421	1c 	. 
	defb 000h		;6422	00 	. 
	defb 001h		;6423	01 	. 
	defb 0c0h		;6424	c0 	. 
	defb 000h		;6425	00 	. 
	defb 038h		;6426	38 	8 
	defb 018h		;6427	18 	. 
	defb 000h		;6428	00 	. 
	defb 001h		;6429	01 	. 
	defb 0c0h		;642a	c0 	. 
	defb 000h		;642b	00 	. 
	defb 0e3h		;642c	e3 	. 
	defb 0c0h		;642d	c0 	. 
	defb 001h		;642e	01 	. 
	defb 0c0h		;642f	c0 	. 
	defb 000h		;6430	00 	. 
	defb 00ch		;6431	0c 	. 
	defb 000h		;6432	00 	. 
	defb 001h		;6433	01 	. 
	defb 0c0h		;6434	c0 	. 
	defb 000h		;6435	00 	. 
	defb 038h		;6436	38 	8 
	defb 000h		;6437	00 	. 
	defb 000h		;6438	00 	. 
	defb 001h		;6439	01 	. 
	defb 0c0h		;643a	c0 	. 
	defb 000h		;643b	00 	. 
	defb 0e1h		;643c	e1 	. 
	defb 0c0h		;643d	c0 	. 
	defb 001h		;643e	01 	. 
	defb 0c0h		;643f	c0 	. 
	defb 000h		;6440	00 	. 
	defb 00eh		;6441	0e 	. 
	defb 000h		;6442	00 	. 
	defb 001h		;6443	01 	. 
	defb 0c0h		;6444	c0 	. 
	defb 000h		;6445	00 	. 
	defb 038h		;6446	38 	8 
	defb 000h		;6447	00 	. 
	defb 000h		;6448	00 	. 
	defb 001h		;6449	01 	. 
	defb 0c0h		;644a	c0 	. 
	defb 000h		;644b	00 	. 
	defb 0e1h		;644c	e1 	. 
	defb 0e0h		;644d	e0 	. 
	defb 001h		;644e	01 	. 
	defb 0c1h		;644f	c1 	. 
	defb 000h		;6450	00 	. 
	defb 006h		;6451	06 	. 
	defb 000h		;6452	00 	. 
	defb 001h		;6453	01 	. 
	defb 0c0h		;6454	c0 	. 
	defb 000h		;6455	00 	. 
	defb 038h		;6456	38 	8 
	defb 000h		;6457	00 	. 
	defb 0c0h		;6458	c0 	. 
	defb 001h		;6459	01 	. 
	defb 0c0h		;645a	c0 	. 
	defb 000h		;645b	00 	. 
	defb 0e0h		;645c	e0 	. 
	defb 0f0h		;645d	f0 	. 
	defb 001h		;645e	01 	. 
	defb 0c0h		;645f	c0 	. 
	defb 080h		;6460	80 	. 
	defb 006h		;6461	06 	. 
	defb 000h		;6462	00 	. 
	defb 001h		;6463	01 	. 
	defb 0c0h		;6464	c0 	. 
	defb 000h		;6465	00 	. 
	defb 038h		;6466	38 	8 
	defb 000h		;6467	00 	. 
	defb 080h		;6468	80 	. 
	defb 001h		;6469	01 	. 
	defb 0c0h		;646a	c0 	. 
	defb 000h		;646b	00 	. 
	defb 0e0h		;646c	e0 	. 
	defb 0f0h		;646d	f0 	. 
	defb 001h		;646e	01 	. 
	defb 0c0h		;646f	c0 	. 
	defb 0c0h		;6470	c0 	. 
	defb 006h		;6471	06 	. 
	defb 000h		;6472	00 	. 
	defb 001h		;6473	01 	. 
	defb 0c0h		;6474	c0 	. 
	defb 000h		;6475	00 	. 
	defb 038h		;6476	38 	8 
	defb 001h		;6477	01 	. 
	defb 080h		;6478	80 	. 
	defb 001h		;6479	01 	. 
	defb 0c0h		;647a	c0 	. 
	defb 000h		;647b	00 	. 
	defb 0e0h		;647c	e0 	. 
	defb 078h		;647d	78 	x 
	defb 001h		;647e	01 	. 
	defb 0c0h		;647f	c0 	. 
	defb 0c0h		;6480	c0 	. 
	defb 00ch		;6481	0c 	. 
	defb 000h		;6482	00 	. 
	defb 001h		;6483	01 	. 
	defb 0c0h		;6484	c0 	. 
	defb 000h		;6485	00 	. 
	defb 038h		;6486	38 	8 
	defb 003h		;6487	03 	. 
	defb 080h		;6488	80 	. 
	defb 001h		;6489	01 	. 
	defb 0c0h		;648a	c0 	. 
	defb 000h		;648b	00 	. 
	defb 0e0h		;648c	e0 	. 
	defb 03ch		;648d	3c 	< 
	defb 001h		;648e	01 	. 
	defb 0c0h		;648f	c0 	. 
	defb 0e0h		;6490	e0 	. 
	defb 00ch		;6491	0c 	. 
	defb 000h		;6492	00 	. 
	defb 003h		;6493	03 	. 
	defb 0e0h		;6494	e0 	. 
	defb 000h		;6495	00 	. 
	defb 07eh		;6496	7e 	~ 
	defb 01fh		;6497	1f 	. 
	defb 080h		;6498	80 	. 
	defb 003h		;6499	03 	. 
	defb 0e0h		;649a	e0 	. 
	defb 001h		;649b	01 	. 
	defb 0f0h		;649c	f0 	. 
	defb 03eh		;649d	3e 	> 
	defb 003h		;649e	03 	. 
	defb 0e0h		;649f	e0 	. 
	defb 078h		;64a0	78 	x 
	defb 038h		;64a1	38 	8 
	defb 000h		;64a2	00 	. 
	defb 00fh		;64a3	0f 	. 
	defb 0f8h		;64a4	f8 	. 
	defb 001h		;64a5	01 	. 
	defb 0ffh		;64a6	ff 	. 
	defb 0ffh		;64a7	ff 	. 
	defb 000h		;64a8	00 	. 
	defb 00fh		;64a9	0f 	. 
	defb 0f8h		;64aa	f8 	. 
	defb 007h		;64ab	07 	. 
	defb 0fch		;64ac	fc 	. 
	defb 01fh		;64ad	1f 	. 
	defb 08fh		;64ae	8f 	. 
	defb 0f8h		;64af	f8 	. 
	defb 06fh		;64b0	6f 	o 
	defb 0e0h		;64b1	e0 	. 
	defb 000h		;64b2	00 	. 
	defb 000h		;64b3	00 	. 
	defb 000h		;64b4	00 	. 
	defb 000h		;64b5	00 	. 
	defb 000h		;64b6	00 	. 
	defb 000h		;64b7	00 	. 
	defb 000h		;64b8	00 	. 
	defb 000h		;64b9	00 	. 
	defb 000h		;64ba	00 	. 
	defb 000h		;64bb	00 	. 
	defb 000h		;64bc	00 	. 
	defb 000h		;64bd	00 	. 
	defb 000h		;64be	00 	. 
	defb 000h		;64bf	00 	. 
	defb 000h		;64c0	00 	. 
	defb 000h		;64c1	00 	. 
tetris_end:

; BLOCK 'data5' (start 0x64c2 end 0x6632)
data5_start:   ; This is a melody...
	defb 0f8h		;64c2	f8 	. 
	defb 03fh		;64c3	3f 	? 
	defb 010h		;64c4	10 	. 
	defb 003h		;64c5	03 	. 
	defb 0f8h		;64c6	f8 	. 
	defb 03fh		;64c7	3f 	? 
	defb 0bah		;64c8	ba 	. 
	defb 002h		;64c9	02 	. 
	defb 0f8h		;64ca	f8 	. 
	defb 03fh		;64cb	3f 	? 
	defb 093h		;64cc	93 	. 
	defb 002h		;64cd	02 	. 
	defb 0f8h		;64ce	f8 	. 
	defb 03fh		;64cf	3f 	? 
	defb 04bh		;64d0	4b 	K 
	defb 002h		;64d1	02 	. 
	defb 0f8h		;64d2	f8 	. 
	defb 03fh		;64d3	3f 	? 
	defb 00bh		;64d4	0b 	. 
	defb 002h		;64d5	02 	. 
	defb 000h		;64d6	00 	. 
	defb 000h		;64d7	00 	. ; Melody end with 2 zeroes
Melody_GameOver:
	defb 0f8h		;64d8	f8 	. 
	defb 03fh		;64d9	3f 	? 
	defb 010h		;64da	10 	. 
	defb 003h		;64db	03 	. 
	defb 0f8h		;64dc	f8 	. 
	defb 03fh		;64dd	3f 	? 
	defb 0bah		;64de	ba 	. 
	defb 002h		;64df	02 	. 
	defb 0f8h		;64e0	f8 	. 
	defb 03fh		;64e1	3f 	? 
	defb 093h		;64e2	93 	. 
	defb 002h		;64e3	02 	. 
	defb 0f8h		;64e4	f8 	. 
	defb 03fh		;64e5	3f 	? 
	defb 04bh		;64e6	4b 	K 
	defb 002h		;64e7	02 	. 
	defb 0f8h		;64e8	f8 	. 
	defb 03fh		;64e9	3f 	? 
	defb 00bh		;64ea	0b 	. 
	defb 002h		;64eb	02 	. 
	defb 0f0h		;64ec	f0 	. 
	defb 07fh		;64ed	7f 	. 
	defb 0bah		;64ee	ba 	. 
	defb 002h		;64ef	02 	. 
	defb 0f0h		;64f0	f0 	. 
	defb 07fh		;64f1	7f 	. 
	defb 00bh		;64f2	0b 	. 
	defb 002h		;64f3	02 	. 
	defb 0f0h		;64f4	f0 	. 
	defb 07fh		;64f5	7f 	. 
	defb 0bah		;64f6	ba 	. 
	defb 002h		;64f7	02 	. 
	defb 000h		;64f8	00 	. 
	defb 000h		;64f9	00 	. ; Melody end with 2 zeroes
	defb 0fch		;64fa	fc 	. 
	defb 01fh		;64fb	1f 	. 
	defb 00bh		;64fc	0b 	. 
	defb 002h		;64fd	02 	. 
	defb 0fch		;64fe	fc 	. 
	defb 01fh		;64ff	1f 	. 
	defb 02ah		;6500	2a 	* 
	defb 002h		;6501	02 	. 
	defb 0fch		;6502	fc 	. 
	defb 01fh		;6503	1f 	. 
	defb 04bh		;6504	4b 	K 
	defb 002h		;6505	02 	. 
	defb 0fch		;6506	fc 	. 
	defb 01fh		;6507	1f 	. 
	defb 06eh		;6508	6e 	n 
	defb 002h		;6509	02 	. 
	defb 0fch		;650a	fc 	. 
	defb 01fh		;650b	1f 	. 
	defb 093h		;650c	93 	. 
	defb 002h		;650d	02 	. 
	defb 0fch		;650e	fc 	. 
	defb 01fh		;650f	1f 	. 
	defb 0bah		;6510	ba 	. 
	defb 002h		;6511	02 	. 
	defb 0fch		;6512	fc 	. 
	defb 01fh		;6513	1f 	. 
	defb 00bh		;6514	0b 	. 
	defb 002h		;6515	02 	. 
	defb 0fch		;6516	fc 	. 
	defb 01fh		;6517	1f 	. 
	defb 02ah		;6518	2a 	* 
	defb 002h		;6519	02 	. 
	defb 0fch		;651a	fc 	. 
	defb 01fh		;651b	1f 	. 
	defb 04bh		;651c	4b 	K 
	defb 002h		;651d	02 	. 
	defb 0fch		;651e	fc 	. 
	defb 01fh		;651f	1f 	. 
	defb 06eh		;6520	6e 	n 
	defb 002h		;6521	02 	. 
	defb 0fch		;6522	fc 	. 
	defb 01fh		;6523	1f 	. 
	defb 093h		;6524	93 	. 
	defb 002h		;6525	02 	. 
	defb 0fch		;6526	fc 	. 
	defb 01fh		;6527	1f 	. 
	defb 0bah		;6528	ba 	. 
	defb 002h		;6529	02 	. 
	defb 000h		;652a	00 	. 
	defb 000h		;652b	00 	. ; Melody end with 2 zeroes
Melody1:
	defb 0ffh		;652c	ff 	. 
	defb 007h		;652d	07 	. 
	defb 00bh		;652e	0b 	. 
	defb 002h		;652f	02 	. 
	defb 0ffh		;6530	ff 	. 
	defb 007h		;6531	07 	. 
	defb 093h		;6532	93 	. 
	defb 002h		;6533	02 	. 
	defb 0ffh		;6534	ff 	. 
	defb 007h		;6535	07 	. 
	defb 04bh		;6536	4b 	K 
	defb 002h		;6537	02 	. 
	defb 000h		;6538	00 	. 
	defb 000h		;6539	00 	. ; Melody end with 2 zeroes
Melody2:
	defb 0ffh		;653a	ff 	. 
	defb 007h		;653b	07 	. 
	defb 00bh		;653c	0b 	. 
	defb 002h		;653d	02 	. 
	defb 0ffh		;653e	ff 	. 
	defb 007h		;653f	07 	. 
	defb 04bh		;6540	4b 	K 
	defb 002h		;6541	02 	. 
	defb 0ffh		;6542	ff 	. 
	defb 007h		;6543	07 	. 
	defb 093h		;6544	93 	. 
	defb 002h		;6545	02 	. 
	defb 0ffh		;6546	ff 	. 
	defb 007h		;6547	07 	. 
	defb 04bh		;6548	4b 	K 
	defb 002h		;6549	02 	. 
	defb 0ffh		;654a	ff 	. 
	defb 007h		;654b	07 	. 
	defb 00bh		;654c	0b 	. 
	defb 002h		;654d	02 	. 
	defb 000h		;654e	00 	. 
	defb 000h		;654f	00 	. ; Melody end with 2 zeroes
Melody3:
	defb 0ffh		;6550	ff 	. 
	defb 007h		;6551	07 	. 
	defb 00bh		;6552	0b 	. 
	defb 002h		;6553	02 	. 
	defb 0ffh		;6554	ff 	. 
	defb 007h		;6555	07 	. 
	defb 04bh		;6556	4b 	K 
	defb 002h		;6557	02 	. 
	defb 0ffh		;6558	ff 	. 
	defb 007h		;6559	07 	. 
	defb 093h		;655a	93 	. 
	defb 002h		;655b	02 	. 
	defb 0ffh		;655c	ff 	. 
	defb 007h		;655d	07 	. 
	defb 04bh		;655e	4b 	K 
	defb 002h		;655f	02 	. 
	defb 0ffh		;6560	ff 	. 
	defb 007h		;6561	07 	. 
	defb 00bh		;6562	0b 	. 
	defb 002h		;6563	02 	. 
	defb 0ffh		;6564	ff 	. 
	defb 007h		;6565	07 	. 
	defb 04bh		;6566	4b 	K 
	defb 002h		;6567	02 	. 
	defb 0ffh		;6568	ff 	. 
	defb 007h		;6569	07 	. 
	defb 093h		;656a	93 	. 
	defb 002h		;656b	02 	. 
	defb 000h		;656c	00 	. 
	defb 000h		;656d	00 	. 
Melody4:    ; Melody for four rows! "wow"
	defb 0ffh		;656e	ff 	. 
	defb 007h		;656f	07 	. 
	defb 00bh		;6570	0b 	. 
	defb 002h		;6571	02 	. 
	defb 0ffh		;6572	ff 	. 
	defb 007h		;6573	07 	. 
	defb 04bh		;6574	4b 	K 
	defb 002h		;6575	02 	. 
	defb 0ffh		;6576	ff 	. 
	defb 007h		;6577	07 	. 
	defb 093h		;6578	93 	. 
	defb 002h		;6579	02 	. 
	defb 0ffh		;657a	ff 	. 
	defb 007h		;657b	07 	. 
	defb 0bah		;657c	ba 	. 
	defb 002h		;657d	02 	. 
	defb 0ffh		;657e	ff 	. 
	defb 007h		;657f	07 	. 
	defb 010h		;6580	10 	. 
	defb 003h		;6581	03 	. 
	defb 0ffh		;6582	ff 	. 
	defb 007h		;6583	07 	. 
	defb 070h		;6584	70 	p 
	defb 003h		;6585	03 	. 
	defb 0ffh		;6586	ff 	. 
	defb 007h		;6587	07 	. 
	defb 0dch		;6588	dc 	. 
	defb 003h		;6589	03 	. 
	defb 0ffh		;658a	ff 	. 
	defb 007h		;658b	07 	. 
	defb 00bh		;658c	0b 	. 
	defb 002h		;658d	02 	. 
	defb 0ffh		;658e	ff 	. 
	defb 007h		;658f	07 	. 
	defb 0dch		;6590	dc 	. 
	defb 003h		;6591	03 	. 
	defb 0ffh		;6592	ff 	. 
	defb 007h		;6593	07 	. 
	defb 070h		;6594	70 	p 
	defb 003h		;6595	03 	. 
	defb 0ffh		;6596	ff 	. 
	defb 007h		;6597	07 	. 
	defb 010h		;6598	10 	. 
	defb 003h		;6599	03 	. 
	defb 0ffh		;659a	ff 	. 
	defb 007h		;659b	07 	. 
	defb 0bah		;659c	ba 	. 
	defb 002h		;659d	02 	. 
	defb 0ffh		;659e	ff 	. 
	defb 007h		;659f	07 	. 
	defb 093h		;65a0	93 	. 
	defb 002h		;65a1	02 	. 
	defb 0ffh		;65a2	ff 	. 
	defb 007h		;65a3	07 	. 
	defb 04bh		;65a4	4b 	K 
	defb 002h		;65a5	02 	. 
	defb 0ffh		;65a6	ff 	. 
	defb 007h		;65a7	07 	. 
	defb 00bh		;65a8	0b 	. 
	defb 002h		;65a9	02 	. 
	defb 000h		;65aa	00 	. ; Melody end with 2 zeroes
	defb 000h		;65ab	00 	. 
Melody_score_table:
  defw Melody1   ;65ac	2c 65	, e 
	defb 028h		;65ae	28 	( 
	defb 000h		;65af	00 	. 

  defw Melody2   ;65b0	3a 65 : e
	defb 064h		;65b2	64 	d 
	defb 000h		;65b3	00 	. 

  defw Melody3   ;65b4 50 65 P e
	defb 02ch		;65b6	2c 	, 
	defb 001h		;65b7	01 	. 

  defw Melody4   ;65b8 6e 65 n e   ; Melody for four rows! "wow"
	defb 0b0h		;65ba	b0 	. 
	defb 004h		;65bb	04 	. 

font1_start:
	defb 000h		;65bc	00 	. 
	defb 000h		;65bd	00 	. 
	defb 000h		;65be	00 	. 
	defb 000h		;65bf	00 	. 
	defb 000h		;65c0	00 	. 
	defb 000h		;65c1	00 	. 
	defb 000h		;65c2	00 	. 
	defb 000h		;65c3	00 	. 
	defb 000h		;65c4	00 	. 
	defb 000h		;65c5	00 	. 
	defb 000h		;65c6	00 	. 
	defb 000h		;65c7	00 	. 
	defb 000h		;65c8	00 	. 
	defb 000h		;65c9	00 	. 
	defb 000h		;65ca	00 	. 
	defb 000h		;65cb	00 	. 
	defb 000h		;65cc	00 	. 
	defb 000h		;65cd	00 	. 
	defb 000h		;65ce	00 	. 
	defb 000h		;65cf	00 	. 
	defb 000h		;65d0	00 	. 
	defb 000h		;65d1	00 	. 
	defb 000h		;65d2	00 	. 
	defb 000h		;65d3	00 	. 
	defb 000h		;65d4	00 	. 
	defb 000h		;65d5	00 	. 
	defb 000h		;65d6	00 	. 
	defb 000h		;65d7	00 	. 
	defb 000h		;65d8	00 	. 
	defb 000h		;65d9	00 	. 
	defb 000h		;65da	00 	. 
	defb 000h		;65db	00 	. 
	defb 000h		;65dc	00 	. 
	defb 000h		;65dd	00 	. 
	defb 000h		;65de	00 	. 
	defb 000h		;65df	00 	. 
	defb 000h		;65e0	00 	. 
	defb 000h		;65e1	00 	. 
	defb 000h		;65e2	00 	. 
	defb 000h		;65e3	00 	. 
	defb 000h		;65e4	00 	. 
	defb 000h		;65e5	00 	. 
	defb 000h		;65e6	00 	. 
	defb 000h		;65e7	00 	. 
	defb 000h		;65e8	00 	. 
	defb 000h		;65e9	00 	. 
	defb 000h		;65ea	00 	. 
	defb 000h		;65eb	00 	. 
	defb 000h		;65ec	00 	. 
	defb 000h		;65ed	00 	. 
	defb 000h		;65ee	00 	. 
	defb 000h		;65ef	00 	. 
	defb 000h		;65f0	00 	. 
	defb 000h		;65f1	00 	. 
	defb 000h		;65f2	00 	. 
	defb 000h		;65f3	00 	. 
	defb 000h		;65f4	00 	. 
	defb 000h		;65f5	00 	. 
	defb 000h		;65f6	00 	. 
	defb 000h		;65f7	00 	. 
	defb 000h		;65f8	00 	. 
	defb 000h		;65f9	00 	. 
	defb 000h		;65fa	00 	. 
	defb 000h		;65fb	00 	. 
	defb 000h		;65fc	00 	. 
	defb 000h		;65fd	00 	. 
	defb 000h		;65fe	00 	. 
	defb 000h		;65ff	00 	. 
	defb 000h		;6600	00 	. 
	defb 000h		;6601	00 	. 
	defb 000h		;6602	00 	. 
	defb 000h		;6603	00 	. 
	defb 000h		;6604	00 	. 
	defb 000h		;6605	00 	. 
	defb 000h		;6606	00 	. 
	defb 000h		;6607	00 	. 
	defb 000h		;6608	00 	. 
	defb 000h		;6609	00 	. 
	defb 000h		;660a	00 	. 
	defb 000h		;660b	00 	. 
	defb 000h		;660c	00 	. 
	defb 000h		;660d	00 	. 
	defb 000h		;660e	00 	. 
	defb 000h		;660f	00 	. 
	defb 054h		;6610	54 	T 
	defb 0aah		;6611	aa 	. 
	defb 054h		;6612	54 	T 
	defb 0aah		;6613	aa 	. 
	defb 054h		;6614	54 	T 
	defb 0aah		;6615	aa 	. 
	defb 054h		;6616	54 	T 
	defb 0aah		;6617	aa 	. 
	defb 054h		;6618	54 	T 
	defb 0aah		;6619	aa 	. 
	defb 054h		;661a	54 	T 
	defb 0aah		;661b	aa 	. 
	defb 000h		;661c	00 	. 
	defb 000h		;661d	00 	. 
	defb 000h		;661e	00 	. 
	defb 000h		;661f	00 	. 
	defb 000h		;6620	00 	. 
	defb 000h		;6621	00 	. 
	defb 000h		;6622	00 	. 
	defb 000h		;6623	00 	. 
	defb 000h		;6624	00 	. 
	defb 000h		;6625	00 	. 
	defb 000h		;6626	00 	. 
	defb 000h		;6627	00 	. 
	defb 000h		;6628	00 	. 
	defb 000h		;6629	00 	. 
	defb 000h		;662a	00 	. 
	defb 000h		;662b	00 	. 
	defb 000h		;662c	00 	. 
	defb 000h		;662d	00 	. 
	defb 000h		;662e	00 	. 
	defb 000h		;662f	00 	. 
	defb 000h		;6630	00 	. 
	defb 000h		;6631	00 	. 
	defb 000h		;6632	00 	. 
	defb 000h		;6633	00 	. 
	defb 000h		;6634	00 	. 
	defb 000h		;6635	00 	. 
	defb 000h		;6636	00 	. 
	defb 01fh		;6637	1f 	. 
	defb 010h		;6638	10 	. 
	defb 010h		;6639	10 	. 
	defb 010h		;663a	10 	. 
	defb 010h		;663b	10 	. 
	defb 010h		;663c	10 	. 
	defb 010h		;663d	10 	. 
	defb 010h		;663e	10 	. 
	defb 010h		;663f	10 	. 
	defb 000h		;6640	00 	. 
	defb 000h		;6641	00 	. 
	defb 000h		;6642	00 	. 
	defb 0f0h		;6643	f0 	. 
	defb 010h		;6644	10 	. 
	defb 010h		;6645	10 	. 
	defb 010h		;6646	10 	. 
	defb 010h		;6647	10 	. 
	defb 010h		;6648	10 	. 
	defb 010h		;6649	10 	. 
	defb 010h		;664a	10 	. 
	defb 010h		;664b	10 	. 
	defb 010h		;664c	10 	. 
	defb 010h		;664d	10 	. 
	defb 010h		;664e	10 	. 
	defb 01fh		;664f	1f 	. 
	defb 000h		;6650	00 	. 
	defb 000h		;6651	00 	. 
	defb 000h		;6652	00 	. 
	defb 000h		;6653	00 	. 
	defb 000h		;6654	00 	. 
	defb 000h		;6655	00 	. 
	defb 000h		;6656	00 	. 
	defb 000h		;6657	00 	. 
	defb 010h		;6658	10 	. 
	defb 010h		;6659	10 	. 
	defb 010h		;665a	10 	. 
	defb 0f0h		;665b	f0 	. 
	defb 000h		;665c	00 	. 
	defb 000h		;665d	00 	. 
	defb 000h		;665e	00 	. 
	defb 000h		;665f	00 	. 
	defb 000h		;6660	00 	. 
	defb 000h		;6661	00 	. 
	defb 000h		;6662	00 	. 
	defb 000h		;6663	00 	. 
	defb 010h		;6664	10 	. 
	defb 010h		;6665	10 	. 
	defb 010h		;6666	10 	. 
	defb 010h		;6667	10 	. 
	defb 010h		;6668	10 	. 
	defb 010h		;6669	10 	. 
	defb 010h		;666a	10 	. 
	defb 010h		;666b	10 	. 
	defb 010h		;666c	10 	. 
	defb 010h		;666d	10 	. 
	defb 010h		;666e	10 	. 
	defb 010h		;666f	10 	. 
	defb 000h		;6670	00 	. 
	defb 000h		;6671	00 	. 
	defb 000h		;6672	00 	. 
	defb 0ffh		;6673	ff 	. 
	defb 000h		;6674	00 	. 
	defb 000h		;6675	00 	. 
	defb 000h		;6676	00 	. 
	defb 000h		;6677	00 	. 
	defb 000h		;6678	00 	. 
	defb 000h		;6679	00 	. 
	defb 000h		;667a	00 	. 
	defb 000h		;667b	00 	. 
	defb 000h		;667c	00 	. 
	defb 000h		;667d	00 	. 
	defb 000h		;667e	00 	. 
	defb 000h		;667f	00 	. 
	defb 000h		;6680	00 	. 
	defb 000h		;6681	00 	. 
	defb 000h		;6682	00 	. 
	defb 000h		;6683	00 	. 
	defb 000h		;6684	00 	. 
	defb 000h		;6685	00 	. 
	defb 000h		;6686	00 	. 
	defb 000h		;6687	00 	. 
	defb 000h		;6688	00 	. 
	defb 000h		;6689	00 	. 
	defb 000h		;668a	00 	. 
	defb 000h		;668b	00 	. 
	defb 000h		;668c	00 	. 
	defb 000h		;668d	00 	. 
	defb 000h		;668e	00 	. 
	defb 000h		;668f	00 	. 
	defb 000h		;6690	00 	. 
	defb 000h		;6691	00 	. 
	defb 000h		;6692	00 	. 
	defb 000h		;6693	00 	. 
	defb 000h		;6694	00 	. 
	defb 000h		;6695	00 	. 
	defb 000h		;6696	00 	. 
	defb 000h		;6697	00 	. 
	defb 000h		;6698	00 	. 
	defb 000h		;6699	00 	. 
	defb 000h		;669a	00 	. 
	defb 000h		;669b	00 	. 
	defb 000h		;669c	00 	. 
	defb 000h		;669d	00 	. 
	defb 000h		;669e	00 	. 
	defb 000h		;669f	00 	. 
	defb 000h		;66a0	00 	. 
	defb 000h		;66a1	00 	. 
	defb 000h		;66a2	00 	. 
	defb 000h		;66a3	00 	. 
	defb 000h		;66a4	00 	. 
	defb 000h		;66a5	00 	. 
	defb 000h		;66a6	00 	. 
	defb 000h		;66a7	00 	. 
	defb 000h		;66a8	00 	. 
	defb 000h		;66a9	00 	. 
	defb 000h		;66aa	00 	. 
	defb 000h		;66ab	00 	. 
	defb 000h		;66ac	00 	. 
	defb 000h		;66ad	00 	. 
	defb 000h		;66ae	00 	. 
	defb 000h		;66af	00 	. 
	defb 000h		;66b0	00 	. 
	defb 000h		;66b1	00 	. 
	defb 000h		;66b2	00 	. 
	defb 000h		;66b3	00 	. 
	defb 000h		;66b4	00 	. 
	defb 000h		;66b5	00 	. 
	defb 000h		;66b6	00 	. 
	defb 000h		;66b7	00 	. 
	defb 000h		;66b8	00 	. 
	defb 000h		;66b9	00 	. 
	defb 000h		;66ba	00 	. 
	defb 000h		;66bb	00 	. 
	defb 000h		;66bc	00 	. 
	defb 000h		;66bd	00 	. 
	defb 000h		;66be	00 	. 
	defb 000h		;66bf	00 	. 
	defb 000h		;66c0	00 	. 
	defb 000h		;66c1	00 	. 
	defb 000h		;66c2	00 	. 
	defb 000h		;66c3	00 	. 
	defb 000h		;66c4	00 	. 
	defb 000h		;66c5	00 	. 
	defb 000h		;66c6	00 	. 
	defb 000h		;66c7	00 	. 
	defb 000h		;66c8	00 	. 
	defb 000h		;66c9	00 	. 
	defb 000h		;66ca	00 	. 
	defb 000h		;66cb	00 	. 
	defb 000h		;66cc	00 	. 
	defb 000h		;66cd	00 	. 
	defb 000h		;66ce	00 	. 
	defb 000h		;66cf	00 	. 
	defb 000h		;66d0	00 	. 
	defb 000h		;66d1	00 	. 
	defb 000h		;66d2	00 	. 
	defb 000h		;66d3	00 	. 
	defb 000h		;66d4	00 	. 
	defb 000h		;66d5	00 	. 
	defb 000h		;66d6	00 	. 
	defb 000h		;66d7	00 	. 
	defb 000h		;66d8	00 	. 
	defb 000h		;66d9	00 	. 
	defb 000h		;66da	00 	. 
	defb 000h		;66db	00 	. 
	defb 000h		;66dc	00 	. 
	defb 000h		;66dd	00 	. 
	defb 000h		;66de	00 	. 
	defb 000h		;66df	00 	. 
	defb 000h		;66e0	00 	. 
	defb 000h		;66e1	00 	. 
	defb 000h		;66e2	00 	. 
	defb 000h		;66e3	00 	. 
	defb 000h		;66e4	00 	. 
	defb 000h		;66e5	00 	. 
	defb 000h		;66e6	00 	. 
	defb 000h		;66e7	00 	. 
	defb 000h		;66e8	00 	. 
	defb 000h		;66e9	00 	. 
	defb 000h		;66ea	00 	. 
	defb 000h		;66eb	00 	. 
	defb 000h		;66ec	00 	. 
	defb 000h		;66ed	00 	. 
	defb 000h		;66ee	00 	. 
	defb 000h		;66ef	00 	. 
	defb 000h		;66f0	00 	. 
	defb 000h		;66f1	00 	. 
	defb 000h		;66f2	00 	. 
	defb 000h		;66f3	00 	. 
	defb 000h		;66f4	00 	. 
	defb 000h		;66f5	00 	. 
	defb 000h		;66f6	00 	. 
	defb 000h		;66f7	00 	. 
	defb 000h		;66f8	00 	. 
	defb 000h		;66f9	00 	. 
	defb 000h		;66fa	00 	. 
	defb 000h		;66fb	00 	. 
	defb 000h		;66fc	00 	. 
	defb 000h		;66fd	00 	. 
	defb 000h		;66fe	00 	. 
	defb 000h		;66ff	00 	. 
	defb 000h		;6700	00 	. 
	defb 000h		;6701	00 	. 
	defb 000h		;6702	00 	. 
	defb 000h		;6703	00 	. 
	defb 000h		;6704	00 	. 
	defb 000h		;6705	00 	. 
	defb 000h		;6706	00 	. 
	defb 000h		;6707	00 	. 
	defb 000h		;6708	00 	. 
	defb 000h		;6709	00 	. 
	defb 000h		;670a	00 	. 
	defb 000h		;670b	00 	. 
	defb 000h		;670c	00 	. 
	defb 000h		;670d	00 	. 
	defb 000h		;670e	00 	. 
	defb 000h		;670f	00 	. 
	defb 000h		;6710	00 	. 
	defb 000h		;6711	00 	. 
	defb 000h		;6712	00 	. 
	defb 000h		;6713	00 	. 
	defb 000h		;6714	00 	. 
	defb 000h		;6715	00 	. 
	defb 000h		;6716	00 	. 
	defb 000h		;6717	00 	. 
	defb 000h		;6718	00 	. 
	defb 000h		;6719	00 	. 
	defb 000h		;671a	00 	. 
	defb 000h		;671b	00 	. 
	defb 000h		;671c	00 	. 
	defb 000h		;671d	00 	. 
	defb 000h		;671e	00 	. 
	defb 000h		;671f	00 	. 
	defb 000h		;6720	00 	. 
	defb 000h		;6721	00 	. 
	defb 000h		;6722	00 	. 
	defb 000h		;6723	00 	. 
	defb 000h		;6724	00 	. 
	defb 000h		;6725	00 	. 
	defb 000h		;6726	00 	. 
	defb 000h		;6727	00 	. 
	defb 000h		;6728	00 	. 
	defb 000h		;6729	00 	. 
	defb 000h		;672a	00 	. 
	defb 000h		;672b	00 	. 
	defb 000h		;672c	00 	. 
	defb 000h		;672d	00 	. 
	defb 000h		;672e	00 	. 
	defb 000h		;672f	00 	. 
	defb 000h		;6730	00 	. 
	defb 000h		;6731	00 	. 
	defb 000h		;6732	00 	. 
	defb 000h		;6733	00 	. 
	defb 000h		;6734	00 	. 
	defb 000h		;6735	00 	. 
	defb 000h		;6736	00 	. 
	defb 000h		;6737	00 	. 
	defb 000h		;6738	00 	. 
	defb 000h		;6739	00 	. 
	defb 000h		;673a	00 	. 
	defb 000h		;673b	00 	. 
	defb 000h		;673c	00 	. 
	defb 000h		;673d	00 	. 
	defb 000h		;673e	00 	. 
	defb 000h		;673f	00 	. 
	defb 000h		;6740	00 	. 
	defb 000h		;6741	00 	. 
	defb 000h		;6742	00 	. 
	defb 000h		;6743	00 	. 
	defb 000h		;6744	00 	. 
	defb 000h		;6745	00 	. 
	defb 000h		;6746	00 	. 
	defb 000h		;6747	00 	. 
	defb 000h		;6748	00 	. 
	defb 010h		;6749	10 	. 
	defb 010h		;674a	10 	. 
	defb 010h		;674b	10 	. 
	defb 010h		;674c	10 	. 
	defb 010h		;674d	10 	. 
	defb 010h		;674e	10 	. 
	defb 000h		;674f	00 	. 
	defb 010h		;6750	10 	. 
	defb 010h		;6751	10 	. 
	defb 000h		;6752	00 	. 
	defb 000h		;6753	00 	. 
	defb 028h		;6754	28 	( 
	defb 028h		;6755	28 	( 
	defb 028h		;6756	28 	( 
	defb 000h		;6757	00 	. 
	defb 000h		;6758	00 	. 
	defb 000h		;6759	00 	. 
	defb 000h		;675a	00 	. 
	defb 000h		;675b	00 	. 
	defb 000h		;675c	00 	. 
	defb 000h		;675d	00 	. 
	defb 000h		;675e	00 	. 
	defb 000h		;675f	00 	. 
	defb 000h		;6760	00 	. 
	defb 000h		;6761	00 	. 
	defb 000h		;6762	00 	. 
	defb 000h		;6763	00 	. 
	defb 03ch		;6764	3c 	< 
	defb 042h		;6765	42 	B 
	defb 040h		;6766	40 	@ 
	defb 042h		;6767	42 	B 
	defb 03ch		;6768	3c 	< 
	defb 008h		;6769	08 	. 
	defb 038h		;676a	38 	8 
	defb 000h		;676b	00 	. 
	defb 010h		;676c	10 	. 
	defb 07ch		;676d	7c 	| 
	defb 092h		;676e	92 	. 
	defb 090h		;676f	90 	. 
	defb 090h		;6770	90 	. 
	defb 07ch		;6771	7c 	| 
	defb 012h		;6772	12 	. 
	defb 012h		;6773	12 	. 
	defb 092h		;6774	92 	. 
	defb 07ch		;6775	7c 	| 
	defb 010h		;6776	10 	. 
	defb 010h		;6777	10 	. 
	defb 060h		;6778	60 	` 
	defb 090h		;6779	90 	. 
	defb 092h		;677a	92 	. 
	defb 064h		;677b	64 	d 
	defb 008h		;677c	08 	. 
	defb 010h		;677d	10 	. 
	defb 020h		;677e	20 	  
	defb 04ch		;677f	4c 	L 
	defb 092h		;6780	92 	. 
	defb 012h		;6781	12 	. 
	defb 00ch		;6782	0c 	. 
	defb 000h		;6783	00 	. 
	defb 000h		;6784	00 	. 
	defb 038h		;6785	38 	8 
	defb 044h		;6786	44 	D 
	defb 044h		;6787	44 	D 
	defb 028h		;6788	28 	( 
	defb 070h		;6789	70 	p 
	defb 092h		;678a	92 	. 
	defb 08ah		;678b	8a 	. 
	defb 084h		;678c	84 	. 
	defb 07ah		;678d	7a 	z 
	defb 000h		;678e	00 	. 
	defb 000h		;678f	00 	. 
	defb 010h		;6790	10 	. 
	defb 010h		;6791	10 	. 
	defb 010h		;6792	10 	. 
	defb 000h		;6793	00 	. 
	defb 000h		;6794	00 	. 
	defb 000h		;6795	00 	. 
	defb 000h		;6796	00 	. 
	defb 000h		;6797	00 	. 
	defb 000h		;6798	00 	. 
	defb 000h		;6799	00 	. 
	defb 000h		;679a	00 	. 
	defb 000h		;679b	00 	. 
	defb 004h		;679c	04 	. 
	defb 008h		;679d	08 	. 
	defb 010h		;679e	10 	. 
	defb 010h		;679f	10 	. 
	defb 020h		;67a0	20 	  
	defb 020h		;67a1	20 	  
	defb 020h		;67a2	20 	  
	defb 010h		;67a3	10 	. 
	defb 010h		;67a4	10 	. 
	defb 008h		;67a5	08 	. 
	defb 004h		;67a6	04 	. 
	defb 000h		;67a7	00 	. 
	defb 040h		;67a8	40 	@ 
	defb 020h		;67a9	20 	  
	defb 010h		;67aa	10 	. 
	defb 010h		;67ab	10 	. 
	defb 008h		;67ac	08 	. 
	defb 008h		;67ad	08 	. 
	defb 008h		;67ae	08 	. 
	defb 010h		;67af	10 	. 
	defb 010h		;67b0	10 	. 
	defb 020h		;67b1	20 	  
	defb 040h		;67b2	40 	@ 
	defb 000h		;67b3	00 	. 
	defb 092h		;67b4	92 	. 
	defb 054h		;67b5	54 	T 
	defb 038h		;67b6	38 	8 
	defb 038h		;67b7	38 	8 
	defb 054h		;67b8	54 	T 
	defb 092h		;67b9	92 	. 
	defb 000h		;67ba	00 	. 
	defb 000h		;67bb	00 	. 
	defb 000h		;67bc	00 	. 
	defb 000h		;67bd	00 	. 
	defb 000h		;67be	00 	. 
	defb 000h		;67bf	00 	. 
	defb 000h		;67c0	00 	. 
	defb 000h		;67c1	00 	. 
	defb 010h		;67c2	10 	. 
	defb 010h		;67c3	10 	. 
	defb 010h		;67c4	10 	. 
	defb 0feh		;67c5	fe 	. 
	defb 010h		;67c6	10 	. 
	defb 010h		;67c7	10 	. 
	defb 010h		;67c8	10 	. 
	defb 000h		;67c9	00 	. 
	defb 000h		;67ca	00 	. 
	defb 000h		;67cb	00 	. 
	defb 000h		;67cc	00 	. 
	defb 000h		;67cd	00 	. 
	defb 000h		;67ce	00 	. 
	defb 000h		;67cf	00 	. 
	defb 000h		;67d0	00 	. 
	defb 000h		;67d1	00 	. 
	defb 000h		;67d2	00 	. 
	defb 018h		;67d3	18 	. 
	defb 018h		;67d4	18 	. 
	defb 008h		;67d5	08 	. 
	defb 010h		;67d6	10 	. 
	defb 000h		;67d7	00 	. 
	defb 000h		;67d8	00 	. 
	defb 000h		;67d9	00 	. 
	defb 000h		;67da	00 	. 
	defb 000h		;67db	00 	. 
	defb 000h		;67dc	00 	. 
	defb 07eh		;67dd	7e 	~ 
	defb 000h		;67de	00 	. 
	defb 000h		;67df	00 	. 
	defb 000h		;67e0	00 	. 
	defb 000h		;67e1	00 	. 
	defb 000h		;67e2	00 	. 
	defb 000h		;67e3	00 	. 
	defb 000h		;67e4	00 	. 
	defb 000h		;67e5	00 	. 
	defb 000h		;67e6	00 	. 
	defb 000h		;67e7	00 	. 
	defb 000h		;67e8	00 	. 
	defb 000h		;67e9	00 	. 
	defb 000h		;67ea	00 	. 
	defb 000h		;67eb	00 	. 
	defb 018h		;67ec	18 	. 
	defb 018h		;67ed	18 	. 
	defb 000h		;67ee	00 	. 
	defb 000h		;67ef	00 	. 
	defb 002h		;67f0	02 	. 
	defb 002h		;67f1	02 	. 
	defb 004h		;67f2	04 	. 
	defb 004h		;67f3	04 	. 
	defb 008h		;67f4	08 	. 
	defb 008h		;67f5	08 	. 
	defb 010h		;67f6	10 	. 
	defb 010h		;67f7	10 	. 
	defb 020h		;67f8	20 	  
	defb 020h		;67f9	20 	  
	defb 040h		;67fa	40 	@ 
	defb 040h		;67fb	40 	@ 
	defb 000h		;67fc	00 	. 
	defb 018h		;67fd	18 	. 
	defb 024h		;67fe	24 	$ 
	defb 042h		;67ff	42 	B 
	defb 042h		;6800	42 	B 
	defb 042h		;6801	42 	B 
	defb 042h		;6802	42 	B 
	defb 042h		;6803	42 	B 
	defb 024h		;6804	24 	$ 
	defb 018h		;6805	18 	. 
	defb 000h		;6806	00 	. 
	defb 000h		;6807	00 	. 
	defb 000h		;6808	00 	. 
	defb 030h		;6809	30 	0 
	defb 050h		;680a	50 	P 
	defb 010h		;680b	10 	. 
	defb 010h		;680c	10 	. 
	defb 010h		;680d	10 	. 
	defb 010h		;680e	10 	. 
	defb 010h		;680f	10 	. 
	defb 010h		;6810	10 	. 
	defb 07ch		;6811	7c 	| 
	defb 000h		;6812	00 	. 
	defb 000h		;6813	00 	. 
	defb 000h		;6814	00 	. 
	defb 03ch		;6815	3c 	< 
	defb 042h		;6816	42 	B 
	defb 042h		;6817	42 	B 
	defb 002h		;6818	02 	. 
	defb 004h		;6819	04 	. 
	defb 008h		;681a	08 	. 
	defb 010h		;681b	10 	. 
	defb 020h		;681c	20 	  
	defb 07eh		;681d	7e 	~ 
	defb 000h		;681e	00 	. 
	defb 000h		;681f	00 	. 
	defb 000h		;6820	00 	. 
	defb 03ch		;6821	3c 	< 
	defb 042h		;6822	42 	B 
	defb 002h		;6823	02 	. 
	defb 002h		;6824	02 	. 
	defb 01ch		;6825	1c 	. 
	defb 002h		;6826	02 	. 
	defb 002h		;6827	02 	. 
	defb 042h		;6828	42 	B 
	defb 03ch		;6829	3c 	< 
	defb 000h		;682a	00 	. 
	defb 000h		;682b	00 	. 
	defb 000h		;682c	00 	. 
	defb 004h		;682d	04 	. 
	defb 00ch		;682e	0c 	. 
	defb 014h		;682f	14 	. 
	defb 024h		;6830	24 	$ 
	defb 044h		;6831	44 	D 
	defb 044h		;6832	44 	D 
	defb 07eh		;6833	7e 	~ 
	defb 004h		;6834	04 	. 
	defb 004h		;6835	04 	. 
	defb 000h		;6836	00 	. 
	defb 000h		;6837	00 	. 
	defb 000h		;6838	00 	. 
	defb 07eh		;6839	7e 	~ 
	defb 040h		;683a	40 	@ 
	defb 040h		;683b	40 	@ 
	defb 07ch		;683c	7c 	| 
	defb 042h		;683d	42 	B 
	defb 002h		;683e	02 	. 
	defb 002h		;683f	02 	. 
	defb 042h		;6840	42 	B 
	defb 03ch		;6841	3c 	< 
	defb 000h		;6842	00 	. 
	defb 000h		;6843	00 	. 
	defb 000h		;6844	00 	. 
	defb 03ch		;6845	3c 	< 
	defb 040h		;6846	40 	@ 
	defb 040h		;6847	40 	@ 
	defb 040h		;6848	40 	@ 
	defb 07ch		;6849	7c 	| 
	defb 042h		;684a	42 	B 
	defb 042h		;684b	42 	B 
	defb 042h		;684c	42 	B 
	defb 03ch		;684d	3c 	< 
	defb 000h		;684e	00 	. 
	defb 000h		;684f	00 	. 
	defb 000h		;6850	00 	. 
	defb 07eh		;6851	7e 	~ 
	defb 002h		;6852	02 	. 
	defb 004h		;6853	04 	. 
	defb 004h		;6854	04 	. 
	defb 008h		;6855	08 	. 
	defb 008h		;6856	08 	. 
	defb 010h		;6857	10 	. 
	defb 010h		;6858	10 	. 
	defb 010h		;6859	10 	. 
	defb 000h		;685a	00 	. 
	defb 000h		;685b	00 	. 
	defb 000h		;685c	00 	. 
	defb 03ch		;685d	3c 	< 
	defb 042h		;685e	42 	B 
	defb 042h		;685f	42 	B 
	defb 042h		;6860	42 	B 
	defb 03ch		;6861	3c 	< 
	defb 042h		;6862	42 	B 
	defb 042h		;6863	42 	B 
	defb 042h		;6864	42 	B 
	defb 03ch		;6865	3c 	< 
	defb 000h		;6866	00 	. 
	defb 000h		;6867	00 	. 
	defb 000h		;6868	00 	. 
	defb 03ch		;6869	3c 	< 
	defb 042h		;686a	42 	B 
	defb 042h		;686b	42 	B 
	defb 042h		;686c	42 	B 
	defb 03eh		;686d	3e 	> 
	defb 002h		;686e	02 	. 
	defb 002h		;686f	02 	. 
	defb 002h		;6870	02 	. 
	defb 03ch		;6871	3c 	< 
	defb 000h		;6872	00 	. 
	defb 000h		;6873	00 	. 
	defb 000h		;6874	00 	. 
	defb 000h		;6875	00 	. 
	defb 000h		;6876	00 	. 
	defb 018h		;6877	18 	. 
	defb 018h		;6878	18 	. 
	defb 000h		;6879	00 	. 
	defb 000h		;687a	00 	. 
	defb 000h		;687b	00 	. 
	defb 018h		;687c	18 	. 
	defb 018h		;687d	18 	. 
	defb 000h		;687e	00 	. 
	defb 000h		;687f	00 	. 
	defb 000h		;6880	00 	. 
	defb 000h		;6881	00 	. 
	defb 000h		;6882	00 	. 
	defb 018h		;6883	18 	. 
	defb 018h		;6884	18 	. 
	defb 000h		;6885	00 	. 
	defb 000h		;6886	00 	. 
	defb 018h		;6887	18 	. 
	defb 018h		;6888	18 	. 
	defb 008h		;6889	08 	. 
	defb 010h		;688a	10 	. 
	defb 000h		;688b	00 	. 
	defb 010h		;688c	10 	. 
	defb 010h		;688d	10 	. 
	defb 010h		;688e	10 	. 
	defb 010h		;688f	10 	. 
	defb 010h		;6890	10 	. 
	defb 010h		;6891	10 	. 
	defb 010h		;6892	10 	. 
	defb 010h		;6893	10 	. 
	defb 010h		;6894	10 	. 
	defb 010h		;6895	10 	. 
	defb 010h		;6896	10 	. 
	defb 010h		;6897	10 	. 
	defb 000h		;6898	00 	. 
	defb 000h		;6899	00 	. 
	defb 000h		;689a	00 	. 
	defb 000h		;689b	00 	. 
	defb 07eh		;689c	7e 	~ 
	defb 000h		;689d	00 	. 
	defb 000h		;689e	00 	. 
	defb 07eh		;689f	7e 	~ 
	defb 000h		;68a0	00 	. 
	defb 000h		;68a1	00 	. 
	defb 000h		;68a2	00 	. 
	defb 000h		;68a3	00 	. 
	defb 000h		;68a4	00 	. 
	defb 010h		;68a5	10 	. 
	defb 010h		;68a6	10 	. 
	defb 000h		;68a7	00 	. 
	defb 010h		;68a8	10 	. 
	defb 010h		;68a9	10 	. 
	defb 010h		;68aa	10 	. 
	defb 010h		;68ab	10 	. 
	defb 010h		;68ac	10 	. 
	defb 010h		;68ad	10 	. 
	defb 000h		;68ae	00 	. 
	defb 000h		;68af	00 	. 
	defb 000h		;68b0	00 	. 
	defb 03ch		;68b1	3c 	< 
	defb 042h		;68b2	42 	B 
	defb 042h		;68b3	42 	B 
	defb 002h		;68b4	02 	. 
	defb 01ch		;68b5	1c 	. 
	defb 010h		;68b6	10 	. 
	defb 000h		;68b7	00 	. 
	defb 010h		;68b8	10 	. 
	defb 010h		;68b9	10 	. 
	defb 000h		;68ba	00 	. 
	defb 000h		;68bb	00 	. 
	defb 000h		;68bc	00 	. 
	defb 000h		;68bd	00 	. 
	defb 006h		;68be	06 	. 
	defb 018h		;68bf	18 	. 
	defb 060h		;68c0	60 	` 
	defb 080h		;68c1	80 	. 
	defb 060h		;68c2	60 	` 
	defb 018h		;68c3	18 	. 
	defb 006h		;68c4	06 	. 
	defb 000h		;68c5	00 	. 
	defb 000h		;68c6	00 	. 
	defb 000h		;68c7	00 	. 
	defb 000h		;68c8	00 	. 
	defb 010h		;68c9	10 	. 
	defb 028h		;68ca	28 	( 
	defb 044h		;68cb	44 	D 
	defb 082h		;68cc	82 	. 
	defb 082h		;68cd	82 	. 
	defb 0feh		;68ce	fe 	. 
	defb 082h		;68cf	82 	. 
	defb 082h		;68d0	82 	. 
	defb 082h		;68d1	82 	. 
	defb 000h		;68d2	00 	. 
	defb 000h		;68d3	00 	. 
	defb 000h		;68d4	00 	. 
	defb 0fch		;68d5	fc 	. 
	defb 042h		;68d6	42 	B 
	defb 042h		;68d7	42 	B 
	defb 042h		;68d8	42 	B 
	defb 07ch		;68d9	7c 	| 
	defb 042h		;68da	42 	B 
	defb 042h		;68db	42 	B 
	defb 042h		;68dc	42 	B 
	defb 0fch		;68dd	fc 	. 
	defb 000h		;68de	00 	. 
	defb 000h		;68df	00 	. 
	defb 000h		;68e0	00 	. 
	defb 03ch		;68e1	3c 	< 
	defb 042h		;68e2	42 	B 
	defb 082h		;68e3	82 	. 
	defb 080h		;68e4	80 	. 
	defb 080h		;68e5	80 	. 
	defb 080h		;68e6	80 	. 
	defb 082h		;68e7	82 	. 
	defb 042h		;68e8	42 	B 
	defb 03ch		;68e9	3c 	< 
	defb 000h		;68ea	00 	. 
	defb 000h		;68eb	00 	. 
	defb 000h		;68ec	00 	. 
	defb 0f8h		;68ed	f8 	. 
	defb 044h		;68ee	44 	D 
	defb 042h		;68ef	42 	B 
	defb 042h		;68f0	42 	B 
	defb 042h		;68f1	42 	B 
	defb 042h		;68f2	42 	B 
	defb 042h		;68f3	42 	B 
	defb 044h		;68f4	44 	D 
	defb 0f8h		;68f5	f8 	. 
	defb 000h		;68f6	00 	. 
	defb 000h		;68f7	00 	. 
	defb 000h		;68f8	00 	. 
	defb 0feh		;68f9	fe 	. 
	defb 080h		;68fa	80 	. 
	defb 080h		;68fb	80 	. 
	defb 080h		;68fc	80 	. 
	defb 0f8h		;68fd	f8 	. 
	defb 080h		;68fe	80 	. 
	defb 080h		;68ff	80 	. 
	defb 080h		;6900	80 	. 
	defb 0feh		;6901	fe 	. 
	defb 000h		;6902	00 	. 
	defb 000h		;6903	00 	. 
	defb 000h		;6904	00 	. 
	defb 0feh		;6905	fe 	. 
	defb 080h		;6906	80 	. 
	defb 080h		;6907	80 	. 
	defb 080h		;6908	80 	. 
	defb 0f8h		;6909	f8 	. 
	defb 080h		;690a	80 	. 
	defb 080h		;690b	80 	. 
	defb 080h		;690c	80 	. 
	defb 080h		;690d	80 	. 
	defb 000h		;690e	00 	. 
	defb 000h		;690f	00 	. 
	defb 000h		;6910	00 	. 
	defb 03ch		;6911	3c 	< 
	defb 042h		;6912	42 	B 
	defb 080h		;6913	80 	. 
	defb 080h		;6914	80 	. 
	defb 09eh		;6915	9e 	. 
	defb 082h		;6916	82 	. 
	defb 082h		;6917	82 	. 
	defb 046h		;6918	46 	F 
	defb 03ah		;6919	3a 	: 
	defb 000h		;691a	00 	. 
	defb 000h		;691b	00 	. 
	defb 000h		;691c	00 	. 
	defb 082h		;691d	82 	. 
	defb 082h		;691e	82 	. 
	defb 082h		;691f	82 	. 
	defb 082h		;6920	82 	. 
	defb 0feh		;6921	fe 	. 
	defb 082h		;6922	82 	. 
	defb 082h		;6923	82 	. 
	defb 082h		;6924	82 	. 
	defb 082h		;6925	82 	. 
	defb 000h		;6926	00 	. 
	defb 000h		;6927	00 	. 
	defb 000h		;6928	00 	. 
	defb 07ch		;6929	7c 	| 
	defb 010h		;692a	10 	. 
	defb 010h		;692b	10 	. 
	defb 010h		;692c	10 	. 
	defb 010h		;692d	10 	. 
	defb 010h		;692e	10 	. 
	defb 010h		;692f	10 	. 
	defb 010h		;6930	10 	. 
	defb 07ch		;6931	7c 	| 
	defb 000h		;6932	00 	. 
	defb 000h		;6933	00 	. 
	defb 000h		;6934	00 	. 
	defb 01eh		;6935	1e 	. 
	defb 004h		;6936	04 	. 
	defb 004h		;6937	04 	. 
	defb 004h		;6938	04 	. 
	defb 004h		;6939	04 	. 
	defb 004h		;693a	04 	. 
	defb 084h		;693b	84 	. 
	defb 084h		;693c	84 	. 
	defb 078h		;693d	78 	x 
	defb 000h		;693e	00 	. 
	defb 000h		;693f	00 	. 
	defb 000h		;6940	00 	. 
	defb 082h		;6941	82 	. 
	defb 084h		;6942	84 	. 
	defb 088h		;6943	88 	. 
	defb 090h		;6944	90 	. 
	defb 0a0h		;6945	a0 	. 
	defb 0d0h		;6946	d0 	. 
	defb 088h		;6947	88 	. 
	defb 084h		;6948	84 	. 
	defb 082h		;6949	82 	. 
	defb 000h		;694a	00 	. 
	defb 000h		;694b	00 	. 
	defb 000h		;694c	00 	. 
	defb 080h		;694d	80 	. 
	defb 080h		;694e	80 	. 
	defb 080h		;694f	80 	. 
	defb 080h		;6950	80 	. 
	defb 080h		;6951	80 	. 
	defb 080h		;6952	80 	. 
	defb 080h		;6953	80 	. 
	defb 080h		;6954	80 	. 
	defb 0feh		;6955	fe 	. 
	defb 000h		;6956	00 	. 
	defb 000h		;6957	00 	. 
	defb 000h		;6958	00 	. 
	defb 082h		;6959	82 	. 
	defb 0c6h		;695a	c6 	. 
	defb 0c6h		;695b	c6 	. 
	defb 0aah		;695c	aa 	. 
	defb 0aah		;695d	aa 	. 
	defb 092h		;695e	92 	. 
	defb 082h		;695f	82 	. 
	defb 082h		;6960	82 	. 
	defb 082h		;6961	82 	. 
	defb 000h		;6962	00 	. 
	defb 000h		;6963	00 	. 
	defb 000h		;6964	00 	. 
	defb 082h		;6965	82 	. 
	defb 082h		;6966	82 	. 
	defb 0c2h		;6967	c2 	. 
	defb 0a2h		;6968	a2 	. 
	defb 092h		;6969	92 	. 
	defb 08ah		;696a	8a 	. 
	defb 086h		;696b	86 	. 
	defb 082h		;696c	82 	. 
	defb 082h		;696d	82 	. 
	defb 000h		;696e	00 	. 
	defb 000h		;696f	00 	. 
	defb 000h		;6970	00 	. 
	defb 07ch		;6971	7c 	| 
	defb 082h		;6972	82 	. 
	defb 082h		;6973	82 	. 
	defb 082h		;6974	82 	. 
	defb 082h		;6975	82 	. 
	defb 082h		;6976	82 	. 
	defb 082h		;6977	82 	. 
	defb 082h		;6978	82 	. 
	defb 07ch		;6979	7c 	| 
	defb 000h		;697a	00 	. 
	defb 000h		;697b	00 	. 
	defb 000h		;697c	00 	. 
	defb 0fch		;697d	fc 	. 
	defb 082h		;697e	82 	. 
	defb 082h		;697f	82 	. 
	defb 082h		;6980	82 	. 
	defb 0fch		;6981	fc 	. 
	defb 080h		;6982	80 	. 
	defb 080h		;6983	80 	. 
	defb 080h		;6984	80 	. 
	defb 080h		;6985	80 	. 
	defb 000h		;6986	00 	. 
	defb 000h		;6987	00 	. 
	defb 000h		;6988	00 	. 
	defb 07ch		;6989	7c 	| 
	defb 082h		;698a	82 	. 
	defb 082h		;698b	82 	. 
	defb 082h		;698c	82 	. 
	defb 082h		;698d	82 	. 
	defb 092h		;698e	92 	. 
	defb 08ah		;698f	8a 	. 
	defb 084h		;6990	84 	. 
	defb 07ah		;6991	7a 	z 
	defb 000h		;6992	00 	. 
	defb 000h		;6993	00 	. 
	defb 000h		;6994	00 	. 
	defb 0fch		;6995	fc 	. 
	defb 082h		;6996	82 	. 
	defb 082h		;6997	82 	. 
	defb 082h		;6998	82 	. 
	defb 0fch		;6999	fc 	. 
	defb 090h		;699a	90 	. 
	defb 088h		;699b	88 	. 
	defb 084h		;699c	84 	. 
	defb 082h		;699d	82 	. 
	defb 000h		;699e	00 	. 
	defb 000h		;699f	00 	. 
	defb 000h		;69a0	00 	. 
	defb 07ch		;69a1	7c 	| 
	defb 082h		;69a2	82 	. 
	defb 080h		;69a3	80 	. 
	defb 080h		;69a4	80 	. 
	defb 07ch		;69a5	7c 	| 
	defb 002h		;69a6	02 	. 
	defb 002h		;69a7	02 	. 
	defb 082h		;69a8	82 	. 
	defb 07ch		;69a9	7c 	| 
	defb 000h		;69aa	00 	. 
	defb 000h		;69ab	00 	. 
	defb 000h		;69ac	00 	. 
	defb 0feh		;69ad	fe 	. 
	defb 010h		;69ae	10 	. 
	defb 010h		;69af	10 	. 
	defb 010h		;69b0	10 	. 
	defb 010h		;69b1	10 	. 
	defb 010h		;69b2	10 	. 
	defb 010h		;69b3	10 	. 
	defb 010h		;69b4	10 	. 
	defb 010h		;69b5	10 	. 
	defb 000h		;69b6	00 	. 
	defb 000h		;69b7	00 	. 
	defb 000h		;69b8	00 	. 
	defb 082h		;69b9	82 	. 
	defb 082h		;69ba	82 	. 
	defb 082h		;69bb	82 	. 
	defb 082h		;69bc	82 	. 
	defb 082h		;69bd	82 	. 
	defb 082h		;69be	82 	. 
	defb 082h		;69bf	82 	. 
	defb 082h		;69c0	82 	. 
	defb 07ch		;69c1	7c 	| 
	defb 000h		;69c2	00 	. 
	defb 000h		;69c3	00 	. 
	defb 000h		;69c4	00 	. 
	defb 082h		;69c5	82 	. 
	defb 082h		;69c6	82 	. 
	defb 082h		;69c7	82 	. 
	defb 044h		;69c8	44 	D 
	defb 044h		;69c9	44 	D 
	defb 044h		;69ca	44 	D 
	defb 028h		;69cb	28 	( 
	defb 028h		;69cc	28 	( 
	defb 010h		;69cd	10 	. 
	defb 000h		;69ce	00 	. 
	defb 000h		;69cf	00 	. 
	defb 000h		;69d0	00 	. 
	defb 082h		;69d1	82 	. 
	defb 082h		;69d2	82 	. 
	defb 082h		;69d3	82 	. 
	defb 092h		;69d4	92 	. 
	defb 092h		;69d5	92 	. 
	defb 092h		;69d6	92 	. 
	defb 0aah		;69d7	aa 	. 
	defb 044h		;69d8	44 	D 
	defb 044h		;69d9	44 	D 
	defb 000h		;69da	00 	. 
	defb 000h		;69db	00 	. 
	defb 000h		;69dc	00 	. 
	defb 082h		;69dd	82 	. 
	defb 082h		;69de	82 	. 
	defb 044h		;69df	44 	D 
	defb 028h		;69e0	28 	( 
	defb 010h		;69e1	10 	. 
	defb 028h		;69e2	28 	( 
	defb 044h		;69e3	44 	D 
	defb 082h		;69e4	82 	. 
	defb 082h		;69e5	82 	. 
	defb 000h		;69e6	00 	. 
	defb 000h		;69e7	00 	. 
	defb 000h		;69e8	00 	. 
	defb 082h		;69e9	82 	. 
	defb 082h		;69ea	82 	. 
	defb 044h		;69eb	44 	D 
	defb 044h		;69ec	44 	D 
	defb 028h		;69ed	28 	( 
	defb 010h		;69ee	10 	. 
	defb 010h		;69ef	10 	. 
	defb 010h		;69f0	10 	. 
	defb 010h		;69f1	10 	. 
	defb 000h		;69f2	00 	. 
	defb 000h		;69f3	00 	. 
	defb 000h		;69f4	00 	. 
	defb 0feh		;69f5	fe 	. 
	defb 002h		;69f6	02 	. 
	defb 004h		;69f7	04 	. 
	defb 008h		;69f8	08 	. 
	defb 010h		;69f9	10 	. 
	defb 020h		;69fa	20 	  
	defb 040h		;69fb	40 	@ 
	defb 080h		;69fc	80 	. 
	defb 0feh		;69fd	fe 	. 
	defb 000h		;69fe	00 	. 
	defb 000h		;69ff	00 	. 
	defb 000h		;6a00	00 	. 
	defb 000h		;6a01	00 	. 
	defb 000h		;6a02	00 	. 
	defb 000h		;6a03	00 	. 
	defb 000h		;6a04	00 	. 
	defb 000h		;6a05	00 	. 
	defb 000h		;6a06	00 	. 
	defb 000h		;6a07	00 	. 
	defb 000h		;6a08	00 	. 
	defb 0ffh		;6a09	ff 	. 
	defb 000h		;6a0a	00 	. 
	defb 0ffh		;6a0b	ff 	. 
	defb 000h		;6a0c	00 	. 
	defb 008h		;6a0d	08 	. 
	defb 00ch		;6a0e	0c 	. 
	defb 00ah		;6a0f	0a 	. 
	defb 009h		;6a10	09 	. 
	defb 009h		;6a11	09 	. 
	defb 009h		;6a12	09 	. 
	defb 00ah		;6a13	0a 	. 
	defb 078h		;6a14	78 	x 
	defb 0f8h		;6a15	f8 	. 
	defb 070h		;6a16	70 	p 
	defb 000h		;6a17	00 	. 
	defb 000h		;6a18	00 	. 
	defb 00fh		;6a19	0f 	. 
	defb 010h		;6a1a	10 	. 
	defb 023h		;6a1b	23 	# 
	defb 024h		;6a1c	24 	$ 
	defb 024h		;6a1d	24 	$ 
	defb 024h		;6a1e	24 	$ 
	defb 024h		;6a1f	24 	$ 
	defb 023h		;6a20	23 	# 
	defb 010h		;6a21	10 	. 
	defb 00fh		;6a22	0f 	. 
	defb 000h		;6a23	00 	. 
	defb 000h		;6a24	00 	. 
	defb 0e0h		;6a25	e0 	. 
	defb 010h		;6a26	10 	. 
	defb 088h		;6a27	88 	. 
	defb 048h		;6a28	48 	H 
	defb 008h		;6a29	08 	. 
	defb 008h		;6a2a	08 	. 
	defb 048h		;6a2b	48 	H 
	defb 088h		;6a2c	88 	. 
	defb 010h		;6a2d	10 	. 
	defb 0e0h		;6a2e	e0 	. 
	defb 000h		;6a2f	00 	. 
	defb 000h		;6a30	00 	. 
	defb 000h		;6a31	00 	. 
	defb 000h		;6a32	00 	. 
	defb 000h		;6a33	00 	. 
	defb 000h		;6a34	00 	. 
	defb 000h		;6a35	00 	. 
	defb 000h		;6a36	00 	. 
	defb 000h		;6a37	00 	. 
	defb 000h		;6a38	00 	. 
	defb 000h		;6a39	00 	. 
	defb 000h		;6a3a	00 	. 
	defb 0ffh		;6a3b	ff 	. 
	defb 000h		;6a3c	00 	. 
	defb 000h		;6a3d	00 	. 
	defb 0c0h		;6a3e	c0 	. 
	defb 030h		;6a3f	30 	0 
	defb 00ch		;6a40	0c 	. 
	defb 002h		;6a41	02 	. 
	defb 00ch		;6a42	0c 	. 
	defb 030h		;6a43	30 	0 
	defb 0c0h		;6a44	c0 	. 
	defb 000h		;6a45	00 	. 
	defb 000h		;6a46	00 	. 
	defb 000h		;6a47	00 	. 
	defb 000h		;6a48	00 	. 
	defb 000h		;6a49	00 	. 
	defb 000h		;6a4a	00 	. 
	defb 000h		;6a4b	00 	. 
	defb 03ch		;6a4c	3c 	< 
	defb 002h		;6a4d	02 	. 
	defb 03eh		;6a4e	3e 	> 
	defb 042h		;6a4f	42 	B 
	defb 042h		;6a50	42 	B 
	defb 03eh		;6a51	3e 	> 
	defb 000h		;6a52	00 	. 
	defb 000h		;6a53	00 	. 
	defb 000h		;6a54	00 	. 
	defb 040h		;6a55	40 	@ 
	defb 040h		;6a56	40 	@ 
	defb 040h		;6a57	40 	@ 
	defb 07ch		;6a58	7c 	| 
	defb 042h		;6a59	42 	B 
	defb 042h		;6a5a	42 	B 
	defb 042h		;6a5b	42 	B 
	defb 042h		;6a5c	42 	B 
	defb 07ch		;6a5d	7c 	| 
	defb 000h		;6a5e	00 	. 
	defb 000h		;6a5f	00 	. 
	defb 000h		;6a60	00 	. 
	defb 000h		;6a61	00 	. 
	defb 000h		;6a62	00 	. 
	defb 000h		;6a63	00 	. 
	defb 03ch		;6a64	3c 	< 
	defb 042h		;6a65	42 	B 
	defb 040h		;6a66	40 	@ 
	defb 040h		;6a67	40 	@ 
	defb 042h		;6a68	42 	B 
	defb 03ch		;6a69	3c 	< 
	defb 000h		;6a6a	00 	. 
	defb 000h		;6a6b	00 	. 
	defb 000h		;6a6c	00 	. 
	defb 002h		;6a6d	02 	. 
	defb 002h		;6a6e	02 	. 
	defb 002h		;6a6f	02 	. 
	defb 03eh		;6a70	3e 	> 
	defb 042h		;6a71	42 	B 
	defb 042h		;6a72	42 	B 
	defb 042h		;6a73	42 	B 
	defb 042h		;6a74	42 	B 
	defb 03eh		;6a75	3e 	> 
	defb 000h		;6a76	00 	. 
	defb 000h		;6a77	00 	. 
	defb 000h		;6a78	00 	. 
	defb 000h		;6a79	00 	. 
	defb 000h		;6a7a	00 	. 
	defb 000h		;6a7b	00 	. 
	defb 03ch		;6a7c	3c 	< 
	defb 042h		;6a7d	42 	B 
	defb 07eh		;6a7e	7e 	~ 
	defb 040h		;6a7f	40 	@ 
	defb 042h		;6a80	42 	B 
	defb 03ch		;6a81	3c 	< 
	defb 000h		;6a82	00 	. 
	defb 000h		;6a83	00 	. 
	defb 000h		;6a84	00 	. 
	defb 00eh		;6a85	0e 	. 
	defb 010h		;6a86	10 	. 
	defb 010h		;6a87	10 	. 
	defb 07eh		;6a88	7e 	~ 
	defb 010h		;6a89	10 	. 
	defb 010h		;6a8a	10 	. 
	defb 010h		;6a8b	10 	. 
	defb 010h		;6a8c	10 	. 
	defb 010h		;6a8d	10 	. 
	defb 000h		;6a8e	00 	. 
	defb 000h		;6a8f	00 	. 
	defb 000h		;6a90	00 	. 
	defb 000h		;6a91	00 	. 
	defb 000h		;6a92	00 	. 
	defb 000h		;6a93	00 	. 
	defb 03eh		;6a94	3e 	> 
	defb 042h		;6a95	42 	B 
	defb 042h		;6a96	42 	B 
	defb 042h		;6a97	42 	B 
	defb 03eh		;6a98	3e 	> 
	defb 002h		;6a99	02 	. 
	defb 03ch		;6a9a	3c 	< 
	defb 000h		;6a9b	00 	. 
	defb 000h		;6a9c	00 	. 
	defb 040h		;6a9d	40 	@ 
	defb 040h		;6a9e	40 	@ 
	defb 040h		;6a9f	40 	@ 
	defb 07ch		;6aa0	7c 	| 
	defb 042h		;6aa1	42 	B 
	defb 042h		;6aa2	42 	B 
	defb 042h		;6aa3	42 	B 
	defb 042h		;6aa4	42 	B 
	defb 042h		;6aa5	42 	B 
	defb 000h		;6aa6	00 	. 
	defb 000h		;6aa7	00 	. 
	defb 000h		;6aa8	00 	. 
	defb 030h		;6aa9	30 	0 
	defb 030h		;6aaa	30 	0 
	defb 000h		;6aab	00 	. 
	defb 070h		;6aac	70 	p 
	defb 010h		;6aad	10 	. 
	defb 010h		;6aae	10 	. 
	defb 010h		;6aaf	10 	. 
	defb 010h		;6ab0	10 	. 
	defb 07ch		;6ab1	7c 	| 
	defb 000h		;6ab2	00 	. 
	defb 000h		;6ab3	00 	. 
	defb 000h		;6ab4	00 	. 
	defb 006h		;6ab5	06 	. 
	defb 006h		;6ab6	06 	. 
	defb 000h		;6ab7	00 	. 
	defb 00eh		;6ab8	0e 	. 
	defb 002h		;6ab9	02 	. 
	defb 002h		;6aba	02 	. 
	defb 002h		;6abb	02 	. 
	defb 002h		;6abc	02 	. 
	defb 042h		;6abd	42 	B 
	defb 03ch		;6abe	3c 	< 
	defb 000h		;6abf	00 	. 
	defb 000h		;6ac0	00 	. 
	defb 040h		;6ac1	40 	@ 
	defb 040h		;6ac2	40 	@ 
	defb 040h		;6ac3	40 	@ 
	defb 044h		;6ac4	44 	D 
	defb 048h		;6ac5	48 	H 
	defb 050h		;6ac6	50 	P 
	defb 068h		;6ac7	68 	h 
	defb 044h		;6ac8	44 	D 
	defb 042h		;6ac9	42 	B 
	defb 000h		;6aca	00 	. 
	defb 000h		;6acb	00 	. 
	defb 000h		;6acc	00 	. 
	defb 070h		;6acd	70 	p 
	defb 010h		;6ace	10 	. 
	defb 010h		;6acf	10 	. 
	defb 010h		;6ad0	10 	. 
	defb 010h		;6ad1	10 	. 
	defb 010h		;6ad2	10 	. 
	defb 010h		;6ad3	10 	. 
	defb 010h		;6ad4	10 	. 
	defb 07ch		;6ad5	7c 	| 
	defb 000h		;6ad6	00 	. 
	defb 000h		;6ad7	00 	. 
	defb 000h		;6ad8	00 	. 
	defb 000h		;6ad9	00 	. 
	defb 000h		;6ada	00 	. 
	defb 000h		;6adb	00 	. 
	defb 0ech		;6adc	ec 	. 
	defb 092h		;6add	92 	. 
	defb 092h		;6ade	92 	. 
	defb 092h		;6adf	92 	. 
	defb 092h		;6ae0	92 	. 
	defb 092h		;6ae1	92 	. 
	defb 000h		;6ae2	00 	. 
	defb 000h		;6ae3	00 	. 
	defb 000h		;6ae4	00 	. 
	defb 000h		;6ae5	00 	. 
	defb 000h		;6ae6	00 	. 
	defb 000h		;6ae7	00 	. 
	defb 05ch		;6ae8	5c 	\ 
	defb 062h		;6ae9	62 	b 
	defb 042h		;6aea	42 	B 
	defb 042h		;6aeb	42 	B 
	defb 042h		;6aec	42 	B 
	defb 042h		;6aed	42 	B 
	defb 000h		;6aee	00 	. 
	defb 000h		;6aef	00 	. 
	defb 000h		;6af0	00 	. 
	defb 000h		;6af1	00 	. 
	defb 000h		;6af2	00 	. 
	defb 000h		;6af3	00 	. 
	defb 03ch		;6af4	3c 	< 
	defb 042h		;6af5	42 	B 
	defb 042h		;6af6	42 	B 
	defb 042h		;6af7	42 	B 
	defb 042h		;6af8	42 	B 
	defb 03ch		;6af9	3c 	< 
	defb 000h		;6afa	00 	. 
	defb 000h		;6afb	00 	. 
	defb 000h		;6afc	00 	. 
	defb 000h		;6afd	00 	. 
	defb 000h		;6afe	00 	. 
	defb 000h		;6aff	00 	. 
	defb 07ch		;6b00	7c 	| 
	defb 042h		;6b01	42 	B 
	defb 042h		;6b02	42 	B 
	defb 042h		;6b03	42 	B 
	defb 07ch		;6b04	7c 	| 
	defb 040h		;6b05	40 	@ 
	defb 040h		;6b06	40 	@ 
	defb 040h		;6b07	40 	@ 
	defb 000h		;6b08	00 	. 
	defb 000h		;6b09	00 	. 
	defb 000h		;6b0a	00 	. 
	defb 000h		;6b0b	00 	. 
	defb 03eh		;6b0c	3e 	> 
	defb 042h		;6b0d	42 	B 
	defb 042h		;6b0e	42 	B 
	defb 042h		;6b0f	42 	B 
	defb 03eh		;6b10	3e 	> 
	defb 002h		;6b11	02 	. 
	defb 002h		;6b12	02 	. 
	defb 002h		;6b13	02 	. 
	defb 000h		;6b14	00 	. 
	defb 000h		;6b15	00 	. 
	defb 000h		;6b16	00 	. 
	defb 000h		;6b17	00 	. 
	defb 04ch		;6b18	4c 	L 
	defb 052h		;6b19	52 	R 
	defb 060h		;6b1a	60 	` 
	defb 040h		;6b1b	40 	@ 
	defb 040h		;6b1c	40 	@ 
	defb 040h		;6b1d	40 	@ 
	defb 000h		;6b1e	00 	. 
	defb 000h		;6b1f	00 	. 
	defb 000h		;6b20	00 	. 
	defb 000h		;6b21	00 	. 
	defb 000h		;6b22	00 	. 
	defb 000h		;6b23	00 	. 
	defb 03eh		;6b24	3e 	> 
	defb 040h		;6b25	40 	@ 
	defb 03ch		;6b26	3c 	< 
	defb 002h		;6b27	02 	. 
	defb 002h		;6b28	02 	. 
	defb 07ch		;6b29	7c 	| 
	defb 000h		;6b2a	00 	. 
	defb 000h		;6b2b	00 	. 
	defb 000h		;6b2c	00 	. 
	defb 010h		;6b2d	10 	. 
	defb 010h		;6b2e	10 	. 
	defb 010h		;6b2f	10 	. 
	defb 07eh		;6b30	7e 	~ 
	defb 010h		;6b31	10 	. 
	defb 010h		;6b32	10 	. 
	defb 010h		;6b33	10 	. 
	defb 010h		;6b34	10 	. 
	defb 00eh		;6b35	0e 	. 
	defb 000h		;6b36	00 	. 
	defb 000h		;6b37	00 	. 
	defb 000h		;6b38	00 	. 
	defb 000h		;6b39	00 	. 
	defb 000h		;6b3a	00 	. 
	defb 000h		;6b3b	00 	. 
	defb 042h		;6b3c	42 	B 
	defb 042h		;6b3d	42 	B 
	defb 042h		;6b3e	42 	B 
	defb 042h		;6b3f	42 	B 
	defb 046h		;6b40	46 	F 
	defb 03ah		;6b41	3a 	: 
	defb 000h		;6b42	00 	. 
	defb 000h		;6b43	00 	. 
	defb 000h		;6b44	00 	. 
	defb 000h		;6b45	00 	. 
	defb 000h		;6b46	00 	. 
	defb 000h		;6b47	00 	. 
	defb 082h		;6b48	82 	. 
	defb 082h		;6b49	82 	. 
	defb 044h		;6b4a	44 	D 
	defb 044h		;6b4b	44 	D 
	defb 028h		;6b4c	28 	( 
	defb 010h		;6b4d	10 	. 
	defb 000h		;6b4e	00 	. 
	defb 000h		;6b4f	00 	. 
	defb 000h		;6b50	00 	. 
	defb 000h		;6b51	00 	. 
	defb 000h		;6b52	00 	. 
	defb 000h		;6b53	00 	. 
	defb 082h		;6b54	82 	. 
	defb 092h		;6b55	92 	. 
	defb 092h		;6b56	92 	. 
	defb 092h		;6b57	92 	. 
	defb 0aah		;6b58	aa 	. 
	defb 044h		;6b59	44 	D 
	defb 000h		;6b5a	00 	. 
	defb 000h		;6b5b	00 	. 
	defb 000h		;6b5c	00 	. 
	defb 000h		;6b5d	00 	. 
	defb 000h		;6b5e	00 	. 
	defb 000h		;6b5f	00 	. 
	defb 044h		;6b60	44 	D 
	defb 028h		;6b61	28 	( 
	defb 010h		;6b62	10 	. 
	defb 028h		;6b63	28 	( 
	defb 044h		;6b64	44 	D 
	defb 082h		;6b65	82 	. 
	defb 000h		;6b66	00 	. 
	defb 000h		;6b67	00 	. 
	defb 000h		;6b68	00 	. 
	defb 000h		;6b69	00 	. 
	defb 000h		;6b6a	00 	. 
	defb 000h		;6b6b	00 	. 
	defb 042h		;6b6c	42 	B 
	defb 042h		;6b6d	42 	B 
	defb 042h		;6b6e	42 	B 
	defb 03eh		;6b6f	3e 	> 
	defb 002h		;6b70	02 	. 
	defb 002h		;6b71	02 	. 
	defb 07ch		;6b72	7c 	| 
	defb 000h		;6b73	00 	. 
	defb 000h		;6b74	00 	. 
	defb 000h		;6b75	00 	. 
	defb 000h		;6b76	00 	. 
	defb 000h		;6b77	00 	. 
	defb 07eh		;6b78	7e 	~ 
	defb 004h		;6b79	04 	. 
	defb 008h		;6b7a	08 	. 
	defb 010h		;6b7b	10 	. 
	defb 020h		;6b7c	20 	  
	defb 07eh		;6b7d	7e 	~ 
	defb 000h		;6b7e	00 	. 
	defb 000h		;6b7f	00 	. 
	defb 000h		;6b80	00 	. 
	defb 008h		;6b81	08 	. 
	defb 008h		;6b82	08 	. 
	defb 000h		;6b83	00 	. 
	defb 008h		;6b84	08 	. 
	defb 038h		;6b85	38 	8 
	defb 040h		;6b86	40 	@ 
	defb 042h		;6b87	42 	B 
	defb 042h		;6b88	42 	B 
	defb 03ch		;6b89	3c 	< 
	defb 000h		;6b8a	00 	. 
	defb 000h		;6b8b	00 	. 
	defb 000h		;6b8c	00 	. 
	defb 082h		;6b8d	82 	. 
	defb 044h		;6b8e	44 	D 
	defb 028h		;6b8f	28 	( 
	defb 010h		;6b90	10 	. 
	defb 0feh		;6b91	fe 	. 
	defb 010h		;6b92	10 	. 
	defb 0feh		;6b93	fe 	. 
	defb 010h		;6b94	10 	. 
	defb 010h		;6b95	10 	. 
	defb 000h		;6b96	00 	. 
	defb 000h		;6b97	00 	. 
	defb 000h		;6b98	00 	. 
	defb 024h		;6b99	24 	$ 
	defb 024h		;6b9a	24 	$ 
	defb 024h		;6b9b	24 	$ 
	defb 07eh		;6b9c	7e 	~ 
	defb 024h		;6b9d	24 	$ 
	defb 07eh		;6b9e	7e 	~ 
	defb 024h		;6b9f	24 	$ 
	defb 024h		;6ba0	24 	$ 
	defb 024h		;6ba1	24 	$ 
	defb 000h		;6ba2	00 	. 
	defb 000h		;6ba3	00 	. 
	defb 000h		;6ba4	00 	. 
	defb 03ch		;6ba5	3c 	< 
	defb 042h		;6ba6	42 	B 
	defb 042h		;6ba7	42 	B 
	defb 044h		;6ba8	44 	D 
	defb 05ch		;6ba9	5c 	\ 
	defb 042h		;6baa	42 	B 
	defb 042h		;6bab	42 	B 
	defb 052h		;6bac	52 	R 
	defb 05ch		;6bad	5c 	\ 
	defb 000h		;6bae	00 	. 
	defb 000h		;6baf	00 	. 
	defb 018h		;6bb0	18 	. 
; font data seems to have ended here but not sure

	defb 024h		;6bb1	24 	$ 
	defb 008h		;6bb2	08 	. 
	defb 024h		;6bb3	24 	$ 
	defb 018h		;6bb4	18 	. 
	defb 000h		;6bb5	00 	. 
	defb 000h		;6bb6	00 	. 
	defb 000h		;6bb7	00 	. 
	defb 000h		;6bb8	00 	. 
	defb 000h		;6bb9	00 	. 
	defb 000h		;6bba	00 	. 
	defb 000h		;6bbb	00 	. 
blanksquare_image:
	defb 000h		;6bbc	00 	. 
	defb 000h		;6bbd	00 	. 
	defb 000h		;6bbe	00 	. 
	defb 000h		;6bbf	00 	. 
	defb 000h		;6bc0	00 	. 
	defb 000h		;6bc1	00 	. 
	defb 000h		;6bc2	00 	. 
	defb 000h		;6bc3	00 	. 
	defb 000h		;6bc4	00 	. 
	defb 000h		;6bc5	00 	. 
	defb 000h		;6bc6	00 	. 
	defb 000h		;6bc7	00 	. 
	defb 000h		;6bc8	00 	. 
	defb 000h		;6bc9	00 	. 
	defb 000h		;6bca	00 	. 
	defb 000h		;6bcb	00 	. 
	defb 000h		;6bcc	00 	. 
	defb 000h		;6bcd	00 	. 
	defb 000h		;6bce	00 	. 
	defb 000h		;6bcf	00 	. 
	defb 000h		;6bd0	00 	. 
	defb 000h		;6bd1	00 	. 
	defb 000h		;6bd2	00 	. 
	defb 000h		;6bd3	00 	. 
	defb 000h		;6bd4	00 	. 
	defb 000h		;6bd5	00 	. 
	defb 000h		;6bd6	00 	. 
	defb 000h		;6bd7	00 	. 
	defb 000h		;6bd8	00 	. 
	defb 000h		;6bd9	00 	. 
	defb 000h		;6bda	00 	. 
	defb 000h		;6bdb	00 	. 

; SQUARES:
; These are the squares that are used to build the tetrominoes.
; There are seven patterns, one for each shape.  This is not the shapes,
; these are the decorative squares used in place of colors.
; Squares are 16x12.  
; This comes out approximately square on the Brother MDA monitor.
; Each is twelve two-byte sequences ending with blank row of two 00's.
SQUARE_PATTERNS:
	defb 0ffh		;6bdc	ff 	. 
	defb 0feh		;6bdd	fe 	. 
	defb 0aah		;6bde	aa 	. 
	defb 0aah		;6bdf	aa 	. 
	defb 0d5h		;6be0	d5 	. 
	defb 056h		;6be1	56 	V 
	defb 0aah		;6be2	aa 	. 
	defb 0aah		;6be3	aa 	. 
	defb 0d5h		;6be4	d5 	. 
	defb 056h		;6be5	56 	V 
	defb 0aah		;6be6	aa 	. 
	defb 0aah		;6be7	aa 	. 
	defb 0d5h		;6be8	d5 	. 
	defb 056h		;6be9	56 	V 
	defb 0aah		;6bea	aa 	. 
	defb 0aah		;6beb	aa 	. 
	defb 0d5h		;6bec	d5 	. 
	defb 056h		;6bed	56 	V 
	defb 0aah		;6bee	aa 	. 
	defb 0aah		;6bef	aa 	. 
	defb 0ffh		;6bf0	ff 	. 
	defb 0feh		;6bf1	fe 	. 
	defb 000h		;6bf2	00 	. 
	defb 000h		;6bf3	00 	. 
	
	defb 0ffh		;6bf4	ff 	. 
	defb 0feh		;6bf5	fe 	. 
	defb 0ffh		;6bf6	ff 	. 
	defb 0feh		;6bf7	fe 	. 
	defb 0c0h		;6bf8	c0 	. 
	defb 006h		;6bf9	06 	. 
	defb 0dfh		;6bfa	df 	. 
	defb 0f6h		;6bfb	f6 	. 
	defb 0d8h		;6bfc	d8 	. 
	defb 036h		;6bfd	36 	6 
	defb 0d8h		;6bfe	d8 	. 
	defb 036h		;6bff	36 	6 
	defb 0d8h		;6c00	d8 	. 
	defb 036h		;6c01	36 	6 
	defb 0dfh		;6c02	df 	. 
	defb 0f6h		;6c03	f6 	. 
	defb 0c0h		;6c04	c0 	. 
	defb 006h		;6c05	06 	. 
	defb 0ffh		;6c06	ff 	. 
	defb 0feh		;6c07	fe 	. 
	defb 0ffh		;6c08	ff 	. 
	defb 0feh		;6c09	fe 	. 
	defb 000h		;6c0a	00 	. 
	defb 000h		;6c0b	00 	. 
	
	defb 0ffh		;6c0c	ff 	. 
	defb 0feh		;6c0d	fe 	. 
	defb 0ffh		;6c0e	ff 	. 
	defb 0feh		;6c0f	fe 	. 
	defb 0e3h		;6c10	e3 	. 
	defb 08eh		;6c11	8e 	. 
	defb 0e3h		;6c12	e3 	. 
	defb 08eh		;6c13	8e 	. 
	defb 0ffh		;6c14	ff 	. 
	defb 0feh		;6c15	fe 	. 
	defb 0ffh		;6c16	ff 	. 
	defb 0feh		;6c17	fe 	. 
	defb 0ffh		;6c18	ff 	. 
	defb 0feh		;6c19	fe 	. 
	defb 0e3h		;6c1a	e3 	. 
	defb 08eh		;6c1b	8e 	. 
	defb 0e3h		;6c1c	e3 	. 
	defb 08eh		;6c1d	8e 	. 
	defb 0ffh		;6c1e	ff 	. 
	defb 0feh		;6c1f	fe 	. 
	defb 0ffh		;6c20	ff 	. 
	defb 0feh		;6c21	fe 	. 
	defb 000h		;6c22	00 	. 
	defb 000h		;6c23	00 	. 
	
	defb 0ffh		;6c24	ff 	. 
	defb 0feh		;6c25	fe 	. 
	defb 0aah		;6c26	aa 	. 
	defb 0aah		;6c27	aa 	. 
	defb 0ffh		;6c28	ff 	. 
	defb 0feh		;6c29	fe 	. 
	defb 0aah		;6c2a	aa 	. 
	defb 0aah		;6c2b	aa 	. 
	defb 0ffh		;6c2c	ff 	. 
	defb 0feh		;6c2d	fe 	. 
	defb 0aah		;6c2e	aa 	. 
	defb 0aah		;6c2f	aa 	. 
	defb 0ffh		;6c30	ff 	. 
	defb 0feh		;6c31	fe 	. 
	defb 0aah		;6c32	aa 	. 
	defb 0aah		;6c33	aa 	. 
	defb 0ffh		;6c34	ff 	. 
	defb 0feh		;6c35	fe 	. 
	defb 0aah		;6c36	aa 	. 
	defb 0aah		;6c37	aa 	. 
	defb 0ffh		;6c38	ff 	. 
	defb 0feh		;6c39	fe 	. 
	defb 000h		;6c3a	00 	. 
	defb 000h		;6c3b	00 	. 
	
	defb 0ffh		;6c3c	ff 	. 
	defb 0feh		;6c3d	fe 	. 
	defb 0feh		;6c3e	fe 	. 
	defb 0feh		;6c3f	fe 	. 
	defb 0feh		;6c40	fe 	. 
	defb 0feh		;6c41	fe 	. 
	defb 0feh		;6c42	fe 	. 
	defb 0feh		;6c43	fe 	. 
	defb 0feh		;6c44	fe 	. 
	defb 0feh		;6c45	fe 	. 
	defb 0c0h		;6c46	c0 	. 
	defb 006h		;6c47	06 	. 
	defb 0feh		;6c48	fe 	. 
	defb 0feh		;6c49	fe 	. 
	defb 0feh		;6c4a	fe 	. 
	defb 0feh		;6c4b	fe 	. 
	defb 0feh		;6c4c	fe 	. 
	defb 0feh		;6c4d	fe 	. 
	defb 0feh		;6c4e	fe 	. 
	defb 0feh		;6c4f	fe 	. 
	defb 0ffh		;6c50	ff 	. 
	defb 0feh		;6c51	fe 	. 
	defb 000h		;6c52	00 	. 
	defb 000h		;6c53	00 	. 
	
	defb 0ffh		;6c54	ff 	. 
	defb 0feh		;6c55	fe 	. 
	defb 0feh		;6c56	fe 	. 
	defb 0feh		;6c57	fe 	. 
	defb 0f9h		;6c58	f9 	. 
	defb 03eh		;6c59	3e 	> 
	defb 0e3h		;6c5a	e3 	. 
	defb 08eh		;6c5b	8e 	. 
	defb 0c7h		;6c5c	c7 	. 
	defb 0c6h		;6c5d	c6 	. 
	defb 08fh		;6c5e	8f 	. 
	defb 0e2h		;6c5f	e2 	. 
	defb 0c7h		;6c60	c7 	. 
	defb 0c6h		;6c61	c6 	. 
	defb 0e3h		;6c62	e3 	. 
	defb 08eh		;6c63	8e 	. 
	defb 0f9h		;6c64	f9 	. 
	defb 03eh		;6c65	3e 	> 
	defb 0fch		;6c66	fc 	. 
	defb 07eh		;6c67	7e 	~ 
	defb 0ffh		;6c68	ff 	. 
	defb 0feh		;6c69	fe 	. 
	defb 000h		;6c6a	00 	. 
	defb 000h		;6c6b	00 	. 
	
	defb 0ffh		;6c6c	ff 	. 
	defb 0feh		;6c6d	fe 	. 
	defb 0ffh		;6c6e	ff 	. 
	defb 0feh		;6c6f	fe 	. 
	defb 0c1h		;6c70	c1 	. 
	defb 006h		;6c71	06 	. 
	defb 0cfh		;6c72	cf 	. 
	defb 0e6h		;6c73	e6 	. 
	defb 0cch		;6c74	cc 	. 
	defb 066h		;6c75	66 	f 
	defb 0fch		;6c76	fc 	. 
	defb 07eh		;6c77	7e 	~ 
	defb 0cch		;6c78	cc 	. 
	defb 066h		;6c79	66 	f 
	defb 0cfh		;6c7a	cf 	. 
	defb 0e6h		;6c7b	e6 	. 
	defb 0c1h		;6c7c	c1 	. 
	defb 006h		;6c7d	06 	. 
	defb 0ffh		;6c7e	ff 	. 
	defb 0feh		;6c7f	fe 	. 
	defb 0ffh		;6c80	ff 	. 
	defb 0feh		;6c81	fe 	. 
	defb 000h		;6c82	00 	. 
	defb 000h		;6c83	00 	. 

TETROMINO_SHAPES:
	defb 011h		;6c84	11 	. ;Square, four orientations.
	defb 021h		;6c85	21 	! 
	defb 012h		;6c86	12 	. 
	defb 022h		;6c87	22 	" 
	defb 011h		;6c88	11 	. 
	defb 021h		;6c89	21 	! 
	defb 012h		;6c8a	12 	. 
	defb 022h		;6c8b	22 	" 
	defb 011h		;6c8c	11 	. 
	defb 021h		;6c8d	21 	! 
	defb 012h		;6c8e	12 	. 
	defb 022h		;6c8f	22 	" 
	defb 011h		;6c90	11 	. 
	defb 021h		;6c91	21 	! 
	defb 012h		;6c92	12 	. 
	defb 022h		;6c93	22 	" 
	defb 010h		;6c94	10 	. ;Long bar, four orientations.
	defb 011h		;6c95	11 	. 
	defb 012h		;6c96	12 	. 
	defb 013h		;6c97	13 	. 
	defb 002h		;6c98	02 	. 
	defb 012h		;6c99	12 	. 
	defb 022h		;6c9a	22 	" 
	defb 032h		;6c9b	32 	2 
	defb 010h		;6c9c	10 	. 
	defb 011h		;6c9d	11 	. 
	defb 012h		;6c9e	12 	. 
	defb 013h		;6c9f	13 	. 
	defb 002h		;6ca0	02 	. 
	defb 012h		;6ca1	12 	. 
	defb 022h		;6ca2	22 	" 
	defb 032h		;6ca3	32 	2 
	defb 001h		;6ca4	01 	. ;S, four orientations.
	defb 011h		;6ca5	11 	. 
	defb 012h		;6ca6	12 	. 
	defb 022h		;6ca7	22 	" 
	defb 020h		;6ca8	20 	  
	defb 021h		;6ca9	21 	! 
	defb 011h		;6caa	11 	. 
	defb 012h		;6cab	12 	. 
	defb 001h		;6cac	01 	. 
	defb 011h		;6cad	11 	. 
	defb 012h		;6cae	12 	. 
	defb 022h		;6caf	22 	" 
	defb 020h		;6cb0	20 	  
	defb 021h		;6cb1	21 	! 
	defb 011h		;6cb2	11 	. 
	defb 012h		;6cb3	12 	. 
	defb 002h		;6cb4	02 	. ;Z, four orientations
	defb 012h		;6cb5	12 	. 
	defb 011h		;6cb6	11 	. 
	defb 021h		;6cb7	21 	! 
	defb 010h		;6cb8	10 	. 
	defb 011h		;6cb9	11 	. 
	defb 021h		;6cba	21 	! 
	defb 022h		;6cbb	22 	" 
	defb 002h		;6cbc	02 	. 
	defb 012h		;6cbd	12 	. 
	defb 011h		;6cbe	11 	. 
	defb 021h		;6cbf	21 	! 
	defb 010h		;6cc0	10 	. 
	defb 011h		;6cc1	11 	. 
	defb 021h		;6cc2	21 	! 
	defb 022h		;6cc3	22 	" 
	defb 000h		;6cc4	00 	. L SHAPE
	defb 001h		;6cc5	01 	. 
	defb 011h		;6cc6	11 	. 
	defb 021h		;6cc7	21 	! 
	defb 010h		;6cc8	10 	. 
	defb 011h		;6cc9	11 	. 
	defb 012h		;6cca	12 	. 
	defb 002h		;6ccb	02 	. 
	defb 001h		;6ccc	01 	. 
	defb 011h		;6ccd	11 	. 
	defb 021h		;6cce	21 	! 
	defb 022h		;6ccf	22 	" 
	defb 010h		;6cd0	10 	. 
	defb 011h		;6cd1	11 	. 
	defb 012h		;6cd2	12 	. 
	defb 020h		;6cd3	20 	  
	defb 001h		;6cd4	01 	. J SHAPE
	defb 011h		;6cd5	11 	. 
	defb 021h		;6cd6	21 	! 
	defb 002h		;6cd7	02 	. 
	defb 010h		;6cd8	10 	. 
	defb 011h		;6cd9	11 	. 
	defb 012h		;6cda	12 	. 
	defb 022h		;6cdb	22 	" 
	defb 001h		;6cdc	01 	. 
	defb 011h		;6cdd	11 	. 
	defb 021h		;6cde	21 	! 
	defb 020h		;6cdf	20 	  
	defb 000h		;6ce0	00 	. 
	defb 010h		;6ce1	10 	. 
	defb 011h		;6ce2	11 	. 
	defb 012h		;6ce3	12 	. 
	defb 001h		;6ce4	01 	. T SHAPE
	defb 011h		;6ce5	11 	. 
	defb 021h		;6ce6	21 	! 
	defb 010h		;6ce7	10 	. 
	defb 010h		;6ce8	10 	. 
	defb 011h		;6ce9	11 	. 
	defb 012h		;6cea	12 	. 
	defb 001h		;6ceb	01 	. 
	defb 001h		;6cec	01 	. 
	defb 011h		;6ced	11 	. 
	defb 021h		;6cee	21 	! 
	defb 012h		;6cef	12 	. 
	defb 010h		;6cf0	10 	. 
	defb 011h		;6cf1	11 	. 
	defb 012h		;6cf2	12 	. 
	defb 021h		;6cf3	21 	! 
; That's the end of the tetromino brick shape data!

; BLOCK 'palace' (start 0x6cf4 end 0x7750)
palace_start:
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,2,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,2,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,2
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,2,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,2,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,7,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0fh
	defb	80h,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,13h,0c0h
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	27h,0e0h,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,27h,0e0h,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,27h,0e0h,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,27h,0e0h
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	23h,0e0h,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,11h,0c0h,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,9,80h,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,9,80h
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	13h,0c0h,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,2fh,0e0h,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,2ah,0a0h,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,4ah,90h
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	4ah,90h,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,52h,50h,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	9ah,0e8h,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,80h
	defb	0,0,92h,48h,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,80h,0,0,0bah,68h,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,80h,0,1,13h,54h
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,80h,0,1
	defb	0bah,24h,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0
	defb	0,0,0,80h,0,1,33h,74h
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,80h,0,1
	defb	0aah,0aah,0,0,0,1,80h,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,80h
	defb	0,3,73h,32h,0,0,0,1
	defb	80h,0,0,0,0,0,0,0
	defb	0,0,0,2,0,0,0,0
	defb	0,80h,0,2,2ah,0aah,0,0
	defb	0,1,80h,0,0,0,0,0
	defb	0,0,0,0,0,2,0,0
	defb	0,0,0,80h,0,3,73h,15h
	defb	0,0,0,1,80h,0,0,0
	defb	0,0,0,0,0,0,0,2
	defb	0,0,0,0,0,80h,0,6
	defb	2ah,0b9h,0,0,0,1,80h,0
	defb	0,0,0,0,0,0,0,0
	defb	0,2,0,0,0,0,0,80h
	defb	0,5,53h,55h,0,0,0,1
	defb	80h,0,0,0,0,0,0,0
	defb	0,0,0,2,0,0,0,0
	defb	0,80h,0,6,0eah,9ah,80h,0
	defb	0,1,80h,0,0,0,0,0
	defb	0,0,0,0,0,2,0,0
	defb	0,0,1,0c0h,0,0ch,53h,5ch
	defb	80h,0,0,1,80h,0,0,0
	defb	0,0,0,0,0,0,0,2
	defb	0,0,0,0,2,20h,0,0ah
	defb	0eah,8ah,0c0h,0,0,1,80h,0
	defb	0,0,0,0,0,0,0,0
	defb	0,2,0,0,0,0,6,30h
	defb	0,0ch,53h,4dh,40h,0,0,1
	defb	80h,0,0,0,0,0,0,0
	defb	0,0,0,2,0,0,0,0
	defb	3,0e0h,0,1ah,0aah,0aah,40h,0
	defb	0,1,80h,0,0,0,0,0
	defb	0,0,0,0,0,7,0,0
	defb	0,0,3,0e0h,0,15h,0d3h,4dh
	defb	20h,0,0,1,80h,0,0,0
	defb	0,0,0,0,0,0,0,7
	defb	0,0,0,0,3,0e0h,0,18h
	defb	0aah,0aeh,0a0h,0,0,3,0c0h,0
	defb	0,0,0,0,0,0,0,0
	defb	0,7,0,0,0,0,6,30h
	defb	0,35h,0d3h,45h,20h,0,0,2
	defb	40h,0,0,0,0,0,0,0
	defb	0,0,0,0fh,80h,0,0,0
	defb	6,30h,0,28h,0aah,0a6h,90h,0
	defb	0,2,40h,0,0,0,0,0
	defb	0,0,0,0,0,8,80h,0
	defb	0,0,0fh,0f0h,0,35h,0d3h,55h
	defb	50h,0,0,7,0e0h,0,0,0
	defb	0,0,0,0,0,0,0,8
	defb	80h,0,0,0,18h,18h,0,29h
	defb	0aah,0a6h,90h,0,0,4,20h,0
	defb	0,0,0,0,0,0,20h,0
	defb	0,3fh,0e0h,0,0,0,7fh,0fch
	defb	0,51h,53h,57h,48h,0,0,0fh
	defb	0f0h,0,0,0,0,0,0,0
	defb	20h,0,0,0eah,0b8h,0,0,0
	defb	0e8h,9eh,0,6bh,0aah,0a2h,0a8h,0
	defb	0,18h,18h,0,0,0,0,0
	defb	0,0,20h,0,1,55h,54h,0
	defb	0,1,98h,8bh,0,51h,53h,53h
	defb	48h,0,0,30h,0ch,0,0,0
	defb	0,0,0,0,20h,0,2,0aah
	defb	0aah,0,0,7,30h,8dh,80h,0abh
	defb	0aah,0aah,0a4h,0,0,0e0h,7,0
	defb	0,0,0,0,0,0,20h,0
	defb	5,55h,55h,0,0,0ch,20h,86h
	defb	0c0h,0d1h,53h,53h,54h,0,1,0ffh
	defb	0ffh,80h,0,0,0,0,0,0
	defb	20h,0,0ah,0aah,0aah,80h,0,38h
	defb	60h,83h,70h,0a2h,0aah,0abh,0a4h,0
	defb	1,0ffh,0ffh,80h,0,0,0,0
	defb	0,0,20h,0,15h,55h,55h,40h
	defb	1,0e0h,0c0h,83h,39h,57h,53h,55h
	defb	52h,0,3,0,0,0c0h,0,0
	defb	0,0,0,0,20h,0,2ah,0aah
	defb	0aah,0a0h,7,83h,80h,81h,99h,2
	defb	2,1,82h,0,6,0,0,60h
	defb	0,0,0,0,0,0,20h,0
	defb	35h,55h,55h,60h,3ch,7,1,0c0h
	defb	0cdh,0ffh,0ffh,0fdh,0fdh,0,7,0ffh
	defb	0ffh,0f0h,0,0,0,0,0,0
	defb	70h,0,3fh,0ffh,0ffh,0e0h,0f4h,5
	defb	1,40h,79h,0ffh,0ffh,0ffh,0ffh,0
	defb	7ch,42h,11h,3bh,80h,0,0,0
	defb	0,0,50h,0,75h,55h,55h,71h
	defb	0ffh,0ffh,0ffh,0ffh,0fdh,14h,0ah,2
	defb	8dh,0,58h,42h,11h,1eh,80h,0
	defb	0,0,0,0,0f8h,0,7fh,0ffh
	defb	0ffh,0e7h,24h,40h,80h,80h,41h,49h
	defb	0e4h,0f9h,35h,0,7fh,0ffh,0ffh,0ffh
	defb	80h,0,0,0,0,1,4,0
	defb	7fh,0ffh,0ffh,0dfh,0ffh,0ffh,0ffh,0ffh
	defb	0f9h,2ah,15h,5,55h,0eh,70h,0
	defb	0,3,0c7h,0,0,0,0,1
	defb	0fch,0,6ah,0aah,0aah,0b9h,0c3h,86h
	defb	2,3,5,2ah,15h,5,55h,0ah
	defb	0ffh,0ffh,0ffh,0ffh,0f5h,0,0,0
	defb	0,1,24h,0,55h,55h,55h,73h
	defb	0c7h,8fh,7,3,85h,22h,11h,4
	defb	55h,0fh,83h,83h,6,30h,1fh,0
	defb	0,0,0,1,24h,0,7fh,0ffh
	defb	0feh,0e6h,0cdh,9bh,0dh,87h,8dh,7eh
	defb	3fh,8fh,0ddh,0eh,0eh,0eh,0ch,38h
	defb	7,0eh,0,0,0,1,24h,3
	defb	0d5h,55h,55h,0e6h,0d9h,0b1h,98h,0cch
	defb	0ddh,0c1h,40h,50h,3dh,0bch,1ch,3ch
	defb	1ch,3eh,3
  defb	0cah,0,0,0,7,0ffh,1,0ffh
	defb	0ffh,0fdh,0ach,0f1h,0e1h,0f0h,0f8h,7dh
	defb	9ch,9fh,27h,95h,0f0h,38h,78h,3ch
	defb	1fh,0,0feh,0,0,0,0dh,55h
	defb	81h,95h,55h,55h,0a8h,0f1h,0c0h,0e0h
	defb	70h,74h,0b6h,0b1h,0ach,0d5h,0c0h,70h
	defb	0f0h,3ch,0fh,80h,38h,0,0,0
	defb	1fh,0ffh,0c0h,0ffh,0ffh,0fdh,0b8h,0e1h
	defb	80h,0c0h,30h,30h,0a2h,0a0h,0a8h,55h
	defb	81h,0e1h,0f0h,3ch,7,0e0h,18h,0
	defb	0,0,35h,55h,60h,0d4h,14h,11h
	defb	0ffh,0ffh,0ffh,0ffh,0ffh,0fch,0a2h,0a0h
	defb	0a8h,55h,3,0c1h,0f0h,3eh,3,0f8h
	defb	0dh,0c0h,0,0,6ah,0aah,0b0h,0a2h
	defb	22h,29h,0a9h,10h,81h,0,40h,8
	defb	22h,20h,88h,45h,7
	defb	0c3h,0f0h,3fh,1,0feh,7,40h,0
	defb	0,0d5h,55h,58h,0c1h,41h,45h,0a9h
	defb	10h,81h,0,40h,0dh,0a3h,0e0h,0f8h
	defb	7dh,0fh,83h,0f0h,3fh,80h,7fh,87h
	defb	0c0h,0,1,0aah,0aah,0ach,0ffh,0ffh
	defb	0fdh,0ffh,0ffh,0fdh,0ffh,0ffh,0fch,64h
	defb	10h,84h,42h,1fh,83h,0f0h,1fh,0c0h
	defb	3fh,0c3h,0,0,3,55h,55h,56h
	defb	49h,10h,88h,0ffh,0ffh,0fdh,0ffh,0ffh
	defb	0f9h,29h,0cah,0f2h,9ah,1fh,83h,0f0h
	defb	0fh,0e0h,0fh,0e1h,80h,0,6,0aah
	defb	0aah,0abh,49h,10h,89h,7fh,0ffh,0fdh
	defb	0ffh,0ffh,0fah,92h,24h,89h,0a5h,1fh
	defb	83h,0f8h,7,0f8h,3,0f1h,80h,0
	defb	0dh,55h,55h,55h,89h,10h,89h,18h
	defb	0,1,0,0,2,54h,14h,85h
	defb	43h,1fh,83h,0fch,3,0fch,0,0f1h
	defb	0f0h,0,1fh,0ffh,0ffh,0ffh,0dfh,0ffh
	defb	0ffh,0efh,0ffh,0fdh,0ffh,0ffh,0fah,54h
	defb	14h,85h,43h,1fh,0c1h,0feh,1,0ffh
	defb	0,79h,0d0h,0,35h,55h,55h,55h
	defb	67h,0ffh,0ffh,0f7h,55h,55h,55h,55h
	defb	52h,54h,14h,85h,43h,1fh,0c0h,0ffh
	defb	0,0ffh,0c0h,39h,0f0h,0,7fh,0ffh
	defb	0ffh,0ffh,0f8h,10h,89h,29h,0aah,0a9h
	defb	0aah,0aah,0aah,44h,10h,84h,43h,0fh
	defb	0e0h,7fh,80h,7fh,0e0h,39h,0c0h,1
	defb	0c0h,0e0h,30h,18h,1dh,0ffh,0ffh,0fch
	defb	0ffh,0fdh,0ffh,0ffh,0fbh,0ffh,0feh,0ffh
	defb	0ffh,7,0e0h,3fh,0c0h,1fh,0f0h,1bh
	defb	0c0h,3,81h,0c0h,0f0h,18h,1eh,10h
	defb	89h,24h,40h,1,0,0,0,0
	defb	0,80h,0,83h,0f0h,0fh,0e0h,7
	defb	0f8h,1bh,0c0h,7,7,3,0e0h,38h
	defb	1fh,3fh,0ffh,0fch,3fh,0fdh,0ffh,0ffh
	defb	0fbh,0ffh,0feh,0ffh,0feh,0c1h,0f8h,7
	defb	0f0h,1,0f8h,1fh,0e0h,0eh,1eh,0fh
	defb	0c0h,78h,1bh,9fh,0ffh,0feh,18h,1
	defb	0,0,2,0efh,0beh,0bfh,7ah,0e0h
	defb	0feh,1,0fch,0,7ch,1fh,0a0h,0eh
	defb	78h,1fh,80h,0f8h,19h,0dfh,0ffh,0feh
	defb	0bh,0bdh,0bfh,0bfh,0b8h,0,0,80h
	defb	0,0f0h,3fh,0,7eh,0,3ch,1fh
	defb	0e0h,1dh,0f0h,7fh,3,0f0h,18h,0e0h
	defb	0,1,6,0a9h,0aah,0aah,0aah,0adh
	defb	0bah,0b7h,5bh,78h,0fh,0c0h,1fh,0
	defb	1eh,1ch,0,1fh,0e0h,0fch,0fh,0e0h
	defb	38h,0efh,0f7h,0ffh,83h,0bdh,0bfh,0bfh
	defb	0b6h,0adh,0bah
	defb	0b7h,5bh,7ch,3,0e0h,7,0c0h,0eh
	defb	18h,0,1fh,0c1h,0f8h,1fh,0c0h,78h
	defb	0edh,0e3h,0deh,83h,0f9h,7fh,0ffh,0f6h
	defb	0adh,0bah,0b7h,5bh,1fh,0,0f8h,3
	defb	0e0h,0eh,3eh,0,1fh,83h,0f0h,3fh
	defb	80h,0f8h,0edh,0c1h,0dch,82h,7,0
	defb	0,6,0adh,0b8h,37h,5bh,0fh,80h
	defb	3ch,0,0f0h,6,6ah,0,1fh,7
	defb	0e0h,7fh,1,0f8h,0edh,88h,0d8h,83h
	defb	9dh,8fh,0ffh,0f6h,0adh,0a3h,97h,5bh
	defb	3,0c0h,1fh,0,78h,7,0ceh,0
	defb	1fh,7,0c0h,0feh,3,0f0h,0e7h,1ch
	defb	71h,83h,72h,65h,55h,46h,0adh,0cfh
	defb	0e7h,5bh,0,0e0h,0fh,80h,3ch,7
	defb	0c0h,0,0fh,7,0c1h,0fch,7,0e0h
	defb	0c2h,36h,22h,80h,0cfh
	defb	32h,0aah,0a4h,0c5h,19h,33h,43h,80h
	defb	70h,7,0c0h,1eh,7,0e0h,0,7
	defb	7,0c1h,0f8h,0fh,0c1h,0d0h,77h,6
	defb	0c3h,0bfh,0dch,55h,6,0b6h,67h,0cch
	defb	94h,1fh,0ffh,0ffh,0ffh,0ffh,0ffh,0a0h
	defb	0,7,83h,0c1h,0f0h,1fh,83h,98h
	defb	0ffh,8fh,0ceh,7fh,0e7h,2ah,0aah,0b8h
	defb	9eh,0f6h,29h,61h,4,8,10h,44h
	defb	4ch,0e0h,0,3,0c3h,0c1h,0e0h,3eh
	defb	7,0,0,0,19h,0e0h,0f9h,95h
	defb	29h,63h,78h,39h,92h,0a1h,4,8
	defb	10h,44h,4eh,0,0,1,0e1h,0c0h
	defb	0e0h,78h,0eh,0ffh,0ffh,0feh,77h,0c4h
	defb	5ch,0eah,0a9h,0cch,0e0h,0eh,0e0h,0afh
	defb	0ffh,0ffh,0ffh,0ffh,0fah,0,0,0
	defb	0f0h,0e0h,60h,70h,1ch,78h,3,0f9h
	defb	0ceh,4eh,4fh,31h,29h,13h,0c4h,47h
	defb	32h,0afh,0ffh,0ffh,0ffh,0ffh,0feh,0
	defb	0,0,38h,30h,20h,60h,38h,0bch
	defb	0e7h,0b3h,38h,44h,43h,9ch,0aeh,6fh
	defb	2,81h,0cch,2ah,0aah,0aah,0aah,0adh
	defb	0b0h,0,0,0,1fh,0ffh,0ffh,0ffh
	defb	0f1h,0bch,0e7h,8ch,0f0h,20h,81h,0e6h
	defb	38h,9ch,1,0,76h,4dh,0d3h,53h
	defb	53h,55h,70h,0,0,0,7,0ffh
	defb	0ffh,0ffh,0e3h,9eh,4fh,39h,0ffh,0ffh
	defb	0ffh,0fbh,0a2h,73h,0ffh,0ffh,39h,82h
	defb	0aah,0aah,0aah,0adh,0b0h,0,0,0
	defb	3,0,0,1,87h,1eh,0eh,67h
	defb	0,0,0,1ch,0c9h,0e0h,0,0
	defb	0eh,41h,0d3h,53h,53h,55h,70h,0
	defb	0,0,3,0ffh,0ffh,0ffh,87h,0fh
	defb	19h,9eh,7fh,0ffh,0ffh,0cfh,73h,9fh
	defb	0ffh,0ffh,0f3h,0b1h,0aah,0aah,0aah,0adh
	defb	0b0h,0,0,0,3,48h,82h,25h
	defb	0eeh,0fh,17h,39h,0e4h,44h,44h,0f3h
	defb	98h,8,8,4,0,0cch,53h,53h
	defb	53h,55h,70h,0,0,0,3,48h
	defb	82h,25h,0ech,47h,8ch,0e7h,84h,44h
	defb	44h,3dh,0eeh,8,8,4,2,36h
	defb	2ah,0aah,0aah,0adh,0b0h,0,0,0
	defb	3,0ffh,0ffh,0ffh,0dch,0e7h,33h,0ceh
	defb	4,44h,44h,1eh,73h,8,8,4
	defb	2,19h,83h,53h,53h,55h,70h,0
	defb	0,0,3,0fbh,0fbh,0fbh,0b8h,0e2h
	defb	0efh,38h,4,44h,44h,0fh,9dh,0c8h
	defb	8,4,2,0eh,62h,0aah,0aah,0adh
	defb	0b0h,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0
	

; BLOCK 'bigfont' (start 0x7784 end 0x7c09)
BIGFONT_DATA:
  defb	0,0,0,0,0,0,0fh,0ffh
	defb	0e0h,1fh,0ffh,0f0h,1fh,0ffh,0f0h,1ch
	defb	0,70h,1ch,0,70h,1ch,0,70h
	defb	1ch,0,70h,1ch,0,70h,1ch,0
	defb	70h,0ch,0,60h,1ch,0,70h,1ch
	defb	0,70h,1ch,0,70h,1ch,0,70h
	defb	1ch,0,70h,1ch,0,70h,1fh,0ffh
	defb	0f0h,1fh,0ffh,0f0h,0fh,0ffh,0e0h,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0,0,38h
	defb	0,0,38h,0,0,38h,0,0
	defb	38h,0,0,38h,0,0,38h,0
	defb	0,38h,0,0,38h,0,0,38h
	defb	0,0,38h
	defb	0,0,38h,0,0,38h,0,0
	defb	38h,0,0,38h,0,0,38h,0
	defb	0,38h,0,0,38h,0,0,38h
	defb	0,0,38h,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0fh,0ffh,0e0h,1fh,0ffh
	defb	0f0h,1fh,0ffh,0f0h,0,0,70h,0
	defb	0,70h,0,0,70h,0,0,70h
	defb	0,0,70h,0fh,0ffh,0f0h,1fh,0ffh
	defb	0f0h,1fh,0ffh,0e0h,1ch,0,0,1ch
	defb	0,0,1ch,0,0,1ch,0,0
	defb	1ch,0,0,1fh,0ffh,0f0h,1fh,0ffh
	defb	0f0h,0fh,0ffh,0f0h,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0fh,0ffh,0e0h,1fh,0ffh
	defb	0f0h,1fh,0ffh,0f0h,0,0,70h,0
	defb	0
  defb	70h,0,0,70h,0
  defb	0,70h,0,0,70h,7,0ffh,0f0h
	defb	7,0ffh,0e0h,7,0ffh,0f0h,0,0
	defb	70h,0,0,70h,0,0,70h,0
	defb	0,70h,0,0,70h,1fh,0ffh,0f0h
	defb	1fh,0ffh,0f0h,0fh,0ffh,0e0h,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,1ch,0,70h
	defb	1ch,0,70h,1ch,0,70h,1ch,0
	defb	70h,1ch,0,70h,1ch,0,70h,1ch
	defb	0,70h,1ch,0,70h,1fh,0ffh,0f0h
	defb	1fh,0ffh,0e0h,0fh,0ffh,0f0h,0,0
	defb	70h,0,0,70h,0,0,70h,0
	defb	0,70h,0,0,70h,0,0,70h
	defb	0,0,70h,0,0,70h,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0fh,0ffh,0e0h
	defb	1fh,0ffh,0f0h,1fh,0ffh,0f0h,1ch,0
	defb	0,1ch,0,0,1ch,0,0,1ch
	defb	0,0,1ch,0,0,1fh,0ffh,0e0h
	defb	1fh,0ffh,0f0h,0fh,0ffh,0f0h,0,0
	defb	70h,0,0,70h,0,0,70h,0
	defb	0,70h,0,0,70h,1fh,0ffh,0f0h
	defb	1fh,0ffh,0f0h,0fh,0ffh,0e0h,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0fh,0ffh,0e0h
	defb	1fh,0ffh,0f0h,1fh,0ffh,0f0h,1ch,0
	defb	0,1ch,0,0,1ch,0,0,1ch
	defb	0,0,1ch,0,0,1fh,0ffh,0e0h
	defb	0fh,0ffh,0f0h,1fh,0ffh,0f0h,1ch,0
	defb	70h,1ch,0,70h,1ch,0,70h,1ch
	defb	0,70h,1ch,0,70h,1fh,0ffh,0f0h
	defb	1fh,0ffh,0f0h,0fh,0ffh,0e0h,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0fh,0ffh,0e0h
	defb	1fh,0ffh,0f0h,1fh,0ffh,0f0h,0,0
	defb	70h,0,0,70h,0,0,70h,0
	defb	0,70h,0,0,70h,0,0,70h
	defb	0,0,60h,0,0,70h,0,0
	defb	70h,0,0,70h,0,0,70h,0
	defb	0,70h,0,0,70h,0,0,70h
	defb	0,0,70h,0,0,60h,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0,0,0fh,0ffh,0e0h
	defb	1fh,0ffh,0f0h,1fh,0ffh,0f0h,1ch,0
	defb	70h,1ch,0,70h,1ch,0,70h,1ch
	defb	0,70h,1ch,0,70h,1fh,0ffh,0f0h
	defb	0fh,0ffh,0e0h,1fh,0ffh,0f0h,1ch,0
	defb	70h,1ch
	defb	0,70h,1ch,0,70h,1ch,0,70h
	defb	1ch,0,70h,1fh,0ffh,0f0h,1fh,0ffh
	defb	0f0h,0fh,0ffh,0e0h,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0fh,0ffh,0e0h,1fh,0ffh
	defb	0f0h,1fh,0ffh,0f0h,1ch,0,70h,1ch
	defb	0,70h,1ch,0,70h,1ch,0,70h
	defb	1ch,0,70h,1fh,0ffh,0f0h,1fh,0ffh
	defb	0e0h,0fh,0ffh,0f0h,0,0,70h,0
	defb	0,70h,0,0,70h,0,0,70h
	defb	0,0,70h,1fh,0ffh,0f0h,1fh,0ffh
	defb	0f0h,0fh,0ffh,0e0h,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0fh,0ffh,0e0h,1fh,0ffh
	defb	0f0h,1fh,0ffh,0f0h,1ch,0,70h,1ch
	defb	0,70h,1ch,0,70h,1ch,0,70h
	defb	1ch,0,70h,1ch,0,70h,0ch,0
	defb	60h,1ch,0,70h,1ch,0,70h,1ch
	defb	0,70h,1ch,0,70h,1ch,0,70h
	defb	1ch,0,70h,1fh,0ffh,0f0h,1fh,0ffh
	defb	0f0h,0fh,0ffh,0e0h,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,1ch,0,70h,1ch,0
	defb	70h,1ch,0,70h,1ch,0,70h,1ch
	defb	0,70h,1ch,0,70h,1ch,0,70h
	defb	1ch,0,70h,1fh,0ffh,0f0h,1fh,0ffh
	defb	0e0h,0fh,0ffh,0f0h,0,0,70h,0
	defb	0,70h,0,0,70h,0,0,70h
	defb	0,0,70h,0,0,70h,0,0
	defb	70h,0,0,70h,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0fh,0ffh,0e0h,1fh,0ffh
	defb	0f0h,1fh,0ffh,0f0h,0,0,70h,0
	defb	0,70h,0,0,70h,0,0,70h
	defb	0,0,70h,0,0,70h,0,0
	defb	60h,0,0,70h,0,0,70h,0
	defb	0,70h,0,0,70h,0,0,70h
	defb	0,0,70h,0,0,70h,0,0
	defb	70h,0,0,60h,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,1ch,3fh,0e0h,1ch,7fh
	defb	0f0h,1ch,7fh,0f0h,1ch,70h,70h,1ch
	defb	70h,70h,1ch,70h,70h,1ch,70h,70h
	defb	1ch,70h,70h,1ch,70h,70h,1ch,30h
	defb	60h,1ch,70h,70h,1ch,70h,70h,1ch
	defb	70h,70h,1ch,70h,70h,1ch,70h,70h
	defb	1ch,70h,70h,1ch,7fh,0f0h,1ch,7fh
	defb	0f0h,1ch,3fh,0e0h,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0
  defb	0,0,1ch,3fh,0e0h,1ch,7fh,0f0h
	defb	1ch,7fh,0f0h,1ch,0,70h,1ch,0
	defb	70h,1ch,0,70h,1ch,0,70h,1ch
	defb	0,70h,1ch,1fh,0f0h,1ch,1fh,0e0h
	defb	1ch,1fh,0f0h,1ch,0,70h,1ch,0
	defb	70h,1ch,0,70h,1ch,0,70h,1ch
	defb	0,70h,1ch,7fh,0f0h,1ch,7fh,0f0h
	defb	1ch,3fh,0e0h,0,0,0,0,0
	defb	0,0,0,0,0,0,0,0
	defb	0,0,0,0ch,18h,0,18h,30h
	defb	0,30h,60h,0,60h,0c0h,0,0c1h
	defb	80h,1,83h,0,3,6,0,6
	defb	0ch,0,0ch,18h,0,18h,30h,0
	defb	0ch,18h,0,6,0ch,0,3,6
	defb	0,1,83h,0,0,0c1h,80h
	defb	0,60h,0c0h,0,30h,60h,0,18h
	defb	30h,0,0ch,18h,0,0,0,0
	defb	0,0,0,0,0
Height_Options_List:
	defb	0,4,7,10,13
RULER2str:
	defb	"*TETRIS  P:1  L:1   COL:1   PITCH:10 L.SP:1.0 KB:I",0
RULERstr:
  defb	" 0....:....L....:....2....:....3....:....4....:....5....:....6....:...",11h
	defb	"7....R....8....:....",0
RULER3str:
	defb 005h		;7c98	05 	. 
	defb 000h		;7c99	00 	. 
COPYRIGHT1:
  defb	"]^1987 V/O Electronorgtechnica (Elorg). All Rights",0
COPYRIGHT2:
  defb	"Reserved.  TETRIS is a trademark of Elorg.",0
COPYRIGHT3:
  defb	"TETRIS copyright and trademark licensed to Sphere, Inc.",0
COPYRIGHT4:
  defb	"and sublicensed to Brother Industries, Ltd.",0
COPYRIGHT5:
  defb	"Original concept by Alexey Pazhitnov.",0
VERSIONstr:
  defb	"Brother version ]^1991 Brother Industries, Ltd.",0
HISCOREstr:
  defb	"HI-SCORE : ",0
SCOREstr:
  defb	"SCORE : ",0
LEVELstr:
  defb	"LEVEL : ",0
LINESstr:
  defb	"LINES : ",0
GAMEOVER_string:
  defb	"GAME OVER",0
PlayAgain:
  defb	"Do you want to play again ?    ",0
PressRETURNorCANCEL:
  defb	"Press RETURN(yes) or CANCEL(no).",0
BlankLine_31ch:
  defb	"                               ",0
NEXTBOXstr:
	defb 0ah,0fh,0fh,"NEXT",0fh,0fh,0bh,0
NEXTBOXSIDELINESstr:
	defb	0eh,"        ",0eh,0
NEXTBOXBOTTOMLINEstr:
	defb	0ch,0fh,0fh,0fh,0fh,0fh,0fh,0fh,0fh,0dh,0
HIGHSCORE1:
	defb 000h		;7e65	00 	. 
	defb 000h		;7e66	00 	. 
HIGHSCORE2:
	defb 000h		;7e67	00 	. 
