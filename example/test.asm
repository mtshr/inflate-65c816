.INCLUDE "./test.inc"
.INCLUDE "../src/inflate.inc"

.BANK 0 SLOT 0
.SECTION "InterruptRoutines"
EndOfInterrupt:
	rti
.ENDS

/****************************************************************************
Boot ROM Routines
****************************************************************************/

.BANK 0 SLOT 0
.SECTION "BootingROMRoutine" FORCE
BootROM:
	sei
	clc
	xce
	rep #$29
	sep #$10

	lda #$1FFF
	tcs

	phk
	plb

	lda #$4200
	tcd
	phd
	tay								; Y = #$(00)00

	sty $00							; disable IRQ, NMI and auto-joypad reading
	ldx #$FF
	stx $01							; IO Port
	stz $0B							; disable DMA and HDMA
	;ldx #$01
	sty $0D							; slow ROM

	lsr a
	tcd								; Direct Register = $2100

	ldx #$80						;\ force blank
	stx $00							;/ Screen display register
	sty $01							; $2101 : OAM Size and character address
	stz $02							; $2102 - $2103 : OAM address and priority

	stz $05							; $2105 : Screen mode; $2106 : mosaic setting
	stz $07							;\
	stz $09							;/ BG tilemap address and size
	stz $0B							; BG character address

	ldx.b #$2114 - $210D
-		sty $0D,x					;\
		sty $0D,x					;/ BG offsets
		sty $1B,x					;\
		sty $1B,x					;/ mode7 matrix and CGRAM
		dex
	bpl -

	ldx #$80
	stx $15
	stz $16							; VRAM address

	sty $1A							; mode7 settings

	ldx.b #$2133 - $2123
-		sty $23,x
		dex
	bpl -

	lda #$420B
	tcd

	ldx #$7E						;\  Initialize WRAM of bank $7E
	stx $F9							; |
	stz $F7							; |
	lda #$1FF0						; | avoid initializing the stack in order to return from the routine
	jsr InitializeWRAMwithDMA		; |
	lda #$2000						; |
	sta $F7							; |
	lda #$E000						; |
	jsr InitializeWRAMwithDMA		;/
	inc $F9							;\ Initialize WRAM of bank $7F
	lda #$0000						; |
	pha								; | push $0000 for direct register
	jsr InitializeWRAMwithDMA		;/

	lda #$1809						;\
	sta $F5							; | Clear VRAM using $7F:0000 as a source. The transfer size is 0x10000 bytes.
	;ldx #$01						; | X = 1 here
	stx $00							;/
	lda #$2208						;\
	sta $F5							; | Clear CGRAM
	lda #$0200						; |
	sta $FA							; |
	stx $00							;/

	pld								; D = $0000

	; copy the LZ4 routine onto WRAM
	rep #$10
	ldx #InflateBinary
	ldy #Inflate & $FFFF
	lda #InflateBinary@End - InflateBinary - 1
	mvn :InflateBinary, Inflate >> 16

	phk
	plb

	lda #:BGDeflate << 8 | $7F
	ldx #BGDeflate
	ldy #$0000
	jsl Inflate

	; Upload decompressed data
	sep #$10
	lda #$1801
	sta $4300
	stz $4302
	ldy #$7F
	sty $4304
	lda #$8000
	sta $4305
	ldx #$01
	stx $420B

	lda #BGTilemap
	sta $4302
	ldy #:BGTilemap
	sty $4304
	lda #BGTilemap@End - BGTilemap
	sta $4305
	stx $420B

	ldy #$00
	sty $2121
	lda #$2202
	sta $4300
	lda #BGPal
	sta $4302
	ldy #:BGPal
	sty $4304
	lda #128 * 2
	sta $4305
	stx $420B

	ldx #$01
	stx $2105						; BG Mode
	ldx #$40
	stx $2107						; BG1 Tilemap address
	ldx #$01
	stx $212C
	ldx #$0F
	stx $2100
	bra -2

.ACCU 16
.INDEX 8
InitializeWRAMwithDMA:
	sta $FA							; DMA size
	sty $211B						;\
	ldx #$00						; | Y * 1
	stx $211B						; |
	inx								; |
	stx $211C						;/
	lda.w #$3480
	sta $F5
	stx $00							; since X = 1
	rts
.ENDS

.SECTION "Inflate code" SUPERFREE
InflateBinary:
	.INCBIN "../src/inflate.ram.bin"
@End:
.ENDS

.SECTION "BG" SUPERFREE
BGDeflate:
	.INCBIN "4bpp.bin.deflate"

BGTilemap:
	.INCBIN "4bpp.tilemap"
@End:

BGPal:
	.INCBIN "4bpp.pal"
.ENDS
