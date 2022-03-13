; DEFLATE decompression
;
; A : source bank << 8 | destination bank
; X : source address
; Y : destination address

.INCLUDE "inflate.inc"

.DEFINE ROUTINE_BANK (Inflate >> 16)
.DEFINE ROUTINE_ADDRESS (Inflate & $FFFF)

; define UNSAFE not to check the crossing of a bank boundary in order to make the decompression routine smaller and slightly faster
;.DEFINE UNSAFE

.MEMORYMAP
DEFAULTSLOT 0
SLOTSIZE $10000
SLOT 0 0
.ENDME

.ROMBANKSIZE $10000
.ROMBANKS 1

.DEFINE MAX_BITS 15
.DEFINE MAX_LITERAL_CODES 286
.DEFINE MAX_DISTANCE_CODES 30
.DEFINE MAX_CODES (MAX_LITERAL_CODES + MAX_DISTANCE_CODES)
.DEFINE FIX_LITERAL_CODES 288

.ENUM $00
	Temp					ds 4
	ReadBitsTemp			dw
	BitCount				dw
	BitBuffer				dw
	Source					ds 3
	Destination				ds 3
	DynamicSymbolCount		dw
	DynamicDistanceCount	dw
.ENDE

.ENUM InflateVariable & $FFFF
Temporary34:				ds 34
CodeBitLength				ds MAX_CODES
BitLengthCount				ds MAX_BITS + 1
CodeToSymbol				ds FIX_LITERAL_CODES * 2
CodeToDistance				ds MAX_DISTANCE_CODES * 2

SymbolCodeUpperLimit		ds (MAX_BITS + 1) * 2				; the size of both must be the same and in this order (UpperLimit -> Adjustment)
SymbolCodeAdjustment		ds (MAX_BITS + 1) * 2

DistanceCodeUpperLimit		ds (MAX_BITS + 1) * 2
DistanceCodeAdjustment		ds (MAX_BITS + 1) * 2
.ENDE

.BASE ROUTINE_BANK
.ORG ROUTINE_ADDRESS

.ACCU 16
.INDEX 16

	phb
	phk
	plb

	sep #$20
	sta.w DecodeBlockLoop@Match@RewriteMoveBank + 1
	sta.w DecodeBlockLoop@Match@RewriteMoveBank + 2
	sta.w Stored@RewriteMoveBank + 1
	sta Destination + 2
	sty Destination
	xba
	sta Source + 2
	stx Source
	stz.w Loop + 1
	rep #$20

	stz.w SymbolCodeUpperLimit
	stz.w SymbolCodeAdjustment
	stz.w DistanceCodeUpperLimit
	stz.w DistanceCodeAdjustment

ClearBuffer:
	stz BitBuffer
	ldy BitBuffer			; Y = 0; saves 1 byte compared to ldy #$0000

Loop:
	lda #$0000
	beq +
		plb
		rtl
+
	lda #$0003
	jsr Read1_5bits
	lsr a
	rol.w Loop + 1
	and #%11
	asl a
	tax
	jmp (TypeTable,x)

TypeTable:
	.dw Stored
	.dw Fixed
	.dw Dynamic

.ACCU 16
.INDEX 16
Stored:
	sep #$10
	lda BitBuffer
-	cpy.b #$08
	bcc @Read16
		beq @Read8
			lsr a
			bra -
@Read16:
		.IFNDEF UNSAFE
		lda Source
		cmp #$FFFE
		bcc +
			jsr CrossBankBoundary
			sta Temp
			lda #$0002
			bra @IncrementPointer
		.ENDIF
+		lda [Source]
		sta Temp
		lda #$0004
		bra @IncrementPointer
@Read8:
	sep #$20
	sta Temp
	lda [Source]
	sta Temp + 1
	rep #$21
	lda #$0003
@IncrementPointer:
	ldx Source + 2
	adc Source				; the carry flag is always cleared here
	.IFNDEF UNSAFE
	bcc +
		ora #$8000
		inc Source + 2
		inx
		clc
	.ENDIF
+	stx.w @RewriteMoveBank + 2
	rep #$10
	tax
	ldy Destination
	.IFNDEF UNSAFE
	adc Temp
	bcc +
	beq +
		sta Temp + 2
		lda.w @RewriteMoveBank + 1
		sta.w @RewriteMoveBankCrossing + 1
		inc.w @RewriteMoveBank + 2
		lda Temp
		sbc Temp + 2
		dec a
@RewriteMoveBankCrossing:
		mvn $55, $55
		ldx #$8000
		inc Source + 2
		lda Temp + 2
		.db $89				; treat next 2 bytes as an immediate 16-bit value
	.ENDIF
+	lda Temp
	dec a
@RewriteMoveBank:
	mvn $55, $55
	phk
	plb
	sty Destination
	.IFNDEF UNSAFE
	cpx #$0000
	bne +
		inc Source + 2
		ldx #$8000
	.ENDIF
+	stx Source
	jmp ClearBuffer

.ACCU 16
.INDEX 16
Fixed:
	sty BitCount
	sep #$20
	lda #$07
	sta.w CodeBitLength + 256
	inc a
	sta.w CodeBitLength + 0
	sta.w CodeBitLength + 280
	inc a
	sta.w CodeBitLength + 144
	rep #$21
	ldx #CodeBitLength
	txy
	iny
	lda #(144 - 1) - 1
	mvn ROUTINE_BANK, ROUTINE_BANK
	inx
	iny
	lda #(256 - 144 - 1) - 1
	mvn ROUTINE_BANK, ROUTINE_BANK
	inx
	iny
	lda #(280 - 256 - 1) - 1
	mvn ROUTINE_BANK, ROUTINE_BANK
	inx
	iny
	lda #(FIX_LITERAL_CODES - 280 - 1) - 1
	mvn ROUTINE_BANK, ROUTINE_BANK

	ldy #FIX_LITERAL_CODES			; table length
	;clc
	jsr Construct
	lda #$0505
	sta.w CodeBitLength
	ldx #CodeBitLength + 1
	txy
	iny
	lda #(MAX_DISTANCE_CODES - 2) - 1
	mvn ROUTINE_BANK, ROUTINE_BANK
	ldy #MAX_DISTANCE_CODES
	;sec
	jsr Construct@NoRewriteSource
	jmp DecodeBlockLoop

.ACCU 16
.INDEX 16
Dynamic:

	lda #$0005
	jsr Read1_5bits
	and #%11111
	adc #257 - 1				; minus 1 for the carry flag being set here
	sta DynamicSymbolCount

	jsr Read1_5bits@StartWithoutRewrite
	and #%11111
	;inc a						; | without 'inc a' distance count is 1 byte fewer than is needed, so later it is adjusted.
	sta DynamicDistanceCount	;/   thanks to this, the carry flag is conserved, and saves 1 byte overall
	;clc
	adc DynamicSymbolCount
	sta.w @BitLengthReadLoop@RewriteLoopCount + 1

	phy							; using stack to save code size rather than speed
	stz.w CodeBitLength
	ldx #CodeBitLength + 1
	txy
	iny
	lda #(19 - 1) - 1
	mvn ROUTINE_BANK, ROUTINE_BANK
	ply

	lda #$0004
	jsr Read1_5bits
	and #%1111
	adc #$0004 - 1
	tax

	lda #LengthIndexForCodeBitLength
	sta Temp
@ReadCodeBitLengthOfLengthIndexLoop:
		lda (Temp)
		and #$00FF
		adc #CodeBitLength
		sta Temp + 2
		inc Temp

		phx
		lda #$0003
		jsr Read1_5bits					; calling subroutine is slow, but here is executed 19 tiems at maximum, so rather save the code size here
		plx
		sep #$20
		and #%111
		sta (Temp + 2)
		dex
		rep #$21
	bne @ReadCodeBitLengthOfLengthIndexLoop

	sty BitCount

	ldy #19							; table length
	;clc
	jsr Construct

	stz Temp
@BitLengthReadLoop:
		sep #$10
		ldy BitCount
		lda #$0000
		tax
@@DecodeLoop:
			cpy.b #$00
			bne @@@SkipLoad
				sta Temp + 2
				.IFNDEF UNSAFE
				lda Source
				cmp #$FFFE
				bcc +
					jsr CrossBankBoundary
					bra ++
				.ENDIF
+				lda [Source]
				inc Source
				inc Source
++				sta BitBuffer
				ldy #16
				lda Temp + 2
@@@SkipLoad:
			lsr BitBuffer
			rol a
			dey
			inx
			inx
			cmp.w SymbolCodeUpperLimit,x
			bpl @@DecodeLoop
		sty BitCount
		sep #$21						; A : 8-bit, and set the carry flag. A's high byte is 0x00 since the longest huffman code here is 7-bit long
		sbc.w SymbolCodeAdjustment,x
		asl a
		tay
		lda.w CodeToSymbol,y			; the table is 19 * 2 bytes long, so safe with 8-bit Y
		cmp #$10
		bcs +
			rep #$10
			ldx Temp
			sta.w CodeBitLength,x
			inx
			rep #$20
			jmp @@Next
.ACCU 8
.INDEX 8
+
		ldy BitCount
		sbc.b #$10
		rep #$30
		bne @@NotCopyPrevious
@@CopyPrevious:
			lda BitBuffer
			cpy.w #2
			bcs ++
				tya
				ora #$0008
				tay
				lda [Source]
				and #$00FF
				cpy #$0008
				beq +
					asl a
+				ora BitBuffer
				inc Source
				.IFNDEF UNSAFE
				bne ++
					jsr NextSourceBank
				.ENDIF
++			tax
			lsr a
			lsr a
			sta BitBuffer
			dey
			dey
			sty BitCount
			txa
			and #%11
			clc
			adc #3 - 1				; using 'adc #2' rather than 'inc a' 2 times to clear the carry flag
			tay						; Y = repeat count - 1
			lda Temp				; the carry flag is cleared here
			jmp @@Move
.ACCU 16
.INDEX 16
@@NotCopyPrevious:
		dec a
		bne @@ZeroRepeatLong
@@ZeroRepeatShort:
			lda BitBuffer
			cpy.w #3
			bcs +
				sep #$30						; 3
				lda.w Inverse + (12 - 2),y		; 4
				sta.w @@@RewriteBRA + 1			; 4
				tya								; 2
				ora #$08						; 2
				tay								; 2
				lda [Source]					; 6
				rep #$30						; 3
@@@RewriteBRA:	bra 55
				asl a
				asl a
				ora BitBuffer
				inc Source
				.IFNDEF UNSAFE
				bne +
					jsr NextSourceBank
				.ENDIF
+			tax
			lsr a
			lsr a
			lsr a
			sta BitBuffer
			tya
			sec
			sbc #3
			sta BitCount
			txa
			and #%111
			;clc
			;adc #3 - 2
			inc a
			tay					; Y = repeat count - 2
			bra @@ZeroClear
.ACCU 16
.INDEX 16
@@ZeroRepeatLong:
		lda BitBuffer
		cpy.w #7
		bcs +
			sep #$30
			lda.w Inverse + (12 - 6),y
			sta.w @@@RewriteBRA + 1
			tya
			ora #$08
			tay
			lda [Source]
			rep #$30
@@@RewriteBRA:
			bra 55
			asl a
			asl a
			asl a
			asl a
			asl a
			asl a
			ora BitBuffer
			inc Source
			.IFNDEF UNSAFE
			bne +
				jsr NextSourceBank
			.ENDIF
+		tax
		lsr a
		lsr a
		lsr a
		lsr a
		lsr a
		lsr a
		lsr a
		sta BitBuffer
		tya
		sec
		sbc #7
		sta BitCount
		txa
		and #%1111111
		;clc
		adc #(11 - 2) - 1	; the last minus 1 is for the carry flag being set
		tay					; Y = repeat count - 2
@@ZeroClear:
		sep #$20
		ldx Temp
		stz.w CodeBitLength,x
		inx
		rep #$21
		txa
@@Move:
		adc #CodeBitLength - 1
		tax
		tya
		txy
		iny
		mvn ROUTINE_BANK, ROUTINE_BANK
		tya
		sbc #CodeBitLength - 1			; minus 1 for carry flag being cleared here
		tax
@@Next:
		stx Temp
@@RewriteLoopCount:
		cpx.w #$5555
	bcs +
	jmp @BitLengthReadLoop
+
	ldy DynamicSymbolCount			; table length
	clc
	jsr Construct@NoRewriteSource

	ldy DynamicDistanceCount
	iny
	lda #CodeBitLength - 1			; carry flag is set here
	adc DynamicSymbolCount
	sec
	jsr Construct@RewriteSource

DecodeBlockLoop:
		sep #$10
@SkipSEP:
		ldy BitCount
		lda #$0000
		tax
@SymbolDecodeLoop:
			cpy.b #$00
			bne @@SkipLoad
				sta Temp + 2
				.IFNDEF UNSAFE
				lda Source
				cmp #$FFFE
				bcc +
					jsr CrossBankBoundary
					bra ++
				.ENDIF
+				lda [Source]
				inc Source
				inc Source
++				sta BitBuffer
				ldy #16
				lda Temp + 2
@@SkipLoad:
			lsr BitBuffer
			rol a
			dey
			inx
			inx
			cmp.w SymbolCodeUpperLimit,x
			bpl @SymbolDecodeLoop
		sty BitCount
		sec
		sbc.w SymbolCodeAdjustment,x
		asl a
		rep #$10						; requires 16-bit index to read a symbol
		tax
		lda.w CodeToSymbol,x
		cmp #257
		bcs @Match
@Literal:
			cmp #256
			beq @End
				sep #$30
				sta [Destination]
				rep #$20
				inc Destination
				bra DecodeBlockLoop@SkipSEP
@End:		jmp Loop
.ACCU 16
.INDEX 16
@Match:
@@LoadLength:
		asl a
		tax
		lda.w LengthBase - 257 * 2,x
		sta.w @@@RewriteLengthBase + 1
		lda.w LengthExtraBits - 257 * 2,x
		sta Temp + 2						; bits in need
		sep #$10
		tax
		eor.w #$0007
		sta.w @@@RewriteBRA2 + 1			; also overwrites an unreachable byte but no problem
		txa
		asl a
		tax

		lda BitBuffer
@@@RewriteExtraBit:
		cpy Temp + 2
		bcs +
			sep #$20
			lda.w Inverse + (12 - 4),y
			sta.w @@@RewriteBRA + 1
			tya
			ora #$08
			tay
			lda [Source]
			rep #$20
@@@RewriteBRA:
			bra 55
			asl a
			asl a
			asl a
			asl a
			ora BitBuffer
			inc Source
			.IFNDEF UNSAFE
			bne +
				jsr NextSourceBank
			.ENDIF
+		sta Temp
@@@RewriteBRA2:
		bra 55
		lsr a					; | unreachable bytes placed here to save cycles
		lsr a					;/
		lsr a
		lsr a
		lsr a
		lsr a
		lsr a
		sta BitBuffer
		tya
		sec
		sbc Temp + 2
		tay
		lda Temp
		and.w BitMask,x
		;clc
@@@RewriteLengthBase:
		adc #$5555						; carry flag is always set here
		sta.w @@RewriteMoveCount + 1	; the value is subtracted by 2 in advance for this carry flag and move count

		lda #$0000
		tax
@@DistanceDecodeLoop:
			cpy.b #$00
			bne @@@SkipLoad
				sta Temp + 2
				.IFNDEF UNSAFE
				lda Source
				cmp #$FFFE
				bcc +
					jsr CrossBankBoundary
					bra ++
				.ENDIF
+				lda [Source]
				inc Source
				inc Source
++				sta BitBuffer
				ldy #16
				lda Temp + 2
@@@SkipLoad:
			lsr BitBuffer
			rol a
			dey
			inx
			inx
			cmp.w DistanceCodeUpperLimit,x
			bpl @@DistanceDecodeLoop
		sty BitCount
		sec
		sbc.w DistanceCodeAdjustment,x
		asl a
		tax
		lda.w CodeToDistance,x

@@LoadDistance:
		asl a
		tax
		lda.w DistanceBase,x
		sta.w @@@RewriteDistanceBase + 1
		lda.w DistanceExtraBits,x
		sta Temp + 2						; bits in need
		asl a
		tax
		sep #$20
		cpy Temp + 2
		bcs @@@HasEnough
@@@LoadNewBuffer:
			lda.w Inverse + (12 - 12),y		; Y = bit_count ; the possible range [0, 12]
			sta.w @@@@RewriteBRA + 1

			tya								; the carry flag is always cleared here
			sbc Temp + 2					; = bit_count - need - 1 ; the possible range  [-14, -2]
			adc #16 + 1						; with the carry flag cleared before 'sbc' and being set before 'adc', the +1 is necessary to cancel out -1.
			sta.w @@@@RewriteBRA2 + 1		; = 16 - (need + bit_count) ; the possible range [3, 15]
			sta BitCount

			rep #$30
			.IFNDEF UNSAFE
			lda Source
			cmp #$FFFE
			bcc +
				jsr CrossBankBoundary
				bra ++
			.ENDIF
+			lda [Source]
			inc Source
			inc Source
++
			tay
@@@@RewriteBRA:
			bra 55
			asl a					; 1
			asl a					; 2
			asl a					; 3
			asl a					; 4
			asl a					; 5
			asl a					; 6
			asl a					; 7
			asl a					; 8
			asl a					; 9
			asl a					; 10
			asl a					; 11
			asl a					; 12
			ora BitBuffer
			sta Temp

			tya
@@@@RewriteBRA2:
			bra 55
			.db "hal"
			;lsr a					; 1			; | unreachable bytes but three placed here to save a few cycles
			;lsr a					; 2			; |
			;lsr a					; 3			;/
			lsr a					; 4
			lsr a					; 5
			lsr a					; 6
			lsr a					; 7
			lsr a					; 8
			lsr a					; 9
			lsr a					; 10
			lsr a					; 11
			lsr a					; 12
			lsr a					; 13
			lsr a					; 14
			lsr a					; 15
			lsr a					; 16		; always executed
			sta BitBuffer
			sec
			bra @@@Shared
.ACCU 8
@@@HasEnough:
		lsr a
		eor #$0F
		sta.w @@@@RewriteBRA + 1
		rep #$30
		lda BitBuffer
		sta Temp
@@@@RewriteBRA:
		bra 55
		lsr a						; 1			; | unreachable bytes
		lsr a						; 2			;/
		lsr a						; 3
		lsr a						; 4
		lsr a						; 5
		lsr a						; 6
		lsr a						; 7
		lsr a						; 8
		lsr a						; 9
		lsr a						; 10
		lsr a						; 11
		lsr a						; 12
		lsr a						; 13
		lsr a						; 14
		lsr a						; 15
		sta BitBuffer
		tya
		sec
		sbc Temp + 2				; this sets the carry flag
		sta BitCount
@@@Shared:
		lda Temp
		and.w BitMask,x
@@@RewriteDistanceBase:
		adc #$5555						; carry flag is always set here
		eor #$FFFF
		inc a
		;clc
		adc Destination
		tax
		ldy Destination
@@RewriteMoveCount:
		lda #$5555
@@RewriteMoveBank:
		mvn $55, $55
		phk
		plb
		sty Destination
	jmp DecodeBlockLoop

/****************************************************************************
****************************************************************************/

.ACCU 16
.INDEX 16
Construct:
	lda #CodeBitLength
@RewriteSource:
	sta.w @RewriteBitLength1 + 1
	sta.w @RewriteBitLength2 + 1
@NoRewriteSource:
	bcs @Distance
		lda #CodeToSymbol - 2
		ldx #SymbolCodeUpperLimit
		bra +
@Distance:
	lda #CodeToDistance - 2
	ldx #DistanceCodeUpperLimit
	clc
+
	sta.w @RewriteTableDestination + 1
	stx.w @RewriteUpperLimit + 1
	inx
	inx
	stx.w @RewriteUpperLimit@Plus2 + 1

	sty.w @RewriteLoopCount + 1
	dey

	txa
	adc #(MAX_BITS + 1) * 2 - 2
	sta.w @RewriteAdjustment + 1
	inc a
	inc a
	sta.w @RewriteAdjustment@Plus2 + 1

	phy
	stz.w BitLengthCount
	ldx #BitLengthCount
	txy
	iny
	lda #MAX_BITS - 1
	mvn ROUTINE_BANK, ROUTINE_BANK

	inc a						; A = 0x0000
	sep #$20
	ply
@RewriteBitLength1:
-		lda.w CodeBitLength,y	; 5     ; | practically X &= 0x00FF
		tax						; 2     ;/
		inc.w BitLengthCount,x	; 7
		dey						; 2
	bpl -						; 2 / 3
	rep #$20

	stz.w Temporary34 + 2
	ldy #$0001
	tyx								; | X = 0x0000
	dex								;/
-		lda.w BitLengthCount,y
		and #$00FF
		sta Temp
		asl a
		adc.w Temporary34 + 2,x
		sta.w Temporary34 + 4,x
@RewriteUpperLimit:
		lda.w $5555,x
		pha
		asl a
		adc Temp
@@Plus2:
		sta.w $5557,x				; if valid, the maximum value here must be $8000, always clearing the carry flag.
		pla
		;clc
@RewriteAdjustment:
		adc.w $6666,x
@@Plus2:
		sta.w $6668,x
		inx
		inx
		iny
		cpy #MAX_BITS + 1
	bcc -

	ldy #$0000
@RewriteBitLength2:
-		lda.w CodeBitLength,y
		and #$00FF
		beq +
			asl a
			tax
			lda.w Temporary34,x				; can be optimized with direct page
			inc a
			inc a
			sta.w Temporary34,x
			tax
			tya
@RewriteTableDestination:
			sta.w $5555,x
+		iny
@RewriteLoopCount:
		cpy.w #$5555
	bcc -

	rts

/****************************************************************************
****************************************************************************/

; this routine is only called at the beginning of the block,
;  so save code size rather than speed
;
; requires bit mask after the return.
; also sets the carry flag in returning.

Read1_5bits:
	sta ReadBitsTemp
	eor #$4A07					; $4A is 'lsr a'
	sta.w @RewriteBRA2 + 1
@StartWithoutRewrite:
	lda BitBuffer
	cpy ReadBitsTemp
	bcs +
		sep #$30
		lda.w Inverse + (12 - 4),y
		sta.w @RewriteBRA1 + 1
		tya
		ora #$08				; |
		tay						;/ guaranteed that the high byte of A is $00 here
		lda [Source]
		rep #$30
@RewriteBRA1:
		bra 55
		asl a
		asl a
		asl a
		asl a
		ora BitBuffer
		inc Source
		.IFNDEF UNSAFE
		bne +
			jsr NextSourceBank
		.ENDIF
+	tax
@RewriteBRA2:
	bra 55
	lsr a
	lsr a
	lsr a
	lsr a
	lsr a
	lsr a
	lsr a
	sta BitBuffer
	tya
	sec
	sbc ReadBitsTemp
	tay
	txa
	rts

.IFNDEF UNSAFE
.ACCU 16
.INDEX 8
CrossBankBoundary:
	inc a
	bne +
		sep #$20			; Source: $xx:FFFF
		lda [Source]
		xba
		stz Source
		lda #$80
		sta Source + 1
		inc Source + 2
		lda [Source]
		inc Source
		xba
		rep #$20
		rts
+	lda [Source]		; Source: $xx:FFFE
	stz Source			; | set $8000
NextSourceBank:
	sec					; |
	ror Source			;/
	inc Source + 2
	rts
.ENDIF

/****************************************************************************
****************************************************************************/

Inverse:
	.db 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0
BitMask:
	.dw $0000, $0001, $0003, $0007, $000F, $001F, $003F, $007F
	.dw $00FF, $01FF, $03FF, $07FF, $0FFF, $1FFF					; for 0 - 13 bits reading

LengthIndexForCodeBitLength:
	.db 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15

LengthBase:
	.dw 3 - 2, 4 - 2, 5 - 2, 6 - 2, 7 - 2, 8 - 2, 9 - 2, 10 - 2
	.dw 11 - 2, 13 - 2, 15- 2, 17 - 2
	.dw 19 - 2, 23 - 2, 27- 2, 31 - 2
	.dw 35 - 2, 43 - 2, 51- 2, 59 - 2
	.dw 67 - 2, 83 - 2, 99- 2, 115 - 2
	.dw 131 - 2, 163 - 2, 195 - 2, 227 - 2
	.dw 258 - 2

LengthExtraBits:
	.dw 0, 0, 0, 0, 0, 0, 0, 0
	.dw 1, 1, 1, 1
	.dw 2, 2, 2, 2
	.dw 3, 3, 3, 3
	.dw 4, 4, 4, 4
	.dw 5, 5, 5, 5
	.dw 0

DistanceBase:
	.dw 1 - 1, 2 - 1, 3 - 1, 4 - 1
	.dw 5 - 1, 7 - 1
	.dw 9 - 1, 13 - 1
	.dw 17 - 1, 25 - 1
	.dw 33 - 1, 49 - 1
	.dw 65 - 1, 97 - 1
	.dw 129 - 1, 193 - 1
	.dw 257 - 1, 385 - 1
	.dw 513 - 1, 769 - 1
	.dw 1025 - 1, 1537 - 1
	.dw 2049 - 1, 3073 - 1
	.dw 4097 - 1, 6145 - 1
	.dw 8193 - 1, 12289 - 1
	.dw 16385 - 1, 24577 - 1

DistanceExtraBits:
	.dw 0, 0, 0, 0
	.dw 1, 1
	.dw 2, 2
	.dw 3, 3
	.dw 4, 4
	.dw 5, 5
	.dw 6, 6
	.dw 7, 7
	.dw 8, 8
	.dw 9, 9
	.dw 10, 10
	.dw 11, 11
	.dw 12, 12
	.dw 13, 13
EOF:
