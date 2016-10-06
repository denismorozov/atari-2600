; Denis Morozov
; 9/22/2016
; CPE 481 - Atari 2600 Assignment 1

; code referenced from Kirk Israel
; http://www.atariage.com/2600/programming/2600_101/03first.html

	processor 6502
	include vcs.h
	org $F000

Start
	sei ; disable interrupts
	cld ; clear BCD math bit
	ldx #$FF ; max X
	txs ; reset stack pointer

	lda #0
ClearMem
	sta 0,X
	dex
	bne ClearMem

	lda #$00
	sta COLUBK ; set background color
	lda #33
	sta COLUP0 ; set p0 color
MainLoop
	lda  #2
	sta  VSYNC
	sta  WSYNC
	sta  WSYNC
	sta  WSYNC

	; TIM64T ticks down every 64 cycles, we have
	; 37(scanlines)*76(cycles/scanline) - 14(scanlines) = 2798 cycles
	; 2798 / 64 ~= 43
	lda  #43
	sta  TIM64T
	lda #0
	sta  VSYNC
	
	ldx #0 ; init movement speed
	; left?
	lda #%01000000
	bit SWCHA
	bne SkipMoveLeft
	ldx #$10 ; go left
SkipMoveLeft

	; right?
	lda #%10000000
	bit SWCHA
	bne SkipMoveRight
	ldx #$F0 ; go right
SkipMoveRight

	stx HMM0

WaitForVblankEnd
	lda INTIM
	bne WaitForVblankEnd
	ldy #191 ; number of scanlines
	sta WSYNC
	sta VBLANK

	sta WSYNC ; line things up
	sta HMOVE ; move
ScanLoop
	sta WSYNC
	lda #2
	sta ENAM0
	dey ; decrement scanline counter
	bne ScanLoop

	lda #2
	sta WSYNC
	sta VBLANK
	ldx #30
OverScanWait
	sta WSYNC
	dex
	bne OverScanWait
	jmp  MainLoop

	org $FFFC
	.word Start
	.word Start