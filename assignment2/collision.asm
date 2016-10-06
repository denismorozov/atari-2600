; Denis Morozov
; 10/04/2016
; CPE 481 - Atari 2600 Assignment 2

; code referenced from Kirk Israel
; http://www.atariage.com/2600/programming/2600_101/03first.html

  processor 6502
  include vcs.h
  org $F000

YPos1 = $80
VisiblePlayerLine1 = $81
YPos2 = $82
VisiblePlayerLine2 = $83
PlayerBuffer1 = $84
PlayerBuffer2 = $85

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
  lda #$72
  sta COLUP1
  lda #80
  sta YPos1
  lda #82
  sta YPos2

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
  lda #%00000000 ; not sure
  sta REFP0 ; not sure
SkipMoveLeft

  ; right?
  lda #%10000000
  bit SWCHA
  bne SkipMoveRight
  ldx #$F0 ; go right
  lda #%00001000 ; not sure
  sta REFP0 ; not sure
SkipMoveRight

  lda #%00010000
  bit SWCHA
  bne SkipMoveDown
  inc YPos1
SkipMoveDown

  lda #%00100000
  bit SWCHA
  bne SkipMoveUp
  dec YPos1
SkipMoveUp

  stx HMP0
  ;stx HMM0

  lda #%00000001
  bit SWCHA
  bne SkipMoveDown2
  inc YPos2
SkipMoveDown2

  lda #%00000010
  bit SWCHA
  bne SkipMoveUp2
  dec YPos2
SkipMoveUp2

  ldx #0
  lda #%00000100
  bit SWCHA
  bne SkipMoveLeft2
  ldx #$10
  lda #%00000000
  sta REFP1
SkipMoveLeft2

  lda #%00001000
  bit SWCHA
  bne SkipMoveRight2
  ldx #$F0
  lda #%00001000
  sta REFP1
SkipMoveRight2
  stx HMP1

  lda #%10000000
  bit CXPPMM
  beq NoCollision
  lda YPos1
  sta COLUBK

NoCollision
  sta CXCLR
  
  lda #0
  sta PlayerBuffer1
  sta PlayerBuffer2

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
  lda PlayerBuffer1
  ;sta ENAM0
  sta GRP0
  lda PlayerBuffer2
  sta GRP1

CheckActivatePlayer1
  cpy YPos1
  bne SkipActivatePlayer1
  lda #8
  sta VisiblePlayerLine1
SkipActivatePlayer1

CheckActivatePlayer2
  cpy YPos2
  bne SkipActivatePlayer2
  lda #8
  sta VisiblePlayerLine2
SkipActivatePlayer2


  lda #0
  sta PlayerBuffer1
  sta PlayerBuffer2

  
  ldx VisiblePlayerLine1
  beq FinishPlayer1
IsPlayerOn
  lda PlayerSprite-1,X
  sta PlayerBuffer1
  dec VisiblePlayerLine1
FinishPlayer1

  ldx VisiblePlayerLine2
  beq FinishPlayer2
IsPlayerOn2
  lda PlayerSprite-1,X
  sta PlayerBuffer2
  dec VisiblePlayerLine2
FinishPlayer2

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

PlayerSprite
  .byte #%00111100
  .byte #%01111110
  .byte #%11000001
  .byte #%10111111
  .byte #%11111111
  .byte #%11101011
  .byte #%01111110
  .byte #%00111100

  org $FFFC
  .word Start
  .word Start