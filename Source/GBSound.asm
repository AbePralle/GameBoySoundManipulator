;===============================================================================
; GBSound.asm
; 2000.03.06 by Abe Pralle
;===============================================================================
MPRINT: MACRO
        push    bc
        ld      a,\4
        ld      [textColor],a
        ld      b,\2
        ld      c,\3
        ld      hl,\1
        call    PrintText
        pop     bc
ENDM

PRINTCURSOR:  MACRO
        ld      a,\2
        ld      [textColor],a
        ld      hl,\1
        call    PrintTextAtCursor
ENDM

UPDATESCREEN: MACRO
        ld      a,1
        ldio    [backBufferReady],a
ENDM

LOADA:  MACRO
        ld      a,\2
        ld      \1,a
ENDM

DPAD_RIGHT    EQU $01
DPAD_LEFT     EQU $02
DPAD_UP       EQU $04
DPAD_DOWN     EQU $08
BUTTON_A      EQU $10
BUTTON_B      EQU $20
BUTTON_SELECT EQU $40
BUTTON_START  EQU $80
BUTTON_ALL    EQU $f0

;---------------------------------------------------------------------
;  Variables
;---------------------------------------------------------------------
SECTION        "UserVars",WRAM0[$c000]
backBuffer:        DS 608       ;must start on even $10 address
waveBuffer:        DS 128       ;must start on even $10 address
padding:           DS 32
attributeBuffer:   DS 608       ;must start on even $10 address
padding2:          DS 160
spriteOAMBuffer:   DS 160       ;must start on even $100 address
curJoy1:           DS 1
curSound:          DS 1         ;sound type being manipulated
textColor:         DS 1
state1_data:       DS 5
state2_data:       DS 4
state3_data:       DS 22
state4_data:       DS 4
curByte:           DS 1
curByteMask:       DS 1
cursor_x:          DS 1
cursor_y:          DS 1
copyWaveChars:     DS 1


SECTION        "User",ROM0
;---------------------------------------------------------------------
; Routine:      UserMain
; Description:  Sets up stuff then goes into main loop
;---------------------------------------------------------------------
UserMain::
        call    InitGraphics
        call    InitSound
        call    ClearScreen

        xor     a
        ld      [copyWaveChars],a

        ;turn on the vblank interrupt
        ld      a,[$ffff]
        or      1
        ld      [$ffff],a

        ;set up initial values for sounds
        LOADA   [state1_data],%00000000     ;sweep
        LOADA   [state1_data+1],%10000000   ;duty+length
        LOADA   [state1_data+2],%11110000   ;envelope
        LOADA   [state1_data+3],%00000000   ;freq_lo
        LOADA   [state1_data+4],%11000001   ;freq_hi

        LOADA   [state2_data],%10000000     ;duty+length
        LOADA   [state2_data+1],%11110000   ;envelope
        LOADA   [state2_data+2],%00000000   ;freq_lo
        LOADA   [state2_data+3],%11000001   ;freq_hi

        LOADA   [state3_data],  %10000000   ;play sound
        LOADA   [state3_data+1],%00000000   ;length
        LOADA   [state3_data+2],%00100000   ;output level
        LOADA   [state3_data+3],%00000000   ;frequency lo
        LOADA   [state3_data+4],%11000001   ;frequency hi

        LOADA   [state4_data],  %00000000   ;length
        LOADA   [state4_data+1],%11110000   ;envelope
        LOADA   [state4_data+2],%00000000   ;frequency
        LOADA   [state4_data+3],%11000000   ;init/consecutive

        ld      hl,state3_data+5
        ld      c,8
        ld      a,$ff
.fill1  ld      [hl+],a
        dec     c
        jr      nz,.fill1

        xor     a
        ld      c,8
.fill2  ld      [hl+],a
        dec     c
        jr      nz,.fill2

        ei
        call    ShowInfo

        ld      a,0
        ld      [curSound],a

        jr      .advanceState

.stateLoop
        ldio    a,[backBufferReady]    ;screen been drawn yet?
        or      a
        jr      nz,.stateLoop

        call    GetInput
        ld      a,[curJoy1]
        and     BUTTON_SELECT
        jr      z,.checkState1

        ;advance state
.advanceState
.waitSelectRelease
        call    GetInput
        ld      a,[curJoy1]
        and     BUTTON_SELECT
        jr      nz,.waitSelectRelease

        xor     a
        ld      [curByte],a

        ld      a,[curSound]
        and     %11
        inc     a
        ;cp      3
        ;jr      nz,.soundOkay
        ;inc     a    ;skip sound 3 for now

.soundOkay
        ld      [curSound],a

        cp      1
        jr      nz,.checkInitSound2

        ld      a,%01110000
        ld      [curByteMask],a
        jp      .checkState1

.checkInitSound2
        cp      2
        jr      nz,.checkInitSound3

        ld      a,1
        ld      [$ff1a],a             ;turn sound on
        ld      a,%11000000
        ld      [curByteMask],a
        jr      .checkState1

.checkInitSound3
        cp      3
        jr      nz,.checkInitSound4

        ld      a,1
        ld      [curByte],a
        ld      a,%11110000
        ld      [curByteMask],a
        jr      .checkState1
.checkInitSound4
        ld      a,%00111111
        ld      [curByteMask],a
        jr      .checkState1

.checkState1
        ;handle value increase/decrease
        ld      a,[curJoy1]
        bit     2,a
        jr      z,.tryDown

        call    IncrementBits
        jr      .tryButtonA

.tryDown
        bit     3,a
        jr      z,.tryButtonA

        call    DecrementBits

.tryButtonA
        ld      a,[curJoy1]
        and     (BUTTON_A | BUTTON_START)
        jr      z,.reallyCheckState1

        call    PlayCurrentSound
.waitRelease
        call    GetInput
        ld      a,[curJoy1]
        and     (BUTTON_A | BUTTON_START)
        jr      nz,.waitRelease

.reallyCheckState1
        ld      a,[curSound]
        cp      a,1
        jr      nz,.checkState2

        call    OnState1
        jp      .stateLoop

.checkState2
        cp      a,2
        jr      nz,.checkState3

        call    OnState2
        jp      .stateLoop

.checkState3
        cp      a,3
        jr      nz,.checkState4

        call    OnState3
        jp      .stateLoop

.checkState4
        cp      a,4
        jr      nz,.done

        call    OnState4
.done
        jp      .stateLoop

;---------------------------------------------------------------------
; Routine:      InitSound
; Description:  Turns on the sound at full volume both output
;               channels
;---------------------------------------------------------------------
InitSound:
        ;turn on sound at master control
        ld      a,%10000000
        ld      [$ff26],a

        ld      a,$ff
        ld      [$ff24],a            ;full volume
        ld      [$ff25],a            ;both channels

        ret


;---------------------------------------------------------------------
; Routine:      InitGraphics
; Alters:       af
; Description:  Sets the display, defines colors, and loads the font.
;---------------------------------------------------------------------
InitGraphics:
        push bc
        push de
        push hl

        call    DisplayOffWaitVRAM
        call    SetupSpriteHandler

        ;set screen offsets to 0
        xor     a
        ldh     [$ff43], a
        ldh     [$ff42], a

        call    SetupColorPalette
        call    LoadFont

        ;turn LCD on
        ld      a,%11000011
        ld      [$ff40], a       ;lcdc control

        pop     hl
        pop     de
        pop     bc
        ret

;---------------------------------------------------------------------
; Routine:      DisplayOffWaitVRAM
; Alters:       af
; Description:  Turns off the screen and waits until the VRAM
;               is accessible
;---------------------------------------------------------------------
DisplayOffWaitVRAM:
        ;turn display off
        ld      a,[$ffff]            ;get interrupts enabled
        push    af                   ;save original value
        and     %11111110            ;turn off vblank interrupt
        ld      [$ffff],a            ;"interrupt THIS!"
.wait   ld      a,[$ff44]            ;get line being drawn
        cp      144                  ;wait until line is >= 144
        jr      c,.wait
        ld      a,[$ff40]            ;LCDC register
        and     %01111111            ;turn off screen
        ld      [$ff40],a
        pop     af                   ;retrieve original interrupt settings
        ld      [$ffff],a

.waitVRAM
        ld      a,[$ff41]            ;STAT register
        and     %00000010            ;bit 1 needs to be zero to access VRAM
        jr      nz,.waitVRAM
        ret

;---------------------------------------------------------------------
; Routine:      SetupSpriteHandler
; Description:  Copies the routine required for sprite DMA to high
;               RAM and initializes the sprites to "off"
;---------------------------------------------------------------------
SetupSpriteHandler:
        ld      hl,oamHandlerStart                       ;src addr
        ld      de,SpriteDMAHandler                      ;dest addr
        ld      c,(oamHandlerFinish - oamHandlerStart)   ;# of bytes

.loop   ld      a,[hl+]
        ld      [de],a
        inc     de

        dec     c
        jr      nz,.loop

        ;clear oam buffer by setting each y-coordinate to zero
        ld      c,40
        ld      hl,spriteOAMBuffer
        ld      de,3                ;add to address hl every time
        xor     a

.loop2  ld      [hl+],a
        add     hl,de

        dec     c
        jr      nz,.loop2

        ret

oamHandlerStart:
        ;addr of start (in $100's)
        ld      a,((spriteOAMBuffer>>8) & $ff)

        ld      [$ff00+$46],a            ;start sprite dma
        ld      a,$28                    ;start a delay loop
.wait        dec     a
        jr      nz,.wait
        ret
oamHandlerFinish:


;---------------------------------------------------------------------
; Routine:      SetupColorPalette
; Alters:       af
; Description:  Sets up the background & sprite color palettes
;---------------------------------------------------------------------
SetupColorPalette:
        push    bc
        push    hl

        ld      a,%00100111       ;background color order (0213)
        ld      [$ff47],a

        ld      a,$80
        ld      [$ff68],a         ;background specifier
        ld      [$ff6a],a         ;sprite specifier

        ld      c,8*4             ;copy 32 bytes
        ld      hl,.paletteData

.loop   ld      a,[hl+]           ;get a byte of data
        ld      [$ff69],a         ;copy to bg color...
        ld      [$ff6b],a         ;...and fg color

        dec     c
        jr      nz,.loop

        pop     bc
        pop     hl
        ret

.paletteData
        ;       black, grey,  white, green
        DW      $0000, $3def, $5ef7, $7fff     ;grey/white
        DW      $23e8, $14a5, $03e0, $0000     ;green bg black fg
        DW      $0000, $ffff, $ffff, $3def     ;grey
        DW      $0000, $14a5, $ffff, $23e8     ;green & grey fg

;---------------------------------------------------------------------
; Routine:      LoadFont
; Alters:       af
; Description:  Loads in the font data to tile pattern memory (bank 0)
;               by copying 2048 bytes from ROM to tile mem.
;---------------------------------------------------------------------
LoadFont:
        push    bc
        push    de
        push    hl

        ;Switch to VRAM bank 0
        xor     a
        ld      [$ff00+$4f],a          ;(VRAM bank 0)

        ld      hl,font       ;source address
        ld      de,$9000      ;destination address

        ld      b,128         ;copy 128 tiles

.outer
        ld      c,16          ;16 bytes per tile

.inner  ld      a,[hl+]
        ld      [de],a
        inc     de

        dec     c
        jr      nz,.inner

        ;first 128 tiles from $9000 to $97ff, second 128 tiles from
        ;$8800-$8fff.  If dest points >= $9800 reset to $8800.
        ld      a,d
        cp      $98
        jr      c,.destPtrOkay

        ld      de,$8800

.destPtrOkay
        dec     b
        jr      nz,.outer

        pop     bc
        pop     de
        pop     hl
        ret

;---------------------------------------------------------------------
; Routine:      ClearScreen
; Alters:       af
; Description:  Clears the background tile & attribute buffers
;---------------------------------------------------------------------
ClearScreen:
        push    bc
        push    de
        push    hl

        ;clear background buffer
        xor     a                   ;data
        ld      hl,backBuffer       ;addr of tile buffer
        ld      de,attributeBuffer  ;addr of attribute buffer
        ld      b,18                ;clear 18 lines

.outer
        ld      c,32                ;32 chars per line

.inner
        ld      [hl+],a
        ld      [de],a
        inc     de
        dec     c
        jr      nz,.inner

        dec     b
        jr      nz,.outer

        pop     hl
        pop     de
        pop     bc
        ret

;---------------------------------------------------------------------
; Routine:      PrintText
; Arguments:    b  - horizonal location (0-19)
;               c  - vertical location (0-18)
;               hl - ptr to null-terminated text
;               [textColor] - palette color of text
; Alters:       af
; Description:  Draws the specified text to the back-buffer
;---------------------------------------------------------------------
PrintTextAtCursor:  ;same as printtext but sets up b & c from cursor
        push    bc
        push    de
        push    hl

        ld      a,[cursor_x]
        ld      b,a
        ld      a,[cursor_y]
        ld      c,a
        jr      PrintTextCommon

PrintText:
        push    bc
        push    de
        push    hl

PrintTextCommon:
        ;store b & c in cursor_x/y
        ld      a,b
        ld      [cursor_x],a
        ld      a,c
        ld      [cursor_y],a

        ld      d,h    ;strptr into de
        ld      e,l

        ;convert bc from indices into a memory location
        ;  bc = (c*32) + b + backBuffer
        xor     a
        sla     c     ;*2
        rla
        sla     c     ;*4
        rla
        sla     c     ;*8
        rla
        sla     c     ;*16
        rla
        sla     c     ;*32
        rla           ;now a+c is a 16 bit value

        ld      h,a   ;save off a
        ld      a,c   ;get low byte
        add     b     ;plus horizontal offset
        ld      c,a   ;is now low byte
        ld      b,h   ;b is high byte, bc is 16 bit offset

        ld      hl,backBuffer
        add     hl,bc ;hl now points to backbuffer

        ;copy the tile indicies
.loop   ld      a,[de]      ;get a character
        inc     de
        or      a           ;finished if it's null
        jr      z,.done
        ld      [hl],a      ;write it to the buffer
        push    hl
        ld      a,h         ;switch to attribute buffer
        add     3
        ld      h,a
        ld      a,[textColor]  ;get the color for this character
        ld      [hl],a         ;write it to attribute buffer

        ld      a,[cursor_x]   ;increment x cursor
        inc     a
        ld      [cursor_x],a

        pop     hl
        inc     hl
        jr      .loop

.done
        pop     hl
        pop     de
        pop     bc
        ret

;---------------------------------------------------------------------
; Routine:      OnVBlank
; Description:  Called every vertical blank.  Sets "vblankFlag" to
;               1 to indicate a vertical blank has occurred
;---------------------------------------------------------------------
OnVBlank::
        push    af   ;save all registers on stack
        push    bc
        push    de
        push    hl

        ld      a,1  ;set "vblankFlag" to 1
        ldio    [vblankFlag],a

        ;is the backBuffer ready for DMA blit to screen?
        ldio    a,[backBufferReady]
        or      a
        jr      z,.done             ;wasn't ready

        xor     a
        ldio    [backBufferReady],a  ;signal we've blitted it

        ;store current VRAM bank in use
        ld      a,[$ff00+$4f]
        push    af

        ;set VRAM bank to 0
        xor     a
        ld      [$ff00+$4f],a

        ;use DMA to transfer sprite OAM buffer to VRAM
        call    SpriteDMAHandler

        ;Initiate DMA transfer of backbuffer to VRAM
        ld      hl,backBuffer
        ld      a,h
        ld      [$ff00+$51],a     ;high byte of source
        ld      a,l
        ld      [$ff00+$52],a     ;low byte of source
        ld      a,$98
        ld      [$ff00+$53],a     ;high byte of dest
        xor     a
        ld      [$ff00+$54],a     ;low byte of dest
        ld      a,37              ;copy (37+1)*16 = 608 bytes
        ld      [$ff00+$55],a     ;store length to start DMA

        ;set VRAM bank to 1 (tile attributes)
        ld      a,1
        ld      [$ff00+$4f],a

        ;Initiate DMA transfer of tile attributes to VRAM
        ld      hl,attributeBuffer
        ld      a,h
        ld      [$ff00+$51],a     ;high byte of source
        ld      a,l
        ld      [$ff00+$52],a     ;low byte of source
        ld      a,$98
        ld      [$ff00+$53],a     ;high byte of dest
        xor     a
        ld      [$ff00+$54],a     ;low byte of dest
        ld      a,37              ;copy (37+1)*16 = 608 bytes
        ld      [$ff00+$55],a     ;store length to start DMA

        ;copy wave chars using dma
        ld      hl,copyWaveChars
        ld      a,[hl]
        or      a
        jr      z,.afterWaveCopy

        xor     a
        ld      [hl],a
        ld      [$ff00+$4f],a     ;set VRAM bank to 0

        ;Initiate DMA transfer of wave tiles to VRAM
        ld      a,((waveBuffer>>8) & $ff)
        ld      [$ff00+$51],a     ;high byte of source
        ld      a,(waveBuffer & $ff)
        ld      [$ff00+$52],a     ;low byte of source
        ld      a,$90
        ld      [$ff00+$53],a     ;high byte of dest
        ld      a,$50
        ld      [$ff00+$54],a     ;low byte of dest
        ld      a,7               ;copy (7+1)*16 = 128 bytes
        ld      [$ff00+$55],a     ;store length to start DMA

.afterWaveCopy
        ;restore VRAM bank that was in use
        pop     af
        ld      [$ff00+$4f],a

.done   pop     hl   ;restore all regs from stack
        pop     de
        pop     bc
        pop     af
        reti         ;return from interrupt


;---------------------------------------------------------------------
; Routine:      GetInput
; Alters:       af
; Description:  Polls the buttons and stores the input in [curJoy1]
;               $80 - Start
;               $40 - Select
;               $20 - B
;               $10 - A
;               $08 - Down
;               $04 - Up
;               $02 - Left
;               $01 - Right
;---------------------------------------------------------------------
GetInput::
        push    bc

        ld      a,$20
        ld      [$ff00],a     ;select P14
        ld      a,[$ff00]
        ld      a,[$ff00]     ;wait a few cycles
        cpl
        and     $0f
        ld      b,a           ;b has direction info
        ld      a,$10         ;select P15
        ld      [$ff00],a
        ld      a,[$ff00]     ;wait mo
        ld      a,[$ff00]     ;wait mo
        ld      a,[$ff00]     ;wait mo
        ld      a,[$ff00]     ;wait mo
        ld      a,[$ff00]     ;wait mo
        ld      a,[$ff00]     ;wait mo
        cpl
        and     $0f
        swap    a
        or      b             ;a has all buttons
        ld      [curJoy1],a
        ld      a,$30         ;deselect P14 and P15
        ld      [$ff00],a

        pop     bc
        ret

;---------------------------------------------------------------------
; Routine:      WaitKey
; Arguments:    a - bits to wait for
; Returns:      a - button pressed
; Alters:       af
; Description:  Waits until all specified bits are not pressed, then
;               waits till one is pressed, then waits till all are
;               released before returning.
;---------------------------------------------------------------------
WaitKey:
        push    bc
        ld      b,a

.waitRelease1
        call    GetInput
        ld      a,[curJoy1]
        and     b
        jr      nz,.waitRelease1

.waitPress
        call    GetInput
        ld      a,[curJoy1]
        and     b
        jr      z,.waitPress

        ld      c,a

.waitRelease2
        call    GetInput
        ld      a,[curJoy1]
        and     b
        jr      nz,.waitRelease2

        ld      c,a

        pop     bc
        ret


;---------------------------------------------------------------------
; Routine:      ShowInfo
; Description:  Displays startup info & waits for a keypress
;---------------------------------------------------------------------
ShowInfo:
        call    ClearScreen

        MPRINT  .introText1,0,0,1
        MPRINT  .introText2,0,1,1
        MPRINT  .introText2_1,0,2,0
        MPRINT  .introText3,0,4,0
        MPRINT  .introText4,0,5,0
        MPRINT  .introText5,0,7,0
        MPRINT  .introText6,0,10,1
        MPRINT  .introText7,0,11,1
        MPRINT  .introText8,0,12,1
        MPRINT  .introText9,0,16,0
        MPRINT  .introText10,0,17,0
        UPDATESCREEN

        ld      a,BUTTON_ALL
        call    WaitKey
        ret

.introText1      DB      "   GameBoy Sound    ",0
.introText2      DB      "    Manipulator     ",0
.introText2_1    DB      "       v1.1.1       ",0
.introText3      DB "   ",15,"2022.01.08 by   ",0
.introText4      DB      "Abe.Pralle@gmail.com",0
.introText5      DB      "                    ",0
.introText6      DB      "SELECT = change type",0
.introText7      DB      " D-PAD = settings   ",0
.introText8      DB      "     A = play sound ",0
.introText9      DB      " May be freely used ",0
.introText10     DB      "  and distributed   ",0

;---------------------------------------------------------------------
; Routine:      OnState1
; Alters:       All registers.
; Description:  Allows fiddling with sound 1 parameters
;---------------------------------------------------------------------
OnState1:
        call    ClearScreen
        MPRINT  .state1Text1,0,0,1
        MPRINT  .state1Text2,0,16,2

        xor     a
        ld      [cursor_x],a
        inc     a
        ld      [cursor_y],a

        ld      c,0       ;display 5 bytes
        ld      hl,state1_data

.loop
        ld      a,[hl+]
        call    DrawByte
        inc     c
        ld      a,c
        cp      5
        jr      nz,.loop

        ld      hl,state1_data
        ld      d,0
        ld      a,[curByte]
        ld      e,a
        add     hl,de
        ld      a,[hl]
        call    DrawBinary

        ;what is the cursor highlighting?
        ld      a,[curByte]
        or      a
        jr      nz,.checkState1

        ;state 0
        ld      a,[curByteMask]
        cp      %01110000
        jr      nz,.check0_3

        ;state 0_64
        ld      a,0
        call    PrintInfo
        ld      a,%00001000
        ld      b,0
        call    NewMaskIfRight
        ld      a,%00000111
        ld      b,4
        call    NewMaskIfLeft
        jp      .done

.check0_3
        cp      %00001000
        jr      nz,.check0_20

        ;state 0_3
        ld      a,1
        call    PrintInfo
        ld      a,%00000111
        ld      b,0
        call    NewMaskIfRight
        ld      a,%01110000
        ld      b,0
        call    NewMaskIfLeft
        jp      .done

.check0_20
        ld      a,2
        call    PrintInfo
        ld      a,%11000000
        ld      b,1
        call    NewMaskIfRight
        ld      a,%00001000
        ld      b,0
        call    NewMaskIfLeft
        jp      .done

.checkState1
        cp      1
        jr      nz,.checkState2

        ld      a,[curByteMask]
        cp      %11000000
        jr      nz,.check1_50

        ;mask 1_76
        ld      a,3
        call    PrintInfo
        ld      a,%00111111
        ld      b,1
        call    NewMaskIfRight
        ld      a,%00000111
        ld      b,0
        call    NewMaskIfLeft
        jp      .done

.check1_50
        ld      a,4
        call    PrintInfo
        ld      a,%11110000
        ld      b,2
        call    NewMaskIfRight
        ld      a,%11000000
        ld      b,1
        call    NewMaskIfLeft
        jp      .done

.checkState2
        cp      2
        jr      nz,.checkState3

        ld      a,[curByteMask]
        cp      %11110000
        jr      nz,.check2_3

        ;state 2_74
        ld      a,5
        call    PrintInfo
        ld      a,%00001000
        ld      b,2
        call    NewMaskIfRight
        ld      a,%00011111
        ld      b,1
        call    NewMaskIfLeft
        jp      .done

.check2_3
        cp      %00001000
        jr      nz,.check2_20

        ld      a,6
        call    PrintInfo
        ld      a,%00000111
        ld      b,2
        call    NewMaskIfRight
        ld      a,%11110000
        ld      b,2
        call    NewMaskIfLeft
        jp      .done


.check2_20
        ld      a,7
        call    PrintInfo
        ld      a,%11110000
        ld      b,3
        call    NewMaskIfRight
        ld      a,%00001000
        ld      b,2
        call    NewMaskIfLeft
        jp      .done

.checkState3
        cp      3
        jr      nz,.checkState4

        ld      a,[curByteMask]
        cp      %11110000
        jr      nz,.check3_30

        ;state 3_74
        ld      a,8
        call    PrintInfo
        ld      a,%00001111
        ld      b,3
        call    NewMaskIfRight
        ld      a,%00000111
        ld      b,2
        call    NewMaskIfLeft
        jp      .done

.check3_30
        ld      a,9
        call    PrintInfo
        ld      a,%01000000
        ld      b,4
        call    NewMaskIfRight
        ld      a,%11110000
        ld      b,3
        call    NewMaskIfLeft
        jr      .done

.checkState4
        ld      a,[curByteMask]
        cp      %01000000
        jr      nz,.check4_20

        ;state 4_6
        ld      a,10
        call    PrintInfo
        ld      a,%00000111
        ld      b,4
        call    NewMaskIfRight
        ld      a,%00001111
        ld      b,3
        call    NewMaskIfLeft
        jr      .done

.check4_20
        ld      a,11
        call    PrintInfo
        ld      a,%11100000
        ld      b,0
        call    NewMaskIfRight
        ld      a,%01000000
        ld      b,4
        call    NewMaskIfLeft

.done
        UPDATESCREEN
        ret

.state1Text1    DB "Sound 1: Quad+Sweep ",0
.state1Text2    DB "REGS FF10-FF14      ",0

;---------------------------------------------------------------------
; Routine:      OnState2
; Alters:       All registers.
; Description:  Allows fiddling with sound 2 parameters
;---------------------------------------------------------------------
OnState2:
        call    ClearScreen
        MPRINT  .state2Text1,0,0,1
        MPRINT  .state2Text2,0,16,2

        xor     a
        ld      [cursor_x],a
        inc     a
        ld      [cursor_y],a

        ld      c,0       ;display 4 bytes
        ld      hl,state2_data

.loop
        ld      a,[hl+]
        call    DrawByte
        inc     c
        ld      a,c
        cp      4
        jr      nz,.loop

        ld      hl,state2_data
        ld      d,0
        ld      a,[curByte]
        ld      e,a
        add     hl,de
        ld      a,[hl]
        call    DrawBinary

        ;what is the cursor highlighting?
        ld      a,[curByte]
        or      a
        jr      nz,.checkState1

.checkState0
        ld      a,[curByteMask]
        cp      %11000000
        jr      nz,.check0_50

        ;mask 0_76
        ld      a,3
        call    PrintInfo
        ld      a,%00111111
        ld      b,0
        call    NewMaskIfRight
        ld      a,%00000111
        ld      b,3
        call    NewMaskIfLeft
        jp      .done

.check0_50
        ld      a,4
        call    PrintInfo
        ld      a,%11110000
        ld      b,1
        call    NewMaskIfRight
        ld      a,%11000000
        ld      b,0
        call    NewMaskIfLeft
        jp      .done

.checkState1
        cp      1
        jr      nz,.checkState2

        ld      a,[curByteMask]
        cp      %11110000
        jr      nz,.check1_3

        ;state 1_74
        ld      a,5
        call    PrintInfo
        ld      a,%00001000
        ld      b,1
        call    NewMaskIfRight
        ld      a,%00011111
        ld      b,0
        call    NewMaskIfLeft
        jp      .done

.check1_3
        cp      %00001000
        jr      nz,.check1_20

        ld      a,6
        call    PrintInfo
        ld      a,%00000111
        ld      b,1
        call    NewMaskIfRight
        ld      a,%11110000
        ld      b,1
        call    NewMaskIfLeft
        jp      .done


.check1_20
        ld      a,7
        call    PrintInfo
        ld      a,%11110000
        ld      b,2
        call    NewMaskIfRight
        ld      a,%00001000
        ld      b,1
        call    NewMaskIfLeft
        jp      .done

.checkState2
        cp      2
        jr      nz,.checkState3

        ld      a,[curByteMask]
        cp      %11110000
        jr      nz,.check2_30

        ;state 2_74
        ld      a,8
        call    PrintInfo
        ld      a,%00001111
        ld      b,2
        call    NewMaskIfRight
        ld      a,%00000111
        ld      b,1
        call    NewMaskIfLeft
        jp      .done

.check2_30
        ld      a,9
        call    PrintInfo
        ld      a,%01000000
        ld      b,3
        call    NewMaskIfRight
        ld      a,%11110000
        ld      b,2
        call    NewMaskIfLeft
        jr      .done

.checkState3
        ld      a,[curByteMask]
        cp      %01000000
        jr      nz,.check3_20

        ;state 3_6
        ld      a,10
        call    PrintInfo
        ld      a,%00000111
        ld      b,3
        call    NewMaskIfRight
        ld      a,%00001111
        ld      b,2
        call    NewMaskIfLeft
        jr      .done

.check3_20
        ld      a,11
        call    PrintInfo
        ld      a,%11100000
        ld      b,0
        call    NewMaskIfRight
        ld      a,%01000000
        ld      b,3
        call    NewMaskIfLeft

.done
        UPDATESCREEN
        ret

.state2Text1    DB "Sound 2: Quadrangulr",0
.state2Text2    DB "REGS FF16-FF19      ",0

;---------------------------------------------------------------------
; Routine:      OnState3
; Alters:       All registers.
; Description:  Allows fiddling with sound 3 parameters
;---------------------------------------------------------------------
OnState3:
        call    ClearScreen
        MPRINT  .state3Text1,0,0,1
        MPRINT  .state3Text2,0,16,2

        xor     a
        ld      [cursor_x],a
        inc     a
        ld      [cursor_y],a

        ld      c,0       ;display 5 bytes
        ld      hl,state3_data

.loop
        ld      a,[hl+]
        call    DrawByte
        inc     c
        ld      a,c
        cp      5
        jr      nz,.loop

        ld      hl,state3_data
        ld      d,0
        ld      a,[curByte]
        ld      e,a
        add     hl,de
        ld      a,[hl]
        call    DrawBinary

        ;draw first line of waveform bytes
        xor     a
        ld      [cursor_x],a
        ld      a,5
        ld      [cursor_y],a

        ld      c,5       ;display next 8 bytes
        ld      hl,state3_data+5

.loop2
        ld      a,[hl+]
        call    DrawByte
        ld      a,[cursor_x]   ;prevent space between bytes
        dec     a
        ld      [cursor_x],a
        inc     c
        ld      a,c
        cp      13
        jr      nz,.loop2

        ;draw second line of waveform bytes
        xor     a
        ld      [cursor_x],a
        ld      a,6
        ld      [cursor_y],a

        ld      c,13       ;display next 8 bytes
        ld      hl,state3_data+13

.loop3
        ld      a,[hl+]
        call    DrawByte
        ld      a,[cursor_x]   ;prevent space between bytes
        dec     a
        ld      [cursor_x],a
        inc     c
        ld      a,c
        cp      21
        jr      nz,.loop3

        ;draw the graphic waveform box
        MPRINT  .state3BoxTop,16,5,3
        MPRINT  .state3BoxBottom,16,6,3

        ;what is the cursor highlighting?
        ld      a,[curByte]
        ;note: will never be byte zero
        cp      1
        jr      nz,.checkState2

        ld      a,[curByteMask]
        cp      %11110000
        jr      nz,.check1_30

        ;state 1_74
        ld      a,15
        call    PrintInfo
        ld      a,%00001111
        ld      b,1
        call    NewMaskIfRight
        ld      a,%00001111
        ld      b,20
        call    NewMaskIfLeft
        jp      .done

.check1_30
        ld      a,16
        call    PrintInfo
        ld      a,%01100000
        ld      b,2
        call    NewMaskIfRight
        ld      a,%11110000
        ld      b,1
        call    NewMaskIfLeft
        jp      .done

.checkState2
        cp      2
        jr      nz,.checkState3

        ;state 2_65
        ld      a,17
        call    PrintInfo
        ld      a,%11110000
        ld      b,3
        call    NewMaskIfRight
        ld      a,%00001111
        ld      b,1
        call    NewMaskIfLeft
        jp      .done

.checkState3
        cp      3
        jr      nz,.checkState4

        ld      a,[curByteMask]
        cp      %11110000
        jr      nz,.check3_30

        ;state 3_74
        ld      a,18
        call    PrintInfo
        ld      a,%00001111
        ld      b,3
        call    NewMaskIfRight
        ld      a,%01100000
        ld      b,2
        call    NewMaskIfLeft
        jp      .done

.check3_30
        ld      a,19
        call    PrintInfo
        ld      a,%01000000
        ld      b,4
        call    NewMaskIfRight
        ld      a,%11110000
        ld      b,3
        call    NewMaskIfLeft
        jp      .done

.checkState4
        cp      4
        jr      nz,.check5

        ld      a,[curByteMask]
        cp      %01000000
        jr      nz,.check4_20

        ;4_6
        ld      a,20
        call    PrintInfo
        ld      a,%00000111
        ld      b,4
        call    NewMaskIfRight
        ld      a,%00001111
        ld      b,3
        call    NewMaskIfLeft
        jp      .done

.check4_20
        ld      a,21
        call    PrintInfo
        ld      a,%11110000
        ld      b,5
        call    NewMaskIfRight
        ld      a,%01000000
        ld      b,4
        call    NewMaskIfLeft
        jp      .done

.check5
        cp      5
        jr      nz,.check20

        call    TestWaveButtonB
        ld      a,[curByteMask]
        cp      %11110000
        jr      nz,.check5_30

        ;check5_74
        ld      a,22
        call    PrintInfo
        ld      a,%00001111
        ld      b,5
        call    NewMaskIfRight
        ld      a,%00000111
        ld      b,4
        call    NewMaskIfLeft
        jp      .done

.check5_30
        ld      a,22
        call    PrintInfo
        ld      a,%11110000
        ld      b,6
        call    NewMaskIfRight
        ld      a,%11110000
        ld      b,5
        call    NewMaskIfLeft
        jp      .done

.check20
        cp      20
        jr      nz,.check6_to_19

        call    TestWaveButtonB
        ld      a,[curByteMask]
        cp      %11110000
        jr      nz,.check20_30

        ;check20_74
        ld      a,22
        call    PrintInfo
        ld      a,%00001111
        ld      b,20
        call    NewMaskIfRight
        ld      a,%00001111
        ld      b,19
        call    NewMaskIfLeft
        jr      .done

.check20_30
        ld      a,22
        call    PrintInfo
        ld      a,%11110000
        ld      b,1
        call    NewMaskIfRight
        ld      a,%11110000
        ld      b,20
        call    NewMaskIfLeft
        jr      .done

.check6_to_19
        call    TestWaveButtonB
        ld      a,[curByteMask]
        cp      %11110000
        jr      nz,.check6_to_19_30

        ;6_to_19_74
        ld      a,22
        call    PrintInfo
        ld      a,[curByte]
        ld      b,a
        ld      a,%00001111
        call    NewMaskIfRight
        ld      a,[curByte]
        dec     a
        ld      b,a
        ld      a,%00001111
        call    NewMaskIfLeft
        jr      .done

.check6_to_19_30
        ld      a,22
        call    PrintInfo
        ld      a,[curByte]
        inc     a
        ld      b,a
        ld      a,%11110000
        call    NewMaskIfRight
        ld      a,[curByte]
        ld      b,a
        ld      a,%11110000
        call    NewMaskIfLeft

.done
        call    RestoreWaveBox
        call    PlotWaveForm

        ld      a,1
        ld      [copyWaveChars],a

        UPDATESCREEN
        ret

.state3Text1     DB "Sound 3: Custom Wave",0
.state3Text2     DB "REGS FF1A-1E,FF30-3F",0
.state3BoxTop    DB 5,6,7,8,0
.state3BoxBottom DB 9,10,11,12,0

;---------------------------------------------------------------------
; Routine:      TestWaveButtonB
; Alters:       af
; Description:  If the B button is pressed, copies the current nibble
;               of wave data to the next one and sets the current
;               byte/nibble to be the next value
;---------------------------------------------------------------------
TestWaveButtonB:
        ld      a,[curJoy1]    ;test for B pressed
        and     BUTTON_B
        ret     z

        ;release button b
.waitRelease
        call    GetInput
        ld      a,[curJoy1]
        and     BUTTON_B
        jr      nz,.waitRelease

;start of routine proper
        push    bc
        push    de
        push    hl

        ;if the last byte then copy the first byte into the pad byte
        ;following
        ld      a,[curByte]
        cp      20
        jr      nz,.determineNibble

        ld      a,[state3_data+5]
        ld      [state3_data+21],a

.determineNibble
        ld      a,[curByteMask]
        cp      %00001111
        jr      z,.lowNibble

        ;high nibble being copied to low nibble
        call    GetCurByteValue
        and     %11110000
        ld      b,a
        swap    b
        or      b
        ld      [hl],a
        ld      a,%00001111
        ld      [curByteMask],a
        jr      .done

.lowNibble      ;being copied to high nibble of next byte
        call    GetCurByteValue
        and     %00001111
        swap    a
        ld      b,a
        inc     hl
        ld      a,[hl]
        and     %00001111
        or      b
        ld      [hl],a
        ld      a,%11110000
        ld      [curByteMask],a
        ld      a,[curByte]
        inc     a
        ld      [curByte],a

        ;did we run off the end into the pad byte?
        ld      a,[curByte]
        cp      21
        jr      nz,.done

        ;if so copy that pad byte back to the beginning
        ld      a,[state3_data+21]
        ld      [state3_data+5],a
        ld      a,5
        ld      [curByte],a

.done
        call    PlayCurrentSound

        pop     hl
        pop     de
        pop     bc
        ret

;---------------------------------------------------------------------
; Routine:      OnState4
; Alters:       All registers.
; Description:  Allows fiddling with sound 4 parameters
;---------------------------------------------------------------------
OnState4:
        call    ClearScreen
        MPRINT  .state4Text1,0,0,1
        MPRINT  .state4Text2,0,16,2

        xor     a
        ld      [cursor_x],a
        inc     a
        ld      [cursor_y],a

        ld      c,0       ;display 4 bytes
        ld      hl,state4_data

.loop
        ld      a,[hl+]
        call    DrawByte
        inc     c
        ld      a,c
        cp      4
        jr      nz,.loop

        ld      hl,state4_data
        ld      d,0
        ld      a,[curByte]
        ld      e,a
        add     hl,de
        ld      a,[hl]
        call    DrawBinary

        ;what is the cursor highlighting?
        ld      a,[curByte]
        or      a
        jr      nz,.checkState1

.check0_50
        ld      a,4
        call    PrintInfo
        ld      a,%11110000
        ld      b,1
        call    NewMaskIfRight
        ld      a,%01000000
        ld      b,3
        call    NewMaskIfLeft
        jp      .done

.checkState1
        cp      1
        jr      nz,.checkState2

        ld      a,[curByteMask]
        cp      %11110000
        jr      nz,.check1_3

        ;state 1_74
        ld      a,5
        call    PrintInfo
        ld      a,%00001000
        ld      b,1
        call    NewMaskIfRight
        ld      a,%00011111
        ld      b,0
        call    NewMaskIfLeft
        jp      .done

.check1_3
        cp      %00001000
        jr      nz,.check1_20

        ld      a,6
        call    PrintInfo
        ld      a,%00000111
        ld      b,1
        call    NewMaskIfRight
        ld      a,%11110000
        ld      b,1
        call    NewMaskIfLeft
        jp      .done


.check1_20
        ld      a,7
        call    PrintInfo
        ld      a,%11110000
        ld      b,2
        call    NewMaskIfRight
        ld      a,%00001000
        ld      b,1
        call    NewMaskIfLeft
        jp      .done

.checkState2
        cp      2
        jr      nz,.checkState3

        ld      a,[curByteMask]
        cp      %11110000
        jr      nz,.check2_3

        ;state 2_74
        ld      a,12
        call    PrintInfo
        ld      a,%00001000
        ld      b,2
        call    NewMaskIfRight
        ld      a,%00000111
        ld      b,1
        call    NewMaskIfLeft
        jp      .done

.check2_3
        cp      %00001000
        jr      nz,.check2_20

        ld      a,13
        call    PrintInfo
        ld      a,%00000111
        ld      b,2
        call    NewMaskIfRight
        ld      a,%11110000
        ld      b,2
        call    NewMaskIfLeft
        jp      .done

.check2_20
        ld      a,14
        call    PrintInfo
        ld      a,%01000000
        ld      b,3
        call    NewMaskIfRight
        ld      a,%00001000
        ld      b,2
        call    NewMaskIfLeft
        jp      .done

.checkState3
        ;state 3_6
        ld      a,10
        call    PrintInfo
        ld      a,%00111111
        ld      b,0
        call    NewMaskIfRight
        ld      a,%00000111
        ld      b,2
        call    NewMaskIfLeft

.done
        UPDATESCREEN
        ret

.state4Text1    DB "Sound 4: White Noise",0
.state4Text2    DB "REGS FF20-FF23      ",0

;---------------------------------------------------------------------
; Routine:      DrawByte
; Arguments:    a - byte to show
;               c - index of byte
; Alters:       af
; Description:  Displays byte A in hex form at the current cursor
;               position.  Highlights the byte or a nibble if the
;               cursor is over it.
;---------------------------------------------------------------------
DrawByte:
        push    bc
        push    de
        push    hl

        ld      b,0               ;highlight assumed false
        ld      e,a               ;save byte

        ld      a,[curByte]       ;are we drawing byte under cursor?
        cp      c
        jr      nz,.after_highlight

;highlight which nibbles?
        ld      a,[curByteMask]
        and     %11110000
        jr      z,.checkHighlightLowNibble

        set     1,b

.checkHighlightLowNibble
        ld      a,[curByteMask]
        and     %00001111
        jr      z,.after_highlight

        set     0,b

.after_highlight
        ld      a,e          ;retrieve byte to draw
        swap    a
        and     %00001111    ;draw high nibble
        push    bc
        srl     b
        call    DrawNibble
        pop     bc

        res     1,b          ;make sure high-nibble highlight flag zero
        ld      a,e
        and     %00001111    ;draw low nibble
        call    DrawNibble

        ;draw a blank space
        xor     a
        ld      [textColor],a

        ld      a,[cursor_x]
        ld      b,a
        inc     a
        ld      [cursor_x],a

        ld      a,[cursor_y]
        ld      c,a
        ld      hl,.blankSpace
        call    PrintText

        pop     hl
        pop     de
        pop     bc
        ret

.blankSpace     DB " ",0

;---------------------------------------------------------------------
; Routine:      DrawNibble
; Arguments:    a - nibble to draw
;               b - 1=highlight, 0=normal
; Alters:       af
; Description:  Displays nibble A in hex form at the current cursor
;               position.  Highlights the byte or a nibble if the
;               cursor is over it.
;---------------------------------------------------------------------
DrawNibble:
        push    bc
        push    de
        push    hl

        ;load hl with ptr to string to draw
        ld      hl,.hexTable
        rlca
        add     l
        ld      l,a
        ld      a,0
        adc     h
        ld      h,a

        ;handle highlight
        ld      a,b
        ld      [textColor],a

        ;get coordinates to draw at
        ld      a,[cursor_x]
        ld      b,a
        inc     a
        ld      [cursor_x],a

        ld      a,[cursor_y]
        ld      c,a

        ;draw it
        call    PrintText

        pop     hl
        pop     de
        pop     bc
        ret

.hexTable
        DB "0",0
        DB "1",0
        DB "2",0
        DB "3",0
        DB "4",0
        DB "5",0
        DB "6",0
        DB "7",0
        DB "8",0
        DB "9",0
        DB "A",0
        DB "B",0
        DB "C",0
        DB "D",0
        DB "E",0
        DB "F",0

;---------------------------------------------------------------------
; Routine:      DrawBinary
; Arguments:    a - byte to draw in binary form
;               [curByteMask] - bits to highlight
; Alters:       af
; Description:  Displays selected byte in binary form
;---------------------------------------------------------------------
DrawBinary:
        push    bc
        push    de
        push    hl

        ld      b,a     ;save data for later
        ld      a,[curByteMask]
        ld      d,a     ;save mask for later

        MPRINT  .percent,0,3,0

        ld      c,8     ;draw 8 bits

.loop   rlc     b       ;rotate b left, old bit 7 to carry
        jr      c,.drawOne

        ;draw zero
        rlc     d       ;look at corresponding bit of mask
        jr      c,.drawZeroHighlighted

        PRINTCURSOR   .zero,0
        jr      .continue

.drawZeroHighlighted
        PRINTCURSOR   .zero,1
        jr      .continue


.drawOne
        rlc     d
        jr      c,.drawOneHighlighted

        PRINTCURSOR   .one,0
        jr      .continue

.drawOneHighlighted
        PRINTCURSOR   .one,1

.continue
        dec     c
        jr      nz,.loop

        pop     hl
        pop     de
        pop     bc
        ret

.percent        DB  "%",0
.zero           DB  "0",0
.one            DB  "1",0

;---------------------------------------------------------------------
; Routines:     IncrementBits
;               DecrementBits
; Alters:       af
; Description:  Adds or subtracts one to [curByteMask] portion of
;               [curByte].
;---------------------------------------------------------------------
IncrementBits:
        push    bc

        ;make mask for value to increment
        ld      a,[curByteMask]
        ld      b,a
        ld      c,a
        sla     a
        xor     b
        and     b
        ld      b,a    ;b has one bit set = proper value to increment

        call    GetCurByteValue
        add     b
        and     c
        ld      b,a
        ld      a,$ff
        xor     c
        and     [hl]
        or      b
        ld      [hl],a

        ;prevent prohibited values for sound 4
        ld      a,[curSound]
        cp      4
        jr      nz,.playSound

        ld      a,[curByte]
        cp      2
        jr      nz,.playSound

        ld      a,[curByteMask]
        cp      %11110000
        jr      nz,.playSound

        ld      a,[hl]
        and     %11100000
        xor     %11100000
        jr      nz,.playSound

        ld      a,%00001111   ;fix prohibited value
        and     [hl]
        ld      [hl],a


.playSound
        call    PlayCurrentSound

.waitRelease
        call    GetInput
        ld      a,[curJoy1]
        and     DPAD_UP
        jr      nz,.waitRelease

        pop     bc
        ret

DecrementBits:
        push    bc

        ;make mask for value to increment
        ld      a,[curByteMask]
        ld      b,a
        ld      c,a
        sla     a
        xor     b
        and     b
        ld      b,a    ;b has one bit set = proper value to increment

        call    GetCurByteValue
        sub     b
        and     c
        ld      b,a
        ld      a,$ff
        xor     c
        and     [hl]
        or      b
        ld      [hl],a

        ;prevent prohibited values for sound 4
        ld      a,[curSound]
        cp      4
        jr      nz,.playSound

        ld      a,[curByte]
        cp      2
        jr      nz,.playSound

        ld      a,[curByteMask]
        cp      %11110000
        jr      nz,.playSound

        ld      a,[hl]
        and     %11100000
        xor     %11100000
        jr      nz,.playSound

        ld      a,%00001111   ;fix prohibited value
        and     [hl]
        or      %11010000
        ld      [hl],a

.playSound
        call    PlayCurrentSound

.waitRelease
        call    GetInput
        ld      a,[curJoy1]
        and     DPAD_DOWN
        jr      nz,.waitRelease

        pop     bc
        ret

;---------------------------------------------------------------------
; Routine:      GetCurByteValue
; Returns:      a - selected byte value
;               hl - addr of selected value
; Alters:       af,hl
;---------------------------------------------------------------------
GetCurByteValue:
        push    de

        ld      a,[curByte]
        ld      e,a
        ld      d,0

        ld      a,[curSound]
        cp      1
        jr      nz,.checkState2

        ld      hl,state1_data
        jr      .done

.checkState2
        cp      2
        jr      nz,.checkState3

        ld      hl,state2_data
        jr      .done

.checkState3
        cp      3
        jr      nz,.checkState4

        ld      hl,state3_data
        jr      .done

.checkState4
        ld      hl,state4_data

.done
        add     hl,de
        ld      a,[hl]

        pop     de
        ret

;---------------------------------------------------------------------
; Routine:      PlayCurrentSound
; Alters:       af
;---------------------------------------------------------------------
PlayCurrentSound:
        push    bc
        push    de
        push    hl

        ld      a,[curSound]
        cp      1
        jr      nz,.checkState2

        ld      hl,state1_data
        ld      a,[hl+]
        ld      [$ff10],a
        ld      a,[hl+]
        ld      [$ff11],a
        ld      a,[hl+]
        ld      [$ff12],a
        ld      a,[hl+]
        ld      [$ff13],a
        ld      a,[hl+]
        ld      [$ff14],a
        jr      .done

.checkState2
        cp      2
        jr      nz,.checkState3

        ld      hl,state2_data
        ld      a,[hl+]
        ld      [$ff16],a
        ld      a,[hl+]
        ld      [$ff17],a
        ld      a,[hl+]
        ld      [$ff18],a
        ld      a,[hl+]
        ld      [$ff19],a
        jr      .done

.checkState3
        cp      3
        jr      nz,.checkState4

        xor     a
        ld      [$ff1a],a        ;turn off sound

        ;copy waveform data to $ff30-$ff40
        ld      c,$30
        ld      hl,state3_data+5
.copyWave
        ld      a,[hl+]
        ld      [c],a
        inc     c
        ld      a,c
        cp      $40
        jr      nz,.copyWave

        ld      hl,state3_data
        ld      a,[hl+]
        ld      [$ff1a],a
        ld      a,[hl+]
        ld      [$ff1b],a
        ld      a,[hl+]
        ld      [$ff1c],a
        ld      a,[hl+]
        ld      [$ff1d],a
        ld      a,[hl+]
        ld      [$ff1e],a

        jr      .done

.checkState4
        ld      hl,state4_data
        ld      a,[hl+]
        ld      [$ff20],a
        ld      a,[hl+]
        ld      [$ff21],a
        ld      a,[hl+]
        ld      [$ff22],a
        ld      a,[hl+]
        ld      [$ff23],a

.done
        pop     hl
        pop     de
        pop     bc
        ret

;---------------------------------------------------------------------
; Routines:     NewMaskIfRight
;               NewMaskIfLeft
; Arguments:    a - new mask if DPAD right/left is pressed
;               b - new byte if DPAD right/left is pressed
; Alters:       af
;---------------------------------------------------------------------
NewMaskIfRight:
        push    bc

        ld      c,a
        ld      a,[curJoy1]
        and     DPAD_RIGHT
        jr      nz,NewMaskCommon

        pop     bc
        ret

NewMaskCommon:
        ;pressed!
        ld      a,b
        ld      [curByte],a

        ld      a,c
        ld      [curByteMask],a

.waitRelease
        call    GetInput
        ld      a,[curJoy1]
        and     (DPAD_RIGHT | DPAD_LEFT)
        jr      nz,.waitRelease

        pop     bc
        ret

NewMaskIfLeft:
        push    bc

        ld      c,a
        ld      a,[curJoy1]
        and     DPAD_LEFT
        jr      nz,NewMaskCommon

        pop     bc
        ret

;---------------------------------------------------------------------
; Routine:      PrintInfo
; Arguments:    a - index of info to print
; Alters:       af
; Description:  Prints helpful text to the screen depending on which
;               byte+mask is selected
;---------------------------------------------------------------------
PrintInfo:
        push    bc
        push    de
        push    hl

        call    .printInfoBody
        MPRINT  .selectString,0,17,2

        pop     hl
        pop     de
        pop     bc
        ret
.selectString   DB "SELECT=change sounds",0

.printInfoBody
.check0
        or      a
        jr      z,.print0
        jp      .check1
.print0
        MPRINT  .t0_1,0,5,1
        MPRINT  .t0_2,0,6,2
        MPRINT  .t0_3,0,7,2
        MPRINT  .t0_4,0,8,2
        MPRINT  .t0_5,0,9,2
        MPRINT  .t0_6,0,10,2
        MPRINT  .t0_7,0,11,2
        MPRINT  .t0_8,0,12,2
        MPRINT  .t0_9,0,13,2
        ret


.check1
        cp      1
        jr      nz,.check2
        MPRINT  .t1_1,0,5,1
        MPRINT  .t1_2,0,6,2
        MPRINT  .t1_3,0,7,2
        ret


.check2
        cp      2
        jr      nz,.check3

        MPRINT  .t2_1,0,5,1
        MPRINT  .t2_2,0,6,2
        MPRINT  .t2_3,0,7,2
        MPRINT  .t2_4,0,8,2
        ret


.check3
        cp      3
        jr      nz,.check4
        MPRINT  .t3_1,0,5,1
        MPRINT  .t3_2,0,6,2
        MPRINT  .t3_3,0,7,2
        MPRINT  .t3_4,0,8,2
        MPRINT  .t3_5,0,9,2
        ret


.check4
        cp      4
        jr      nz,.check5
        MPRINT  .t4_1,0,5,1
        MPRINT  .t4_2,0,6,2
        MPRINT  .t4_3,0,8,2
        MPRINT  .t4_4,0,9,2
        ret


.check5
        cp      5
        jr      nz,.check6
        MPRINT  .t5_1,0,5,1
        MPRINT  .t5_2,0,6,2
        MPRINT  .t5_3,0,7,2
        ret


.check6
        cp      6
        jr      nz,.check7
        MPRINT  .t6_1,0,5,1
        MPRINT  .t6_2,0,6,2
        MPRINT  .t6_3,0,7,2
        ret


.check7
        cp      7
        jr      nz,.check8
        MPRINT  .t7_1,0,5,1
        MPRINT  .t7_2,0,6,2
        MPRINT  .t7_3,0,7,2
        MPRINT  .t7_4,0,8,2
        MPRINT  .t7_5,0,9,2
        ret


.check8
        cp      8
        jr      nz,.check9
        MPRINT  .t8_1,0,5,1
        MPRINT  .t8_2,0,6,2
        MPRINT  .t8_3,0,7,2
        MPRINT  .t8_4,0,9,2
        MPRINT  .t8_5,0,10,2
        ret


.check9
        cp      9
        jr      nz,.check10
        MPRINT  .t9_1,0,5,1
        MPRINT  .t9_2,0,6,2
        MPRINT  .t9_3,0,7,2
        MPRINT  .t9_4,0,9,2
        MPRINT  .t9_5,0,10,2
        ret


.check10
        cp      10
        jr      nz,.check11
        MPRINT  .t10_1,0,5,1
        MPRINT  .t10_2,0,6,2
        MPRINT  .t10_3,0,7,2
        ret


.check11
        cp      11
        jr      nz,.check12
        MPRINT  .t11_1,0,5,1
        MPRINT  .t11_2,0,6,2
        MPRINT  .t11_3,0,7,2
        MPRINT  .t11_4,0,9,2
        MPRINT  .t11_5,0,10,2
        ret

.check12
        cp      12
        jr      z,.print12
        jp      .check13

.print12
        MPRINT  .t12_1,0,5,1
        MPRINT  .t12_2,0,6,2
        MPRINT  .t12_3,0,7,2
        MPRINT  .t12_4,0,9,2
        MPRINT  .t12_5,0,10,2
        MPRINT  .t12_6,0,11,2
        MPRINT  .t12_7,0,12,2
        MPRINT  .t12_8,0,13,2
        ret

.check13
        cp      13
        jr      nz,.check14
        MPRINT  .t13_1,0,5,1
        MPRINT  .t13_2,0,6,2
        MPRINT  .t13_3,0,7,2
        ret

.check14
        cp      14
        jr      z,.print14
        jp      .check15
.print14
        MPRINT  .t14_1,0,5,1
        MPRINT  .t14_2,0,6,2
        MPRINT  .t14_3,0,7,2
        MPRINT  .t14_4,0,8,2
        MPRINT  .t14_5,0,9,2
        MPRINT  .t14_6,0,10,2
        MPRINT  .t14_7,0,11,2
        MPRINT  .t14_8,0,12,2
        MPRINT  .t14_9,0,13,2
        MPRINT  .t14_10,0,14,2
        ret

.check15
        cp      15
        jr      nz,.check16
        MPRINT  .t15_1,0,8,1
        MPRINT  .t15_2,0,9,2
        MPRINT  .t15_3,0,10,2
        MPRINT  .t15_4,0,11,2
        MPRINT  .t15_5,0,12,2
        MPRINT  .t15_6,0,13,2
        ret

.check16
        cp      16
        jr      nz,.check17
        MPRINT  .t16_1,0,8,1
        MPRINT  .t16_2,0,9,2
        MPRINT  .t15_3,0,10,2
        MPRINT  .t15_4,0,11,2
        MPRINT  .t15_5,0,12,2
        MPRINT  .t15_6,0,13,2
        ret

.check17
        cp      17
        jr      nz,.check18
        MPRINT  .t17_1,0,8,1
        MPRINT  .t17_2,0,9,2
        MPRINT  .t17_3,0,10,2
        MPRINT  .t17_4,0,11,2
        MPRINT  .t17_5,0,12,2
        ret

.check18
        cp      18
        jr      nz,.check19
        MPRINT  .t8_1,0,8,1
        MPRINT  .t8_2,0,9,2
        MPRINT  .t8_3,0,10,2
        MPRINT  .t8_4,0,11,2
        MPRINT  .t8_5,0,12,2
        ret

.check19
        cp      19
        jr      nz,.check20
        MPRINT  .t9_1,0,8,1
        MPRINT  .t9_2,0,9,2
        MPRINT  .t9_3,0,10,2
        MPRINT  .t9_4,0,11,2
        MPRINT  .t9_5,0,12,2
        ret

.check20
        cp      20
        jr      nz,.check21
        MPRINT  .t10_1,0,8,1
        MPRINT  .t10_2,0,9,2
        MPRINT  .t10_3,0,10,2
        ret

.check21
        cp      21
        jr      nz,.check22
        MPRINT  .t11_1,0,8,1
        MPRINT  .t11_2,0,9,2
        MPRINT  .t11_3,0,10,2
        MPRINT  .t11_4,0,11,2
        MPRINT  .t11_5,0,12,2
        ret

.check22
        MPRINT  .t22_1,0,8,1
        MPRINT  .t22_2,0,9,2
        MPRINT  .t22_3,0,10,2
        MPRINT  .t22_4,0,11,2
        ret

.t0_1   DB      "Sweep Time          ",0
.t0_2   DB      "000 - off",0
.t0_3   DB      "001 1/128 Hz (short)",0
.t0_4   DB      "010 2/128 Hz",0
.t0_5   DB      "011 3/128 Hz",0
.t0_6   DB      "100 4/128 Hz",0
.t0_7   DB      "101 5/128 Hz",0
.t0_8   DB      "110 6/128 Hz",0
.t0_9   DB      "111 7/128 Hz (long)",0

.t1_1   DB      "Sweep increase/decr ",0
.t1_2   DB      " 0: Freq increases",0
.t1_3   DB      " 1: Freq decreases",0

.t2_1   DB      "# Sweep Shifts      ",0
.t2_2   DB      "000: None",0
.t2_3   DB      "001: 1 (quick shift)",0
.t2_4   DB      "111: 7 (long shift)",0

.t3_1   DB      "Waveform Duty       ",0
.t3_2   DB      "00: 12.5% ",1,2,3,3,3,3,3,3,1,2,0
.t3_3   DB      "01: 25%   ",1,2,3,3,1,2,3,3,1,2,0
.t3_4   DB      "10: 50%   ",1,2,1,2,1,2,1,2,1,2,0
.t3_5   DB      "11: 75%   ",1,4,4,2,1,4,4,2,1,4,0

.t4_1   DB      "Sound Length        ",0
.t4_2   DB      "= (64-t)*(1/256) sec",0
.t4_3   DB      "00000: long  1/4 sec",0
.t4_4   DB      "11111: short 1/256 s",0

.t5_1   DB      "Initial Envelope Vol",0
.t5_2   DB      " 0000: mute",0
.t5_3   DB      " 1111: loud",0

.t6_1   DB      "Envelope Up/Down    ",0
.t6_2   DB      "0: Decrease (quietr)",0
.t6_3   DB      "1: Increase (louder)",0

.t7_1   DB      "# Envelope Steps    ",0
.t7_2   DB      "000: No change (off)",0
.t7_3   DB      "001: Shorter fade",0
.t7_4   DB      " ...",0
.t7_5   DB      "111: Longer fade",0

.t8_1   DB      "Frequency [7:4]     ",0
.t8_2   DB      "Mid 4 bits of",0
.t8_3   DB      "11-bit frequency.",0
.t8_4   DB      "Hz=131072/(2048-gb)",0
.t8_5   DB      "gb=2048-(131072/Hz)",0

.t9_1   DB      "Frequency [3:0]     ",0
.t9_2   DB      "Lower 4 bits of",0
.t9_3   DB      "11-bit frequency.",0
.t9_4   DB      "Hz=131072/(2048-gb)",0
.t9_5   DB      "gb=2048-(131072/Hz)",0

.t10_1  DB      "Consecutive flag    ",0
.t10_2  DB      " 0:  Repeat",0
.t10_3  DB      " 1:  No Repeat",0

.t11_1   DB     "Frequency [11:8]    ",0
.t11_2   DB     "Upper 3 bits of",0
.t11_3   DB     "11-bit frequency.",0
.t11_4   DB     "Hz=131072/(2048-gb)",0
.t11_5   DB     "gb=2048-(131072/Hz)",0

.t12_1   DB     "Shift Clk Frequency ",0
.t12_2   DB     "0000: Ratio * 1/2",0
.t12_3   DB     "0001: Ratio * 1/4",0
.t12_4   DB     "0010: Ratio * 1/8",0
.t12_5   DB     " ...",0
.t12_6   DB     "1101: Ratio * 1/2",13,0  ;char 13 = "14" superscript
.t12_7   DB     "1110: Illegal Code",0
.t12_8   DB     "1111: Illegal Code",0

.t13_1   DB     "Step for Counter    ",0
.t13_2   DB     " 0: 15 steps",0
.t13_3   DB     " 1:  7 steps",0

.t14_1   DB     "Dividing Ratio      ",0
.t14_2   DB     " f=(4.194304/8) Mhz",0
.t14_3   DB     "000: f * 2",0
.t14_4   DB     "001: f * 1",0
.t14_5   DB     "010: f * 1/2",0
.t14_6   DB     "011: f * 1/3",0
.t14_7   DB     "100: f * 1/4",0
.t14_8   DB     "101: f * 1/5",0
.t14_9   DB     "110: f * 1/6",0
.t14_10  DB     "111: f * 1/7",0

.t15_1   DB     "Sound Length [7:4]  ",0
.t15_2   DB     "Upper 4 bits of",0
.t15_3   DB     "8-bit duration.",0
.t15_4   DB     "=(256-t)*(1/256) sec",0
.t15_5   DB     "   0: longer",0
.t15_6   DB     " 255: shorter",0

.t16_1   DB     "Sound Length [3:0]  ",0
.t16_2   DB     "Lower 4 bits of",0

.t17_1   DB     "Output Level        ",0
.t17_2   DB     " 00: Mute ",0
.t17_3   DB     " 01: Normal ",0
.t17_4   DB     " 10: >> 1 ",0
.t17_5   DB     " 11: >> 2 ",0

.t22_1   DB     "Wave Data           ",0
.t22_2   DB     "Press B to copy",0
.t22_3   DB     "current nibble to",0
.t22_4   DB     "next nibble",0

;---------------------------------------------------------------------
; Routine:      RestoreWaveBox
; Alters:       af
; Description:  Restores the wave-form box buffer to its empty state.
;---------------------------------------------------------------------
RestoreWaveBox:
        push    bc
        push    de
        push    hl

        ld      hl,.emptyWaveBox
        ld      de,waveBuffer

        ld      c,16*8

.loop   ld      a,[hl+]
        ld      [de],a
        inc     de
        dec     c
        jr      nz,.loop

        pop     hl
        pop     de
        pop     bc
        ret

.emptyWaveBox
        DW      `22222222
        DW      `20000000
        DW      `20222000
        DW      `20220000
        DW      `20200000
        DW      `20000000
        DW      `20000000
        DW      `20000000

        DW      `22222222
        DW      `00000002
        DW      `00000000
        DW      `00000000
        DW      `00000002
        DW      `00000002
        DW      `00000000
        DW      `00000000

        DW      `22222222,`00000000,`20000000,`20000000
        DW      `00000000,`00000000,`20000000,`20000000

        DW      `22222222
        DW      `00000002
        DW      `00000002
        DW      `00000002
        DW      `00000002
        DW      `00000002
        DW      `00000002
        DW      `00000002

        DW      `20000000
        DW      `20000000
        DW      `20000000
        DW      `20222000
        DW      `20202000
        DW      `20222000
        DW      `20000000
        DW      `22222222

        DW      `00000002
        DW      `00000002
        DW      `00000000
        DW      `00000000
        DW      `00000002
        DW      `00000002
        DW      `00000000
        DW      `22222222

        DW      `00000000, `00000000, `20000000, `20000000
        DW      `00000000, `00000000, `20000000, `22222222

        DW      `00000002
        DW      `00000002
        DW      `00000002
        DW      `00000002
        DW      `00000002
        DW      `00000002
        DW      `00000002
        DW      `22222222

;---------------------------------------------------------------------
; Routine:      PlotWavePoint
; Arguments:    b - x coord of point (0-31)
;               c - y coord of point (0-15)
; Alters:       af
; Description:  Alters the characters 5-8 and 9-12 which form a
;               graphical representation of a waveform by placing a
;               green pixel at the specified position (x,y) away from
;               the BOTTOM left corner of the 3x2 box of characters.
;---------------------------------------------------------------------
PlotWavePoint:
        push    bc
        push    de
        push    hl

        ld      a,c
        and     %00001111
        ld      c,a

        ;reverse vertical axis so that 0 is topmost "normal"
        ld      a,15
        sub     c
        ld      c,a

        ld      hl,waveBuffer

        ;does point lie within top half or bottom half of box?
        ld      a,c
        cp      8
        jr      c,.findHorizontal   ;coords are in top half

        ;bottom half
        ld      de,16*4       ;skip 4 tiles (the top)
        add     hl,de
        sub     8
        ld      c,a

.findHorizontal
        ld      a,b
        cp      8
        jr      c,.foundTile

        ld      de,16
        add     hl,de
        sub     8
        ld      b,a
        jr      .findHorizontal

.foundTile      ;hl now points to tile to change. b is 0-7, c is 0-7
        ld      a,c
        or      a
        jr      z,.atCorrectVertical

        inc     hl
        inc     hl
        dec     c
        jr      .foundTile

.atCorrectVertical
        ;make a bit mask of of c using shifting
        ld      c,%10000000
        ld      a,b
        or      a
        jr      z,.haveMask

.makeMask
        srl     c
        dec     b
        jr      nz,.makeMask

.haveMask
        ld      a,c    ;make green by setting bits to 1 and 1
        or      [hl]
        ld      [hl+],a
        ld      a,c
        or      [hl]
        ld      [hl],a

        pop     hl
        pop     de
        pop     bc
        ret

;---------------------------------------------------------------------
; Routine:      PlotWaveForm
; Alters:       af
; Description:  Uses PlotWavePoint() to plot the sound 3 custom
;               waveform to the box
;---------------------------------------------------------------------
PlotWaveForm:
        push    bc
        push    de
        push    hl

        ;copy the first byte to one past the end to avoid hassle
        ld      a,[state3_data+5]
        ld      [state3_data+21],a

        ld      b,0
        ld      hl,state3_data+5

.loop   ld      a,[hl+]
        ld      c,a
        swap    c
        call    PlotWavePoint
        ld      a,c
        swap    a
        call    ExtendLineToNeighbor
        swap    c
        inc     b
        call    PlotWavePoint
        ld      a,[hl]
        swap    a
        call    ExtendLineToNeighbor

        inc     b
        ld      a,b
        cp      32
        jr      nz,.loop

        pop     hl
        pop     de
        pop     bc
        ret

;---------------------------------------------------------------------
; Routine:      ExtendLineToNeighbor
; Arguments:    b,c - (x,y) coord of line
;               a - y coord of next line
; Alters:       af
; Description:  If cur y is more than 1 different from next y, plots
;               points in the waveform box to close that gap
;---------------------------------------------------------------------
ExtendLineToNeighbor:
        push    bc
        push    de
        push    hl

        and     %00001111        ;restrict A 0-15
        ld      d,a

        ld      a,c
        and     %00001111
        ld      c,a

.testForGap
        ld      a,d            ;dest
        sub     c              ;minus cur
        jr      nc,.haveAbsValue

        cpl                    ;make negative a positive
        add     1

.haveAbsValue
        cp      2
        jr      c,.done        ;less than 2 different then okay

        ;need to adjust c towards d then plot point there
        ld      a,c
        cp      d
        jr      c,.c_lt_d

        ;c > d
        dec     c
        jr      .plot

.c_lt_d
        inc     c

.plot   call    PlotWavePoint
        jr      .testForGap

.done
        pop     hl
        pop     de
        pop     bc
        ret

;---------------------------------------------------------------------
SECTION "fontdata",ROMX
;---------------------------------------------------------------------
font:
  INCBIN "Data/Font.bin"

;---------------------------------------------------------------------
SECTION "HimemSpriteDMAHandler",HRAM[$FF80]
;---------------------------------------------------------------------
SpriteDMAHandler:
        DS      (oamHandlerFinish - oamHandlerStart)
vblankFlag:       DS 1
backBufferReady:  DS 1
