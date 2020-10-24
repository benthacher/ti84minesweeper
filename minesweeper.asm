#INCLUDE "./includes/ti83plus.inc"
#define MINEBIT 4
#define COVEREDBIT 5
#define FLAGBIT 6
#define TILE_SIZE 5


.ORG    $9D93
.DB     t2ByteTok, tAsmCmp ;I have no clue why but it breaks without this line

    b_call(_RunIndicOff)

;---------------------------------------------------------------------

    ; Clear screen
    LD BC, 12*64
    LD HL, PlotSScreen
_ClearScreen:
    DEC BC
    LD (HL), 0
    INC HL
    LD A, B
    OR C
    JR NZ, _ClearScreen
    b_call(_GrBufCpy)
    ; Set up parameters for drawing tile
    ; A = index of tile sprite
    ; D = row
    ; E = column
    LD B, 5
    LD A, B
    LD D, 5
    LD E, 5
    PUSH BC
    CALL DrawTile
    POP BC
    
    DEC B

    LD A, B
    LD D, 10
    LD E, 10
    CALL DrawTile

KeyLoop:
    b_call(_GetKey)
    CP      kClear     ; If the CLEAR key was pressed.
    RET     Z
    JR      KeyLoop

;---------------------------------------------------------------------
; KeyLoop:
;     b_call(_GetKey)
;     CP      kUp        ; If the up arrow key was pressed.
;     JR      Z, Up
;     CP      kDown      ; If the down arrow key was pressed.
;     JR      Z, Down
;     CP      kLeft      ; If the down arrow key was pressed.
;     JR      Z, Left
;     CP      kRight     ; If the down arrow key was pressed.
;     JR      Z, Right
;     CP      kClear     ; If the CLEAR key was pressed.
;     RET     Z
;     JR      KeyLoop    ; If any other key was pressed, redo _GetKey.
; Up:
;     LD      A, (cursorY)
;     ; CP      0        ; dont go up
;     JR      Z, KeyLoop
;     DEC     A
;     LD      (cursorY), A
;     JR      Draw    ; draw o
; Down:
;     LD      A, (cursorY)
;     CP      7        ; dont go down if y is 7
;     JR      Z, KeyLoop
;     INC     A
;     LD      (cursorY), A
;     JR      Draw    ; draw o
; Right:
;     LD      A, (cursorX)
;     CP      15        ; dont go right if x is 15
;     JR      Z, KeyLoop
;     INC     A
;     LD      (cursorX), A
;     JR      Draw    ; draw o
; Left:
;     LD      A, (cursorX)
;     CP      0        ; dont go left if x is zero
;     JR      Z, KeyLoop
;     DEC     A
;     LD      (cursorX), A
;     JR      Draw    ; draw o


DrawTile: ; A = index of tile sprite, D = row, E = column, B = index of tile from outer loop
    LD HL, tiles ; load first tile location
    
    LD C, TILE_SIZE ; C is the counter for drawing each sprite lines
    PUSH BC ; saves the value of B and C so we can pop it off at the end

    CP 0 ; If A (index of tile sprite) is 0 (covered tile) then
    JR Z, _SkipLoop ; Skip the multiplication loop

    PUSH DE ; we need row and column later, so push it 
    LD B, A; loop the amount of times it takes to get to the tile index specified
    LD DE, TILE_SIZE ; Increment by TILE_SIZE everytime, taking into account the possible 8 bit overflow by using the 16 bit register
_TileIndexLoop:
    ADD HL, DE
    DJNZ _TileIndexLoop ; Offsets HL by index * TILE_SIZE
    POP DE
_SkipLoop: ; At this point the correct index of the tile sprite is in HL
    
    LD A, D
    ; Multiply row by 12
    ADD    A, A ; A = 2D
    ADD    A, D ; A = 3D
    ADD    A, A ; A = 6D
    ADD    A, A ; A = 12D

    LD C, E ; load column into C
    SRL C
    SRL C
    SRL C ; divide by 8 to get the byte number (column byte) we want to draw to
    ADD A, C ; A = row * 12 + column = byte offset for screen
    LD D, A ; load byte offset back into D
; We now need the pixel start bit
    LD A, E ; Load column into A
    AND 7   ; Mod 8, if A is zero, set Z flag
    LD E, A ; E = bit offset for screen byte to draw to

    LD IX, PlotSScreen ; load address of screen buffer
    LD B, 0
    LD C, D ; load byte offset for screen byte we want to draw to

    ADD IX, BC ; add offset to PlotSScreen
    ; IX = address of screen bytes, D is free

; Create the screen mask
    LD A, %11111111 ; right side of sprite
    LD C, %00000111 ; left side of sprite
    
    LD B, E ; Set counter to bit offset

    JR NZ, _ScreenMaskLoop ; if bit offset is not zero, loop

    LD B, A
    PUSH BC ; C is LEFT SIDE of mask, B is RIGHT SIDE !VERY IMPORTANT!

    JR _DrawSpriteLine

_ScreenMaskLoop:
    RRC C ; C register is rotated to the right
    RRA   ; A register is rotated right and the previous carry flag is copied to bit 7
    DJNZ _ScreenMaskLoop

    LD B, A
    PUSH BC ; C is LEFT SIDE of mask, B is RIGHT SIDE !VERY IMPORTANT!
_DrawSpriteLine:
    POP BC
    ; B is right side, C is left
    ; Mask each screen byte with each mask byte and write values to buffer
    LD A, C
    AND (IX)
    LD (IX), A
    LD A, B
    AND (IX + 1)
    LD (IX + 1), A

    PUSH BC ; Push mask back onto stack for next line

    LD B, E ; Set counter to bit offset again
    LD A, B
    CP 0 ; if bit offset is zero, Z is 1

    LD A, 0    ; Right side of sprite in A
    LD C, (HL) ; Left side of sprite in C

    JR Z, _SkipSpriteShiftLoop ; if bit offset is zero, skip this garbage

_SpriteShiftLoop:
    RRC C ; C register is rotated to the right
    RRA   ; A register is rotated right and the previous carry flag is copied to bit 7
    RES 7, C
    DJNZ _SpriteShiftLoop

_SkipSpriteShiftLoop:

    ; A is right side, C is left
    ; OR each screen byte with each sprite byte and write values to buffer
    OR (IX + 1)
    LD (IX + 1), A
    LD A, C
    OR (IX)
    LD (IX), A
    ; Line has been draw!!!!!
    ; Draw another

    POP AF ; AF contains the screen mask
    POP BC ; B contains original tile index and C contains sprite line count
    DEC C
    PUSH BC ; decrement count and push it back onto stack
    PUSH AF ; Push screen mask back onto stack
    
    PUSH DE ; save bit offset
    LD DE, 12
    ADD IX, DE ; move screen byte down to operate on
    INC HL ; increment HL so sprite line is increased
    POP DE ; get bit offset back

    LD A, C
    CP 0

    JR NZ, _DrawSpriteLine

    POP BC ; get screen mask off of stack
    POP BC ; returns B to original index value for outer loop

    b_call(_GrBufCpy)

    RET

; Usable bytes: 768
board = AppBackUpScreen ; size = 16 * 12 = 192 bytes
selector = AppBackUpScreen + 192 ; size = 1 (single byte representing offset of selected tile)

tiles:
    ; Covered Tile (0)
    .DB %11111000
    .DB %10000000
    .DB %10000000
    .DB %10000000
    .DB %10000000
    ; 1
    .DB %11111000
    .DB %11101000
    .DB %11001000
    .DB %11101000
    .DB %11000000
    ; 2
    .DB %11111000
    .DB %11001000
    .DB %11110000
    .DB %11001000
    .DB %11000000
    ; 3
    .DB %11111000
    .DB %10001000
    .DB %11110000
    .DB %11100000
    .DB %10001000
    ; 4
    .DB %11111000
    .DB %10101000
    .DB %10101000
    .DB %10000000
    .DB %11101000
    ; 5
    .DB %11111000
    .DB %11000000
    .DB %11001000
    .DB %11110000
    .DB %11001000
    ; 6
    .DB %11111000
    .DB %11100000
    .DB %11011000
    .DB %11000000
    .DB %11100000
    ; 7
    .DB %11111000
    .DB %11000000
    .DB %11110000
    .DB %11101000
    .DB %11101000
    ; 8
    .DB %11111000
    .DB %11001000
    .DB %11010000
    .DB %10101000
    .DB %11001000
    ; Mine Tile (9)
    .DB %11111000
    .DB %11010000
    .DB %10001000
    .DB %11000000
    .DB %10101000
    ; Flag Tile (10)
    .DB %11111000
    .DB %11001000
    .DB %11000000
    .DB %11011000
    .DB %10001000
    ; Empty Tile (11)
    .DB %11111000
    .DB %11111000
    .DB %11111000
    .DB %11111000
    .DB %11111000

.END
