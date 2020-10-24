#INCLUDE "./includes/ti83plus.inc"

#define MINEBIT 4
#define COVEREDBIT 5
#define FLAGBIT 6

#define TILE_SIZE 5
#define BOARD_SIZE 192

#define MINE_TILE 9
#define FLAG_TILE 10
#define EMPTY_TILE 11
#define COVERED_TILE 12

#define IS_RUNNING 0
#define LOST

.ORG    $9D93
.DB     t2ByteTok, tAsmCmp ;I have no clue why but it breaks without this line

;----------------------------- MAIN --------------------------
    b_call(_RunIndicOff)

    ; Clear screen
    LD BC, 12*64
    LD HL, PlotSScreen
ClearScreen:
    DEC BC
    LD (HL), 0
    INC HL
    LD A, B
    OR C
    JR NZ, ClearScreen
    b_call(_GrBufCpy)

    LD IX, board ; Load board address into index register
    LD C, 0         ; Index of mine in board list
InitializeLoop:
    ; C = index
    LD (IX), 0 ; set the tile data at IX to zero
    SET COVEREDBIT, (IX) ; cover the tile at IX

    CALL RowAndColFromIndex ; D and E are now tile row and col
    ; Get Random number
    CALL Random ; Random number stored in A
    CP 30 ; test if random number between 0 and 255 is less than 30 (for creating the mine)

    JR C, _SetMine
    ; else set MINEBIT to zero and draw a normal tile
    RES MINEBIT, (IX)
    JR _DrawCurrentTile
_SetMine:
    SET MINEBIT, (IX)  ; set mine bit of current tile
    LD HL, totalMines ; Load address of totalMines variable to HL
    INC (HL)
    
_DrawCurrentTile:
    ; Set up parameters for drawing tile
    ; A = index of tile sprite
    ; D = tile row
    ; E = tile column
    PUSH BC ; save outer index
    PUSH IX ; save address of current tile
    LD A, COVERED_TILE ; set tile index to COVERED_TILE
    CALL DrawTile
    POP IX ; get them back
    POP BC

    INC C
    INC IX

    LD A, C
    CP BOARD_SIZE ; If C is at the end of the board list
    JR NZ, InitializeLoop

    LD A, (totalMines) ; Load address of flagsLeft variable to HL
    LD (flagsLeft), A

    ; Once mines have been set, copy graph buffer (much faster than drawing every loop)
    b_call(_GrBufCpy)

KeyLoop:
    b_call(_GetKey)
    CP      kClear     ; If the CLEAR key was pressed.
    JR NZ, KeyLoop

    RET
; ------------------- END MAIN --------------

; Destroys A
RowAndColFromIndex: ; C = index. Returns D = tile row, E = tile col
    ; Get Row
    LD A, C ; load index into A
    SRL A
    SRL A
    SRL A
    SRL A ; divide by 16 to get row
    LD D, A ; load row into D
    ; Get Col
    LD A, C
    AND 15 ; Mod 16 to get column
    LD E, A ; load col into E

    RET

; D = tile row, E = tile column
; Destroys HL
TileToScreen: ; multiply row and col by 5 to get screen
    LD H, D ; Load tile row into H
    LD L, E ; Load tile col into L

    ADD HL, DE ; HL = 2DE
    ADD HL, DE ; HL = 3DE
    ADD HL, DE ; HL = 4DE
    ADD HL, DE ; HL = 5DE

    EX DE, HL ; Swap DE and HL so that DE = 5DE

    RET

; D = screen row, E = screen column
; Returns HL = screen byte offset, E = bit offset, Z = (E == 0)
; Destroys HL, BC, A
GetPixel:
    LD H, 0
    LD L, D ; HL = D
    LD B, H
    LD C, D ; BC = D
    ; Multiply row by 12
    ADD HL, HL ; HL = 2D
    ADD HL, BC ; HL = 3D
    ADD HL, HL ; HL = 6D
    ADD HL, HL ; HL = 12D

    LD C, E ; load column into C
    SRL C
    SRL C
    SRL C ; divide by 8 to get the byte number (column byte) we want to draw to
    ADD HL, BC ; HL = row * 12 + column = byte offset for screen
; We now need the pixel start bit
    LD A, E ; Load column into A
    AND 7   ; Mod 8, if A is zero, set Z flag
    LD E, A ; E = bit offset for screen byte to draw to

    CP 0
    RET

DrawTile: ; A = index of tile sprite, D = tile row, E = tile column, B = index of tile from outer loop
    LD C, TILE_SIZE ; C is the counter for drawing each sprite lines
    PUSH BC ; saves the value of B and C so we can pop it off at the end
    
    LD HL, tiles ; load first tile location

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
    PUSH HL ; save tile address
    
    ; Make tile row and column into screen row and column for GetPixel procedure
    CALL TileToScreen
    CALL GetPixel

    LD IX, PlotSScreen ; load address of screen buffer
    
    LD B, H ; load offset into BC
    LD C, L

    ADD IX, BC ; add offset to PlotSScreen
    ; IX = address of screen bytes, D is free
    POP HL ; load tile index back into HL

; Create the screen mask
    LD A, %11111111 ; right side of sprite
    LD C, %00000111 ; left side of sprite
    
    LD B, E ; Set counter to bit offset

    JR _ScreenMaskLoop ; if bit offset is not zero, loop

    LD B, A
    PUSH BC ; C is LEFT SIDE of mask, B is RIGHT SIDE !VERY IMPORTANT!

    JR _DrawSpriteLine

_ScreenMaskLoop:
    RRC C ; C register is rotated to the right
    RRA   ; A register is rotated right and the previous carry flag is copied to bit 7
    SET 7, C
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

    LD A, E ; Load bit offset into A
    POP DE ; DE now contains the screen mask
    POP BC ; B contains original tile index and C contains sprite line count
    DEC C
    PUSH BC ; decrement count and push it back onto stack
    PUSH DE ; Push screen mask back onto stack
    
    LD DE, 12
    ADD IX, DE ; move screen byte down to operate on
    INC HL ; increment HL so sprite line is increased
    
    LD E, A ; Load bit offset back into E

    LD A, C
    CP 0

    JR NZ, _DrawSpriteLine

    POP BC ; get screen mask off of stack
    POP BC ; returns B to original index value for outer loop

    ; b_call(_GrBufCpy)

    RET

Random: ; Gets a random number in A register, preserves every other register (not F though)
    PUSH    HL
    PUSH    DE
    LD      HL,(seed)
    LD      A,R
    LD      D,A
    LD      E,(HL)
    ADD     HL,DE
    ADD     A,L
    XOR     H
    LD      (seed),HL
    POP     DE
    POP     HL
    RET
    
; Usable bytes: 768
board        = AppBackUpScreen       ; size = 16 * 12 = 192 bytes
selector     = AppBackUpScreen + 192 ; size = 1 (single byte representing offset of selected tile)
seed         = AppBackUpScreen + 193 ; single byte for seed to live
totalMines   = AppBackUpScreen + 194 ; can only be 16*12=192 mines, less than 255
flagsLeft    = AppBackUpScreen + 195 ; same size as totalMines
coveredTiles = AppBackUpScreen + 196 ; same size as flagsLeft
gameState    = AppBackUpScreen + 197 ; single byte with game flags

tiles:
    ; (0) ?? index of zero doesn't work
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
    ; Covered Tile (12)
    .DB %11111000
    .DB %10000000
    .DB %10000000
    .DB %10000000
    .DB %10000000
    ; Text 1 (17) (set and reset bit #4)
    .DB %00100000
    .DB %01100000
    .DB %00100000
    .DB %00100000
    .DB %01110000
    ; Text 2 (18) (set and reset bit #4)
    .DB %00100000
    .DB %01100000
    .DB %00100000
    .DB %00100000
    .DB %01110000

.END
