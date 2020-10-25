#INCLUDE "./includes/ti83plus.inc"

#define MINEBIT 4
#define COVEREDBIT 5
#define FLAGBIT 6

#define TILE_SIZE 5
#define BOARD_SIZE 192

#define MINE_TILE 9
#define FLAG_TILE 10
#define BIG_FLAG_TILE 11
#define COVERED_TILE 12
#define OPEN_EYE_TILE 13
#define DEAD_EYE_TILE 14
#define SUNGLASSES_TILE 15
#define LEFT_SMILE_TILE 26
#define RIGHT_SMILE_TILE 27
#define LEFT_SAD_TILE 28
#define RIGHT_SAD_TILE 29

#define FACE_ROW 10
#define FACE_COL 83

#define FLAGS_ROW 32
#define FLAGS_COL 83

#define STATE_SELECTING 0

.ORG    $9D93
.DB     t2ByteTok, tAsmCmp ;I have no clue why but it breaks without this line

; ---------------------------- MAIN --------------------------
    b_call(_RunIndicOff)


; ----------------------- Clear screen -----------------------
    LD A, 0
    LD (totalMines), A
    LD (selector), A

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
; ----------------------- Initialize board -----------------------
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
    CALL TileToScreen
    LD A, COVERED_TILE ; set tile index to COVERED_TILE
    CALL DrawTile
    POP IX ; get them back
    POP BC

    INC C
    INC IX

    LD A, C
    CP BOARD_SIZE ; If C is at the end of the board list
    JR NZ, InitializeLoop

    LD A, (totalMines) ; Copy totalMines to flagsLeft
    LD (flagsLeft), A

    CALL DrawEpicFace
    CALL DisplayFlagsLeft
    
    LD A, BIG_FLAG_TILE ; Draw big flag
    LD D, FLAGS_ROW
    LD E, FLAGS_COL
    CALL DrawTile

    ; Flip the selected tile, which copies the graph buffer (much faster than drawing every loop)
    CALL FlipSelectedTile
; ----------------------- End Initialize -----------------------

KeyLoop:
    b_call(_GetKey)

; Key procedures
    CP kEnter     
    CALL Z, Uncover

    CP kUp    
    CALL Z, Up
    CP kDown  
    CALL Z, Down
    CP kLeft   
    CALL Z, Left
    CP kRight  
    CALL Z, Right

    CP kClear
    RET Z
    JR KeyLoop

; ------------------- END MAIN --------------

Up:
    CALL FlipSelectedTile
    LD A, (selector) ; subtract width from selector (move up)
    SBC A, 16
    LD (selector), A
    CALL FlipSelectedTile

    RET

Down:
    CALL FlipSelectedTile
    LD A, (selector) ; add width to selector (move down)
    ADD A, 16
    LD (selector), A
    CALL FlipSelectedTile

    RET

Left:
    CALL FlipSelectedTile
    LD HL, selector ; decrement selector (move left)
    DEC (HL)
    CALL FlipSelectedTile

    RET

Right:
    CALL FlipSelectedTile
    LD HL, selector ; increment selector (move left)
    INC (HL)
    CALL FlipSelectedTile

    RET

DecrementFlags:
    LD HL, flagsLeft
    LD A, 0
    ADD A, (HL)
    INC A
    LD (HL), A

    CALL DisplayFlagsLeft

    RET

Uncover:
; Flip selector tile
    ; CALL FlipSelectedTile
    NOP
    RET

FlipSelectedTile:
    LD A, (selector) ; load selector into C (index of tile)
    LD C, A
    CALL RowAndColFromIndex
    CALL TileToScreen
    
    LD HL, gameState 
    SET STATE_SELECTING, (HL) ; Set the STATE_SELECTING bit of the game state so that the selected tile is flipped
    CALL DrawTile ; D and E are set correctly, A (tile index) doesn't matter
    LD HL, gameState
    RES STATE_SELECTING, (HL) ; Reset selecting state bit so that other tiles are drawn normally
    b_call(_GrBufCpy)

    RET


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

DrawTile: ; A = index of tile sprite, D = screen row, E = screen column, B = index of tile from outer loop
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

    JR NZ, _ScreenMaskLoop ; if bit offset is not zero, loop

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

    ; Check if tile address in HL is tile + 30
    PUSH HL ; Save HL

    LD HL, gameState
    BIT STATE_SELECTING, (HL) ; Check if the selector tile is meant to be flipped

    POP HL ; Get HL back

    POP BC
    ; B is right side, C is left
    ; Mask each screen byte with each mask byte and write values to buffer

    JR Z, _NormalTile

    ; If tile index is the selector, flip the bits at the location specified with XOR instead of AND
    LD A, C
    CPL
    XOR (IX)
    LD (IX), A
    LD A, B
    CPL
    XOR (IX + 1)
    LD (IX + 1), A

    PUSH BC ; Push mask back onto stack for next line

    JR _SkipNormalTile

_NormalTile:
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

_SkipNormalTile:

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

DrawDeadFace:
    LD A, DEAD_EYE_TILE
    LD D, FACE_ROW
    LD E, FACE_COL
    CALL DrawTile

    LD A, DEAD_EYE_TILE
    LD D, FACE_ROW
    LD E, FACE_COL + 6
    CALL DrawTile

    LD A, LEFT_SAD_TILE
    LD D, FACE_ROW + 7
    LD E, FACE_COL + 1
    CALL DrawTile

    LD A, RIGHT_SAD_TILE
    LD D, FACE_ROW + 7
    LD E, FACE_COL + 6
    CALL DrawTile

    RET

DrawHappyFace:
    LD A, OPEN_EYE_TILE
    LD D, FACE_ROW
    LD E, FACE_COL
    CALL DrawTile

    LD A, OPEN_EYE_TILE
    LD D, FACE_ROW
    LD E, FACE_COL + 5
    CALL DrawTile

    LD A, LEFT_SMILE_TILE
    LD D, FACE_ROW + 7
    LD E, FACE_COL + 1
    CALL DrawTile

    LD A, RIGHT_SMILE_TILE
    LD D, FACE_ROW + 7
    LD E, FACE_COL + 6
    CALL DrawTile

    RET

DrawEpicFace:
    LD A, SUNGLASSES_TILE
    LD D, FACE_ROW
    LD E, FACE_COL + 1
    CALL DrawTile

    LD A, SUNGLASSES_TILE
    LD D, FACE_ROW
    LD E, FACE_COL + 6
    CALL DrawTile

    LD A, LEFT_SMILE_TILE
    LD D, FACE_ROW + 7
    LD E, FACE_COL + 1
    CALL DrawTile

    LD A, RIGHT_SMILE_TILE
    LD D, FACE_ROW + 7
    LD E, FACE_COL + 6
    CALL DrawTile

    RET

DisplayFlagsLeft:
    LD A, (flagsLeft)
    LD B, 0
_SubtractionLoop: ; Subtract 10 from A
    INC B ; increment C
    ADD A, 246 ; Two's complement of 10
    JP P, _SubtractionLoop
    ADD A, 10
    DEC B ; Now C is the integer quotient of flagsLeft / 10, A is the remainder (flagsLeft % 10)
    
    PUSH BC ; save 10s place
    SET 4, A ; A is the number we want in the ones place, so we set bit 4 to make it a large tile
    LD D, FLAGS_ROW + 6
    LD E, FLAGS_COL + 5
    CALL DrawTile
    
    POP BC
    LD A, B

    SET 4, A ; A is the number we want in the ones place, so we set bit 4 to make it a large tile
    LD D, FLAGS_ROW + 6
    LD E, FLAGS_COL
    CALL DrawTile
    b_call(_GrBufCpy)

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
    ; 0 (Empty tile, selector tile)
    .DB %11111000
    .DB %11111000
    .DB %11111000
    .DB %11111000
    .DB %11111000
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
    ; Big Flag (11)
    .DB %01100000
    .DB %01110000
    .DB %01111000
    .DB %01000000
    .DB %11110000
    ; Covered Tile (12)
    .DB %11111000
    .DB %10000000
    .DB %10000000
    .DB %10000000
    .DB %10000000
    ; Open Eye (13)
    .DB %00000000
    .DB %00110000
    .DB %00110000
    .DB %00000000
    .DB %00000000
    ; Dead Eye (14)
    .DB %00000000
    .DB %01010000
    .DB %00100000
    .DB %01010000
    .DB %00000000
    ; Sunglasses Eye (15)
    .DB %00000000
    .DB %11111000
    .DB %11110000
    .DB %01100000
    .DB %00000000
    ; Text 0 (16) (set and reset bit #4)
    .DB %01110000
    .DB %01010000
    .DB %01010000
    .DB %01010000
    .DB %01110000
    ; Text 1 (17) (set and reset bit #4)
    .DB %00100000
    .DB %01100000
    .DB %00100000
    .DB %00100000
    .DB %01110000
    ; Text 2 (18) (set and reset bit #4)
    .DB %01100000
    .DB %00010000
    .DB %00100000
    .DB %01000000
    .DB %01110000
    ; Text 3 (19) (set and reset bit #4)
    .DB %01110000
    .DB %00010000
    .DB %00110000
    .DB %00010000
    .DB %01110000
    ; Text 4 (20) (set and reset bit #4)
    .DB %01010000
    .DB %01010000
    .DB %01110000
    .DB %00010000
    .DB %00010000
    ; Text 5 (21) (set and reset bit #4)
    .DB %01110000
    .DB %01000000
    .DB %01110000
    .DB %00010000
    .DB %01110000
    ; Text 6 (22) (set and reset bit #4)
    .DB %00110000
    .DB %01000000
    .DB %01110000
    .DB %01010000
    .DB %01110000
    ; Text 7 (23) (set and reset bit #4)
    .DB %01110000
    .DB %00010000
    .DB %00010000
    .DB %00010000
    .DB %00010000
    ; Text 8 (24) (set and reset bit #4)
    .DB %01110000
    .DB %01010000
    .DB %01110000
    .DB %01010000
    .DB %01110000
    ; Text 9 (25) (set and reset bit #4)
    .DB %01110000
    .DB %01010000
    .DB %01110000
    .DB %00010000
    .DB %00010000
    ; Left smile (26)
    .DB %10000000
    .DB %01000000
    .DB %00111000
    .DB %00000000
    .DB %00000000
    ; Right smile (27)
    .DB %00010000
    .DB %00100000
    .DB %11000000
    .DB %00000000
    .DB %00000000
    ; Left sad face (28)
    .DB %00111000
    .DB %01000000
    .DB %10000000
    .DB %00000000
    .DB %00000000
    ; Right sad face (29)
    .DB %11000000
    .DB %00100000
    .DB %00010000
    .DB %00000000
    .DB %00000000

.END
