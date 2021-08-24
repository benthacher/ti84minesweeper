#INCLUDE "./includes/ti83plus.inc"

#define MINEBIT 4
#define COVEREDBIT 5
#define FLAGBIT 6
#define FLAGBITMASK 1 << FLAGBIT

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

; Defines for game state bits
; 0: is current tile the selector?
; 1: is out of bounds?
; 2: lost the game?
#define STATE_SELECTING 0
#define STATE_OUTOFBOUNDS 1
#define STATE_LOSTGAME 2
#define STATE_WONGAME 3

.ORG    $9D93
.DB     $BB,$6D

; ---------------------------- MAIN --------------------------
    b_call(_RunIndicOff)
    bcall(_DisableApd)

Restart:
; ----------------------- Clear screen -----------------------
    LD A, 0
    LD (totalMines), A
    LD (selector), A
    LD (gameState), A
    LD A, BOARD_SIZE
    LD (coveredTiles), A

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
    
    CP 4 ; test if random number between 0 and 255 is less than 30 (for creating the mine)

    JR C, _SetMine
    ; else reset MINEBIT and draw a normal tile
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

;---------------------- Count surrounding mines ------------------
    LD IX, board ; Load board address into index register
    LD C, 0         ; Index of mine in board list

MineCountLoop:
    ; C = index
    CALL CountSurrounding ; adds number of surrounding mines to tile at IX

    INC C
    INC IX

    LD A, C
    CP BOARD_SIZE ; If C is at the end of the board list
    JR NZ, MineCountLoop
; ------------------------End Mine count-------------

    LD A, (totalMines) ; Copy totalMines to flagsLeft
    LD (flagsLeft), A

    CALL DrawHappyFace
    CALL DisplayFlagsLeft
    
    LD A, BIG_FLAG_TILE ; Draw big flag
    LD D, FLAGS_ROW
    LD E, FLAGS_COL
    CALL DrawTile

    ; Flip the selected tile, which copies the graph buffer (much faster than drawing every loop)
    CALL FlipSelectedTile
; ----------------------- End Initialize -----------------------

KeyLoop:
    ; Check game state
    LD HL, gameState
    BIT STATE_LOSTGAME, (HL)
    JR NZ, _LostGame ; if we've lost, jump to _LostGame

    BIT STATE_WONGAME, (HL) ; if we've won, jump to _WonGame
    JR NZ, _WonGame
    JR _ContinueGame ; else just continue getting key input

_LostGame:
    CALL LoseGame
    JP Restart

_WonGame:
    CALL WinGame
    JP Restart

_ContinueGame:
    b_call(_GetKey)

; Key procedures
    CP kStat
    JP Z, Uncover

    CP kPrgm
    JP Z, Flag

    CP kUp    
    JR Z, Up

    CP kDown  
    JR Z, Down
    
    CP kLeft   
    JR Z, Left
    
    CP kRight  
    JR Z, Right

    CP kClear
    JP Z, Done

    JR KeyLoop

; ------------------- END MAIN --------------

Up:
    LD A, (selector)
    CP 16 ; if selector - 16 is negative, return back to key loop
    JR C, KeyLoop

    CALL FlipSelectedTile
    LD A, (selector) ; subtract width from selector (move up)
    SBC A, 16
    LD (selector), A
    CALL FlipSelectedTile

    JR KeyLoop

Down:
    LD A, (selector)
    CP BOARD_SIZE - 16 ; if selector - (BOARD_SIZE - 16) is positive, return back to key loop
    JR NC, KeyLoop

    CALL FlipSelectedTile
    LD A, (selector) ; add width to selector (move down)
    ADD A, 16
    LD (selector), A
    CALL FlipSelectedTile

    JR KeyLoop

Left:
    LD A, (selector) ; subtract width from selector (move up)
    AND 15 ; mod 16 to check where selector is in column
    ; if selector col is zero, return back to key loop
    JR Z, KeyLoop

    CALL FlipSelectedTile
    LD HL, selector ; decrement selector (move left)
    DEC (HL)
    CALL FlipSelectedTile

    JP KeyLoop

Right:
    LD A, (selector) ; subtract width from selector (move up)
    AND 15 ; mod 16 to check where selector is in column
    CP 15 ; if selector col is zero, return back to key loop
    JP Z, KeyLoop

    CALL FlipSelectedTile
    LD HL, selector ; increment selector (move left)
    INC (HL)
    CALL FlipSelectedTile

    JP KeyLoop

CountSurrounding: ; C is index of center tile, IX is actual tile address
    ; offset is always in the range [-17, 17], but it's added to 16 bit values,
    ; so we have to be careful when it's negative
    ; Load desired constant offset into DE
    ; This means if it's negative, DE = correct value in 16 bit 2's complement
    ; If we need to use the 8 bit offset, just use the E register
    LD DE, -17 
    CALL _BoundsTestBlock ; C is not changed, IX is not changed, everyone is happy (except HL and DE)
    LD DE, -16
    CALL _BoundsTestBlock
    LD DE, -15
    CALL _BoundsTestBlock
    LD DE, -1
    CALL _BoundsTestBlock
    LD DE, 1
    CALL _BoundsTestBlock
    LD DE, 15
    CALL _BoundsTestBlock
    LD DE, 16
    CALL _BoundsTestBlock
    LD DE, 17
    CALL _BoundsTestBlock

    RET

_BoundsTestBlock: ; C is index of center tile, DE is offset
    CALL CheckBounds
    LD HL, gameState
    BIT STATE_OUTOFBOUNDS, (HL) ; check if CheckBounds procedure set out of bounds flag
    RET NZ ; if Zero flag is reset, out of bounds is true, return

    ; Now we need to move the tile address index stored in IX to the correct tile to check if it's a mine
    PUSH IX ; Push IX to the stack
    POP HL ; Pop it back into HL to do addition (might be a better way, but can't load IX directly into HL)
    ADD HL, DE ; move tile address to correct checking tile

    BIT MINEBIT, (HL) ; check if mine bit if set at current mine
    RET Z ; if Zero flag is set, tile is not a mine, return and do not increment tile
    INC (IX) ; increment count tile at IX (center tile)
    
    RET

; Destroys HL, A
CheckBounds: ; C is index of tile, DE is offset to check
    LD HL, gameState
    SET STATE_OUTOFBOUNDS, (HL) ; set the out of bounds flag, reset if not out of bounds
; CheckRight:
    LD A, C
    AND 15 ; mod 16 on index
    CP 15 ; if 15, we're on the right edge
    JR NZ, _CheckLeft ; if not on right, check the left
    ; else check if we're looking right
    LD A, E
    AND 15 ; mod 16 on offset
    CP 1 ; if 1, we're looking to the right and looking right (bad)
    RET Z ; return having set the out of bounds bit high

_CheckLeft:
    LD A, C
    AND 15 ; mod 16 on index
    ; if 0, we're on the left edge
    JR NZ, _CheckUp ; if not on left, check up edge
    ; else check if we're looking left
    LD A, E
    AND 15 ; mod 16 on offset
    CP 15 ; if 15, we're looking to the left and looking left (bad)
    RET Z ; return having set the out of bounds bit high

_CheckUp:
    LD A, C
    ADD A, E ; add offset to index
    ; if < 0, we're on the top looking up (bad)
    CP 0
    RET C
; CheckDown:
    ; A is already the sum of the index and the offset
    CP BOARD_SIZE
    ; if > BOARD_SIZE, we're on the top looking up (bad)
    RET NC

    ; If we get to this point, we're in bounds, so reset the out of bounds flag
    ; HL is still the address of the game state variable
    RES STATE_OUTOFBOUNDS, (HL)
    
    RET

Uncover:

    CALL DisplayFlagsLeft
    
    LD A, (selector)
    LD B, 0
    LD C, A ; load selected index into BC
    LD IX, board
    ADD IX, BC ; Get address of selected tile

    LD A, (IX)

    BIT COVEREDBIT, A ; check if selected tile is covered
    JP Z, KeyLoop ; if not covered, return

    BIT FLAGBIT, A ; check if selected tile is flagged
    JP NZ, KeyLoop ; if it is, return

    ; Uncover tile
    RES COVEREDBIT, (IX)
    PUSH BC ; save selected index
    PUSH IX ; save selected address
    CALL RowAndColFromIndex
    CALL TileToScreen
    POP IX
    LD A, (IX)
    AND 15 ; mod 16 for number to get count
    PUSH IX
    ; Now A is the count (the correct tile to be drawn)
    CALL DrawTile
    CALL FlipSelectedTile ; (copies graph buffer)

    LD HL, coveredTiles
    DEC (HL) ; decrement coveredTiles

    POP IX ; get selected address back
    POP BC ; get selected index back

    LD A, (IX)
    AND 15 ; mod 16 again to get count
    CP 0
    CALL Z, UncoverRecursive ; if count is zero (empty tile), call UncoverRecursive
    b_call(_GrBufCpy) ; Draw the newly uncovered tiles
    BIT MINEBIT, (IX) ; check if uncovered tile is a mine
    ; if uncovered tile isn't a mine, just return
    JP Z, KeyLoop

    ; else, set the lost bit in the gameState
    LD HL, gameState
    SET STATE_LOSTGAME, (HL)
    
    JP KeyLoop

UncoverRecursive: ; C is selected index, IX is selected address
    ; For each of the test blocks around the center, push IX to save center,
    ; test around it, pop it back
    LD DE, -17
    PUSH IX
    PUSH BC
    CALL _UncoverTestBlock
    POP BC
    POP IX

    LD DE, -16
    PUSH IX
    PUSH BC
    CALL _UncoverTestBlock
    POP BC
    POP IX
    
    LD DE, -15
    PUSH IX
    PUSH BC
    CALL _UncoverTestBlock
    POP BC
    POP IX
    
    LD DE, -1
    PUSH IX
    PUSH BC
    CALL _UncoverTestBlock
    POP BC
    POP IX

    LD DE, 1
    PUSH IX
    PUSH BC
    CALL _UncoverTestBlock
    POP BC
    POP IX

    LD DE, 15
    PUSH IX
    PUSH BC
    CALL _UncoverTestBlock
    POP BC
    POP IX

    LD DE, 16
    PUSH IX
    PUSH BC
    CALL _UncoverTestBlock
    POP BC
    POP IX

    LD DE, 17
    PUSH IX
    PUSH BC
    CALL _UncoverTestBlock
    POP BC
    POP IX

    RET

_UncoverTestBlock: ; DE is offset, C is index, IX is center address
    CALL CheckBounds
    LD HL, gameState
    BIT STATE_OUTOFBOUNDS, (HL) ; check if CheckBounds procedure set out of bounds flag
    RES STATE_OUTOFBOUNDS, (HL) ; reset the out of bounds state flag no matter what
    RET NZ ; if Zero flag is reset, out of bounds is true, return

    ; Now we need to move the tile address index stored in IX to the correct tile to check if it's a mine
    
    ADD IX, DE ; move tile address to correct checking tile

    LD A, C
    ADD A, E
    LD C, A

    ; IX is now the test address
    ; C is now the test index

    BIT FLAGBIT, (IX) ; check if flag bit is set at current tile
    RET NZ ; if Zero flag is set, tile is flagged mine, return
    
    BIT COVEREDBIT, (IX) ; check if tile is covered
    RET Z ; return if uncovered already

    ; Uncover tile
    RES COVEREDBIT, (IX)
    PUSH BC ; save selected index
    PUSH DE ; save offset
    PUSH IX ; save test address
    CALL RowAndColFromIndex ; C -> DE as tile coords
    CALL TileToScreen ; DE -> DE as screen coords
    LD A, (IX)
    AND 15 ; mod 16 for number to get count
    ; Now A is the count (the correct tile to be drawn)
    CALL DrawTile
    b_call(_GrBufCpy) ; take this out to make it faster

    CALL DisplayFlagsLeft

    LD A, (coveredTiles)
    DEC A ; decrement covered tiles
    LD (coveredTiles), A ; store coveredTiles and keep it in A to compare with totalMines

    ; --------- Compare coveredTiles to totalMines to check if player has won ----------

    LD B, A ; copy coveredTiles over to B
    LD A, (totalMines) ; load totalMines into A
    CP B ; check if equal
    JR NZ, _ContinueUncover ; if not equal, jump to continue uncover

    ; else, set the wongame flag
    LD HL, gameState
    SET STATE_WONGAME, (HL) ; set the won game bit

_ContinueUncover:
    POP IX ; get selected address back
    POP DE ; get offset back
    POP BC ; get selected index back

    LD A, (IX)
    AND 15
    CP 0 ; if tile we're checking is not zero, return

    RET NZ
    ; else, call uncover recursive with the test tile as the center

    CALL Z, UncoverRecursive
    RET

Flag:
    LD A, (selector)
    LD B, 0
    LD C, A ; load selected index into BC
    LD IX, board
    ADD IX, BC ; Get address of selected tile

    BIT COVEREDBIT, (IX) ; check if the selected tile is covered
    JP Z, KeyLoop ; if it's not covered, just return and don't add a flag

    ; Draw tile and flip flag bit
    CALL RowAndColFromIndex
    CALL TileToScreen

    BIT FLAGBIT, (IX)
    JR Z, _SetFlag ; if tile isn't a flag, set it
    LD A, COVERED_TILE ; else, reset flag bit and draw covered tile
    RES FLAGBIT, (IX)

    ; Increment flags left
    LD HL, flagsLeft
    INC (HL)

    JR _DrawFlagTile
_SetFlag:
    LD A, FLAG_TILE
    SET FLAGBIT, (IX)

    ; Decrement flags left
    LD HL, flagsLeft
    DEC (HL)

_DrawFlagTile:
    CALL DrawTile

    CALL FlipSelectedTile

    CALL DisplayFlagsLeft

    JP KeyLoop

WinGame:
    CALL DrawEpicFace
    b_call(_GrBufCpy)
    
    JR _RestartLoop ; wait for input before restarting

LoseGame:
    CALL DrawDeadFace

    LD IX, board ; Load board address into index register
    LD C, 0         ; Index of mine in board list
_DisplayAllMinesLoop:
    ; C = index
    BIT MINEBIT, (IX)
    JR Z, _Continue ; if tile at IX is not a mine, do nothing

    PUSH IX
    PUSH BC
    CALL RowAndColFromIndex
    CALL TileToScreen
    LD A, MINE_TILE
    CALL DrawTile
    POP BC
    POP IX

_Continue:

    INC C
    INC IX

    LD A, C
    CP BOARD_SIZE ; If C is at the end of the board list
    JR NZ, _DisplayAllMinesLoop

    b_call(_GrBufCpy)
    
_RestartLoop:
    b_call(_GetKey)

    CP kUp ; if you pressed action key, restart
    RET Z
    CP kDown
    RET Z
    CP kLeft
    RET Z
    CP kRight
    RET Z
    CP kPrgm
    RET Z
    CP kStat
    RET Z

    JP _RestartLoop

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
    LD A, (coveredTiles)
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
seed         = AppBackUpScreen + 193 ; 2 bytes for seed to live
totalMines   = AppBackUpScreen + 195 ; can only be 16*12=192 mines, less than 255
flagsLeft    = AppBackUpScreen + 196 ; same size as totalMines
coveredTiles = AppBackUpScreen + 197 ; same size as flagsLeft
gameState    = AppBackUpScreen + 198 ; single byte with game flags
stack        = AppBackUpScreen + 199 ; 2 bytes to store SP at program start

tiles:
    ; 0 (Empty tile, selector tile)
    .DB %11111000
    .DB %11111000
    .DB %11111000
    .DB %11111000
    .DB %11111000
    ; 1
    .DB %11111000
    .DB %11011000
    .DB %10011000
    .DB %11011000
    .DB %10001000
    ; 2
    .DB %11111000
    .DB %10011000
    .DB %11101000
    .DB %10011000
    .DB %10001000
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
    .DB %10001000
    .DB %10011000
    .DB %11101000
    .DB %10011000
    ; 6
    .DB %11111000
    .DB %11001000
    .DB %10111000
    .DB %10001000
    .DB %11001000
    ; 7
    .DB %11111000
    .DB %10001000
    .DB %11101000
    .DB %11011000
    .DB %11011000
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
    .DB %10000000
    .DB %00110000
    .DB %00111000
    .DB %00100000
    .DB %01110000
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

Done:
    ; bcall(_EnableApd)
    ; bcall(_HomeUp)
    ; bcall(_ClrScrn)
    ; bcall(_ClrScrnFull)
    RET

.END
.END
