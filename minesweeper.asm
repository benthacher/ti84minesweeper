#INCLUDE "./includes/ti83plus.inc"
#define MINEBIT 4
#define COVEREDBIT 5
#define FLAGBIT 6
#define TILE_SIZE 5


.ORG    $9D93
.DB     t2ByteTok, tAsmCmp ;I have no clue why but it breaks without this line

    b_call(_RunIndicOff)

;---------------------------------------------------------------------
    JR      Draw
    RET

;---------------------------------------------------------------------
KeyLoop:
    b_call(_GetKey)
    CP      kUp        ; If the up arrow key was pressed.
    JR      Z, Up
    CP      kDown      ; If the down arrow key was pressed.
    JR      Z, Down
    CP      kLeft      ; If the down arrow key was pressed.
    JR      Z, Left
    CP      kRight     ; If the down arrow key was pressed.
    JR      Z, Right
    CP      kClear     ; If the CLEAR key was pressed.
    RET     Z
    JR      KeyLoop    ; If any other key was pressed, redo _GetKey.
Up:
    LD      A, (cursorY)
    ; CP      0        ; dont go up
    JR      Z, KeyLoop
    DEC     A
    LD      (cursorY), A
    JR      Draw    ; draw o
Down:
    LD      A, (cursorY)
    CP      7        ; dont go down if y is 7
    JR      Z, KeyLoop
    INC     A
    LD      (cursorY), A
    JR      Draw    ; draw o
Right:
    LD      A, (cursorX)
    CP      15        ; dont go right if x is 15
    JR      Z, KeyLoop
    INC     A
    LD      (cursorX), A
    JR      Draw    ; draw o
Left:
    LD      A, (cursorX)
    CP      0        ; dont go left if x is zero
    JR      Z, KeyLoop
    DEC     A
    LD      (cursorX), A
    JR      Draw    ; draw o
Draw: 
    LD      A, (cursorY)       ; set cursor to cursor coordinates
    LD      (CurRow), A
    LD      A, (cursorX)       ; set cursor to cursor coordinates
    LD      (CurCol), A

    b_call(_ClrLCDFull)     ; clear screen
    LD      A, 'o'
    b_call(_PutC)           ; print 'o'
    JR      KeyLoop    ; Get another key.





DrawTile: ; D = index of tile sprite, C = column, A = row
    LD E, B ; Save value of B
    LD HL, () ; load first tile 

    LD B, 12 ; load 12 into B to loop 12 times
_TileIndexLoop: ; multiplies D by 12 (number of bytes in a row)
    ADD A, A
    DJNZ _TileIndexLoop
    ; out of loop
    LD A, C ; Load columb into A
    AND 7   ; Mod 8
    LD B, A ; Set up loop to shift 
_PixelShiftLoop:


    SRA C
    SRA C
    SRA C ; divide by 8 to get the byte number we want to draw to


; Usable bytes: 768
board = AppBackUpScreen ; size = 16 * 12 = 192 bytes
selector = AppBackUpScreen + 192 ; size = 1 (single byte representing offset of selected tile)

tiles:
    ; Empty tile
    .DB %00011111
    .DB %00011111
    .DB %00011111
    .DB %00011111
    .DB %00011111
    ; 1
    .DB %00011111
    .DB %00011101
    .DB %00011001
    .DB %00011101
    .DB %00011000
    ; 2
    .DB %00011111
    .DB %00011001
    .DB %00011110
    .DB %00011001
    .DB %00011000
    ; 3
    .DB %00011111
    .DB %00010001
    .DB %00011110
    .DB %00011100
    .DB %00010001
    ; 4
    .DB %00011111
    .DB %00010101
    .DB %00010101
    .DB %00010000
    .DB %00011101
    ; 5
    .DB %00011111
    .DB %00011000
    .DB %00011001
    .DB %00011110
    .DB %00011001
    ; 6
    .DB %00011111
    .DB %00011100
    .DB %00011011
    .DB %00011000
    .DB %00011100
    ; 7
    .DB %00011111
    .DB %00011000
    .DB %00011110
    .DB %00011101
    .DB %00011101
    ; 8
    .DB %00011111
    .DB %00011001
    .DB %00011010
    .DB %00010101
    .DB %00011001
    ; Mine Tile
    .DB %00011111
    .DB %00011010
    .DB %00010001
    .DB %00011000
    .DB %00010101
    ; Flag Tile
    .DB %00011111
    .DB %00011001
    .DB %00011000
    .DB %00011011
    .DB %00010001

.END
