#INCLUDE "./includes/ti83plus.inc"
#define MINEBIT 4
#define COVEREDBIT 5
#define FLAGBIT 6
#define TILE_SIZE 5


.ORG    $9D93
.DB     t2ByteTok, tAsmCmp ;I have no clue why but it breaks without this line

    b_call(_RunIndicOff)

;---------------------------------------------------------------------
    ;JR      Draw
    LD     HL, picdata
    LD     DE, PlotSScreen+(19*12)    ;Start at nineteenth row of display
    LD     BC, 25*12                  ;25 rows of data
    LDIR                      
    b_call(_GrBufCpy)
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

DrawTile:
    LD E, B ; Save value of B
    LD B, 12 ; load 12 into B to loop 12 times
_TileIndexLoop: ; multiplies 

    DJNZ _TileIndexLoop

cursorY:
    .db 4
cursorX:
    .db 4
.END
