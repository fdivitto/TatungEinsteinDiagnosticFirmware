
SFTRST:     equ 00H ; RESET PORT, FDC/PSG

PSG_SEL:    equ 02H	; LATCH ADDRESS
PSG_RD:     equ 02H	; READ FROM PSG
PSG_WR:     equ 03H	; WRITE TO PSG

VDP_VRAM:	  equ 08H	; VRAM READ/WRITE
VDP_REG:	  equ 09H	; Register data
VDP_STAT:   equ 09H	; READ VDP STATUS

PCI_REG:	  equ 10H ; PCI Data register
PCI_STAT:   equ 11H ; Control/Status register

FDC_CMD:    equ 18H ; Status / Command register
FDC_TRACK:  equ 19H ; Track register
FDC_SECTOR: equ 1AH ; Sector register
FDC_DATA:   equ 1BH ; Data register

CTC_CH0:    equ 28H ; CTC channel 0
CTC_CH1:    equ 29H ; CTC channel 1
CTC_CH2:    equ 2AH ; CTC channel 2
CTC_CH3:    equ 2BH ; CTC channel 3

PCI_DATA:   equ 10h ; PCI 8251 Read / Write Data
PCI_CTRL:   equ 11h ; PCI 8251 Read Status / Write Control

PIO_DATA_A: equ 30h ; PIO Data A
PIO_CTRL_A: equ 31h ; PIO CTRL A
PIO_DATA_B: equ 32h ; PIO Data B
PIO_CTRL_B: equ 33h ; PIO CTRL B

ALPHA:      equ 22H ; TOGGLES ALPHA LED

DRVSEL:     equ 23H ; DRIVE SELECT

RAMROM:     equ 24H ; SELECT RAM/ROM


; MCAL functions
ARITH:   equ 080H ; ARITHMETIC COMMAND
BAUD:    equ 081H ; Set baud rate
COPY:    equ 082H ; HL = Start, DE = Finish, BC = destination
DECIML:  equ 083H ; MOS 'D' (Decimal), HL = value
EXEC:    equ 084H ; MOS 'E' (Execute), HL = break point
MFILL:   equ 085H ; MOS 'F' (Fill), HL = Start, DE = Finish, C = value
GOTO:    equ 086H ; MOS 'G' (Goto), HL = exec address, DE = break point (0 = no break point)
HEX:     equ 087H ; MOS 'H' (Hexadecimal), DE = pointer to hex text (zero terminated). Output displayed and returned also in HL
MODIFY:  equ 08CH ; MOS 'M' (Modify), HL = address to modify
RDBLOK:  equ 091H ; MOS 'R' (Read), DE = pointer to text
TBLATE:  equ 093H ; MOS 'T' (Tabulate), HL = Start address, DE = Finish address, C = columns (0 = 8 columns)
WRBLOK:  equ 096H ; MOS 'W' (Write), DE = pointer to text
COLD:    equ 097H ; MOS 'X' (Cold Start)
WARM:    equ 098H ; MOS 'Y' (Warm Start)
REGSTR:  equ 099H ; MOS 'Z' (Register examine)
ZINIT:   equ 09AH ; Re-entry to MOS
ZRSCAN:  equ 09BH ; Repeat key scan, A = key pressed (0 = no key)
ZKEYIN:  equ 09CH ; Wait Input key, A = key pressed
ZGETLN:  equ 09DH ; Get text from keyboard (until ENTER), DE = destination buffer
ZOUTC:   equ 09EH ; Character output, A = char to output
ZPOUT:   equ 09FH ; Output to parallel printer, A = char to output
ZSLOUT:  equ 0A0H ; Output to serial port, A = char to output
ZSRLIN:  equ 0A1H ; Serial input, A = char read
ZRSECT:  equ 0A2H ; Read a sector (512 bytes) from disk
ZWSECT:  equ 0A3H ; Write a sector (512 bytes) to disk
ZRBLK:   equ 0A4H ; Read a block (minimum 512 bytes) from disk, A = drive (0..3), HL = start address, DE = finish address, B = sector (0..9), C = Track (0..39)
ZWBLK:   equ 0A5H ; Write a block (minimum 512 bytes) to disk
ZCRLF:   equ 0A6H ; Outputs CR+LF (0x0D + 0x0A)
ZCRLFZ:  equ 0A7H ; Outputs CR+LF if cursor is not at column zero
ZSPACE:  equ 0A8H ; Outputs one space
ZPR4HX:  equ 0A9H ; Outputs 4 hex digits representing HL content
ZP2HXZ:  equ 0AAH ; Outputs 2 hex digits representing A content followed by a space
ZPR2HX:  equ 0ABH ; Outputs 2 hex digits representing A content
ZFC4HX:  equ 0ACH ; Get a hex number (up to 4 digits) into HL. DE points to text. Number ends on a non hex char.
ZFC2HX:  equ 0ADH ; Get a hex number (up to 2 digits) into A. Number ends on a non hex char.
ZDCMD:   equ 0AEH ; Outputs a command to FDC. Ret A: 0 = OK, FF = error.
ZHMDSC:  equ 0AFH ; Takes drive head to track 0 of current drive. Z = 1 on success
ZIGBLK:  equ 0B0H ; Ret a value in A, pointed by DE (ROM if < 0x8000, RAM if >0x7FFF). Commas and spaces ignored.
ZRDMEM:  equ 0B1H ; Like a "LD A, (HL)", perfomed on RAM
ZRCPYU:  equ 0B2H ; Like a LDIR, perfomed on RAM
ZRCPYD:  equ 0B3H ; Like a LDDR, perfomed on RAM
ZMOUT:   equ 0B4H ; Outputs value in B to PSG port in C
ZKSCAN:  equ 0B5H ; Return key pressed in A (0 = no key). Unaffected by key repeat speed
ZSLDSC:  equ 0B6H ; Select disc drive (in C), returns disc definition table in HL
ZSETRK:  equ 0B7H ; Set track (C)
ZSETSC:  equ 0B8H ; Set sector (C)
ZSETBF:  equ 0B9H ; Set buffer address (BC)
ZRD128:  equ 0BAH ; Read 128 bytes sector from disc (use ZSLDSC, ZSETRK, ZSETSC and ZSETBF to set parameters)
ZWR128:  equ 0BBH ; Write 128 bytes sector to disc (use ZSLDSC, ZSETRK, ZSETSC and ZSETBF to set parameters)
ZZTIME:  equ 0BCH ; Setup CTC ch 2 and 3 to generate 1s interrupts
ZFDRST:  equ 0BDH ; Resets FDC after an error. Resets also PSG. A = drive number
ZSRSRS:  equ 0BEH ; Clears the screen to 40 cols, resets all chars, remove sprites, resets FDC and PSG, masks keyboard, fire and ADC interrupts.
ZLOGO:   equ 0BFH ; Outputs logo
ZPINIT:  equ 0C0H ; Sets PSG register 7 to 7F and all other registers to 0
ZSREG:   equ 0C1H ; Sends address in BC to VRAM. Data out on port 8. Delay necessary.
ZVRIN:   equ 0C2H ; Gets address in BC from VRAM.
ZVROUT:  equ 0C3H ; Writes data in A to VRAM address BC.
ZPLOT:   equ 0C4H ; Plot a pixel
ZPLTXY:  equ 0C5H ; Plot a point with line type
ZPOINT:  equ 0C6H ; Returns pixel status
ZPNTXY:  equ 0C7H ; Returns pixel status
ZDRWTO:  equ 0C8H ; Draw a line
ZPOLYG:  equ 0C9H ; Draw a polygon
ZORGCO:  equ 0CAH ; Alter X and Y coords
ZCALAD:  equ 0CBH ; Returns VRAM address at X Y coords
ZSETCL:  equ 0CCH ; Set pattern generator
ZFILL:   equ 0CDH ; Fill are in screen
ZIMULT:  equ 0CEH ; Multiplies DE by BC, return value in DEHL (DE most significant)
ZPRM:    equ 0CFH ; Output message to screen. Data follows the call (next to 0xCF). Message terminated adding 0x80 to the last character in message. RAM only!
                  ; To print on ROM, use RST 18H instead (macro MCAL_ROM_ZPRM).
ZVOUT:   equ 0D0H ; Outputs A to screen, without changing cursor pos.
ZSCURS:  equ 0D1H ; Returns VRAM addresses relating current cursor pos
ZROM:    equ 0D2H ; Starts exec from ROM 0x4004
Z80INIT: equ 0D3H ;
ZST80:   equ 0D4H ;


; text foreground and background colors
TCOLR:   equ 0FB38H   ; 0..3 = background, 4..7 = foreground

COLOR_TRANSPARENT:  equ 0
COLOR_BLACK:        equ 1
COLOR_MEDIUM_GREEN: equ 2
COLOR_LIGHT_GREEN:  equ 3
COLOR_DARK_BLUE:    equ 4
COLOR_LIGHT_BLUE:   equ 5
COLOR_DARK_RED:     equ 6
COLOR_CYAN:         equ 7
COLOR_MEDIUM_RED:   equ 8
COLOR_LIGHT_RED:    equ 9
COLOR_DARK_YELLOW:  equ 10
COLOR_LIGHT_YELLOW: equ 11
COLOR_DARK_GREEN:   equ 12
COLOR_MAGENTA:      equ 13
COLOR_GREY:         equ 14
COLOR_WHITE:        equ 15

CUSCOL: equ 0FB4AH		; cursor column (0..31/39/79)
CUSROW: equ 0FB4BH		; cursor row (0..23)

; AY-3-8910 registers
PSG_R0:  equ 0  ; Channel A Fine Tone Period    (8 bit)
PSG_R1:  equ 1  ; Channel A Coarse Tone Period  (4 bit)
PSG_R2:  equ 2  ; Channel B Fine Tone Period    (8 bit)
PSG_R3:  equ 3  ; Channel B Coarse Tone Period  (4 bit)
PSG_R4:  equ 4  ; Channel C Fine Tone Period    (8 bit)
PSG_R5:  equ 5  ; Channel C Coarse Tone Period  (4 bit)
PSG_R6:  equ 6  ; Noise Period                  (5 bit)
PSG_R7:  equ 7  ; /Enable                       (7=/IOB, 6=/IOA, 5=/NoiseC, 4=/NoiseB, 3=/NoiseA, 2=/ToneC, 1=/ToneB, 0=/ToneA)
PSG_R10: equ 8  ; Channel A Amplitude           (4=M, 3..0=L)
PSG_R11: equ 9  ; Channel B Amplitude           (4=M, 3..0=L)
PSG_R12: equ 10 ; Channel C Amplitude           (4=M, 3..0=L)
PSG_R13: equ 11 ; Envelope Period Fine Tune E   (8 bit)
PSG_R14: equ 12 ; Envelope Period Coarse Tune E (8 bit)
PSG_R15: equ 13 ; Envelope Shape/Cycle          (3=CONT, 2=ATT, 1=ALT, 0=HOLD)
PSG_R16: equ 14 ; I/O Port A Data Store         (8 bit) - output to keyboard
PSG_R17: equ 15 ; I/O Port B Data Store         (8 bit) - input from keyboard



MCAL: macro mosFunc
  RST 08H
  defb mosFunc
endm

MCAL_ROM_ZPRM: macro
  RST 18H
endm
