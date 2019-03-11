;   Diagnostic ROM for the Tatung Einstein TC-01
;
;   Burn this firmware on a 2764 EPROM and place it in the free slot next to the original firmware.
;   Currently it tests for:
;     - 64K dynamic RAMs (I008, I010, I011, I012, I013, I014, I015 and I016 - 8x4164)
;     - PSG (AY-3-8910) and connected keyboard
;     - VDP (TMS9129) and VRAM
;     - FDC (FD1770)
;     - CTC
;     - PCI (8251 USART)
;     - Z80PIO
;
;
;   Copyright 2019 by Fabrizio Di Vittorio (fdivitto2013@gmail.com - https://github.com/fdivitto)
;
;   This program is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, either version 3 of the License, or
;   (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program.  If not, see <http://www.gnu.org/licenses/>.


include "einstein.inc"


; alternate ROM always mapped at 4000h
org 4000h


; original firmware (Xtal MOS) checks for 00h at 4000h to detect alternate ROM presence
  defb 00h


;********************************************************
ROMEntry:
    DI

    ; setup stack on the high RAM
    LD SP, 9000h

    ; set backdrop color to black
    LD C, 07h
    LD B, 0F1h
    CALL setVDPRegister

    CALL printInfo
    CALL checkLowRAM
    CALL NC, checkHighRAM
    CALL NC, checkPSG
    CALL NC, checkVDP
    CALL NC, checkFDC
    CALL NC, checkCTC
    CALL NC, checkPCI
    CALL NC, checkPIO

    LD A, COLOR_WHITE
    CALL setTextColor
    EI
    MCAL ZINIT


;********************************************************
; printInfo
printInfo:
    MCAL_ROM_ZPRM
    defm "\n         System Diagnostic Tool\r", '\n' + 80h
    MCAL_ROM_ZPRM
    defm "     2019 - By Fabrizio Di Vittorio\r\n", '\n' + 80h
    RET


;********************************************************
; setVDPRegister
; C = register
; B = value
setVDPRegister:	
	LD	A, B
	OUT	(VDP_REG), A
	PUSH	AF
	POP	AF
	LD	A, C
  OR 80h
	OUT	(VDP_REG), A
	RET


;********************************************************
;* Delay
;* Params:   delay is BC
;* Changes:  BC, A
delay:
    EX	(SP),HL    ; waste 19 cycles
    EX	(SP),HL    ; waste 19 cycles
    DEC BC
    LD A, B
    OR C
    RET Z
    JR delay


;********************************************************
;* setColor
;* Set foreground and background colors
;* B = foreground color (see colors in einstein.inc)
;* C = background color (see colors in einstein.inc)
setColor:
    LD A, B
    SLA A
    SLA A
    SLA A
    SLA A
    OR C
    LD (TCOLR), A
    RET

;********************************************************
;* setTextColor
;* Set text color only
;* A = text color (see colors in einstein.inc)
setTextColor:
    LD B, A
    LD A, (TCOLR)
    AND 0Fh
    SLA B
    SLA B
    SLA B
    SLA B
    OR B
    LD (TCOLR), A
    RET


;********************************************************
;* setCursorCol
;* Set cursor column
;* A = column (0..31/39/79)
;* B = row (0..23)
setCursorCol:
    LD (CUSCOL), A
    RET


;********************************************************
;* setCursorRow
;* Set cursor column
;* A = row (0..23)
setCursorRow:
    LD (CUSROW), A
    RET


;********************************************************
;* printPageAddress
;* A = starting pos (cursor pos)
;* print hex value in HL only if L = 0
printPageAddress:    
    CALL setCursorCol

    LD A, L
    OR A
    RET NZ

    ; print HL
    MCAL ZPR4HX
    LD A, '\r'
    MCAL ZOUTC
    RET


;********************************************************
;* showRAMError
;* HL = address
;* B = expected value
;* A = read value
showRAMError:
    LD E, A   ; now E is the read value
    LD D, B   ; now D is the expected value

    LD A, 19
    CALL printFAIL

    LD A, COLOR_CYAN
    CALL setTextColor

    MCAL_ROM_ZPRM
    defm "\rAt", ' ' + 80h

    ; print HL
    MCAL ZPR4HX

    MCAL_ROM_ZPRM
    defm " reading", ' ' + 80h

    ; print read value + space
    LD A, E
    MCAL ZP2HXZ

    MCAL_ROM_ZPRM
    defm "instead of", ' ' + 80h

    ; print expected value
    LD A, D
    MCAL ZPR2HX

    MCAL_ROM_ZPRM
    defm "\r\nTry:", ' ' + 80h

    ; chip detect, bits in D with 1 have different value
    LD A, D
    XOR E
    LD D, A

    BIT 0, D
    JR Z, next1
    MCAL_ROM_ZPRM
    defm "I008", ' ' + 80h
  next1:
    BIT 1, D
    JR Z, next2
    MCAL_ROM_ZPRM
    defm "I010", ' ' + 80h
  next2:
    BIT 2, D
    JR Z, next3
    MCAL_ROM_ZPRM
    defm "I011", ' ' + 80h
  next3:
    BIT 3, D
    JR Z, next4
    MCAL_ROM_ZPRM
    defm "I012", ' ' + 80h
  next4:
    BIT 4, D
    JR Z, next5
    MCAL_ROM_ZPRM
    defm "I013", ' ' + 80h
  next5:
    BIT 5, D
    JR Z, next6
    MCAL_ROM_ZPRM
    defm "I014", ' ' + 80h
  next6:
    BIT 6, D
    JR Z, next7
    MCAL_ROM_ZPRM
    defm "I015", ' ' + 80h
  next7:
    BIT 7, D
    JR Z, showRAMError_end
    MCAL_ROM_ZPRM
    defm "I016", ' ' + 80h
  showRAMError_end:
    MCAL ZCRLF  ; CR + LF
    SCF ; carry=1 signals error
    RET


;********************************************************
; error reading RAM, expected 00h, got A
RAM_error_00:
    LD B, 0
    JP showRAMError


;********************************************************
; error reading RAM, expected FFh, got A
RAM_error_FF:
    LD B, 0FFh
    JP showRAMError


;********************************************************
; printOK
; print OK and NL in green
printOK:
    LD A, COLOR_LIGHT_GREEN
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "OK", 15h, '\r', '\n' + 80h  ; 15 = CTRL-U (delete to end of line)
    RET


;********************************************************
; printFAIL
; print FAIL and NL in red
; A = starting column
printFAIL:
    CALL setCursorCol
    LD A, COLOR_LIGHT_RED
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "FAIL!", 15h, '\r', '\n' + 80h  ; 15 = CTRL-U (delete to end of line)
    RET


;********************************************************
; checkLowRAM (0000h - 7FFFh)
; We are executing ROM on lower memory, hence we need ZRDMEM to read RAM. Writing can be done directly.
checkLowRAM:
    LD A, COLOR_LIGHT_YELLOW
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "Checking LO DRAM...", ' ' + 80h

    LD HL, 0000h

  checkLowRAM_loop1:
    LD A, 19
    CALL printPageAddress

    ; save original value
    MCAL ZRDMEM ; perform "LD A, (HL)" in RAM
    LD D, A

    ; store 00h
    LD (HL), 00h
    MCAL ZRDMEM ; perform "LD A, (HL)" in RAM
    CP 00h
    JR NZ, RAM_error_00

    ; store FFh
    LD (HL), 0FFh
    MCAL ZRDMEM ; perform "LD A, (HL)" in RAM
    CP 0FFh
    JR NZ, RAM_error_FF

    ; restore original value
    LD (HL), D

    ; next address
    INC HL

    ; end of lower RAM? (H = 80h)
    BIT 7, H
    JR Z, checkLowRAM_loop1

    ; test OK
    CALL printOK
    OR A   ; carry=0 signals OK
    RET


;********************************************************
; checkHighRAM (8000h - FFFFh)
checkHighRAM:
    LD A, COLOR_LIGHT_YELLOW
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "Checking HI DRAM...", ' ' + 80h

    LD HL, 8000h

  checkHighRAM_loop1:
    LD A, 19
    CALL printPageAddress

    ; save original value
    LD D, (HL)

    ; try 00h
    LD (HL), 00h
    LD A, (HL)
    OR A
    JP NZ, RAM_error_00

    ; try FFh
    LD (HL), 0FFh
    LD A, (HL)
    CP 0FFh
    JP NZ, RAM_error_FF

    ; restore original value
    LD (HL), D

    ; next address
    INC HL

    ; end of high RAM? (check HL = 0000h)
    LD A, 00h
    CP H
    JR NZ, checkHighRAM_loop1

    ; test OK
    CALL printOK
    OR A   ; carry=0 signals OK
    RET


;********************************************************
; checkPSGRegister
; A = PSG register
; C = expected value
; out:
;   flag Z = 1 success
;   E = PSG register (copy of A)
checkPSGRegister:
    LD E, A   ; saves selected register in E
    OUT (PSG_SEL), A
    ; read value of selected register
    IN A, (PSG_RD)
    CP C
    RET


;********************************************************
; setPSGRegister
; A = register
; C = value
setPSGRegister:
    OUT (PSG_SEL), A
    LD A, C
    OUT (PSG_WR), A
    RET


;********************************************************
; checkPSG (AY-3-8910 - I030)
checkPSG:
    LD A, COLOR_LIGHT_YELLOW
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "\rChecking PSG..", '.' + 80h

    ; softreset (I030 - I028 - I026)
    XOR A
    LD B, 020h
  checkPSG_1:
    OUT (SFTRST), A
    EX	(SP),HL    ; waste 19 cycles
    EX	(SP),HL    ; waste 19 cycles
    DJNZ checkPSG_1

    ; read and check registers (except 0E and 0F)
    LD B, 0Eh
  checkPSG_2:
    ; select register B - 1
    LD A, B
    DEC A
    LD C, 00h
    CALL checkPSGRegister
    JP NZ, PSGResetError
    DJNZ checkPSG_2

    ; read and check IO registers 0Eh and 0Fh (port A and B)
    LD B, 2h
  checkPSG_3:
    ; select register 'B' + 0D
    LD A, B
    ADD 0Dh
    LD C, 0FFh
    CALL checkPSGRegister
    JP NZ, PSGResetError
    DJNZ checkPSG_3

    ; MOS reset PSG and FDC
    MCAL ZFDRST

    ; '1'     31h (PA1 -> PB0)
    MCAL_ROM_ZPRM
    defm "Wait beep. Press '1'    ", '\r' + 80h
    CALL delayBeepAndWait
    CP 31h
    JP NZ, PSGKeyError

    ; 'ENTER' 0Dh (PA0 -> PB5)
    LD A, 15
    CALL setCursorCol
    MCAL_ROM_ZPRM
    defm "Wait beep. Press 'ENTER'", '\r' + 80h
    CALL delayBeepAndWait
    CP 0Dh
    JP NZ, PSGKeyError

    ; 'L'     4Ch (PA2 -> PB1)
    LD A, 15
    CALL setCursorCol
    MCAL_ROM_ZPRM
    defm "Wait beep. Press 'L'    ", '\r' + 80h
    CALL delayBeepAndWait
    CP 4Ch
    JP NZ, PSGKeyError

    ; '5'     35h (PA4 -> PB2)
    LD A, 15
    CALL setCursorCol
    MCAL_ROM_ZPRM
    defm "Wait beep. Press '5'    ", '\r' + 80h
    CALL delayBeepAndWait
    CP 35h
    JP NZ, PSGKeyError

    ; '8'     38h (PA3 -> PB3)
    LD A, 15
    CALL setCursorCol
    MCAL_ROM_ZPRM
    defm "Wait beep. Press '8'    ", '\r' + 80h
    CALL delayBeepAndWait
    CP 38h
    JP NZ, PSGKeyError

    ; 'E'     45h (PA5 -> PB4)
    LD A, 15
    CALL setCursorCol
    MCAL_ROM_ZPRM
    defm "Wait beep. Press 'E'    ", '\r' + 80h
    CALL delayBeepAndWait
    CP 45h
    JP NZ, PSGKeyError

    ; 'A'     41h (PA6 -> PB6)
    LD A, 15
    CALL setCursorCol
    MCAL_ROM_ZPRM
    defm "Wait beep. Press 'A'    ", '\r' + 80h
    CALL delayBeepAndWait
    CP 41h
    JP NZ, PSGKeyError

    ; 'F6'    86h (PA7 -> PB7)
    LD A, 15
    CALL setCursorCol
    MCAL_ROM_ZPRM
    defm "Wait beep. Press 'F6'   ", '\r' + 80h
    CALL delayBeepAndWait
    CP 86h
    JP NZ, PSGKeyError

    ; delay to avoid last key up
    LD BC, 0FFFFh
    CALL delay

    ; tests  OK
    LD A, 15
    CALL setCursorCol
    CALL printOK

    ; MOS reset PSG and FDC
    MCAL ZFDRST
    OR A   ; carry=0 signals OK
    RET

  checkPSG_exit_ERROR:
    ; MOS reset PSG and FDC
    MCAL ZFDRST
    SCF    ; carry=1 signals error
    RET

  PSGResetError:
    LD D, A
    CALL showPSGResetError
    JP checkPSG_exit_ERROR

  PSGKeyError:
    LD A, 15
    CALL printFAIL
    LD A, COLOR_CYAN
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "Try I030 or Keyboard\r", '\n' + 80h
    JP checkPSG_exit_ERROR

;********************************************************
; waitAnyKey
; wait a key until timeout
; Return: flag Z: 1=timeout, 0=key pressed
; Return: A: character read or 00 on timeout
waitAnyKey:
    LD BC, 04000h
  waitAnyKey_1:
    PUSH BC
    MCAL ZKSCAN
    POP BC
    OR A    ; key pressed (A!=0)?
    RET NZ  ; yes, exit
    DEC BC
    LD A, B
    OR C    ; timeout (A==B==0)?
    RET Z   ; yes, exit
    JR waitAnyKey_1


;********************************************************
; delayBeepAndWait
; Return: flag Z: 1=timeout, 0=key pressed
; Return: A: character read or 00 on timeout
delayBeepAndWait:
    ; delay
    LD BC, 0FFFFh
    CALL delay
    ; beep
    LD A, 07h
    MCAL ZOUTC
    ; wait key
    CALL waitAnyKey
    RET

;********************************************************
; showPSGResetError
; E = register
; D = got value
; C = expected
showPSGResetError:
    LD A, 15
    CALL printFAIL

    LD A, COLOR_CYAN
    CALL setTextColor

    MCAL_ROM_ZPRM
    defm "Register", ' ' + 80h

    ; print register + space
    LD A, E
    MCAL ZP2HXZ

    MCAL_ROM_ZPRM
    defm "is", ' ' + 80h

    ; print got value + space
    LD A, D
    MCAL ZP2HXZ

    MCAL_ROM_ZPRM
    defm "instead of", ' ' + 80h

    ; print expected value
    LD A, C
    MCAL ZPR2HX

    MCAL_ROM_ZPRM
    defm "\r\nTry I030 I028 I026\r", '\n' + 80h

    RET


;********************************************************
; writeVRAM
; HL = address
; A = value to write
; changes B
writeVRAM:
    LD B, A
    LD A, L
    OUT (09h), A  ; load low address
    LD A, H
    OR 40h
    OUT (09h), A  ; load high address
    LD A, B
    OUT (08h), A  ; write value
    RET


;********************************************************
; readVRAM
; HL = address
; A = value read
readVRAM:
    LD A, L
    OUT (09h), A  ; load low address  
    LD A, H
    AND 3Fh
    OUT (09h), A  ; load high address
    ; delay required by the VDC to read RAM
    EX	(SP),HL    ; waste 19 cycles
    EX	(SP),HL    ; waste 19 cycles
    EX	(SP),HL    ; waste 19 cycles
    EX	(SP),HL    ; waste 19 cycles
    IN A, (08h)    ; read value
    RET


;********************************************************
; checkVDP (TMS9129 - I038)
checkVDP:
    LD A, COLOR_LIGHT_YELLOW
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "Checking VDP...", '.' + 80h

    ; test VRAM
    LD HL, 0000h
checkVDP_1:

    ; change backdrop color
    LD C, 07h
    LD B, L
    CALL setVDPRegister

    ; print working address
    LD A, 15
    CALL printPageAddress

    ; change backdrop color
    LD C, 07h
    LD B, 0F1h
    CALL setVDPRegister

    ; ** save previous value (in C)    
    CALL readVRAM
    LD C, A

    ; ** write FFh    
    LD A, 0FFh
    CALL writeVRAM

    ; ** read value (should be FFh)
    CALL readVRAM
    CP 0FFh
    JR NZ, checkVDP_ramError

    ; ** write previous value (from C)    
    LD A, C
    CALL writeVRAM

    ; move to next address
    INC HL
    LD A, H
    CP 40h  ; reached 4000h?
    JR NZ, checkVDP_1 ; no, repeat

    ; tests OK
    LD A, 15
    CALL setCursorCol
    CALL printOK
    OR A   ; carry=0 signals OK

  checkVDP_exit:
    RET

  checkVDP_ramError:
    LD A, 15
    CALL printFAIL
    LD A, COLOR_CYAN
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "VRAM Error.\r\nTry I038 I040 I041\r", '\n' + 80h
    SCF    ; carry=1 signals error
    JP checkVDP_exit


;********************************************************
; checkFDCRegiter
; C = register
; B = value
; out Z = 1 on OK
checkFDCRegiter:
  OUT (C), B
  IN A, (C)
  CP B
  RET

;********************************************************
; checkFDC (FD1770 - I042)
checkFDC:
    LD A, COLOR_LIGHT_YELLOW
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "Checking FDC...", '.' + 80h

    ; write/read data register
    ; test for 00h
    ;LD C, FDC_DATA
    ;LD B, 00h
    ;CALL checkFDCRegiter
    ;JR NZ, checkFDC_error
    ; test for FFh
    ;LD B, 0FFh
    ;CALL checkFDCRegiter
    ;JR NZ, checkFDC_error

    ; reset PSG and FDC
    MCAL ZFDRST

    ; select drive 0, side 0
    LD A, 1
    OUT	(DRVSEL),A

    ; move to track 0
    MCAL ZHMDSC
    JP NZ, checkFDC_error

    ; move head to track 39
    LD A, 39
    OUT (FDC_DATA), A
    LD A, 19h    ; seek, no spinup sequence, no verify, 12ms
    MCAL ZDCMD   ; ZDCMD returns A=FF on command not accepted (on timeout), A=00 on ok
    OR A
    JR NZ, checkFDC_error

    LD BC, 4000h
    CALL delay

    ; wait for command ends or timeout
    LD BC, 0FFFFh
  checkFDC_1:
    DEC BC
    LD A, B
    OR C
    JR Z, checkFDC_error  ; timeout?
    IN A, (FDC_CMD)       ; read status
    AND 01h
    JR NZ, checkFDC_1    ; command not still completed, repeat

    ; check track is correct
    IN A, (FDC_TRACK)
    CP 39
    JR NZ, checkFDC_error

    ; tests OK
    LD A, 15
    CALL setCursorCol
    CALL printOK
    OR A   ; carry=0 signals OK

  checkFDC_exit:
    RET

  checkFDC_error:
    LD A, 15
    CALL printFAIL
    LD A, COLOR_CYAN
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "Try I042 I043 I044 I045 I046 I026\r", '\n' + 80h
    SCF    ; carry=1 signals error
    JP checkVDP_exit


;********************************************************
; checkCTCChannel
; C = channel (CTC_CH0, CTC_CH1...)
; z = 1 -> error
; z = 0 -> ok
checkCTCChannel:
    ; write control word
    LD A, 15h;        ; no int, timer mode, 16 prescaler, rising edge, auto trigger on load, time constant follows, continue operation, control word
    OUT (C), A  
    ; time constant
    LD A, 0FFh
    OUT (C), A

    ; check counter 256 times
    LD B, 0FFh
  checkCTCChannel_1:
    IN A, (C)
    LD D, A
    NOP
    NOP
    IN A, (C)
    CP D
    RET Z                   ; counter didn't move, error!
    DJNZ checkCTCChannel_1  ; test again

    ; ok
    OR 0FFh  ; z = 0
    RET


;********************************************************
; checkCTC (CTC - I058)
checkCTC:
    LD A, COLOR_LIGHT_YELLOW
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "Checking CTC...", '.' + 80h

    ; check channel 0
    LD C, CTC_CH0
    CALL checkCTCChannel
    JP Z, checkCTC_error

    ; check channel 1
    LD C, CTC_CH1
    CALL checkCTCChannel
    JP Z, checkCTC_error

    ; check channel 2
    LD C, CTC_CH2
    CALL checkCTCChannel
    JP Z, checkCTC_error

    ; check channel 3
    LD C, CTC_CH3
    CALL checkCTCChannel
    JP Z, checkCTC_error

    ; tests OK
    LD A, 15
    CALL setCursorCol
    CALL printOK
    OR A   ; carry=0 signals OK

  checkCTC_exit:
    RET

  checkCTC_error:
    LD A, 15
    CALL printFAIL
    LD A, COLOR_CYAN
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "Try I058 I026 \r", '\n' + 80h
    SCF    ; carry=1 signals error
    JP checkCTC_exit


;********************************************************
; checkPCI (8251 USART - I060)
checkPCI:
    LD A, COLOR_LIGHT_YELLOW
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "Checking PCI...", '.' + 80h

    ; read status (should be 05h -> TxRDY, TxEMPTY)
    IN A, (PCI_CTRL)
    CP 05h
    JP NZ, checkPCI_error
    
    ; write one byte
    LD A, 0FFh
    OUT (PCI_DATA), A

    ; read status (now should be 04h -> TxEMPTY)
    IN A, (PCI_CTRL)
    CP 04h
    JP NZ, checkPCI_error

    ; tests OK
    LD A, 15
    CALL setCursorCol
    CALL printOK
    OR A   ; carry=0 signals OK

  checkPCI_exit:
    RET

  checkPCI_error:
    LD A, 15
    CALL printFAIL
    LD A, COLOR_CYAN
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "Try I060 I026 \r", '\n' + 80h
    SCF    ; carry=1 signals error
    JP checkCTC_exit



;********************************************************
; checkPIO (Z80 PIO - I063)
checkPIO:
    LD A, COLOR_LIGHT_YELLOW
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "Checking PIO...", '.' + 80h

    ; configure port A for mode 3
    LD A, 0CFh
    OUT (PIO_CTRL_A), A
    ; configure all bits of port A as output
    XOR A
    OUT (PIO_CTRL_A), A
    ; write 00h and test
    OUT (PIO_DATA_A), A
    IN A, (PIO_DATA_A)
    OR A
    JR NZ, checkPIO_error
    ; write FFh and test
    LD A, 0FFh
    OUT (PIO_DATA_A), A
    IN A, (PIO_DATA_A)
    CP 0FFh
    JR NZ, checkPIO_error

    ; tests OK
    LD A, 15
    CALL setCursorCol
    CALL printOK
    OR A   ; carry=0 signals OK

  checkPIO_exit:
    RET

  checkPIO_error:
    LD A, 15
    CALL printFAIL
    LD A, COLOR_CYAN
    CALL setTextColor
    MCAL_ROM_ZPRM
    defm "Try I063 I026 \r", '\n' + 80h
    SCF    ; carry=1 signals error
    JP checkCTC_exit
