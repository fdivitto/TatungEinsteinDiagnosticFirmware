# TatungEinsteinDiagnosticFirmware
Diagnostic ROM for the Tatung Einstein TC-01


Burn this firmware on a 2764 EPROM and place it in the free slot next to the original firmware.

Currently it tests for:
  * 64K dynamic RAMs (8x4164)
  * PSG (AY-3-8910) and connected keyboard
  * VDP (TMS9129) and VRAM
  * FDC (FD1770)
  * CTC (timer)
  * PCI (8251 USART)
  * PIO (parallel IO)
     


![alt text](https://raw.githubusercontent.com/fdivitto/TatungEinsteinDiagnosticFirmware/branch/path/to/img.png)
