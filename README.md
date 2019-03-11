# TatungEinsteinDiagnosticFirmware
Diagnostic ROM for the Tatung Einstein TC-01


Burn this firmware (rom.bin) on a 2764 EPROM and place it in the free slot next to the original firmware.

Currently it tests for:
  * 64K dynamic RAMs (8x4164)
  * PSG (AY-3-8910) and connected keyboard
  * VDP (TMS9129) and VRAM
  * FDC (FD1770)
  * CTC (timer)
  * PCI (8251 USART)
  * PIO (parallel IO)
     


### Example of all tests passed:
![GitHub Logo](/images/img1.png)


### Example of PSG (Sound Generator and Keyboard controller) not passed:
![GitHub Logo](/images/img2.png)
