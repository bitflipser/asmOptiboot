; ################################################################################ ;
;                                                                                  ;
; asmOptiboot - Optiboot rewrite in Assembler with many extras                     ; 
;              (https://github.com/bitflipser/asmOptiboot)                         ;
;                                                                                  ;
; by bitflipser, 2018                                                              ;
;                                                                                  ;
; based on Optiboot (https://githib.com/Optiboot/optiboot)                         ;
;  "Although it has evolved considerably, Optiboot builds on the original work     ;
;   of Jason P. Kyle (stk500boot.c), Arduino group (bootloader),                   ;
;   Spiff (1K bootloader), AVR-Libc group and Ladyada (Adaboot).                   ;
;                                                                                  ;
;   Optiboot is the work of Peter Knight (aka Cathedrow). Despite some             ;
;   misattributions, it is not sponsored or supported by any organisation          ;
;   or company including Tinker London, Tinker.it! and Arduino.                    ;
;   Maintenance of optiboot was taken over by Bill Westfield (aka WestfW) in 2011.";
;                                                                                  ;
; fits into 256 words of FLASH                                                     ; 
;      ( minimum size: 390 bytes/185 words)                                        ;
;      ( maximum size: 502 bytes/251 words)                                        ;
;                                                                                  ;
; extended functions:                                                              ;
;   - save MCUSR reset status in R2                                                ;
;   - read/write EEPROM                                                            ;
;   - read SIGNATURE from MCU                                                      ;
;   - read FUSE                                                                    ;
;   - read OSCCAL                                                                  ;
;   - read/write LOCK bits                                                         ;
;   - auto increment address                                                       ;
;   - increased USART speed                                                        ;
;   - Do_spm vector                                                                ;
;   - WRITE_FLASH_PAGE function for user application                               ;
;                                                                                  ;
; MIT License                                                                      ;
;                                                                                  ;
; Copyright (c) 2018 bitflipser                                                    ;
;                                                                                  ;
; Permission is hereby granted, free of charge, to any person obtaining a copy     ;
; of this software and associated documentation files (the "Software"), to deal    ;
; in the Software without restriction, including without limitation the rights     ;
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell        ;
; copies of the Software, and to permit persons to whom the Software is            ;
; furnished to do so, subject to the following conditions:                         ;
;                                                                                  ;
; The above copyright notice and this permission notice shall be included in all   ;
; copies or substantial portions of the Software.                                  ;
;                                                                                  ;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR       ;
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,         ;
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE      ;
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER           ;
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,    ;
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE    ;
; SOFTWARE.                                                                        ;
;                                                                                  ;
; ################################################################################ ;

asmOptiboot supports higher baud rates - initial setup is 250.000 baud

easy to port to ATmega48/88/168/644
