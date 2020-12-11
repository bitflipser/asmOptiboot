asmOptiboot provides an user application callable function to write one(!) FLASH page with data from SRAM

This might be helpful in some applications for storing bigger amounts of data then fit into EEPROM 
but small enpugh to fit into FLASH. But beware of the limited write/erase cycles of 10.000 for FLASH (compared to 100.000 for EEPROM)
(notice Atmel's AppNote 'AVR105: Power Efficient High Endurance Parameter Storage in Flash Memory')

This function is for experienced users only, because it can destroy user application by writing invalid data there.

It can even write into bootloader section (as long as it is not write-protected by the LOCK bits) and 'brick' your device in the means, that it has to be restored with a programming device

WRITE_FLASH_PAGE will always write a full page of FLASH memory (64 words/128 bytes on ATmega328/P)

To call WRITE_FLASH_PAGE from assembler:

- place the data source address in SRAM into R25:R24

- place the destination address in FLASH multiplied by 2 into R23:R22

- call SMALLBOOTSTART+2 resp. FLASHEND-0xfd (0x3f02 on ATmega328/P)

The destination address in FLASH has to be placed as byte address(!), that is the FLASH address multiplied by 2 (as it is needed for FLASH access with LPM and SPM instructions)

The incremented addresses will be returned in R25:R24 and R23:R22 and can be used for successive writing

The following registers will be scrached and have to be saved before calling WRITE_PAGE_FLASH if they contain valid data

- R0, R1, R19, R27:R26 (X), R31:R30 (Z)
