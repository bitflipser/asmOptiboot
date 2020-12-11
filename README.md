asmOptiboot - small size ARDUINO UNO / ATmega328 bootloader with EEPROM, FUSE and LOCK-bits access

based on Optiboot (https://githib.com/Optiboot/optiboot)

fits into 256 words of FLASH
     ( minimum size: 390 bytes/185 words, maximum size: 502 bytes/251 words)

extended functions:
  - save MCUSR reset status in R2
  - read/write EEPROM
  - read SIGNATURE from MCU
  - read FUSE
  - read OSCCAL
  - read/write LOCK bits
  - auto increment address
  - increased USART speed
  - Do_spm vector
  - WRITE_FLASH_PAGE function for user application

asmOptiboot supports higher baud rates - initial setup is 250.000 baud

easy to port to ATmega48/88/168/644
