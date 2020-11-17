tools\as8085 -o -l AIRBLv25.LST AIRBLv25.asm
tools\aslink -i AIRBLv25.rel
tools\hex2bin -l 0x8000 AIRBLv25.ihx
copy AIRBLv25.ihx ROM.HEX
copy AIRBLv25.BIN ROM.BIN

tools\as8085 -o -l AIRSRv26.LST AIRSRv26.asm
tools\aslink -i AIRSRv26.rel
tools\hex2bin AIRSRv26.ihx
copy AIRSRv26.ihx BIOS.HEX
