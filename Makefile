# make
#SRC=crtc.asm
#SRC=filter.asm
SRC=sidN.asm
OUT=sid.bin
SNAPFILE=cpc.sna

# 1024 = $0400 im Snapshot = $0300 im RAM
SEEK=1024

# 4352 = $1100 im Snapshot = $1000 im RAM
#SEEK=4352

# 33024 = $8100 => $8000
#SEEK=33024
#SEEK=28928

sid:
	/Applications/zasm -w $(SRC) -b -o $(OUT)
	dd bs=1 if=$(OUT) of=$(SNAPFILE) seek=$(SEEK) conv=notrunc
	
fm:
	/Applications/zasm -w1 fm1.asm -b -o fm.bin
	dd bs=1 if=fm.bin of=$(SNAPFILE) seek=$(SEEK) conv=notrunc

backup:
	cp -v *.asm bak/

