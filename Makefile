SCHEME=/usr/local/bin/scheme
LIBDIRS=/Users/zhengyu/Documents/语言学习/scheme/libraries

all: run

run: 
	$(SCHEME) main.ss --libdirs $(LIBDIRS)