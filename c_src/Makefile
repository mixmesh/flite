#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

APP=flite
CFLAGS+=-std=c99 -Wall -O3
LDFLAGS+=-shared 

OBJS = flite_nif.o
LIBS = -lflite_usenglish -lflite_cmulex -lflite -lm

all: ../priv/$(APP)_nif.so

../priv/$(APP)_nif.so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) $(LIBS) -o ../priv/$(APP)_nif.so

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d
