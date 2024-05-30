VERSION = 0.1
#CFLAGS = -std=c99 -pedantic -Wall -Os -DVERSION=\"$(VERSION)\"
PREFIX = /usr/local

INCS = $(PREFIX)/include
CFLAGS = -std=c99 -pedantic -O3 -DVERSION=\"$(VERSION)\" -I$(INCS) -I.
LDFLAGS = -lm -lgmp -L$(PREFIX)/lib
CC = cc

SRC = LinApartStandalone.c LinApart.c
OBJ = $(SRC:.c=.o)

all: options standalone

options:
	@echo dmenu build options:
	@echo "CFLAGS   = $(CFLAGS)"
	@echo "LDFLAGS  = $(LDFLAGS)"
	@echo "CC       = $(CC)"

.c.o:
	$(CC) -c $(CFLAGS) $<


standalone: ${OBJ}
	$(CC) -o LinApart ${OBJ} ${LDFLAGS}

clean:
	rm -f LinApart $(OBJ)

.PHONY: all options clean standalone
