VERSION = 0.1

# Detect the operating system
.if defined(OS) && $(OS) == "Windows_NT"
 SYSTEM = WINDOWS
.else
 SYSTEM != uname -s
.endif

# Set default compiler
CC ?= cc

# Base flags
CFLAGS = -std=c99 -pedantic -Wall -O3 -DVERSION=\"$(VERSION)\"
LDFLAGS = -lm -lgmp

# OS-specific settings
.if $(SYSTEM) == "Darwin"
 # macOS-specific settings
 BREW_PREFIX != brew --prefix 2>/dev/null || echo "/opt/homebrew"
 CFLAGS += -I$(BREW_PREFIX)/include
 LDFLAGS += -L$(BREW_PREFIX)/lib
.elif $(SYSTEM) == "Linux"
 # Linux-specific settings
 CFLAGS += -I/usr/include
 LDFLAGS += -L/usr/lib
.elif $(SYSTEM) == "WINDOWS"
# Windows-specific settings (assumes MinGW or similar)
 CC = gcc
 CFLAGS += -I/mingw64/include
 LDFLAGS += -L/mingw64/lib
 TARGET = LinApart.exe
.else
 # Generic Unix-like system
 PREFIX ?= /usr/local
 CFLAGS += -I$(PREFIX)/include
 LDFLAGS += -L$(PREFIX)/lib
.endif

SRC = LinApartStandalone.c LinApart.c
OBJ = $(SRC:.c=.o)
TARGET ?= LinApart

.PHONY: all options clean standalone

all: options standalone

options:
	@echo "Build options:"
	@echo "CFLAGS   = $(CFLAGS)"
	@echo "LDFLAGS  = $(LDFLAGS)"
	@echo "CC       = $(CC)"

.c.o:
	$(CC) -c $(CFLAGS) $<

standalone: $(OBJ)
	$(CC) -o $(TARGET) $(OBJ) $(LDFLAGS)

clean:
	rm -f $(TARGET) $(OBJ)
