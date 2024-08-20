VERSION := 0.1

# Detect the operating system
UNAME_S := $(shell uname -s)

# Set default compiler
CC ?= cc

# Base flags
CFLAGS := -std=c99 -pedantic -Wall -O3 -DVERSION=\"$(VERSION)\"
LDFLAGS := -lm -lgmp

# OS-specific settings
ifeq ($(UNAME_S),Darwin)
	# macOS-specific settings
	BREW_PREFIX := $(shell brew --prefix 2>/dev/null || echo "/opt/homebrew")
	CFLAGS += -I$(BREW_PREFIX)/include
	LDFLAGS += -L$(BREW_PREFIX)/lib
else ifeq ($(UNAME_S),Linux)
	# Linux-specific settings
	CFLAGS += -I/usr/include
	LDFLAGS += -L/usr/lib
else
	# Generic Unix-like system
	PREFIX ?= /usr/local
	CFLAGS += -I$(PREFIX)/include
	LDFLAGS += -L$(PREFIX)/lib
endif

SRC := LinApartStandalone.c LinApart.c
OBJ := $(SRC:.c=.o)
TARGET := LinApart

.PHONY: all options clean

all: options $(TARGET)

options:
	@echo "Build options:"
	@echo "CFLAGS   = $(CFLAGS)"
	@echo "LDFLAGS  = $(LDFLAGS)"
	@echo "CC       = $(CC)"

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

$(TARGET): $(OBJ)
	$(CC) -o $@ $(OBJ) $(LDFLAGS)

clean:
	rm -f $(TARGET) $(OBJ)
