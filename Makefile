all: build

.PHONY: build install

DESTDIR ?=
PREFIX ?= /usr/local
BASEDIR := "$(DESTDIR)/$(PREFIX)"
BINDIR := $(BASEDIR)/bin

NAME := mesabox
TARGET_PATH := $(BINDIR)/$(NAME)

build:
	cargo build --release

install: build
	mkdir -p $(BINDIR)
	install -m755 target/release/mesabox $(TARGET_PATH)
	cd $(BINDIR); \
	for cmd in `./mesabox dump-cmds`; do \
		ln -s $(NAME) $$cmd; \
	done

clean:
	cargo clean

uninstall:
	for cmd in `$(TARGET_PATH) dump-cmds`; do \
		rm -f $(BINDIR)/$$cmd; \
	done
	rm -f $(TARGET_PATH)
