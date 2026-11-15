FPM ?= fpm
PREFIX ?= $(HOME)/.local
# OpenMP renders the page.
FLAGS = FPM_FFLAGS="$${FPM_FFLAGS:-} -O2 -fopenmp" FPM_LDFLAGS="$${FPM_LDFLAGS:-} -fopenmp"

.PHONY: all build run test install clean

all: build

build:
	$(FLAGS) $(FPM) build

run:
	$(FLAGS) $(FPM) run -- $(ARGS)

test:
	$(FLAGS) $(FPM) test

# The program and a desktop entry, so file managers offer "Open With fortphoto"
# for JPEG and PNG files.
install:
	$(FLAGS) $(FPM) install --prefix "$(CURDIR)/build/stage"
	# A new file, not an overwrite: a running fortphoto keeps its program.
	install -Dm755 build/stage/bin/fortphoto "$(PREFIX)/bin/fortphoto"
	install -Dm644 data/fortphoto.desktop "$(PREFIX)/share/applications/fortphoto.desktop"
	sed -i 's|^Exec=.*|Exec=$(PREFIX)/bin/fortphoto %f|' \
		"$(PREFIX)/share/applications/fortphoto.desktop"
	-update-desktop-database "$(PREFIX)/share/applications"

clean:
	$(FPM) clean --skip
