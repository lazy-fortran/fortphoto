FPM ?= fpm
PREFIX ?= $(HOME)/.local
# fortui's raylib window and CUPS printing service; OpenMP renders the page.
FLAGS = FPM_FFLAGS="$${FPM_FFLAGS:-} -O2 -fopenmp -cpp -DENABLE_GUI=1 -DENABLE_PRINT=1" \
	FPM_CFLAGS="$${FPM_CFLAGS:-} -DENABLE_GUI=1" \
	FPM_LDFLAGS="$${FPM_LDFLAGS:-} -fopenmp -lraylib -lcups"

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
	$(FLAGS) $(FPM) install --prefix "$(PREFIX)"
	install -Dm644 data/fortphoto.desktop "$(PREFIX)/share/applications/fortphoto.desktop"
	sed -i 's|^Exec=.*|Exec=$(PREFIX)/bin/fortphoto %f|' \
		"$(PREFIX)/share/applications/fortphoto.desktop"
	-update-desktop-database "$(PREFIX)/share/applications"

clean:
	$(FPM) clean --skip
