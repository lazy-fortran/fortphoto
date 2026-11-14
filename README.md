# fortphoto

fortphoto opens a photo, shows exactly how it will land on the paper, and prints
it. The preview and the print come from the same page computation, so what you
see is what the printer gets.

- **Fill and crop** covers the sheet and cuts off what sticks out, for example
  4:3 photos on 3:2 paper (10 x 15 cm). Drag the photo to choose what stays, and
  zoom in to crop more. The cut-off parts show dimmed around the sheet.
- **Fit whole photo** prints the whole photo inside the printable area and
  leaves the rest of the sheet white.
- **Borderless** whenever the printer offers it for the chosen size.
- The paper that is actually loaded, with its tray and media type, comes from
  the printer and is preselected; other sizes, media types and trays can be
  chosen. Print quality is Draft, Normal or High (default), plus copies.
- The sheet turns with the photo; landscape photos are turned for the printer.
- Low ink and too few photo pixels per inch for a sharp print are shown.

Pages go to the printer as PWG raster at its native resolution (600 dpi on a
Canon TS8700), composed in Fortran, so no driver or filter rescales or adds a
border.

## Use

```sh
fortphoto photo.jpg          # or open one from the file manager: Open With
fortphoto --hold photo.jpg   # print jobs wait in the queue: lp -i JOB -H resume
```

Left and right arrows step through the JPEG and PNG files in the photo's folder.

## Build and install

```sh
make            # build
make test
make install    # ~/.local/bin/fortphoto and a desktop entry for JPEG and PNG
```

`make install` registers fortphoto for JPEG and PNG files, so file managers
such as Thunar offer it under "Open With". fortphoto needs a graphical display
and a printer set up in CUPS; it was tested with an IPP Everywhere queue.

## Dependencies and licences

fortphoto is MIT licensed ([LICENSE](LICENSE)).

| Dependency | Used for | Licence |
|---|---|---|
| [FortUI](https://github.com/lazy-fortran/fortui) | window, controls, preview canvas, printing service | MIT |
| raylib 5 or later | FortUI's graphical backend | zlib |
| libcups 2.3 or later | printing (through FortUI) | Apache-2.0 |
| libturbojpeg (libjpeg-turbo 3) | JPEG decoding | BSD-3-Clause, IJG, zlib |
| libpng 1.6 | PNG decoding | libpng/PNG Reference Library |
| gfortran with OpenMP, fpm | building | GPL-3.0 with runtime exception; MIT |

On Arch Linux: `pacman -S gcc-fortran raylib cups libjpeg-turbo libpng`.
All C libraries are reached through `iso_c_binding`; there is no C code in
fortphoto. JPEG and PNG decoding live in `src/photo_image.f90` alone, so they
can later be replaced by a Fortran decoder.

## Known limits

- With raylib, a click whose press and release arrive before the next frame is
  lost (raylib keeps no queue of mouse button events). Very quick touchpad
  taps can therefore go unnoticed; a normal click works.
- No colour management: photos are sent as sRGB and the printer converts them.
- The window does not respond while a page is composed and sent.
