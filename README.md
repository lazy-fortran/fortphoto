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

fortphoto looks like your FortUI theme: `FORTUI_THEME=fluent|fluent-light|fluent-dark|classic`
or `theme = classic` in `~/.config/fortui/config`.

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
| [FortUI](https://github.com/lazy-fortran/fortui) | window (Vulkan), controls, preview canvas, printing over IPP | MIT |
| [fortimg](https://github.com/lazy-fortran/fortimg) | JPEG and PNG decoding | MIT |
| gfortran with OpenMP, fpm | building | GPL-3.0 with runtime exception; MIT |

fortphoto and its Fortran dependencies need no C library at build time. At run
time a window needs X11 and a Vulkan driver, which FortUI opens itself, plus the
fonts of the FortUI theme you use (Classic: `chicago95-fonts`, `ttf-perfectdos`;
Fluent: Selawik or Segoe UI, `ttf-cascadia-code`). Printing talks IPP directly
to the CUPS server or printer.

## Known limits

- No colour management: photos are sent as sRGB and the printer converts them.
- The window does not respond while a page is composed and sent.

## Open dialog and platforms

Choose **Open photo...**, or press Ctrl+O (Cmd+O on macOS). The chooser supports
folder navigation, filename filtering and keyboard selection. Escape cancels.
An unreadable or invalid photo leaves the current photo intact. Without a printer,
the preview uses A4 paper.

`FORTUI_THEME=fluent` follows the desktop light/dark preference and any schedule
managed by the desktop. `fluent-light`, `fluent-dark` and `classic` stay fixed.
The chooser also works with `FORTUI_BACKEND=tui`.

Linux, Windows and macOS builds require fpm 0.13 or later and a C/Fortran compiler.
Use MinGW-w64 UCRT64 on Windows and gfortran on macOS. Run `fpm run` with OpenMP
compiler and linker flags (`-O2 -fopenmp` and `-fopenmp`). Native Windows and macOS
windows use the CPU renderer. Printing uses CUPS/IPP on POSIX systems. The Windows
build currently opens and previews photos without a native Windows print service.

The shared FortUI chooser also supports browser file uploads. FortPhoto's desktop
printer workflow is not a browser application.
