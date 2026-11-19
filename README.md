# fortphoto

fortphoto is a photo editor for the work before printing or publishing:
selections, crop, rotate and straighten, levels, colour, sharpening, resizing,
pen annotations, JPEG and PNG export, and printing with an exact preview.

- **Edit list.** Every edit is a step applied to the original, so any step can
  be undone or redone, also from the History tab. The screen shows a preview
  of the steps; export and print render them at full resolution.
- **Select.** Rectangle, ellipse, lasso and magic wand (tolerance, contiguous
  or global), combined as new, add, subtract or intersect. Levels, colour and
  sharpening apply inside the selection; Crop to selection crops to its box.
- **Crop and turn.** Crop freely or at 1:1, 3:2, 4:3, 16:9 or the photo's own
  ratio; rotate by quarter turns, flip, and straighten by up to 15 degrees
  with the largest crop that has no empty corners.
- **Tone and colour.** Live histogram; black point, midtones and white point
  with Auto levels; exposure, brightness, contrast, saturation, temperature
  and tint; a white-balance picker; unsharp masking.
- **Size.** Pixel size with or without proportions, resolution and the print
  size it gives; Lanczos resampling.
- **Pen.** The notebook pen of FortShell in six colours.
- **Export and print.** JPEG (quality 92, resolution stored) or PNG next to
  the photo, never replacing a file. Print opens the exact preview:

The print preview and the print come from the same page computation, so what
you see is what the printer gets.

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

Page Up and Page Down step through the JPEG and PNG files in the photo's folder.
Ctrl+O opens, Ctrl+Z and Ctrl+Shift+Z undo and redo, Ctrl+D deselects, Ctrl+P
prints, Enter applies a crop and Escape cancels a gesture or the selection.

## Build and install

Pixel operations come from [FortPix](https://code.tugraz.at/yard/fortpix),
files from [FortImg](https://code.tugraz.at/yard/fortimg), the pen model from
[FortInk](https://code.tugraz.at/yard/fortink) and the interface from
[FortUI](https://code.tugraz.at/yard/fortui).

```sh
make            # build
make test
make install    # ~/.local/bin/fortphoto and a desktop entry for JPEG and PNG
```

`make install` registers fortphoto for JPEG and PNG files, so file managers
such as Thunar offer it under "Open With". fortphoto needs a graphical display
and a printer set up in CUPS; it was tested with an IPP Everywhere queue.

## Browser

`tools/build-web.sh` builds the editor for the browser into `build/web` with
LFortran and Emscripten, from checkouts of the pinned dependencies next to this
repository. Files open through the browser's picker and exports download;
printing needs the desktop application.

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
