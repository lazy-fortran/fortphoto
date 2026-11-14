# Test fixtures

Synthetic images made with ImageMagick 7.1:

- `red-16x8.jpg`: `magick -size 16x8 xc:'rgb(200,30,60)' -quality 95 -sampling-factor 1x1 -strip red-16x8.jpg`
- `rgb-2x2.png`: red and green in the top row, blue below:
  `magick -size 2x1 xc:'rgb(255,0,0)' \( -size 2x1 xc:'rgb(0,0,255)' \) -append -fill 'rgb(0,255,0)' -draw 'point 1,0' -define png:color-type=2 -strip rgb-2x2.png`
