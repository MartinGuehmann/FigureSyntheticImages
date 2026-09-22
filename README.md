# SyntheticDataMouseKidneyGlomeruli — SyntheticImages

Code and figure for a publication comparing real and synthetically generated
microscopy images of mouse kidney glomeruli, showing the source image and
successive synthetic iterations side by side with a scalebar and workflow
arrows.

## Contents

- `MakeFigure.R` — R script that builds the multipanel figure (`Figure1_SyntheticImages`).
  It reads the input images, labels each panel (A, B, C, ...), adds a scalebar
  to the first panel, inserts directional arrows between the source image and
  its synthetic derivatives, and arranges everything into two rows.
- `07.jpg`, `08.jpg` — source (real) glomerulus images, one per row.
- `1_3i.png`, `2_3i.png`, `3_3i.png` — synthetic iterations derived from `07.jpg`.
- `1_5i.png`, `2_5i.png`, `3_5i.png` — synthetic iterations derived from `08.jpg`.
- `Figure1_SyntheticImages.{pdf,svg,eps,tiff}` — generated figure output (not
  tracked in git; produced by running the script, see `.gitignore`).

## Requirements

R with the following packages:

- cowplot
- ggplot2
- png
- jpeg
- grid
- svglite

## Usage

Open `MakeFigure.R` in RStudio and run it, or from the command line:

```sh
Rscript MakeFigure.R
```

The script sets its working directory to its own location, reads the image
files listed under `files` in the `# ---- Files ----` section, and writes
`Figure1_SyntheticImages.pdf`, `.svg`, `.eps`, and `.tiff` to the same
directory.

## License

- **Code** (`MakeFigure.R`): [MIT License](LICENSE).
- **Images, figures, and data** (`.png`, `.jpg`, `.tiff`, `.pdf`, `.svg`, `.eps`
  files): [CC BY 4.0](LICENSE-DATA.txt).
