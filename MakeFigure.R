#' Generate multipanel figure with labeled image grid and directional arrows
#'
#' This script constructs a multipanel figure from a set of input images.
#' Each image is rendered as a square raster panel with a left-aligned label
#' above it. Panels are arranged in a grid with configurable horizontal and
#' vertical spacing. Directional arrow annotations are inserted between
#' selected panels to indicate a workflow or transformation sequence.
#'
#' The final figure is exported in both PDF and SVG formats with a fixed
#' aspect ratio derived from the panel layout configuration.
#'
#' @section Pipeline overview:
#' 1. Read image files from working directory
#' 2. Convert images into raster grobs
#' 3. Add labels above each image panel
#' 4. Insert directional arrow annotation grobs
#' 5. Arrange panels into rows with controlled spacing
#' 6. Combine rows into final multipanel layout
#' 7. Export figure to PDF and SVG
#'
#' @section Layout parameters:
#' - `ncol_panels`: Number of image panels per row
#' - `nrow_panels`: Number of rows in final figure
#' - `hgap_frac`: Horizontal spacing between panels
#' - `vgap_frac`: Vertical spacing between rows
#' - `img_fraction`: Relative vertical space allocated to image region
#'
#' @section Input:
#' The script expects a vector of image filenames in the working directory.
#' Supported formats: PNG, JPG, JPEG.
#'
#' @section Output:
#' - multipanel.pdf
#' - multipanel.svg
#'
#' @section Dependencies:
#' - cowplot
#' - ggplot2
#' - grid
#' - png
#' - jpeg
#' - svglite
#'
#' @section Notes:
#' - Image aspect ratio is preserved via raster grobs and controlled panel sizing.
#' - Spacing is implemented via explicit layout manipulation in `cowplot::plot_grid()`.
#' - Arrow alignment depends on `img_fraction` and assumes fixed panel geometry.
#'
#' @export
NULL

#####################
# Load libaries     #
#####################

library(cowplot)
library(ggplot2)
library(png)
library(jpeg)
library(grid)
library(svglite)

#####################
# Working directory #
#####################

# Set working directory to script directory
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  # In RStudio: Path of the active script
  script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  # Outside RStudio: Path via Rscript arguments
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_dir <- dirname(normalizePath(sub("--file=", "", file_arg)))
  } else {
    # Interactive or no file given: Current working directory
    script_dir <- getwd()
  }
}

# Set working directory
setwd(script_dir)
cat("Working directory set to:", getwd(), "\n")

#####################
# Functions         #
#####################

#' Read an image file into a raster grob
#'
#' Loads an image file (PNG or JPEG) and converts it into a `rasterGrob`
#' suitable for use in `ggplot2` or `cowplot` layouts.
#'
#' The image is scaled to fill the entire plotting panel without preserving
#' margins or centering, making it suitable for tiled multipanel figures.
#'
#' @param path Character string. Path to the image file.
#'
#' @return A `grob` object representing the image.
#'
#' @details Supported formats are PNG and JPEG/JPG. Other formats will
#' trigger an error.
#'
#' @importFrom grid rasterGrob unit
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @export
read_image_grob <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  img <- switch(
    ext,
    png = png::readPNG(path),
    jpg = jpeg::readJPEG(path),
    jpeg = jpeg::readJPEG(path),
    stop("Unsupported format")
  )
  
  rasterGrob(
    img,
    interpolate = TRUE,
    width = unit(1, "npc"),
    height = unit(1, "npc")  # ensures full fill (no centering)
  )
}

#' Add a left-aligned label above an image grob
#'
#' Combines a text label and an image into a single cowplot panel.
#' The label is placed in the upper-left corner, and a small vertical
#' gap is introduced between label and image for readability.
#'
#' @param img_grob A grob object representing an image.
#' @param label Character string. Text label to display.
#' @param img_fraction Numeric between 0 and 1. Fraction of panel height
#'        allocated to the image.
#' @param label_gap Numeric. Small vertical spacing between label and image.
#'
#' @return A combined `ggdraw` object containing label and image.
#'
#' @importFrom cowplot ggdraw draw_label draw_grob
#' @export
add_top_label <- function(img_grob, label, img_fraction, label_gap = 0.01) {
  
  image_height <- img_fraction - label_gap
  
  ggdraw() +
    
    # label (top-left, fixed)
    draw_label(label,
               x = 0, y = 1,
               hjust = 0, vjust = 1,
               fontface = "bold",
               size = 18) +
    
    # image pushed slightly downward to create real gap
    draw_grob(img_grob,
              x = 0,
              y = 0,
              width = image_height,
              height = image_height)
}

#' Create a directional arrow annotation grob
#'
#' Generates a custom polygon-based arrow with centered text label.
#' The arrow is designed to visually connect panels in multipanel figures,
#' typically used in workflow or pipeline diagrams.
#'
#' @param label Character string. Text displayed inside the arrow.
#' @param fill Fill color of the arrow body.
#' @param outline_colour Color of the arrow outline.
#' @param text_color Color of the label text.
#' @param head_length Numeric. Relative length of arrow head (0–1 scale).
#' @param body_height Numeric. Thickness of the arrow body.
#' @param img_fraction Numeric. Vertical alignment reference to image layout.
#' @param text_y_offset Numeric. Fine adjustment for vertical text position.
#'
#' @return A `ggplot` object representing the arrow.
#'
#' @importFrom ggplot2 ggplot aes geom_polygon annotate theme_void coord_cartesian
#' @export
make_arrow_grob <- function(label = "Synthesize",
                            fill = "black",
                            outline_colour = "black",
                            text_color = "white",
                            head_length = 0.2,
                            body_height = 0.15,
                            img_fraction = 0.92,
                            text_y_offset = 0.0) {
  
  # ---- alignment with image region ----
  y_mid <- img_fraction / 2
  
  # ---- geometry ----
  x_left  <- 0.05
  x_right <- 0.95
  x_head_start <- x_right - head_length
  
  h <- body_height / 2
  
  arrow_df <- data.frame(
    x = c(
      x_left,
      x_head_start,
      x_head_start,
      x_right,
      x_head_start,
      x_head_start,
      x_left
    ),
    y = c(
      y_mid - h,
      y_mid - h,
      y_mid - 2*h,
      y_mid,
      y_mid + 2*h,
      y_mid + h,
      y_mid + h
    )
  )
  
  ggplot(arrow_df, aes(x, y)) +
    geom_polygon(fill = fill, colour = outline_colour) +
    annotate("text",
             x = (x_left + x_head_start) / 2,
             y = y_mid + text_y_offset,
             label = label,
             color = text_color,
             fontface = "bold",
             size = 6) +
    theme_void() +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)
}

#' Assemble a horizontal row of plots with spacing
#'
#' Combines a list of plots (or grobs) into a single horizontal row
#' with configurable spacing between elements. Spacing is implemented
#' using alternating plot and empty slots via `cowplot::plot_grid()`.
#'
#' This function is primarily intended for constructing multipanel
#' figures where consistent horizontal gaps between panels are required.
#'
#' @param row_plots A list of ggplot objects, ggdraw objects, or grobs
#'        to be arranged in a single row.
#' @param hgap_frac Numeric. Relative width of the horizontal gap between
#'        adjacent panels (as a fraction of panel width).
#'
#' @return A `cowplot` object representing a single row of arranged plots.
#'
#' @details Internally, the function interleaves `NULL` placeholders between
#' plots and uses `rel_widths` to enforce spacing. This approach allows
#' fine-grained control over spacing without modifying individual plots.
#' This approach is a workaround for the lack of native gap support in
#' cowplot::plot_grid() and relies on inserting empty plot slots.
#'
#' @importFrom cowplot plot_grid
#' @export
make_row <- function(row_plots, hgap_frac) {
  n <- length(row_plots)
  
  plot_grid(
    plotlist = unlist(
      Map(function(p) list(p, NULL), row_plots),
      recursive = FALSE
    )[-(2*n)],
    nrow = 1,
    rel_widths = rep(c(1, hgap_frac), n)[-(2*n)]
  )
}

add_scalebar <- function(plot,
                         bar_width = 0.5,      # fraction of image width
                         bar_height = 0.015,   # thickness
                         x_offset = 0.05,      # left margin
                         y_offset = 0.05,      # bottom margin
                         label = "100 µm",
                         text_size = 5
                         color = "white") {
  
  plot +
    draw_line(
      x = c(x_offset, x_offset + bar_width),
      y = c(y_offset, y_offset),
      size = 1.2,
      color = color
    ) +
    draw_label(
      label,
      x = x_offset + bar_width / 2,
      y = y_offset + 0.04,
      size = text_size,
      color = color,
      vjust = 0
    )
}

####################
# Global variables #
####################

# ---- SETTINGS ----
ncol_panels <- 5
nrow_panels <- 2

hgap_frac <- 0.06
vgap_frac <- 0.06

img_fraction <- 0.92

# ---- Files ----
files <- c(
  "07.jpg","1_3i.png","2_3i.png","3_3i.png",
  "08.jpg","1_5i.png","2_5i.png","3_5i.png"
)

output_file_name_base <- "Figure1_SyntheticImages"

####################
# Build figure     #
####################

# ---- Load + label ----
grobs <- lapply(files, read_image_grob)

labeled <- mapply(add_top_label,
                  grobs,
                  LETTERS[1:length(grobs)],
                  img_fraction,
                  SIMPLIFY = FALSE)

labeled[[1]] <- add_scalebar(labeled[[1]],
                             bar_width = 0.5,
                             label = "100 µm")

# ---- Add arrows to grobs ----
arrow <- make_arrow_grob()

row1_plots <- append(labeled[1:4], list(arrow), after = 1)
row2_plots <- append(labeled[5:8], list(arrow), after = 1)

# ---- Build rows ----
row1 <- make_row(row1_plots, hgap_frac)
row2 <- make_row(row2_plots, hgap_frac)

# ---- Combine rows ----
final_plot <- plot_grid(
  row1,
  NULL,
  row2,
  ncol = 1,
  rel_heights = c(1, vgap_frac, 1)
)

# ---- Compute correct aspect ratio ----
total_width_units  <- ncol_panels + (ncol_panels - 1) * hgap_frac
total_height_units <- nrow_panels + (nrow_panels - 1) * vgap_frac

width <- 12
height <- width * (total_height_units / total_width_units)

# ---- Save ----
ggsave(paste0(output_file_name_base, ".pdf"), final_plot, width = width, height = height)
ggsave(paste0(output_file_name_base, ".svg"), final_plot, width = width, height = height)
