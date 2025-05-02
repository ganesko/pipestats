library(hexSticker)
library(magick)
library(ggplot2)
library(showtext)

font_add_google("Space Grotesk", "Medium 500")

pipe <- "https://png.pngtree.com/png-vector/20220917/ourmid/pngtree-icon-signifying-gas-leakage-and-polluting-gas-pipeline-vector-png-image_33812240.png"

sticker(pipe, package="pipestats", p_size=15, s_x=1, s_y=.8, s_width=.6, s_height=1.1, p_family = "Medium 500",
        h_fill="white", h_color="darkorange1", p_color="darkorange1",
        filename="data-raw/pipestats_sticker.png", white_around_sticker = T)
