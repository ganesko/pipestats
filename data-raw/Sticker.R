library(hexSticker)
library(magick)
library(ggplot2)
library(showtext)

font_add_google("Space Grotesk", "Medium 500")

pipe <- "https://img.freepik.com/free-vector/flat-design-damaged-metal-pipe_23-2148278668.jpg?t=st=1746030399~exp=1746033999~hmac=5a83830bd1de08c22de38a8ffd630cbb92092ec816b3aab50da655b9903b7bb2&w=1380"

sticker(pipe, package="pipestats", p_size=15, s_x=1, s_y=.6, s_width=.85, s_height=2.5, p_family = "Medium 500",
        h_fill="white", h_color="darkorange1", p_color="darkorange1",
        filename="data-raw/pipestats_sticker.png", white_around_sticker = T)
