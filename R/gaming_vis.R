# Makeover Monday #7, 2016-02-15
# Makeover challenge and data at \url{http://vizwiz.blogspot.com/p/makeover-monday-challenges.html}
# Original infographic at \url{http://www.forbes.com/sites/kevinanderton/2015/12/31/adults-vs-teens-video-game-edition-infographic/#1533b0356956}
# Goal: create a more intelligible and more attractive visualization of the data.

library(extrafont)
library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)
library(grid)
#library(gridExtra)
library(ggplot2)
library(scales)

df <- read_excel("data-raw/Video%20Game%20Usage%20Rates.xlsx")
colnames(df) <- make.names(colnames(df))

df2 <- df %>% filter(Category == "Adults" | Category == "Teens") %>% 
  spread(Category, Usage.Rate)

df3 <- df %>% filter(Category == "Parents" | Category == "Non-Parents") %>% 
  spread(Category, Usage.Rate)
colnames(df3) <- make.names(colnames(df3))
levels_order <- df2[order(df2$Teens-df2$Adults),]$Gaming.Device
df2$Gaming.Device <- factor(df2$Gaming.Device, levels = levels_order)
df$Gaming.Device <- factor(df$Gaming.Device, levels = levels_order)
df3$Gaming.Device <- factor(df3$Gaming.Device, levels = levels_order)

theme_vg_info <- function() {
  theme_minimal() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(), 
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          text = element_text(size = 12),
          legend.position = "top")
}

plot1 <- ggplot() +
  geom_segment(data = df2, aes(x = Gaming.Device, y = Adults, yend = Teens, xend = Gaming.Device), colour = "grey") +
  geom_point(data = df %>% filter(Category == "Adults" | Category == "Teens"), aes(x = Gaming.Device, y = Usage.Rate, colour = Category, shape = Category), size = 3) +
  scale_y_continuous(name = "Percent Gamers", labels = percent) +
  scale_shape_manual(values = c(19, 4)) +
  scale_color_manual(values = c("#0072B2", "#D55E00")) +
  ggtitle("teens play on more devices") +
  coord_flip() +
  theme_vg_info()

plot2 <- ggplot() +
  geom_segment(data = df3, aes(x = Gaming.Device, y = Non.Parents, yend = Parents, xend = Gaming.Device), colour = "grey") +
  geom_point(data = df %>% filter(Category == "Parents" | Category == "Non-Parents"), aes(x = Gaming.Device, y = Usage.Rate, colour = Category, shape = Category), size = 3) +
  scale_y_continuous(name = "Percent Gamers", labels = percent) +
  scale_shape_manual(values = c(19, 4)) +
  scale_color_manual(values = c("#56B4E9", "#E69F00")) +
  ggtitle("as do parents") +
  coord_flip() +
  theme_vg_info() +
  theme(axis.text.y = element_blank())

png("figs/vidgames.png", width = 9, height = 6, units = "in", res = 500)
grid.newpage()

vp_par <- viewport(width = unit(1, "npc"), height = unit(1, "npc"))
pushViewport(vp_par)
vp_title <- viewport(x = 0, y = 1, width = unit(1, "npc"), height = unit(0.2, "npc"), just = c("left","top"))
vp_highlight <- viewport(x = 0, y = 0.8, width = unit(1, "npc"), height = unit(0.25, "npc"), just = c("left","top"))
vp_graph <- viewport(x = 0, y = 0.55, width = unit(1, "npc"), height = unit(0.55, "npc"), just = c("left","top"))

pushViewport(vp_title)
#grid.rect()
grid.text(label = "Video Game Culture", x = unit(0.5, "npc"), y = unit(0.5,"npc"), gp = gpar(fontfamily = "Impact", col = "#3366CC", cex = 4.5))
upViewport()
pushViewport(vp_highlight)
grid.rect(gp = gpar(fill = "#EEEEEE", col = "#EEEEEE"))
grid.text(label = "Teens", x = 0.23, just = c("left", "bottom"), gp = gpar(cex = 4 * 0.5, col = "#D55E00"))
grid.text(label = "97%", x = 0.49, just = c("right", "bottom"), gp = gpar(cex = 4 * 0.97, col = "#D55E00"))
grid.text(label = "53%", x = 0.51, just = c("left", "bottom"), gp = gpar(cex = 4 * 0.53, col = "#003399"))
grid.text(label = "Adults", x = 0.6, just = c("left", "bottom"), gp = gpar(cex = 4 * 0.5, col = "#003399"))
grid.text(label = "play video games", y = 0.25, gp = gpar(cex = 4 * 0.5, col = "#333333"))
upViewport()
pushViewport(vp_graph)
vp_graph_in <- viewport(h = 0.75, w = 0.9, x = 0.5, y = 0.75, just = c("centre","top"))
grid.text(label = "Of those playing video games...", x = 0.5, y = 0.85, just = c("centre","bottom"), gp = gpar(cex = 2, col = "#333333"))
#grid.rect(gp = gpar(fill = "#DDDDDD", col = "#DDDDDD"))
pushViewport(vp_graph_in)
#grid.rect(gp = gpar(fill = "#FFFFFF", col = "#FFFFFF"))
vp_g1 <- viewport(0, 0.5, width = .58, just = c("left", "center"))
vp_g2 <- viewport(0.65, 0.5, width = 0.35, just = c("left", "center"))
print(plot1, vp = vp_g1)
print(plot2, vp = vp_g2)

dev.off()
