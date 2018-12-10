library(viridis)
library(haven)
library(tidyverse)
library(ggridges)
library(ggplot2)
library(grid)

mikz <- read_dta("mz_trend_R.dta")


wtmr <- mikz$wtmr
jahr <- factor(mikz$jahr)
east <- factor(mikz$east)
p25 <- mikz$p25
p50 <- mikz$p50
p75 <- mikz$p75
datmik <- tibble(jahr, wtmr, east, p25, p50, p75)


# Get the share information 
mikshare <- read_dta("mz_share_R.dta")
share <- as.tibble(mikshare$share)
share2 <- mikshare %>% 
  mutate(Rshare = formatC(share, format = "f", digits = 2))

# Graph path
graphs <- "Z:/Mitarbeiter/Grafiken/"
# Graph path
graphs <- "Z:/Mitarbeiter/Grafiken/"

plot <- ggplot(datmik, aes(x = wtmr, y = jahr, fill = factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient",
                      bandwidth = 1.3,
                      scale = 9,
                      calc_ecdf = TRUE, 
                      quantile_lines = TRUE, 
                      quantiles = 4, 
                      from = 0, to = 100, 
                      color = "white", 
                      alpha = .6, 
                      panel_scaling = TRUE) +
  scale_fill_viridis(discrete = TRUE, 
                     name = "Quartiles",
                     labels = c("First" , "Second", "Third", "Fourth"), 
                     alpha = .8,
                     begin = .2,
                     end = .7,
                     direction = -1, 
                     option = "cividis") +
  theme_ridges(grid = FALSE) + 
  theme(legend.position = "top") +
  scale_x_continuous(name = "Contribution of the woman's income to couple's income in %" ,  
                     breaks = c(0, 10, 20 , 30, 40, 50, 60, 70, 80, 90, 100) , 
                     expand = c(0.0, 0.0)) +
  scale_y_discrete(name = NULL, expand = c(0.0, 0),
                   position = "right") + 
  annotate("text", x = 76, y = 18.6, label = "% Women contributing at least equally:" , size = 4) + 
  annotate("text", x = 90, y = 18.2, label = share2[18,6] , size = 4) + 
  annotate("text", x = 90, y = 17.2, label = share2[17,6] , size = 4) + 
  annotate("text", x = 90, y = 16.2, label = share2[16,6] , size = 4) + 
  annotate("text", x = 90, y = 15.2, label = share2[15,6] , size = 4) + 
  annotate("text", x = 90, y = 14.2, label = share2[14,6] , size = 4) + 
  annotate("text", x = 90, y = 13.2, label = share2[13,6] , size = 4) + 
  annotate("text", x = 90, y = 12.2, label = share2[12,6] , size = 4) + 
  annotate("text", x = 90, y = 11.2, label = share2[11,6] , size = 4) + 
  annotate("text", x = 90, y = 10.2, label = share2[10,6] , size = 4) + 
  annotate("text", x = 90, y = 9.2, label = share2[9,6] , size = 4) + 
  annotate("text", x = 90, y = 8.2, label = share2[8,6] , size = 4) + 
  annotate("text", x = 90, y = 7.2, label = share2[7,6] , size = 4) + 
  annotate("text", x = 90, y = 6.2, label = share2[6,6] , size = 4) + 
  annotate("text", x = 90, y = 5.2, label = share2[5,6] , size = 4) + 
  annotate("text", x = 90, y = 4.2, label = share2[4,6] , size = 4) + 
  annotate("text", x = 90, y = 3.2, label = share2[3,6] , size = 4) + 
  annotate("text", x = 90, y = 2.2, label = share2[2,6] , size = 4) + 
  annotate("text", x = 90, y = 1.2, label = share2[1,6] , size = 4) + 
  annotate("text", x = 46.3, y = 19 , label = "P75", color = "black", size = 4) +
  annotate("text", x = 31.8, y = 19 , label = "P50", color = "black", size = 4) +
  annotate("text", x = 15.7, y = 19 , label = "P25", color = "black", size = 4) 

ggsave("ridgeline-mz-all.pdf", width=12, height=10, dpi=300)


