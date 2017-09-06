library(tidyverse)
library(lubridate)
library(forcats)
library(readr)
library(stringi)
library(tictoc)
library(pryr)
#library(hrbrthemes)
#library(ggthemes)
library(plot3D)
library(plotly)

getwd()

# загрузка и первичная очистка исходных данных -------------------
load_var <- function(angle, axis_name){
  fname <- stri_join("./data/current", axis_name, "(angle=", angle, ").csv")
  print(fname)
  read_delim(fname, delim=";", col_names=c("timestamp", "angle", "value"), col_types="ddd") %>%
    mutate(axis=axis_name, angle_str=angle)
}

raw_df <- tidyr::expand(tibble(angle=c("0.786398", "-0.784398"),
                               axis_name=c("x", "y")), 
                        angle, axis_name) %>%
  {map2_df(.$angle, .$axis_name, ~load_var(.x, .y))}

clean_df <- raw_df %>%
  spread(axis, value) %>%
  filter(complete.cases(.))

plus_df <- clean_df %>%
  filter(angle>0)
minus_df <- clean_df %>%
  filter(angle<0)

write_csv(select(plus_df, -angle_str), "./data/combined_plus.csv")
write_csv(select(minus_df, -angle_str), "./data/combined_minus.csv")

# попробуем порисовать через plot3D
# http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization

y0 <- minus_df$timestamp
z0 <- minus_df$y
x0 <- minus_df$x

y1 <- plus_df$timestamp
z1 <- plus_df$y
x1 <- plus_df$x


scatter3D(x0, y0, z0, phi=0, bty="g", type="l",  ticktype="detailed", lwd=1)
scatter3D(x0, y0, z0, colvar=NULL, col="blue", pch=19, cex=0.5)
scatter3D(x1, y1, z1, colvar=NULL, col="red", pch=19, cex=0.5, add=TRUE)


# Open a new png device to print the figure out to (or use tiff, pdf, etc).
png(filename = "figure.png", width = 3600, height = 2000, units = 'px', res=300)
# scatter3D(x, y, z, colvar = NULL, col = "blue", pch = 19, cex = 0.5)
scatter3D(x, y, z, phi = 0, bty = "g", type = "l",  ticktype = "detailed", lwd = 4)
scatter3D(x0, y0, z0, colvar=NULL, col="blue", pch=19, cex=0.5)
scatter3D(x1, y1, z1, colvar=NULL, col="red", pch=19, cex=0.5, add=TRUE)
# print(pp) #end of print statement
dev.off() #close the png device to save the figure. 

# попробуем порисовать через plot3D
# https://plot.ly/r/3d-line-plots/
sample_df <- df0 %>%
  sample_frac(0.1, replace=FALSE) %>%
  arrange(timestamp)

plot_ly(sample_df, x = ~x, y = ~y, z = ~timestamp, type = 'scatter3d', mode = 'lines',
             line=list(width = 2))
