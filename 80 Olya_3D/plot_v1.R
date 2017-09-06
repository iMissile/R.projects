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
library(plot3Drgl)

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
  filter(complete.cases(.)) %>%
  mutate(xx=x, yy=timestamp) %>%
  mutate(zz=y*10^15) %>% # для Re
  mutate(zz=y) # для Im

plus_df <- clean_df %>%
  filter(angle>0)
minus_df <- clean_df %>%
  filter(angle<0)

write_csv(select(plus_df, -angle_str), "./data/combined_plus.csv")
write_csv(select(minus_df, -angle_str), "./data/combined_minus.csv")

# попробуем порисовать через plot3D
# http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization

x_m <- minus_df$xx
y_m <- minus_df$yy
z_m <- minus_df$zz

x_p <- plus_df$xx
y_p <- plus_df$yy
z_p <- plus_df$zz


# Open a new png device to print the figure out to (or use tiff, pdf, etc).
png(filename = "figure.png", width = 3600, height = 2000, units = 'px', res=300)

# нарисуем минусовой угол
scatter3D(x_m, y_m, z_m, bty="b2", colvar=NULL, col="chartreuse4", 
          ticktype="detailed",
          xlim=c(min(clean_df$xx), max(clean_df$xx)),
          zlim=c(min(clean_df$zz), max(clean_df$zz)),
          xlab = "Ex", ylab ="Время", zlab="Ey",
          theta=20, phi=20, 
          alpha=0.15,
          pch=16, cex=0.5)
# нарисуем плюсовой угол
scatter3D(x_p, y_p, z_p, bty="b2", colvar=NULL, col="brown1", 
          #ticktype="detailed",
          alpha=0.15,
          pch=16, cex=0.5, add=TRUE)

# нарисуем тень плюсового угла на плоскости xy
scatter3D(x_p, y_p, rep_along(y_p, min(clean_df$zz)), bty="b2", colvar=NULL, col="lightpink", 
          #ticktype="detailed",
          alpha=0.15,
          pch=16, cex=0.5, add=TRUE)
# нарисуем тень плюсового угла на плоскости zy
scatter3D(rep_along(y_p, min(clean_df$xx)), y_p, z_p, bty="b2", colvar=NULL, col="lightpink", 
          #ticktype="detailed",
          alpha=0.15,
          pch=16, cex=0.5, add=TRUE)

# нарисуем тень минусового угла на плоскости xy
scatter3D(x_m, y_m, rep_along(y_m, min(clean_df$zz)), bty="b2", colvar=NULL, col="darkseagreen1", 
          #ticktype="detailed",
          alpha=0.15,
          pch=16, cex=0.5, add=TRUE)
# нарисуем тень минусового угла на плоскости zy
scatter3D(rep_along(y_m, min(clean_df$xx)), y_m, z_m, bty="b2", colvar=NULL, col="darkseagreen1", 
          #ticktype="detailed",
          alpha=0.15,
          pch=16, cex=0.5, add=TRUE)

dev.off() #close the png device to save the figure. 

stop()

scatter3D(x2, y0, z0, bty="b2", colvar=NULL, col="green", add=TRUE)


scatter3D(x0, y0, z0, phi=0, bty="g", type="l",  ticktype="detailed", lwd=1, col=gg.col(10))
scatter3D(x0, y0, z0, phi=0, bty="g", type="p",  ticktype="detailed", lwd=1, col=gg.col(10))

# plotrgl()

scatter3D(x1, y1, z1, colvar=NULL, col="red", pch=19, cex=0.5, add=TRUE)
plotrgl()


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
sample_df <- minus_df %>%
  sample_frac(0.1, replace=FALSE) %>%
  arrange(timestamp)

plot_ly(sample_df, x = ~x, y = ~y, z = ~timestamp, type = 'scatter3d', mode = 'lines', line=list(width = 2))
plot_ly(sample_df, x = ~x, y = ~y, z = ~timestamp, type = 'scatter3d', mode = 'points', line=list(width = 2))
