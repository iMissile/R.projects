library(plotly)

Sys.setenv("plotly_username"="iMissile")
Sys.setenv("plotly_api_key"="g6c59jc0jq")


p <- plot_ly(midwest, x = percollege, color = state, type = "box")

p

# plotly_POST(p, filename = "r-docs/midwest-boxplots", world_readable=TRUE)
plotly_POST(p, filename = "r-docs/midwest-boxplots", fileopt = "overwrite", sharing="public")

plotly_POST(p, filename='privacy-secret', sharing='hidden')
