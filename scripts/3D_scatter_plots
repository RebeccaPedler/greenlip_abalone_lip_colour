install.packages(c("tidyr", "dplyr", "rpart", "rattle", "ggplot2", "car", "plotly", "rgl"))
library(tidyr)
library(dplyr)
library(rpart)
library(rattle)
library(ggplot2)
library(rpart.plot)
library(stringr)
library(car)
library(plotly)
library(rgl)

getwd()
setwd("C:/Users/RebeccaPedler/OneDrive - Yumbah/Documents/R&D/Industry PhD/Trials/Container Trial/R_datasets")
lip_data <- read.csv("lip_colour_data_all.csv", colClasses = (c("factor", "numeric", "integer", "factor", "character", "factor", "numeric")))
str(lip_data)

lip_wide <- lip_data %>% pivot_wider(
    names_from  = measurement,
    values_from = result
  )
str(lip_wide)

#3D scatter plot
lip_18_SP <- lip_wide %>% 
  filter(water_temp == 18, diet %in% c("SP", "CONTROL")) %>%
  mutate(treatment = sub("_[0-9]{2}$", "", treatment))

#Lab spirulina
plot_ly(
  lip_18_SP,
  x = ~L, 
  y = ~a, 
  z = ~b,
  type = 'scatter3d',
  mode = 'markers',
  symbol = ~treatment,   
  symbols = c('circle','diamond','square','triangle-up'), 
  color = ~diet,            
  colors = c("black", "green"), 
  size = ~B,        
  marker = list(size = 5),
  showlegend = TRUE
) %>%
layout(
  title = list(
    text = "<b>B)</b>",
    x = 0.05,
    xanchor = "left",
    font = list(size = 16)
  ),
  scene = list(
    xaxis = list(
      title = 'Luminance',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE    # adds border lines along the outside
    ),
    yaxis = list(
      title = 'a (green-red)',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE
    ),
    zaxis = list(
      title = 'b (blue-yellow)',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE,
      range = c(5, 30)
    )
  )
)

#3D scatter plot
lip_20_SP <- lip_wide %>% 
  filter(water_temp %in% c("20"), diet %in% c("SP", "CONTROL")) %>%
  mutate(treatment = sub("_[0-9]{2}$", "", treatment))

#Lab spirulina
plot_ly(
  lip_20_SP,
  x = ~L, 
  y = ~a, 
  z = ~b,
  type = 'scatter3d',
  mode = 'markers',
  symbol = ~treatment,   
  symbols = c('circle','diamond','square','triangle-up'), 
  color = ~diet,            
  colors = c("black", "green"), 
  size = ~b,        
  marker = list(size = 5),
  showlegend = TRUE
) %>%
layout(
  title = list(
    text = "<b>B)</b>",
    x = 0.05,
    xanchor = "left",
    font = list(size = 16)
  ),
  scene = list(
    xaxis = list(
      title = 'Luminance',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE    # adds border lines along the outside
    ),
    yaxis = list(
      title = 'a (green-red)',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE
    ),
    zaxis = list(
      title = 'b (blue-yellow)',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE,
      range = c(5, 30)
    )
  )
)

#3D scatter plot
lip_25 <- lip_wide %>% 
  filter(water_temp %in% c("25"), diet %in% c("SP", "CONTROL", "WK")) %>%
  mutate(treatment = sub("_[0-9]{2}$", "", treatment))
str(lip_25)


#Lab spirulina
plot_ly(
  lip_25,
  x = ~L, 
  y = ~a, 
  z = ~b,
  type = 'scatter3d',
  mode = 'markers',
  symbol = "circle",   
  color = ~diet,            
  colors = c("black", "green", "orange"), 
  size = ~b,        
  marker = list(size = 5),
  showlegend = TRUE
) %>%
layout(
  title = list(
    text = "<b>B)</b>",
    x = 0.05,
    xanchor = "left",
    font = list(size = 16)
  ),
  scene = list(
    xaxis = list(
      title = 'Luminance',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE,
      range = c(30, 60)
    ),
    yaxis = list(
      title = 'a (green-red)',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE,
      range = c(-4, 6)
    ),
    zaxis = list(
      title = 'b (blue-yellow)',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE,
      range = c(5, 30)
    )
  )
)

#Lab wakame (conditioning phase)
#3D scatter plot
lip_18_WK <- lip_wide %>% 
  filter(water_temp == "18", diet %in% c("WK", "CONTROL")) %>%
  mutate(treatment = sub("_[0-9]{2}$", "", treatment))

print(lip_18_WK, n = 35)

plot_ly(
  lip_18_WK,
  x = ~L, 
  y = ~a, 
  z = ~b,
  type = 'scatter3d',
  mode = 'markers',
  symbol = ~treatment,   
  symbols = c('circle','diamond','square','triangle-up'), 
  color = ~diet,            
  colors = c("black", "orange"), 
  size = ~B,        
  marker = list(size = 5),
  showlegend = TRUE
) %>%
layout(
  title = list(
    text = "<b>B)</b>",
    x = 0.05,
    xanchor = "left",
    font = list(size = 16)
  ),
  scene = list(
    xaxis = list(
      title = 'Luminance',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE    # adds border lines along the outside
    ),
    yaxis = list(
      title = 'a (green-red)',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE
    ),
    zaxis = list(
      title = 'b (blue-yellow)',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE,
      range = c(5, 30)
    )
  )
)

#3D scatter plot
lip_20_WK <- lip_wide %>% 
  filter(water_temp %in% c("20"), diet %in% c("WK", "CONTROL")) %>%
  mutate(treatment = sub("_[0-9]{2}$", "", treatment))

#Lab wakame (20)
plot_ly(
  lip_20_WK,
  x = ~L, 
  y = ~a, 
  z = ~b,
  type = 'scatter3d',
  mode = 'markers',
  symbol = ~treatment,   
  symbols = c('circle','diamond','square','triangle-up'), 
  color = ~diet,            
  colors = c("black", "orange"), 
  size = ~b,        
  marker = list(size = 5),
  showlegend = TRUE
) %>%
layout(
  title = list(
    text = "<b>B)</b>",
    x = 0.05,
    xanchor = "left",
    font = list(size = 16)
  ),
  scene = list(
    xaxis = list(
      title = 'Luminance',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE    # adds border lines along the outside
    ),
    yaxis = list(
      title = 'a (green-red)',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE
    ),
    zaxis = list(
      title = 'b (blue-yellow)',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE,
      range = c(5, 30)
    )
  )
)

#3D scatter plot
lip_25_WK <- lip_wide %>% 
  filter(water_temp %in% c("25"), diet %in% c("WK", "CONTROL")) %>%
  mutate(treatment = sub("_[0-9]{2}$", "", treatment))

#Lab wakame (25)
plot_ly(
  lip_25_WK,
  x = ~L, 
  y = ~a, 
  z = ~b,
  type = 'scatter3d',
  mode = 'markers',
  symbol = ~treatment,   
  symbols = c('circle','diamond','square','triangle-up'), 
  color = ~diet,            
  colors = c("black", "orange"), 
  size = ~b,        
  marker = list(size = 5),
  showlegend = TRUE
) %>%
layout(
  title = list(
    text = "<b>B)</b>",
    x = 0.05,
    xanchor = "left",
    font = list(size = 16)
  ),
  scene = list(
    xaxis = list(
      title = 'Luminance',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE    # adds border lines along the outside
    ),
    yaxis = list(
      title = 'a (green-red)',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE
    ),
    zaxis = list(
      title = 'b (blue-yellow)',
      showgrid = TRUE,
      gridcolor = "lightgray",
      gridwidth = 2,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      linecolor = "black",
      linewidth = 1,
      showbackground = TRUE,
      backgroundcolor = "white",
      mirror = TRUE,
      range = c(5, 30)
    )
  )
)
