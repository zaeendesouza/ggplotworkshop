# Ggplot Workshop
These are my slides from a two-day workshop I did data visualisation using R, and specifically, ggplot. All the data, which was taken from the [State of Working India, 2021](https://cse.azimpremjiuniversity.edu.in/state-of-working-india/swi-2021/) report, can be accesed [here](https://drive.google.com/drive/u/1/folders/16QUOR3eGLghk5dTdPHrZhsEfp1wbFWEM). The report is written by the [Centre for Sustainable Employment](https://cse.azimpremjiuniversity.edu.in/what-is-cse/), which is a research centre at Azim Premji University.

Re: the slides, I'm still learning, so any feedback/suggestons/changes are welcome! :)

## The packages required for doing the exercises are below:
```{r, eval=F, echo=F}
install.packages("pacman")
library(pacman)
p_load(tidyverse, dplyr, ggthemes, 
       ggmap, ggalluvial, grid, 
       gridExtra, gridtext, extrafont, 
       extrafontdb, ggalt, ggtext, 
       cowplot, ggforce, sf, 
       geojsonio)
```
