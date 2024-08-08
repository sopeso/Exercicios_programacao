## Resolucao da lista 10

##  Aluna : Sophia Araújo de Moraes


#Carregando pacotes
install.packages("ggplot")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggiraph")
install.packages("gganimate")
install.packages("rgl")
install.packages("leaflet")
install.packages("RColorBrewer")
install.packages("gifski")
install.packages("readxl")
install.packages("sf")
install.packages("geobr")
install.packages("covid19br")
install.packages("plotly")
install.packages("devtools")

library("ggplot")
library("tidyverse")
library("ggplot2")
library("ggiraph")
library("gganimate")
library("rgl")
library("leaflet")
library("RColorBrewer")
library("gifski")
library("readxl")
library("sf")
library("geobr")
library("covid19br")
library("plotly")
library("devtools")


############################################################

bd <- downloadCovid19("states")
View(bd)
bd= na.omit(bd)
attach(bd)

############################################################
#Questão 1
bd$date <- as.Date(bd$date, format = "%Y-%m-%d")

analise0 <-  bd %>% select(state,newCases,date)  %>% 
  filter(date >="2021-01-01" & (state=="MG" | state== "BA")  )
View(analise0)

analisep <-  analise0 %>% 
  filter(date < "2021-03-01" )
                 
grafico1 <- ggplot(analisep, aes(newCases, date, group = state, color = factor(state))) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Days", y = "newCases") +
  theme(legend.position = "top")  
ggiraph(code = print(grafico1))

###########################################################
#
# Questão 2
library(rgl)

x<-c(-5:5)
y<- c(-5:5)
f <- ((cos(x^2 + y^2))/2)

valor <- data.frame(x, y,f)
with(valor, plot3d(x,y,f, type = "l", col = "red"))

############################################################
#Questão 3
library(gganimate)
library(gapminder)
library(ggplot2)

bd3 <- bd%>%
  group_by(bd$region) 


p<-ggplot(bd3, aes(x=state, y = accumCases, fill= accumCases)) +   geom_bar(stat="identity") +
  labs(x = "Estados", y = "Casos de covid acumulados")+ 
  transition_states(state, wrap = F) +  shadow_mark() +
  enter_grow() +
  enter_fade()
  

anim = p +  transition_time(date) 

animate(anim, 
        nframes = 312, 
        renderer = gifski_renderer("gif1.gif"), 
        duration = 15)


############################################################
#Questão 4
df_covid = bd %>% 
  filter(date <= "2021-02-26")

estadosbr = read_state(year=2019,showProgress = F)
estados = dplyr::inner_join(estadosbr, df_covid, by = c("abbrev_state" = "state"))

# Construindo a escala
pal =  colorBin("Blues", domain =  log2(estados$accumDeaths + 1), bins = 5)

leaflet(data = estados) %>%
  setView(lng = 0, lat = 0, zoom = 02) %>%
  addPolygons(fillColor = ~ pal(log2(accumDeaths+1)),
              fillOpacity = 1,
              color = "#BDBDC3",
              layerId = ~ abbrev_state,
              weight = 1,
              popup = paste(estados$abbrev_state,  "<br>",
                            "obitos: ", estados$accumDeaths)) %>% 
  
  addLegend("bottomright", pal = pal, values = ~log2(estados$accumDeaths + 1), 
            title = "Escala ", opacity = 1, labFormat=labelFormat(transform = function(x)2^(x)-1, digits = 1)) %>%
  addControl("Clique no mapa para ver detalhes", position = "topright")



############################################################
#Questão 5
install.packages("ggvis")
library(ggvis)
install.packages("shiny")
library(shiny)

bd %>% ggvis(x = ~accumCases) %>%
  layer_densities(
    adjust = input_slider(.1, 2, value = 1, step = .1, label = "CasosAcum"),
    kernel = input_select(
      c("Gaussian" = "gaussian",
        "Epanechnikov" = "epanechnikov",
        "Rectangular" = "rectangular",
        "Triangular" = "triangular",
        "Biweight" = "biweight",
        "Cosine" = "cosine",
        "Optcosine" = "optcosine"),
      label = "Kernel")
  )

############################################################
#Questão 6
require(tidyverse)
data(iris)
View(iris)
attach(iris)
names(iris)

#a
plot_ly(data = iris, x = iris$Petal.Length, y = iris$Sepal.Width,color= Species)

#b
analise1 <-  bd %>% select(state,newCases, newDeaths,date)  %>% 
  filter(date >="2021-01-01" & (state %in% c("RS", "SC", "PR")))
analise2 <-  analise1 %>% 
  filter(date < "2021-07-01" )
attach(analise2)
p1 <- ggplot(analise2, aes(newCases, newDeaths,color= state)) +
  geom_point(aes(frame = as.factor(date)))+
  scale_x_log10()
p1
# Transformando em uma animação. 
p <- ggplotly(p1) 
p


##########################################################
#Questão 7
install.packages("trelliscopejs")
library(trelliscopejs)

bd <- downloadCovid19("states")
bd= na.omit(bd)
attach(bd)

qplot(date, newCases, data = bd) +
  theme_bw() +
  facet_trelliscope(~ region, nrow = 2, ncol = 3, width = 300)
