library(tidyverse)
library(officer)
library(rvg)
library(broom)
library(purrr)
library(RColorBrewer)
library(scales)
library(tidyverse)
library(sf)
library(here)
library(rmapshaper)
library(rgdal)
setwd("~/Datos")
shapefile<-readOGR(dsn = "comunidades_mapa/comunidades-autonomas-espanolas.shp") ##Descargar en https://public.opendatasoft.com/explore/dataset/comunidades-autonomas-espanolas/export/?sort=comunidade_autonoma
dat <- tidy(shapefile)

nombres <- tibble(shapefile$texto) %>% 
  mutate(id = as.character(seq(0, nrow(.)-1))) %>% mutate(codigo=shapefile$codigo) 

dat<- dat %>% left_join(nombres, by=c("id"="id")) %>% 
  rename("texto"="shapefile$texto") %>% mutate(codigo=as.numeric(codigo))

dat <- dat %>% mutate(lat1=case_when(codigo==5 ~ lat + 7.8,
                                     codigo!=5 ~ lat),
                      long1=case_when(codigo==5 ~ long + 6.35,
                                      codigo!=5 ~ long),
                      codigo=as.numeric(codigo))

dat1<- data.frame(codigo=c(1:19),
                  porc=c(18.49, 2.73, 2.11,
                         2.57, 4.91, 1.22,
                         4.87, 4.30, 16.26,
                         10.61, 2.26, 5.51,
                         14.38, 3.21, 1.36,
                         4.55, 0.66, NA, NA))

dat <- dat %>% left_join(dat1, by=c("codigo"="codigo")) %>% filter(codigo<=17)

dat2<- dat %>% group_by(texto, porc) %>% summarise(long=mean(long1), lat=mean(lat1)) 





dat %>% 
  ggplot() +
  geom_polygon(aes(x= long1, y = lat1,group=group, fill=porc),
               color="white") + coord_equal() + 
  geom_text(aes(x=long, y=lat,label=paste0(porc,"%")), data=dat2) +
  geom_rect(xmin=-12, xmax=-7, ymin=35.4, ymax=37.1, color="black", alpha=0,
            size=1.25) +
  theme_void() + 
  theme(legend.position = "none") + 
  scale_fill_gradient(low = "snow3", high = "lawngreen", labels=number)
