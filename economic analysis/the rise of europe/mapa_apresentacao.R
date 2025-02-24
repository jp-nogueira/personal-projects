setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(stringr)
library(sf)
library(geojsonio)
library(ggthemes)
library(paletteer)
library(extrafont)
library(ggspatial)
library(magick)

loadfonts(device = "win") #Importa fontes

spdf <- read_sf("europe_.geojson") #Lê o arquivo com o mapa

df <- readxl::read_excel("dados.xlsx") %>%
  dplyr::rename("name"=Country)

df$name <- str_replace(string=df$name,pattern="Czechia",replacement="Czech Republic")
df$name <- str_replace(string=df$name,pattern="England",replacement="United Kingdom")

df_list <- split(df,df$Year) #Divide o DataFrame em uma lista de DataFrames anuais

##Dados para os plots:
MapData_1300 <- left_join(spdf,df_list[[4]],by="name")
MapData_1400 <- left_join(spdf,df_list[[5]],by="name")
MapData_1500 <- left_join(spdf,df_list[[6]],by="name")
MapData_1600 <- left_join(spdf,df_list[[7]],by="name")
MapData_1700 <- left_join(spdf,df_list[[8]],by="name")
MapData_1800 <- left_join(spdf,df_list[[10]],by="name")
MapData_1850 <- left_join(spdf,df_list[[11]],by="name")  %>%
  dplyr::mutate(outline= case_when(
    name=="United Kingdom" ~ "steelblue",
    name=="Spain" ~ "steelblue",
    name=="Portugal" ~ "steelblue",
    name=="Netherlands" ~ "steelblue",
    name=="France" ~ "steelblue",
    TRUE ~ "aliceblue"))

###### Plotando Urbanização ########

m1300 <- ggplot(MapData_1300,aes(fill=as.double(Urbanization))) +
  geom_sf(color=MapData_1850$outline,linewidth=ifelse(MapData_1850$outline=="aliceblue",0.2,0.4)) +
  scale_x_continuous(limits = c(-15, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_paletteer_c("ggthemes::Orange",
                         na.value="lightgrey",
                        limits=c(0,50),
                        guide=guide_colorbar(                #opções da colorbar 
                          direction = "vertical",
                          position = "right",
                          label.position= "right",
                          barwidth = unit(0.4, "cm"),         # width of the colorbar
                          barheight = unit(7, "cm"),          # height of the colorbar
                          ticks = TRUE
                        )) +
  labs(title = "População Urbana (em %)",
       subtitle= "1300",
       caption = "Nota: Fronteiras em azul denotam \"Atlantic Trader\". Elaboração prórpia com dados de Acemoglu, Johnson e Robinson (2008).") +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.grid.major =  element_line(color = grey(.8),linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill="aliceblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0,vjust = -0.2, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 6,face="italic")) +
  annotation_scale(location="bl",height = unit(0.17,"cm"),plot_unit = "km")

ggsave(filename = "m1300.png",plot = m1300,scale=1)
d.m1300 <- image_read("m1300.png")

m1400 <- ggplot(MapData_1400,aes(fill=as.double(Urbanization))) +
  geom_sf(color=MapData_1850$outline,linewidth=ifelse(MapData_1850$outline=="aliceblue",0.2,0.4)) +
  scale_x_continuous(limits = c(-15, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_paletteer_c("ggthemes::Orange",
                         na.value="lightgrey",
                         limits=c(0,50),
                         guide=guide_colorbar(                #opções da colorbar 
                           direction = "vertical",
                           position = "right",
                           label.position= "right",
                           barwidth = unit(0.4, "cm"),         # width of the colorbar
                           barheight = unit(7, "cm"),          # height of the colorbar
                           ticks = TRUE
                         )) +
  labs(title = "População Urbana (em %)",
       subtitle= "1400",
       caption = "Nota: Fronteiras em azul denotam \"Atlantic Trader\". Elaboração prórpia com dados de Acemoglu, Johnson e Robinson (2008).") +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.grid.major =  element_line(color = grey(.8),linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill="aliceblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0,vjust = -0.2, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 6,face="italic")) +
  annotation_scale(location="bl",height = unit(0.17,"cm"),plot_unit = "km")

ggsave(filename = "m1400.png",plot = m1400,scale=1)
d.m1400 <- image_read("m1400.png")

m1500 <- ggplot(MapData_1500,aes(fill=as.double(Urbanization))) +
  geom_sf(color=MapData_1850$outline,linewidth=ifelse(MapData_1850$outline=="aliceblue",0.2,0.4)) +
  scale_x_continuous(limits = c(-15, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_paletteer_c("ggthemes::Orange",
                         na.value="lightgrey",
                         limits=c(0,50),
                         guide=guide_colorbar(                #opções da colorbar 
                           direction = "vertical",
                           position = "right",
                           label.position= "right",
                           barwidth = unit(0.4, "cm"),         # width of the colorbar
                           barheight = unit(7, "cm"),          # height of the colorbar
                           ticks = TRUE
                         )) +
  labs(title = "População Urbana (em %)",
       subtitle= "1500",
       caption = "Nota: Fronteiras em azul denotam \"Atlantic Trader\". Elaboração prórpia com dados de Acemoglu, Johnson e Robinson (2008).") +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.grid.major =  element_line(color = grey(.8),linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill="aliceblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0,vjust = -0.2, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 6,face="italic")) +
  annotation_scale(location="bl",height = unit(0.17,"cm"),plot_unit = "km")

ggsave(filename = "m1500.png",plot = m1500,scale=1)
d.m1500 <- image_read("m1500.png")

m1600 <- ggplot(MapData_1600,aes(fill=as.double(Urbanization))) +
  geom_sf(color=MapData_1850$outline,linewidth=ifelse(MapData_1850$outline=="aliceblue",0.2,0.4)) +
  scale_x_continuous(limits = c(-15, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_paletteer_c("ggthemes::Orange",
                         na.value="lightgrey",
                         limits=c(0,50),
                         guide=guide_colorbar(                #opções da colorbar 
                           direction = "vertical",
                           position = "right",
                           label.position= "right",
                           barwidth = unit(0.4, "cm"),         # width of the colorbar
                           barheight = unit(7, "cm"),          # height of the colorbar
                           ticks = TRUE
                         )) +
  labs(title = "População Urbana (em %)",
       subtitle= "1600",
       caption = "Nota: Fronteiras em azul denotam \"Atlantic Trader\". Elaboração prórpia com dados de Acemoglu, Johnson e Robinson (2008).") +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.grid.major =  element_line(color = grey(.8),linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill="aliceblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0,vjust = -0.2, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 6,face="italic")) +
  annotation_scale(location="bl",height = unit(0.17,"cm"),plot_unit = "km")

ggsave(filename = "m1600.png",plot = m1600,scale=1)
d.m1600 <- image_read("m1600.png")

m1700 <- ggplot(MapData_1700,aes(fill=as.double(Urbanization))) +
  geom_sf(color=MapData_1850$outline,linewidth=ifelse(MapData_1850$outline=="aliceblue",0.2,0.4)) +
  scale_x_continuous(limits = c(-15, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_paletteer_c("ggthemes::Orange",
                         na.value="lightgrey",
                         limits=c(0,50),
                         guide=guide_colorbar(                #opções da colorbar 
                           direction = "vertical",
                           position = "right",
                           label.position= "right",
                           barwidth = unit(0.4, "cm"),         # width of the colorbar
                           barheight = unit(7, "cm"),          # height of the colorbar
                           ticks = TRUE
                         )) +
  labs(title = "População Urbana (em %)",
       subtitle= "1700",
       caption = "Nota: Fronteiras em azul denotam \"Atlantic Trader\". Elaboração prórpia com dados de Acemoglu, Johnson e Robinson (2008).") +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.grid.major =  element_line(color = grey(.8),linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill="aliceblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0,vjust = -0.2, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 6,face="italic")) +
  annotation_scale(location="bl",height = unit(0.17,"cm"),plot_unit = "km")

ggsave(filename = "m1700.png",plot = m1700,scale=1)
d.m1700 <- image_read("m1700.png")

m1800 <- ggplot(MapData_1800,aes(fill=as.double(Urbanization))) +
  geom_sf(color=MapData_1850$outline,linewidth=ifelse(MapData_1850$outline=="aliceblue",0.2,0.4)) +
  scale_x_continuous(limits = c(-15, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_paletteer_c("ggthemes::Orange",
                         na.value="lightgrey",
                         limits=c(0,50),
                         guide=guide_colorbar(                #opções da colorbar 
                           direction = "vertical",
                           position = "right",
                           label.position= "right",
                           barwidth = unit(0.4, "cm"),         # width of the colorbar
                           barheight = unit(7, "cm"),          # height of the colorbar
                           ticks = TRUE
                         )) +
  labs(title = "População Urbana (em %)",
       subtitle= "1800",
       caption = "Nota: Fronteiras em azul denotam \"Atlantic Trader\". Elaboração prórpia com dados de Acemoglu, Johnson e Robinson (2008).") +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.grid.major =  element_line(color = grey(.8),linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill="aliceblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0,vjust = -0.2, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 6,face="italic")) +
  annotation_scale(location="bl",height = unit(0.17,"cm"),plot_unit = "km")

ggsave(filename = "m1800.png",plot = m1800,scale=1)
d.m1800 <- image_read("m1800.png")

m1850 <- ggplot(MapData_1850,aes(fill=as.double(Urbanization))) +
  geom_sf(color=MapData_1850$outline,linewidth=ifelse(MapData_1850$outline=="aliceblue",0.2,0.4)) +
  scale_x_continuous(limits = c(-15, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_paletteer_c("ggthemes::Orange",
                         na.value="lightgrey",
                         limits=c(0,50),
                         guide=guide_colorbar(                #opções da colorbar 
                           direction = "vertical",
                           position = "right",
                           label.position= "right",
                           barwidth = unit(0.4, "cm"),         # width of the colorbar
                           barheight = unit(7, "cm"),          # height of the colorbar
                           ticks = TRUE
                         )) +
  labs(title = "População Urbana (em %)",
       subtitle= "1850",
       caption = "Nota: Fronteiras em azul denotam \"Atlantic Trader\". Elaboração prórpia com dados de Acemoglu, Johnson e Robinson (2008).") +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.grid.major =  element_line(color = grey(.8),linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill="aliceblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0,vjust = -0.2, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 6,face="italic")) +
  annotation_scale(location="bl",height = unit(0.17,"cm"),plot_unit = "km")

ggsave(filename = "m1850.png",plot = m1850,scale=1)
d.m1850 <- image_read("m1850.png")

img <- c(d.m1300,d.m1400,d.m1500,d.m1600,d.m1700,d.m1800,d.m1850)
my_gif <- image_animate(image_scale(img,"600x600"),fps = 100,delay = 100,dispose="previous")
image_write(my_gif,"urbanization.gif")

###### Plotando Instituições #######

i1300 <- ggplot(MapData_1300,aes(fill=as.double(Institutions))) +
  geom_sf(color=MapData_1850$outline,linewidth=ifelse(MapData_1850$outline=="aliceblue",0.2,0.4)) +
  scale_x_continuous(limits = c(-15, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_paletteer_c("ggthemes::Orange",
                         na.value="lightgrey",
                         limits=c(1,7),
                         guide=guide_colorbar(                #opções da colorbar 
                           direction = "vertical",
                           position = "right",
                           label.position= "right",
                           barwidth = unit(0.4, "cm"),         # width of the colorbar
                           barheight = unit(7, "cm"),          # height of the colorbar
                           ticks = TRUE
                         )) +
  labs(title = "Restrições ao Poder Executivo",
       subtitle= "1300",
       caption = "Nota: Fronteiras em azul denotam \"Atlantic Trader\". Elaboração prórpia com dados de Acemoglu, Johnson e Robinson (2008).") +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.grid.major =  element_line(color = grey(.8),linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill="aliceblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0,vjust = -0.2, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 6,face="italic")) +
  annotation_scale(location="bl",height = unit(0.17,"cm"),plot_unit = "km")

ggsave(filename = "fig-8.png",plot = i1300,scale=1)

i1400 <- ggplot(MapData_1400,aes(fill=as.double(Institutions))) +
  geom_sf(color=MapData_1850$outline,linewidth=ifelse(MapData_1850$outline=="aliceblue",0.2,0.4)) +
  scale_x_continuous(limits = c(-15, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_paletteer_c("ggthemes::Orange",
                         na.value="lightgrey",
                         limits=c(1,7),
                         guide=guide_colorbar(                #opções da colorbar 
                           direction = "vertical",
                           position = "right",
                           label.position= "right",
                           barwidth = unit(0.4, "cm"),         # width of the colorbar
                           barheight = unit(7, "cm"),          # height of the colorbar
                           ticks = TRUE
                         )) +
  labs(title = "Restrições ao Poder Executivo",
       subtitle= "1400",
       caption = "Nota: Fronteiras em azul denotam \"Atlantic Trader\". Elaboração prórpia com dados de Acemoglu, Johnson e Robinson (2008).") +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.grid.major =  element_line(color = grey(.8),linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill="aliceblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0,vjust = -0.2, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 6,face="italic")) +
  annotation_scale(location="bl",height = unit(0.17,"cm"),plot_unit = "km")

ggsave(filename = "fig-9.png",plot = i1400,scale=1)

i1500 <- ggplot(MapData_1500,aes(fill=as.double(Institutions))) +
  geom_sf(color=MapData_1850$outline,linewidth=ifelse(MapData_1850$outline=="aliceblue",0.2,0.4)) +
  scale_x_continuous(limits = c(-15, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_paletteer_c("ggthemes::Orange",
                         na.value="lightgrey",
                         limits=c(1,7),
                         guide=guide_colorbar(                #opções da colorbar 
                           direction = "vertical",
                           position = "right",
                           label.position= "right",
                           barwidth = unit(0.4, "cm"),         # width of the colorbar
                           barheight = unit(7, "cm"),          # height of the colorbar
                           ticks = TRUE
                         )) +
  labs(title = "Restrições ao Poder Executivo",
       subtitle= "1500",
       caption = "Nota: Fronteiras em azul denotam \"Atlantic Trader\". Elaboração prórpia com dados de Acemoglu, Johnson e Robinson (2008).") +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.grid.major =  element_line(color = grey(.8),linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill="aliceblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0,vjust = -0.2, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 6,face="italic")) +
  annotation_scale(location="bl",height = unit(0.17,"cm"),plot_unit = "km")

ggsave(filename = "fig-10.png",plot = i1500,scale=1)

i1600 <- ggplot(MapData_1600,aes(fill=as.double(Institutions))) +
  geom_sf(color=MapData_1850$outline,linewidth=ifelse(MapData_1850$outline=="aliceblue",0.2,0.4)) +
  scale_x_continuous(limits = c(-15, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_paletteer_c("ggthemes::Orange",
                         na.value="lightgrey",
                         limits=c(1,7),
                         guide=guide_colorbar(                #opções da colorbar 
                           direction = "vertical",
                           position = "right",
                           label.position= "right",
                           barwidth = unit(0.4, "cm"),         # width of the colorbar
                           barheight = unit(7, "cm"),          # height of the colorbar
                           ticks = TRUE
                         )) +
  labs(title = "Restrições ao Poder Executivo",
       subtitle= "1600",
       caption = "Nota: Fronteiras em azul denotam \"Atlantic Trader\". Elaboração prórpia com dados de Acemoglu, Johnson e Robinson (2008).") +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.grid.major =  element_line(color = grey(.8),linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill="aliceblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0,vjust = -0.2, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 6,face="italic")) +
  annotation_scale(location="bl",height = unit(0.17,"cm"),plot_unit = "km")

ggsave(filename = "fig-11.png",plot = i1600,scale=1)

i1700 <- ggplot(MapData_1700,aes(fill=as.double(Institutions))) +
  geom_sf(color=MapData_1850$outline,linewidth=ifelse(MapData_1850$outline=="aliceblue",0.2,0.4)) +
  scale_x_continuous(limits = c(-15, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_paletteer_c("ggthemes::Orange",
                         na.value="lightgrey",
                         limits=c(1,7),
                         guide=guide_colorbar(                #opções da colorbar 
                           direction = "vertical",
                           position = "right",
                           label.position= "right",
                           barwidth = unit(0.4, "cm"),         # width of the colorbar
                           barheight = unit(7, "cm"),          # height of the colorbar
                           ticks = TRUE
                         )) +
  labs(title = "Restrições ao Poder Executivo",
       subtitle= "1700",
       caption = "Nota: Fronteiras em azul denotam \"Atlantic Trader\". Elaboração prórpia com dados de Acemoglu, Johnson e Robinson (2008).") +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.grid.major =  element_line(color = grey(.8),linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill="aliceblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0,vjust = -0.2, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 6,face="italic")) +
  annotation_scale(location="bl",height = unit(0.17,"cm"),plot_unit = "km")

ggsave(filename = "fig-12.png",plot = i1700,scale=1)

i1800 <- ggplot(MapData_1800,aes(fill=as.double(Institutions))) +
  geom_sf(color=MapData_1850$outline,linewidth=ifelse(MapData_1850$outline=="aliceblue",0.2,0.4)) +
  scale_x_continuous(limits = c(-15, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_paletteer_c("ggthemes::Orange",
                         na.value="lightgrey",
                         limits=c(1,7),
                         guide=guide_colorbar(                #opções da colorbar 
                           direction = "vertical",
                           position = "right",
                           label.position= "right",
                           barwidth = unit(0.4, "cm"),         # width of the colorbar
                           barheight = unit(7, "cm"),          # height of the colorbar
                           ticks = TRUE
                         )) +
  labs(title = "Restrições ao Poder Executivo",
       subtitle= "1800",
       caption = "Nota: Fronteiras em azul denotam \"Atlantic Trader\". Elaboração prórpia com dados de Acemoglu, Johnson e Robinson (2008).") +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.grid.major =  element_line(color = grey(.8),linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill="aliceblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0,vjust = -0.2, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 6,face="italic")) +
  annotation_scale(location="bl",height = unit(0.17,"cm"),plot_unit = "km")

ggsave(filename = "fig-13.png",plot = i1800,scale=1)

i1850 <- ggplot(MapData_1850,aes(fill=as.double(Institutions))) +
  geom_sf(color=MapData_1850$outline,linewidth=ifelse(MapData_1850$outline=="aliceblue",0.2,0.4)) +
  scale_x_continuous(limits = c(-15, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_paletteer_c("ggthemes::Orange",
                         na.value="lightgrey",
                         limits=c(1,7),
                         guide=guide_colorbar(                #opções da colorbar 
                           direction = "vertical",
                           position = "right",
                           label.position= "right",
                           barwidth = unit(0.4, "cm"),         # width of the colorbar
                           barheight = unit(7, "cm"),          # height of the colorbar
                           ticks = TRUE
                         )) +
  labs(title = "Restrições ao Poder Executivo",
       subtitle= "1850",
       caption = "Nota: Fronteiras em azul denotam \"Atlantic Trader\". Elaboração prórpia com dados de Acemoglu, Johnson e Robinson (2008).") +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.grid.major =  element_line(color = grey(.8),linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill="aliceblue"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0,vjust = -0.2, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 6,face="italic")) +
  annotation_scale(location="bl",height = unit(0.17,"cm"),plot_unit = "km")

ggsave(filename = "fig-14.png",plot = i1850,scale=1)
