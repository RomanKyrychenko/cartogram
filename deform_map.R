Sys.setlocale(,"Ru_ru")

library(ggplot2)
library(sp)
library(rgdal)
library(dplyr)
library(scales)
library(maptools)
library(ggiraph)
library(stringr)
library(knitr)
library(rmarkdown)
library(readr)
library(RColorBrewer)
library(Rcartogram)
library(getcartr)
library(readxl)
library(ggplot2)
library(ggalt)
library(ggiraph)
library(ggthemes)
library(viridis)
library(stringi)

production2016 <- read_excel("~/RGI/production.xls",sheet = "2016")
production2015 <- read_excel("~/RGI/production.xls",sheet = "2015")
production2014 <- read_excel("~/RGI/production.xls",sheet = "2014")
production2013 <- read_excel("~/RGI/production.xls",sheet = "2013")

#ukraina_adm <- readShapePoly("UKR_adm_shp/UKR_adm2.shp")
ukraina_obl <- readShapePoly("UKR_adm_shp/UKR_adm1.shp")

map_data <- read_csv("UKR_adm_shp/UKR_adm1.csv")
map_data$VARNAME_1 <- c(unlist(lapply(ukraina_obl@data$VARNAME_1,as.character)))
map_data <- left_join(map_data,production2016,by=c("VARNAME_1"="Область"))

ukraina_obl@data <- map_data

#ukraina_obl@data$VARNAME_1 <- c(unlist(lapply(ukraina_obl@data$VARNAME_1,as.character)))

#ukraina_obl@data <- left_join(ukraina_obl@data,production2016, by=c("VARNAME_1"="Область")) 

##Make a cartogram
pgrs_carto <- quick.carto(spdf=ukraina_obl,v=ukraina_obl$Овочі,res=27)

pgrs_carto2 <- quick.carto(spdf=ukraina_obl,v=ukraina_obl$Картопля,res=27)

pgrs_carto3 <- quick.carto(spdf=ukraina_obl,v=ukraina_obl$`Насіння соняшнику                       (у вазі після доробки)`,res=27)

pgrs_carto4 <- quick.carto(spdf=ukraina_obl,v=ukraina_obl$`Цукрові буряки  (фабричні)`,res=27)

pgrs_carto5 <- quick.carto(spdf=ukraina_obl,v=ukraina_obl$`Зернові та зернобобові культури  (у вазі після доробки)`,res=27)

#Овочі
pgrs_carto@data <- data.frame(pgrs_carto@data)
plot.data <- as_data_frame(fortify(pgrs_carto, region = "VARNAME_1"))
plot.data <- left_join(plot.data,production2016,by=c("id"="Область"))

plot.data$tip <- paste0(
  "<b>", plot.data$id, "</b> : ",
  format(plot.data$Овочі, big.mark = ",")," тис. т","\n",
  "<b>","Частка регіону", "</b> : ",round(plot.data$Овочі/9413.9*100,2),"%")


plot.data$onclick <- sprintf('
                             <div class="tipchart">
                             <h4>%s</h4>
                             <span class="tipspanstyle">Arrests per crime per 100K</span>
                             <table>
                             <tr class="tiprow">
                             <td class="tipbarticks">Murder</td>
                             <td class="tipbardiv"><div class="tipbar" style="width:%dpx;">%3.1f</div></td>
                             </tr>
                             <tr class="tiprow">
                             <td class="tipbarticks">Rape</td>
                             <td class="tipbardiv"><div class="tipbar" style="width:%dpx;">%3.1f</div></td>
                             </tr>
                             <tr class="tiprow">
                             <td class="tipbarticks">Assault</td>
                             <td class="tipbardiv"><div class="tipbar" style="width %dpx;">%3.1f</div></td>
                             </tr>
                             </table>
                             </div>',
                             stri_trans_totitle(plot.data$id),
                             round(3000), 3000,
                             round(2000), 2000,
                             round(1000), 1000)

# javascript is too dumb to deal with line breaks in strings well
crimes$tip <- gsub("\\\n", "", crimes$tip) 





#Картопля
pgrs_carto2@data <- data.frame(pgrs_carto2@data)
plot.data2 <- as_data_frame(fortify(pgrs_carto2, region = "VARNAME_1"))
plot.data2 <- left_join(plot.data2,production2016,by=c("id"="Область"))

plot.data2$tip <- paste0(
  "<b>", plot.data2$id, "</b> : ",
  format(plot.data2$Картопля, big.mark = ",")," тис. т","\n",
  "<b>","Частка регіону", "</b> : ",round(plot.data2$Картопля/21745.8*100,2),"%")

#Насіння соняшнику 
pgrs_carto3@data <- data.frame(pgrs_carto3@data)
plot.data3 <- as_data_frame(fortify(pgrs_carto3, region = "VARNAME_1"))
plot.data3 <- left_join(plot.data3,production2016,by=c("id"="Область"))

plot.data3$tip <- paste0(
  "<b>", plot.data3$id, "</b> : ",
  format(plot.data3$`Насіння соняшнику                       (у вазі після доробки)`, big.mark = ",")," тис. т","\n",
  "<b>","Частка регіону", "</b> : ",round(plot.data3$`Насіння соняшнику                       (у вазі після доробки)`/13604.5*100,2),"%")

#Цукрові буряки  (фабричні)
pgrs_carto4@data <- data.frame(pgrs_carto4@data)
plot.data4 <- as_data_frame(fortify(pgrs_carto4, region = "VARNAME_1"))
plot.data4 <- left_join(plot.data4,production2016,by=c("id"="Область"))

plot.data4$tip <- paste0(
  "<b>", plot.data4$id, "</b> : ",
  format(plot.data4$`Цукрові буряки  (фабричні)`, big.mark = ",")," тис. т","\n",
  "<b>","Частка регіону", "</b> : ",round(plot.data4$`Цукрові буряки  (фабричні)`/13827.4*100,2),"%")

#Зернові та зернобобові культури  (у вазі після доробки)
pgrs_carto5@data <- data.frame(pgrs_carto5@data)
plot.data5 <- as_data_frame(fortify(pgrs_carto5, region = "VARNAME_1"))
plot.data5 <- left_join(plot.data5,production2016,by=c("id"="Область"))

plot.data5$tip <- paste0(
  "<b>", plot.data5$id, "</b> : ",
  format(plot.data5$`Зернові та зернобобові культури  (у вазі після доробки)`, big.mark = ",")," тис. т","\n",
  "<b>","Частка регіону", "</b> : ",round(plot.data5$`Зернові та зернобобові культури  (у вазі після доробки)`/65951.5*100,2),"%")


#geom_path(data=plot.data, aes(x=long,y=lat, group=group), 
#          color="black", size=0.25) + 
#g <- ggplot(data=plot.data) + 
#  geom_polygon(aes(x=long,y=lat, group=group, fill=Овочі), 
#                     color="grey50", size=0.25) + 
#  scale_fill_gradient(low="red", high = "orange", 
#                           name= "Kreise", guide = guide_legend(reverse = TRUE), 
#                           na.value = "white") + 
#  coord_equal() + 
#  theme_void()
#
#plot.data$tip <- paste0(
#  "<b>", plot.data$id, "</b> : ",
#  format(plot.data$Овочі, big.mark = "."))
#
#geom_path(data=laender.df, aes(x=long,y=lat, group=group),color="black", size=0.25) + 
#
#gi <- ggplot(data=plot.data) + 
#  geom_polygon_interactive(aes(x = long, y = lat, group = group, fill = Овочі,
#      tooltip = tip, data_id = id, use_jquery = TRUE), color="grey50", size=0.25) + 
#  scale_fill_gradient(low="white", high = "red", 
#                      name="Овочі", guide = guide_legend(reverse = TRUE), 
#                      na.value = "white") + 
#  coord_equal() + 
#  theme_void()
#ggiraph(code = {print(gi)}, hover_css = "stroke-width:2px;")
