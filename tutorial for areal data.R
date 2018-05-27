#### library loading ####
library(rgdal)
library(ggplot2)
library(foreign)
library(dplyr)
library(magrittr)
library(maps)
library(maptools)
library(spdep)
library(mapproj)
library(RColorBrewer)


#### reading shapefiles in R ####
pathname <- "bnd_sigungu_00_2016"
shp_sigungu <- readOGR(dsn= pathname, layer="bnd_sigungu_00_2016")
dbf_sigungu <- read.dbf(file=paste(pathname, "/bnd_sigungu_00_2016.dbf", sep=""), as.is=T)
shp_sigungu@data <- dbf_sigungu
head(shp_sigungu@data)


#### change coordinate system ####

# 지리정보 좌표계 : UTM-K(GRS80 타원체) -> 위경도, 네이버 지도 기준
name1 <- paste("+proj=tmerc +lat_0",
               "=38 +lon_0=127.5 +k",
               "=0.9996 +x_0=1000000 +y_0",
               "=2000000 +ellps",
               "=GRS80 +units=m +no_defs",
               sep="")

proj4string(shp_sigungu) <- CRS(name1)

name2 <- paste("+proj=longlat + ellps=WGS84 +datum",
               "=WGS84 +no_defs", sep="")

shp_sigungu %<>% spTransform(CRS(name2))


#### mapping shape files ####

# 서울(SIGUNGU_CD="11")을 구 단위로 그려보자.
shp_sigungu_seoul <- shp_sigungu %>% subset(
  substr(SIGUNGU_CD %>% as.character, 1, 2)=="11"
)

shp_sigungu_seoul@data$rand_num <- rnorm(shp_sigungu_seoul %>% length) # 이 부분을 분석 결과 값으로 대체하면 된다.
brks <- quantile(shp_sigungu_seoul@data$rand_num, seq(0, 1, 1/5)) # 색상을 다르게 하기 위함
interval <- cut(shp_sigungu_seoul@data$rand_num, brks, include.lowest = T)

cols <- brewer.pal(5, "Spectral")[5:1]
plot(shp_sigungu_seoul, col=cols[interval], axes=F)
box()
degAxis(1)
degAxis(2)
legend("topleft", fill = cols, legend=interval %>% table %>% names, bty="o", title="rand_num", cex=1, y.inter=1)

# 시군구 전체
# ggplot 이용하여 그리기 -fortify 이용 <- 실행시간 오래걸림
shp_sigungu@data$id <- 0:(length(shp_sigungu)-1) %>% as.character()
shp_sigungu@data$rand_num <- rnorm(shp_sigungu %>% length) # 이 부분을 분석 결과 값으로 대체하면 된다.
brks <- quantile(shp_sigungu@data$rand_num, seq(0,1, length.out = 12))
shp_sigungu@data$interval <- cut(shp_sigungu@data$rand_num, brks, include.lowest = T)

shp_sigungu_fort <- fortify(shp_sigungu) %>% left_join(shp_sigungu@data, by="id")

ggplot(shp_sigungu_fort, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=interval), colour = NA) +
  coord_map(projection = "lambert", parameters = c(shp_sigungu@bbox[2,1]-0.005, shp_sigungu@bbox[2,2]+0.005))+
  scale_fill_brewer(palette = "Spectral")


#### Construct an adjacent matrix ####

# spdep 패키지의 함수들을 이용해 주어진 shape들의 neighbor 리스트를 만들고 행렬로 표현해보자.

shp_sigungu_seoul@data$id <- 0:(length(shp_sigungu_seoul)-1) %>% as.character()
shp_sigungu_seoul_fort <- fortify(shp_sigungu_seoul) %>% left_join(shp_sigungu_seoul@data, by="id")

nb_sigungu_seoul <- poly2nb(shp_sigungu_seoul)
nb_sigungu_mat_seoul <- nb2mat(nb_sigungu_seoul, style="B")

shp_sigungu_seoul_fort %<>% left_join(data.frame(id=0:24 %>% as.character(), n.nb=apply(nb_sigungu_mat_seoul, 2, sum)), by="id")

ggplot(shp_sigungu_seoul_fort, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=n.nb %>% as.factor), colour = "black") +
  coord_equal() + scale_fill_brewer(palette = "Blues")
