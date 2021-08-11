library(jsonlite)
library(dplyr)
library(sf)
library(animation)
library(ggplot2)
library(lubridate)

rm(list=ls())

#https://rpubs.com/ktgrstsh/775867のlayer_autoline_okinawaを使用
layer_autoline_okinawa <- function(
  x = c(129, 132.5, 138),
  xend = c(132.5, 138, 138),
  y = c(40, 40, 42),
  yend = c(40, 42, 46),
  size = ggplot2::.pt / 15
){
  ggplot2::annotate("segment",
                    x = x,
                    xend = xend,
                    y = y,
                    yend = yend,
                    size = .pt / 15
  )
}

#日本地図のデータを読込む
d_map <- read_sf(system.file("shapes/jpn.shp", package = "NipponMap")[1],
               crs = "+proj=longlat +datum=WGS84")
d_map$SP_ID <- as.numeric(d_map$SP_ID)

#都道府県別累計陽性者数のデータを読込む
d_covid_json <- fromJSON("data/Covid19JapanAll.json")
d_covid <- d_covid_json$itemList %>%
  rename(pref = name_jp,
         npatients_acc = npatients) %>%
  mutate(date = as.Date(date),
         date_week = floor_date(date,"week"),
         npatients_acc = as.numeric(npatients_acc),
         SP_ID = case_when(
           pref == "北海道" ~ 1,
           pref == "青森県" ~ 2,
           pref == "岩手県" ~ 3,
           pref == "宮城県" ~ 4,
           pref == "秋田県" ~ 5,
           pref == "山形県" ~ 6,
           pref == "福島県" ~ 7,
           pref == "茨城県" ~ 8,
           pref == "栃木県" ~ 9,
           pref == "群馬県" ~ 10,
           pref == "埼玉県" ~ 11,
           pref == "千葉県" ~ 12,
           pref == "東京都" ~ 13,
           pref == "神奈川県" ~ 14,
           pref == "新潟県" ~ 15,
           pref == "富山県" ~ 16,
           pref == "石川県" ~ 17,
           pref == "福井県" ~ 18,
           pref == "山梨県" ~ 19,
           pref == "長野県" ~ 20,
           pref == "岐阜県" ~ 21,
           pref == "静岡県" ~ 22,
           pref == "愛知県" ~ 23,
           pref == "三重県" ~ 24,
           pref == "滋賀県" ~ 25,
           pref == "京都府" ~ 26,
           pref == "大阪府" ~ 27,
           pref == "兵庫県" ~ 28,
           pref == "奈良県" ~ 29,
           pref == "和歌山県" ~ 30,
           pref == "鳥取県" ~ 31,
           pref == "島根県" ~ 32,
           pref == "岡山県" ~ 33,
           pref == "広島県" ~ 34,
           pref == "山口県" ~ 35,
           pref == "徳島県" ~ 36,
           pref == "香川県" ~ 37,
           pref == "愛媛県" ~ 38,
           pref == "高知県" ~ 39,
           pref == "福岡県" ~ 40,
           pref == "佐賀県" ~ 41,
           pref == "長崎県" ~ 42,
           pref == "熊本県" ~ 43,
           pref == "大分県" ~ 44,
           pref == "宮崎県" ~ 45,
           pref == "鹿児島県" ~ 46,
           pref == "沖縄県" ~ 47)) %>%
  group_by(pref) %>%
  arrange(date) %>%
  mutate(npatients = c(min(npatients_acc),diff(npatients_acc))) %>%
  filter(date > as.Date("2020-04-22") & between(date_week,as.Date("2020-04-26"),as.Date("2021-07-25"))) %>% #初日の感染者数は分からないため削除 & 1週間分のデータがない期間は削除
  ungroup()
d_covid[d_covid$npatients < 0,"npatients"] <- 0 #なぜか累計感染者数が時間とともに減る(感染者数が負になる)ことがあるので、0で置き換える

#陽性者数の週次平均を計算する
d_covid_map <- d_covid %>%
  group_by(date_week,SP_ID,pref) %>%
  summarise(npatients = mean(npatients) + 10^(0)) %>% #後で対数表記を行うので、log(0)を避けるために微小な値を足しておく
  ungroup() %>%
  left_join(.,d_map %>% select(SP_ID,geometry),key = "SP_ID")
  
#地図上の沖縄県の位置を右上に移動する(https://twitter.com/shoei05/status/1398945565845581825を参考に)
d_covid_map$geometry[grep("沖縄県", d_covid_map$pref)] <-
  d_covid_map$geometry[grep("沖縄県", d_covid_map$pref)] + c(5, 17)

#GIFアニメーションのオプション設定
ani.options(loop = TRUE, outdir=getwd(), convert = 'program/ImageMagick-7.1.0-4-portable-Q16-x64/convert.exe')

#GIFアニメーションの作成
plotdate_list <- d_covid_map %>% arrange(date_week) %>% distinct(date_week) %>% pull(date_week)
saveGIF({
  for (n in 1:length(plotdate_list)){
    #各週の陽性者数のグラフを描く
    plotdate_s <- plotdate_list[n]
    plotdate_e <- plotdate_s + 6
    sf_plot <- st_as_sf(d_covid_map %>% filter(date_week == plotdate_s))
    p <- ggplot(sf_plot)
    p <- p + geom_sf(aes(fill = npatients))
    p <- p + layer_autoline_okinawa()
    p <- p + scale_fill_gradient(low="white", high="#E60012", trans="log10", limits=c(10^(0),10^4), name="陽性者数")
    p <- p + labs(title = paste0("都道府県別新規陽性者数(週次平均)\n",as.character(plotdate_s),"~",as.character(plotdate_e))) 
    p <- p + theme(
      panel.background = element_rect(fill = "midnightblue"),
      panel.grid  = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
    plot(p)
  }
}, movie.name="covid19_JapanPref.gif", interval=0.25)
