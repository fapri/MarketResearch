
library(readr)
library(ggplot2)
library(lubridate)

Corn_FuturesMarket <- read_csv("Data/Corn_FuturesMarket.csv")

my1Int = interval(ymd(20080101), ymd(20090101))

saleDate = mdy(Corn_FuturesMarket$Date[15])
deliveryDate = mdy("12-14-2008")

theme_update(text = element_text(size=20))

ggplot(data = Corn_FuturesMarket[which(mdy(Corn_FuturesMarket$Date) %within% my1Int), ]) + 
  geom_line(aes(x = mdy(Date), y = DecNC, color = "Futures Price"), 
            group = 1, size = 1) + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")  + 
  scale_y_continuous(breaks = round(seq(3, 8, by = 0.5),1), 
                     labels=scales::dollar_format()) + 
  geom_point(aes(x = saleDate, 
                 y = Corn_FuturesMarket$DecNC[15],
                 color = "Sale Price"), 
             size = 3) + 
  geom_segment(aes(x = saleDate, 
                   y = 7.88, 
                   xend = deliveryDate, 
                   yend = 7.88,
                   color = "Max Price"),
               linetype = "dashed", 
               size = 1.25) + 
  scale_color_manual(values = c("black", "purple", "green")) + 
  labs(x = "Date", y = "December Futures Price", color = "Key", 
       title = "Price Objective Margin Example (2008)")



