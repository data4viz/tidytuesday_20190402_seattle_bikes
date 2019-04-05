---
title: "Bikes in Seattle"
author: "@data4viz"
output:
    html_document:
      keep_md: true
      
---


Importing packages

```{r Packges, message=FALSE, warning=FALSE}

library(tidyverse)
library(lubridate)
library(zoo)
library(cowplot)

```

Data import and first cleaning. Adding date columns with lubridate. Removing 2013 and 2019 data since it's only partial.

```{r Data Import}

raw_data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv", col_types = "ccfdd")
  
rides <- raw_data %>% 
          mutate(datetime = mdy_hms(date),
                 date = date(datetime),
                 year = year(datetime),
                 month = month(datetime),
                 yearmonth = date(floor_date(datetime, unit = "month"))) %>% 
  filter(year != 2013, year != 2019)

```


- number of bikes for each month in a year
- cumulative number of bikes in each year since January
- month-over-month growth rate
- year-over-year growth rate


```{r Cumulative bikes, warning=FALSE}

rides_monthly <- rides %>%
  group_by(yearmonth, year, month) %>% 
  summarise(number_of_bikes = sum(bike_count,na.rm = TRUE)) %>% 
  ungroup() %>%
  group_by(year) %>% 
  mutate(cumsum = cumsum(number_of_bikes)) %>% 
ungroup() %>% 
mutate(ma12 = zoo::rollmean(number_of_bikes, 12, align = "right", fill = NA)) #moving average of the last 12 periods (months)



#total number of bikes by year
bikes_by_year <- 
rides_monthly %>% 
  ggplot(aes(x = as.factor(month(yearmonth)), y = cumsum, group = year, col = as.factor(year))) +
  geom_line(size = 1.5) +
  theme_light() +
  scale_color_discrete(name = "Year:")+ #change legend name
  scale_y_continuous(labels = scales::number_format()) +
  labs(x = "Month", title = "There has been less bikes each year",subtitle = "Total number of bikes since the beginning of the year", y = " ") +
  theme(legend.position = "top")
  
bikes_by_year


bikes_ma <-
rides_monthly %>% 
  ggplot(aes(x = yearmonth, y = ma12, group = 1)) +
   geom_col(aes(y = number_of_bikes), fill = "steelblue") +
  geom_line(color = "black", size = 1.5) +
  scale_y_continuous(labels = scales::number_format()) +
  theme_light() +
  labs(title = "There is high seasonality in the number of bikes", subtitle = "Total number of bikes and 12-month moving average", x = " ", y = " " )
 
bikes_ma

```

Making a heatmap


```{r Growth by direction}

rides_direction <- rides %>%
  group_by(yearmonth, direction) %>% 
  summarise(number_of_bikes = sum(bike_count,na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(yoy = number_of_bikes/lag(number_of_bikes, 12)-1,
         mom = number_of_bikes/lag(number_of_bikes, 1)-1) %>% 
filter(yearmonth  >= "2016-01-01", yearmonth  <= "2018-12-01")


heatmap <-
rides_direction %>% 
  ggplot(aes(x = direction, y = yearmonth)) +
  geom_tile(aes(fill = number_of_bikes), width = 1) +
  scale_fill_gradient(low = "white", high = "darkgreen", name = "Number of bikes") +
  scale_x_discrete(expand = c(0,0)) +
  labs(x = "Direction", y = "Time", title = "There is big difference across the regions",subtitle = "Heatmap" ) +
  theme(panel.background = element_blank())
  

heatmap


```

Importing an image and combining plots


```{r Plotting with Cowplot, warning=FALSE}

#importing a picture
bike_pic <- 
  ggdraw() + 
  draw_image("cycle_path.jpg", scale = 1)


combined_plot <-
plot_grid(bikes_by_year, bikes_ma, heatmap, bike_pic, ncol = 2)


cowplot::ggsave("combined_plot.jpg", width = 800, height = 400, units = "mm")

```

