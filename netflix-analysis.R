# Definindo diretório
setwd("/home/nikolas/Documentos/data/") # Defina o seu caminho. 

# Pacotes
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("zoo")
install.packages("ggplot2")
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)

# Dados
netflix <- read.csv("NetflixViewingHistory.csv") # Troque pelo .csv dos seus dados.
head(netflix)
str(netflix)
netflix$Date <- dmy(netflix$Date)

# Maratonas de séries
net_serie <- netflix %>%
  separate(col = Title, into = c("title", "season", "title_episode"), sep = ': ')
head(net_serie)
net_serie <- net_serie[!is.na(net_serie$season),]
net_serie <- net_serie[!is.na(net_serie$title_episode),]
head(net_serie)

net_serie_binge <- net_serie %>%
  count(title, Date)
net_serie_binge

# Considerando que mais de 5 títulos por dia é uma maratona
net_serie_binge <- net_serie_binge[net_serie_binge$n >= 5,]
net_serie_binge
net_serie_binge <- net_serie_binge[order(net_serie_binge$Date),]
net_serie_binge

# Gráfico das séries mais assistidas

# Organizando os dados
net_serie_binge_n <- net_serie_binge %>% 
  group_by(title) %>% 
  summarise(n = sum(n)) %>%
  arrange(desc(n))
net_serie_binge_n

# Fazendo o grafico
net_serie_binge_top_5_plot <- net_serie_binge_n %>% 
  top_n(5) %>%
  ggplot(aes(x = reorder(title, n), y = n)) +
  geom_col(fill = "#F9B62F") +
  coord_flip() +
  labs(x = "", y = "Episodes watched", title= 'Top 5 series') +
  theme_minimal()
net_serie_binge_top_5_plot

# Estatísticas

# Metricas basicas
mean(net_serie_binge_n$n)
hist(net_serie_binge_n$n)

first_title <- as.character(netflix$Title[netflix$Date == min(netflix$Date)][1])
first_title

last_title <- as.character(netflix$Title[netflix$Date == max(netflix$Date)][1])
last_title

# Mais gráficos

# Quantos títulos por dia?
netflix_title_day <- netflix %>%
  count(Date) %>%
  arrange(desc(n))
head(netflix_title_day)

# Grafico de títulos por dia
netflix_title_day_plot <- ggplot(aes(x = Date, y = n, color = n), data = netflix_title_day) +
  geom_col(color = c("#0B2E3A")) +
  theme_minimal() +
  xlab("Data") +
  ylab("Títulos assistidos")
netflix_title_day_plot

# Títulos diariamente por mês

# Heatmap: calendário com o número de títulos assistidos por dia
netflix_title_day <- netflix_title_day[order(netflix_title_day$Date),]
netflix_title_day$weekday <- wday(netflix_title_day$Date)
netflix_title_day$weekdayf <- weekdays(netflix_title_day$Date, abbreviate = T)
netflix_title_day$monthf <- months(netflix_title_day$Date, abbreviate = T)

netflix_title_day$weekdayf <-factor(netflix_title_day$weekday,
                                    levels = rev(1:7),
                                    labels = rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
                                    ordered = TRUE) 
netflix_title_day$monthf <- factor(month(netflix_title_day$Date),
                                   levels = as.character(1:12), 
                                   labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                                   ordered = TRUE)

netflix_title_day$yearmonth <- factor(as.yearmon(netflix_title_day$Date)) 
netflix_title_day$week <- as.numeric(format(netflix_title_day$Date,"%W"))
netflix_title_day$monthweek <- ceiling(day(netflix_title_day$Date) / 7)

netflix_title_day_calendario <- ggplot(netflix_title_day, aes(monthweek, weekdayf, fill = netflix_title_day$n)) + 
  geom_tile(colour = "white") + 
  facet_grid(year(netflix_title_day$Date) ~ monthf) + 
  scale_fill_gradient(low = "#42C0BB", high = "#0B2E3A") + 
  xlab("Week day") + 
  ylab("") + 
  ggtitle("Heatmap: episodes by day calendar") + 
  labs(fill = "Titles")
netflix_title_day_calendario

# Meses
month_watch <- netflix_title_day %>%
  count(monthf)
month_watch

month_watch_plot <- month_watch %>% 
  ggplot(aes(monthf, n)) +
  geom_col(fill = "#F9B62F") +
  coord_polar()  +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  labs(title = "Episodes watched by month")
month_watch_plot