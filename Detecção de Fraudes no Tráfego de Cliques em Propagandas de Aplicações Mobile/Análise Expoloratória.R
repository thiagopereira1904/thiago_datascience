# Análise Exploratória
# Definindo diretório de trabalho
options(warn=-1)
setwd("C:/Users/Thiago Pereira/Desktop/dsa/BigDataRAzure/Projeto 1")
getwd()


# Carregando pacotes
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(Hmisc)
library(ggpubr)
library(gridExtra)
library(treemap)
library(grid)
library(forcats)
library(corrplot)
library(tidyr)


# Carregando os dados
df <- read_csv('train_sample.csv')


head(df)


# Dimensões do df
dim(df)


# Tipos das colunas
str(df)

Descrições sobre as colunas:
  
  ip: endereço ip do click <br>
  app: id do aplicátivo para marketing <br>
  device: tipo do celular, ex: Iphone 6 plus, Iphone 7 plus, Hawuei mate 7, etc... <br>
  os: versão do so do aparelho celular <br>
  channel: id do canal do editor de anúncios para celular <br>
  click_time: hora do click no anúncio(utc) <br>
  attributed_time: se o usuário fez o download do app depois de clicar no ad, hora do download do app <br>
  is_attributed: variável alvo a ser predita, indicando se o app foi baixado <br>
  OBS: ip, app, device, os e channel são dados encriptografados.

# Análise Exploratória
### Qual a porcentagem de ips duplicados em relação ao conjunto de dados?

duplicated_ips <- df[duplicated(df$ip),]
length(duplicated_ips$ip) / length(df$ip)
#65% da base de dados são ip duplicados.

### Qual a porcentagem de ocorrências da variável alvo (is_attributed == 1) em relação ao conjunto de dados?

nrow(filter(df, is_attributed == 1)) / nrow(df)
# Observa-se um fenômeno em que a variável alvo é rara com 0.22% de ocorrência.

### Usuários que acessam mais de uma vez, tem maior porcentagem de conversão (realizar o download do app)? 

df$ip_duplicado = ifelse(duplicated(df$ip), 1, 0)
grouped_1 <- df %>% group_by(is_attributed, ip_duplicado) %>% summarise(count = n())
grouped_1
# É possível observar que o volume para variável alvo(is_attributed) == 0 e o "ip" é duplicado(ip_duplicado == 1) é 
# quase 2x maior comparado à variável alvo(is_attributed) == 0 e o "ip" não é duplicado(ip_duplicado == 0).
# No caso da variável alvo(is_attributed == 1) ocorre ao contrário, usuários que fazem o download do aplicativo, acabam
# não acessando o anúncio mais de uma vez.
# Conclusão: 
# 1) Usuários que acessam o anúncio e não o consomem, acabam acessando-o por mais vezes e mesmo assim não baixam o aplicativo.
# 2) A maioria dos usuários que acessam o anúncio e acabam consumindo o conteúdo do anúncio, só acessam este anúncio uma única vez.
# A variável ip_duplicado será de grande valia, pois quando o registro for 0, o usuário terá maiores chances de realizar o download# do aplicativo. OBS: NÃO INDICA CAUSALIDADE.

# Qual a média e mediana em minutos que o usuário leva para acessar o anúncio e fazer o download do app?

df$dateDiff <- ifelse(is.na(df$attributed_time), 0, difftime(df$attributed_time, df$click_time, units = "mins"))
boxplot(df %>% filter(dateDiff > 0) %>% select(dateDiff), main = "Boxplot dateDiff", xlab = "dateDiff", ylab = "Minutos")
# Média em minutos que o usuário leva para fazer o download depois de clicar no anúncio:
lapply(df %>% filter(dateDiff > 0) %>% select(dateDiff), mean)
# Visualizando o boxplot, percebe-se a ocorrência de muitos outliers que acabam puxando 
# a média para cima, 74.99288 minutos, o que não é bom para mensurarmos esse tempo.
# Mediana em minutos que o usuário leva para fazer o download depois de clicar no anúncio:
lapply(df %>% filter(dateDiff > 0) %>% select(dateDiff), median)
# A melhor medida para mensurarmos o tempo em que o usuário leva para
# clicar no anúncio e realizar o download do aplicativo é de 3.3 minutos, segundo a mediana.

## Quantos dias de registros existem nesses dados?

difftime(max(df$click_time), min(df$click_time), units="days")
# Temos 3 dias de registros nesses dados.

## Qual a distribuição do volume de cliques por hora para cada dia?
{r message = FALSE}
df$day <- factor(day(as.POSIXct(df$click_time, format = "%Y/%m/%d")))
df$hour <- factor(hour(as.POSIXct(df$click_time, format = "%Y/%m/%d")))
clicks_by_day <- df %>% group_by(day) %>% summarise(clicks_dia = n())
# Filtrando cada dia para análise indivual
day_6 <- df %>% filter(day == "6")
day_6 <- day_6 %>% group_by(hour) %>% summarise(count = n())

day_7 <- df %>% filter(day == "7")
day_7 <- day_7 %>% group_by(hour) %>% summarise(count = n())

day_8 <- df %>% filter(day == "8")
day_8 <- day_8 %>% group_by(hour) %>% summarise(count = n())

day_9 <- df %>% filter(day == "9")
day_9 <- day_9 %>% group_by(hour) %>% summarise(count = n())
# Construindo os gráficos
chart_day6 <- ggplot(day_6, aes(x = hour, y = count, group = 1)) +
  geom_line() +
  ggtitle("Dia 6")+
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"))

chart_day7 <- ggplot(day_7, aes(x = hour, y = count, group = 1)) +
  geom_line() +
  ggtitle("Dia 7")+
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"))
chart_day8 <- ggplot(day_8, aes(x = hour, y = count, group = 1)) +
  geom_line() +
  ggtitle("Dia 8")+
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"))

chart_day9 <- ggplot(day_9, aes(x = hour, y = count, group = 1)) +
  geom_line() +
  ggtitle("Dia 9")+
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"))

grid.arrange(chart_day6, chart_day7, chart_day8, chart_day9, nrow = 2, ncol = 2)
# Os volumes de clicks por hora, a cada dia, seguem a mesma distribuição.

### Qual o volume de downloads por hora para cada dia?
{r message = FALSE}
# Filtrando dias individuais quando is_attributed == 1 para análises.
alvo1_day6 <- df %>% filter(day == "6", is_attributed == 1) %>%
  group_by(hour) %>%
  summarise(count = n())

alvo1_day7 <- df %>% filter(day == "7", is_attributed == 1) %>%
  group_by(hour) %>%
  summarise(count = n())

alvo1_day8 <- df %>% filter(day == "8", is_attributed == 1) %>%
  group_by(hour) %>%
  summarise(count = n())

alvo1_day9 <- df %>% filter(day == "9", is_attributed == 1) %>%
  group_by(hour) %>%
  summarise(count = n())
# Construindo os gráficos para análise
chart_alvo1_day6 <- ggplot(alvo1_day6, aes(x = hour, y = count, group = 1)) +
  geom_line() +
  ggtitle("Volume de Downloads dia 6")+
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"))

chart_alvo1_day7 <- ggplot(alvo1_day7, aes(x = hour, y = count, group = 1)) +
  geom_line() +
  ggtitle("Volume de Downloads dia 7")+
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"))

chart_alvo1_day8 <- ggplot(alvo1_day8, aes(x = hour, y = count, group = 1)) +
  geom_line() +
  ggtitle("Volume de Downloads dia 8")+
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"))

chart_alvo1_day9 <- ggplot(alvo1_day9, aes(x = hour, y = count, group = 1)) +
  geom_line() +
  ggtitle("Volume de Downloads dia 9")+
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"))

grid.arrange(chart_alvo1_day6, chart_alvo1_day7, chart_alvo1_day8, chart_alvo1_day9, nrow = 2, ncol = 2)


### Em quais horas, o volume de downloads do aplicativo são maiores?
{r message = FALSE}
downloads_byHour <- df %>% filter(is_attributed == 1)
downloads_byHour <- downloads_byHour %>% group_by(hour) %>% summarise(clicks_hour = n())
downloads_byHour

g2 <- ggplot(downloads_byHour, aes(x = hour, y = clicks_hour, group = 1)) +
  geom_line() +
  labs(x = "Horas", y = "Volume de downloads") +
  ggtitle("Volume de downloads por hora") +
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        axis.text.x = element_text(size= 10, face = "bold"),
        axis.text.y = element_text(size= 10, face = "bold"), 
        axis.title.x = element_text(size=13, face = "bold"),
        axis.title.y = element_text(size=13, face = "bold")) +
  stat_smooth(method = "lm", size = 1, se = FALSE)
# Plotando o gráfico 2
g2
#.

Analisando o gráfico, a linha de tendência(representada pela linha azul), está decrescente, 
isso quer dizer que, no decorrer do dia, o volume de cliques no anúncio tende a diminuir.  <br>
  Observa-se um pico de volume de acesso às 4 horas. <br>
  O volume de cliques varia, no entanto se mantém em alta até às 14 horas, após, o 
volume de acessos vai caindo até o vale(que é o período com menor volume de acessos), às 20 horas. <br><br

Conclusão: O volume de cliques e download variam durante as horas, porém quando chega às 14 horas, o volume cai drasticamente,
provavelmente, o volume de pessoas online está menor.  <br>
  O momento ideal para se fazer alguma campanha seria após às 0 horas, até às 14, pois o volume de pessoas e pessoas convertidas(download do app),
são maiores.

### Qual a distribuição variável app quando is_attributed == 1?

# Construindo histograma
app <- df %>% filter(is_attributed == 1) %>% select(app)
g1 <- ggplot(app, aes(x = app)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, color = "#BFD7F2", position = "identity") +
  theme_bw() + 
  xlab("app") +
  geom_density(alpha = .5, fill = "#375270") +
  geom_vline(aes(xintercept=mean(app)),
             color="#5C89BD", linetype="dashed", size=1, alpha = .3) +
  ggtitle("Distribuição da varável app quando is_attributed == 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"))

# Plotando histograma para visualizar a densidade de ocorrências em cada faixa de app.
g1


order_app <- df %>% filter(is_attributed == 1) %>% group_by(app) %>% summarise(count = n())
order_app$percent <- round(order_app$count / sum(order_app$count), digits=2)
order_app <- order_app[order(order_app$percent, decreasing = TRUE),]
# Visualizando as 10 maiores porcentagens de ocorrência da variável app quando is_attributed == 1
order_app

### Qual a distribuição variável device quando is_attributed == 1?

device <- df %>% filter(is_attributed == 1) %>% select(device)
# Construindo histograma para visualização
g1 <- ggplot(device, aes(x = device)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, color = "#BFD7F2", position = "identity") +
  theme_bw() + 
  xlab("app") +
  geom_density(alpha = .5, fill = "#375270") +
  geom_vline(aes(xintercept=mean(device)),
             color="#5C89BD", linetype="dashed", size=1, alpha = .3) +
  ggtitle("Distribuição da varável device quando is_attributed == 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"))

# Plotando o histograma
g1


order_device <- df %>% filter(is_attributed == 1) %>% group_by(device) %>% summarise(count = n())
order_device$percent <- round(order_device$count / sum(order_device$count), digits=2)
order_device <- order_device[order(order_device$percent, decreasing = TRUE),]
# Visualizando as 10 maiores porcentagens de ocorrência da variável device quando is_attributed == 1
order_device

### Qual a distribuição variável OS quando is_attributed == 1?

os <- df %>% filter(is_attributed == 1) %>% select(os)
# Construindo histograma para visualização
g1 <- ggplot(os, aes(x = os)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, color = "#BFD7F2", position = "identity") +
  theme_bw() + 
  xlab("app") +
  geom_density(alpha = .5, fill = "#375270") +
  geom_vline(aes(xintercept=mean(os)),
             color="#5C89BD", linetype="dashed", size=1, alpha = .3) +
  ggtitle("Distribuição da varável os quando is_attributed == 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"))

# Plotando o histograma
g1


order_channel <- df %>% filter(is_attributed == 1) %>% group_by(channel) %>% summarise(count = n())
order_channel$percent <- round(order_channel$count / sum(order_channel$count), digits=2)
order_channel <- order_channel[order(order_channel$percent, decreasing = TRUE),]
# Visualizando as 10 maiores porcentagens de ocorrência da variável channel quando is_attributed == 1
order_channel

### Qual a distribuição variável channel quando is_attributed == 1?

channel <- df %>% filter(is_attributed == 1) %>% select(channel)
# Construindo histograma para visualização
g1 <- ggplot(channel, aes(x = channel)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, color = "#BFD7F2", position = "identity") +
  theme_bw() + 
  xlab("app") +
  geom_density(alpha = .5, fill = "#375270") +
  geom_vline(aes(xintercept=mean(channel)),
             color="#5C89BD", linetype="dashed", size=1, alpha = .3) +
  ggtitle("Distribuição da varável channel quando is_attributed == 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"))

# Plotando o histograma
g1


order_channel <- df %>% filter(is_attributed == 1) %>% group_by(channel) %>% summarise(count = n())
order_channel$percent <- round(order_channel$count / sum(order_channel$count), digits=2)
order_channel <- order_channel[order(order_channel$percent, decreasing = TRUE),]
# Visualizando as 10 maiores porcentagens de ocorrência da variável channel quando is_attributed == 1
order_channel





















