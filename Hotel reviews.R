install.packages("ggplot2")
install.packages("dplyr")
install.packages("wordcloud")
install.packages("tm")
install.packages("leaflet")
install.packages("knitr")
install.packages("mapview")

webshot::install_phantomjs() #?em?lapio eksportavimui

#--------------------------------Libraries

library(dplyr)
library(ggplot2) 
library(stringr) 
library(tm) 
library(wordcloud) 
library(leaflet) 
library(mapview) 
library(VIM)
#--------------------------------Reading data

path2 <- "C:/Users/Lukas/Desktop/R2/Hotel Reviews/Input/Hotel_Reviews.csv"
data <- read.csv(path2, header = TRUE)

#--------------------------------Exploring data

dim(data)
colnames(data)
str(data)
summary(data)
tail(data, 1)

missing_values <- lapply(data,function(x) sum(is.na(x)))
missing_values[missing_values>0]



#praleistos reiksmes stulpeliuose

table_with_missing_values <- data[which(is.na(data$lat == TRUE)), ] 
length(unique(table_with_missing_values$Hotel_Address))

unique(data2$Hotel_Address) #Kiek viesbuciu turi praleistas koordinaciu reiksmes

range(data$Reviewer_Score) #didziausias ir maziausias vertinimas

#Tusti neigiami atsiliepimai
neg <- data[data$Negative_Review == " ", ]
plot(table(neg$Reviewer_Score))

teig <- data[data$Positive_Review == " ", ]
plot(table(teig$Positive_Review))

#pasalinimas dublikatu
data <- distinct(data)
#praleistu reiksmiu uzpildymas
data[data$Hotel_Address == "20 Rue De La Ga t 14th arr 75014 Paris France", ncol(data)] <- 2.323509
data[data$Hotel_Address == "20 Rue De La Ga t 14th arr 75014 Paris France", ncol(data)-1] <- 48.840747
data[data$Hotel_Address == "4 rue de la P pini re 8th arr 75008 Paris France", ncol(data)] <- 2.322156
data[data$Hotel_Address == "4 rue de la P pini re 8th arr 75008 Paris France", ncol(data)-1] <- 48.875382
data[data$Hotel_Address == "Gr nentorgasse 30 09 Alsergrund 1090 Vienna Austria", ncol(data)] <- 16.356793
data[data$Hotel_Address == "Gr nentorgasse 30 09 Alsergrund 1090 Vienna Austria", ncol(data)-1] <- 48.224874
data[data$Hotel_Address == "Josefst dter Stra e 10 12 08 Josefstadt 1080 Vienna Austria", ncol(data)] <- 16.353097
data[data$Hotel_Address == "Josefst dter Stra e 10 12 08 Josefstadt 1080 Vienna Austria", ncol(data)-1] <- 48.209445
data[data$Hotel_Address == "Landstra er G rtel 5 03 Landstra e 1030 Vienna Austria", ncol(data)] <- 16.393673
data[data$Hotel_Address == "Landstra er G rtel 5 03 Landstra e 1030 Vienna Austria", ncol(data)-1] <- 48.211162
data[data$Hotel_Address == "Pau Clar s 122 Eixample 08009 Barcelona Spain", ncol(data)] <- 2.170299
data[data$Hotel_Address == "Pau Clar s 122 Eixample 08009 Barcelona Spain", ncol(data)-1] <- 41.394611
data[data$Hotel_Address == "Sep lveda 180 Eixample 08011 Barcelona Spain", ncol(data)] <- 2.160687
data[data$Hotel_Address == "Sep lveda 180 Eixample 08011 Barcelona Spain", ncol(data)-1] <- 41.384845
data[data$Hotel_Address == "Taborstra e 8 A 02 Leopoldstadt 1020 Vienna Austria", ncol(data)] <- 16.414759
data[data$Hotel_Address == "Taborstra e 8 A 02 Leopoldstadt 1020 Vienna Austria", ncol(data)-1] <- 48.209254
data[data$Hotel_Address == "W hringer Stra e 33 35 09 Alsergrund 1090 Vienna Austria", ncol(data)] <- 16.356514
data[data$Hotel_Address == "W hringer Stra e 33 35 09 Alsergrund 1090 Vienna Austria", ncol(data)-1] <- 48.224946
data[data$Hotel_Address == "23 Rue Damr mont 18th arr 75018 Paris France", ncol(data)] <- 2.342705
data[data$Hotel_Address == "23 Rue Damr mont 18th arr 75018 Paris France", ncol(data)-1] <- 48.888148
data[data$Hotel_Address == "Bail n 4 6 Eixample 08010 Barcelona Spain", ncol(data)] <- 2.177693
data[data$Hotel_Address == "Bail n 4 6 Eixample 08010 Barcelona Spain", ncol(data)-1] <- 41.391972
data[data$Hotel_Address == "Hasenauerstra e 12 19 D bling 1190 Vienna Austria", ncol(data)] <- 16.345239
data[data$Hotel_Address == "Hasenauerstra e 12 19 D bling 1190 Vienna Austria", ncol(data)-1] <- 48.233705
data[data$Hotel_Address == "Josefst dter Stra e 22 08 Josefstadt 1080 Vienna Austria", ncol(data)] <- 16.351014
data[data$Hotel_Address == "Josefst dter Stra e 22 08 Josefstadt 1080 Vienna Austria", ncol(data)-1] <- 48.209659
data[data$Hotel_Address == "Paragonstra e 1 11 Simmering 1110 Vienna Austria", ncol(data)] <- 16.418367
data[data$Hotel_Address == "Paragonstra e 1 11 Simmering 1110 Vienna Austria", ncol(data)-1] <- 48.171124
data[data$Hotel_Address == "Savoyenstra e 2 16 Ottakring 1160 Vienna Austria", ncol(data)] <- 16.302941
data[data$Hotel_Address == "Savoyenstra e 2 16 Ottakring 1160 Vienna Austria", ncol(data)-1] <- 48.215164
data[data$Hotel_Address == "Sieveringer Stra e 4 19 D bling 1190 Vienna Austria", ncol(data)] <- 16.341607
data[data$Hotel_Address == "Sieveringer Stra e 4 19 D bling 1190 Vienna Austria", ncol(data)-1] <- 48.245945
data[data$Hotel_Address == "W hringer Stra e 12 09 Alsergrund 1090 Vienna Austria", ncol(data)] <- 16.359787
data[data$Hotel_Address == "W hringer Stra e 12 09 Alsergrund 1090 Vienna Austria", ncol(data)-1] <- 48.216853

#Zmoniu tautybes, kurie vertino viesbucius
data %>% select(Reviewer_Nationality) %>% group_by(Reviewer_Nationality) %>% count() %>%
  arrange(desc(n)) %>% head(10)

#vidutiniai viesbuciu ivertinimai

data%>%select(Average_Score,Hotel_Address)%>%distinct(Average_Score,Hotel_Address)%>%
  ggplot(aes(x=Average_Score))+
  geom_histogram(color='black',fill='brown', bins=30)+xlab("Average Review Score") +
  scale_x_continuous(breaks = seq(5,10,0.5))

#10 visbuciu su geriausiai ivertinimais (daugiau negu 10)
data %>% select(Hotel_Name, Reviewer_Score) %>% group_by(Hotel_Name) %>%
  mutate(Daznis = n()) %>% filter(Daznis > 10) %>%
  summarize(Vertinimas = mean(Reviewer_Score)) %>%
  arrange(desc(Vertinimas)) %>% head(10)

#10 visbuciu su prasciausiais ivertinimais (daugiau negu 10)
data %>% select(Hotel_Name, Reviewer_Score) %>% group_by(Hotel_Name) %>%
  mutate(Daznis = n()) %>% filter(Daznis > 20) %>%
  summarize(Vertinimas = mean(Reviewer_Score)) %>%
  arrange((Vertinimas))

#viesbuciai kurie turi daugiausiai ivertinimu
data %>% select(Hotel_Name, Total_Number_of_Reviews) %>% 
  arrange(desc(Total_Number_of_Reviews)) %>% distinct() %>%
  head(10)

#populiariausi lietuvoje
liet <- data %>% select(Reviewer_Nationality, Hotel_Name, Reviewer_Score) %>%  filter(trimws(Reviewer_Nationality) == "Lithuania") %>% 
  group_by(Hotel_Name) %>% summarise(Daznis = n(), Vidurkis = mean(Reviewer_Score)) %>% arrange(desc(Daznis)) %>%
  head(10)

#koki vidutini bala dave zmones is lietuvos 
data %>% select(Reviewer_Nationality, Reviewer_Score) %>%  filter(trimws(Reviewer_Nationality) == "Lithuania") %>%
  summarise(mean(Reviewer_Score))
mean(data$Reviewer_Score)

#dienos kada daugiausiai balsavo
data %>% select(Review_Date) %>% group_by(Review_Date) %>% count() %>% arrange(desc(n)) %>% head(10) %>% 
  ggplot(., aes(x = Review_Date, y = n)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


#Kiek is viso karto vertino viesbucius vertintojai is visu saliu
data %>% select(Total_Number_of_Reviews_Reviewer_Has_Given, Reviewer_Nationality) %>% group_by(Total_Number_of_Reviews_Reviewer_Has_Given) %>%
  count() %>% head(10) %>%
  ggplot(., aes(Total_Number_of_Reviews_Reviewer_Has_Given, n)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq_len(10))

#Kiek is viso karto vertino viesbucius vertintojai is Lietuvos
data %>% filter(trimws(Reviewer_Nationality) == "Lithuania") %>% select(Total_Number_of_Reviews_Reviewer_Has_Given) %>%
  group_by(Total_Number_of_Reviews_Reviewer_Has_Given) %>% 
  count() %>% head(10) %>%
  ggplot(., aes(Total_Number_of_Reviews_Reviewer_Has_Given, n)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq_len(10)) 


#daugiausia apvalgu aprases zmogus
max(data$Total_Number_of_Reviews_Reviewer_Has_Given)

#teigiamu ir neigiamu zodziu kiekio palyginimas
sum(data$Review_Total_Negative_Word_Counts)
sum(data$Review_Total_Positive_Word_Counts)
cor(data$Reviewer_Score, data$Review_Total_Negative_Word_Counts, method = c("spearman"))
cor(data$Reviewer_Score, data$Review_Total_Negative_Word_Counts)
cor(data$Reviewer_Score, data$Review_Total_Positive_Word_Counts)
cor(data$Reviewer_Score, data$Review_Total_Positive_Word_Counts, method = c("spearman"))

#koki atsiliepima dazniausiai parase vertintojas
ggplot(data = data, aes(x = data$Reviewer_Score)) +
  geom_histogram(bins = 40, fill = "palegreen4") +
  scale_x_continuous(breaks = seq(min(data$Reviewer_Score), max(data$Reviewer_Score), 0.5)) + 
  scale_y_continuous(breaks = seq(0,120000,20000)) +
  xlab("Reviewer Score") + ylab("Frequency") +
  ggtitle("Reviewer Score Frequency") +  
  theme(plot.title = element_text(hjust = 0.5))

#koki menesi parase atsiliepima
month <- sapply(data$Review_Date, function(x) as.numeric(str_split(as.character(x), "/")[[1]][1]))
month %>% as.data.frame %>% 
  ggplot(., aes(.)) +
  geom_bar(fill = "lightsalmon1") + 
  scale_x_continuous(breaks = seq_len(12)) + 
  xlab("Month") + ylab("Frequency") +
  ggtitle("Review Date Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))


#wordcloud sudarymas is lietuviu komentaru
liet <- data %>% select(Reviewer_Nationality, Positive_Review, Negative_Review) %>%
  filter(trimws(Reviewer_Nationality) == "Lithuania")
liet2 <- liet[liet$Positive_Review != "No Positive" & liet$Negative_Review != "No Negative", ] 

#teksto struktura
pos2 <- Corpus(VectorSource(liet2$Positive_Review)) #Corpus duomenu failas su metaduomenimis apie zodzius
pos2 <- tm_map(pos2, content_transformer(tolower))
pos2 <- tm_map(pos2, removeNumbers)          
pos2 <- tm_map(pos2, removeWords, stopwords("en"))  #pasalinimas tokiu zodziu kaip "a", "the", "an" ir pan.
pos2 <- tm_map(pos2, stripWhitespace) #pasalina tarpus
tdm <- TermDocumentMatrix(pos2) #isskirsto zodzius
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
wordcloud(d$word, d$freq, random.order = FALSE, rot.per = 0.3, max.words = 200, colors = brewer.pal(7, "Dark2"))

#teksto struktura
neg <- Corpus(VectorSource(liet2$Negative_Review))  #Corpus duomenu failas su metaduomenimis apie zodzius
neg <- tm_map(neg, content_transformer(tolower))
neg <- tm_map(neg, removeNumbers)          
neg <- tm_map(neg, removeWords, stopwords("en"))  #pasalinimas tokiu zodziu kaip "a", "the", "an" ir pan.
neg <- tm_map(neg, stripWhitespace) #pasalina tarpus
tdm2 <- TermDocumentMatrix(neg) #pasalina tarpus
m2 <- as.matrix(tdm2)
v2 <- sort(rowSums(m2), decreasing = TRUE)
d2 <- data.frame(word = names(v2), freq = v2)
wordcloud(d2$word, d2$freq, random.order = FALSE, rot.per = 0.3, max.words = 150, colors = brewer.pal(8, "Accent"))


#leaflet creation

zem <- data %>% select(Hotel_Name, lat, lng, Average_Score) %>% distinct()
koor <- cbind(zem$lng, zem$lat)

c <- leaflet() %>% 
  addProviderTiles('OpenStreetMap.Mapnik') %>%
  addMarkers(data = koor,
             popup = paste0("Hotel: ", zem$Hotel_Name,
                            "<br>Average score: ", zem$Average_Score),
             clusterOptions = markerClusterOptions()) #Grupiu sudarymas

c
mapshot(c, file = "~/Rplot.png") #zemelapio eksportavimas i .png 

