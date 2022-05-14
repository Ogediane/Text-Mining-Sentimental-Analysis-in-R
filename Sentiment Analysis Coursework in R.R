#I will use TM package and Wordcloud package 
#to create my function for own sentiment analysis instead of using predefined functions
#this is part of my data processing steps in this implementation

#Set Working directory and verify

setwd("C:/Users/ogech/OneDrive/Desktop/ASDM Coursework - Sentiment Analysis")
getwd()

# Import the file with the selected 30 restaurants
Survey30 <- read.csv("My_30_Selected_Restaurants.csv", header = TRUE)

# Visualize the distribution of the selected restaurants
counts <- table(Surve$name)
barplot(counts, main="30 Selected Restaurants",
        xlab="Frequency", col="pink")




#install and activate packages required

#install and activate tm package

# install.packages("tm")
library(tm)

#install and activate wordcloud package

# install.packages("wordcloud")
library(wordcloud)

#Read data into a dataframe

Survey <- read.csv("tourist_accommodation_reviews.csv", header = TRUE)
View(Survey)

#Perform data exploration

names(Survey)
head(Survey)
tail(Survey)
summary(Survey)
str(Survey)
dim(Survey)

#Create subsets of overall survey data for 30 different restaurants

Sweet_and_sour <- subset(Survey, name=="Sweet and Sour")
Yorkshire <- subset(Survey, name=="Yorkshire Hotel Restaurant")
Mamma_mia <- subset(Survey, name=="Mamma Mia Grill & Restaurant Bangtao")
Karon_Cafe <- subset(Survey, name=="Karon Cafe Steakhouse & Thai Cuisine")
Mr_Coffee <- subset(Survey, name=="Mr.Coffee")
Madras_cafe <- subset(Survey, name=="Madras Cafe")
Gallery_Cafe <- subset(Survey, name=="Gallery Cafe by Pinky")
Leonardo_Davinci <- subset(Survey, name=="Leonardo Davinci")
Siam_Smile <- subset(Survey, name=="Siam Smile Wine & Restaurant")
White_Orchid <- subset(Survey,name=="White Orchid")
Home_Kitchen <- subset(Survey, name=="Home Kitchen, Bar & Bed")
Oiy_Restaurant <- subset(Survey, name=="Oiy Restaurant")
Linda_Seafood <- subset(Survey, name=="Linda Seafood") 
HQ_Beach <- subset(Survey, name=="HQ Beach Lounge")
Laem_Seafood <- subset(Survey, name=="Laem Hin Seafood")
Terrazzo_Bar <-  subset(Survey, name=="Terrazzo Ristorante Bar")
Smile_Bar <- subset(Survey, name=="Smile Bar")
Ying_Restaurant <- subset(Survey, name=="Ying Restaurant")
Sea_Almond <- subset(Survey, name=="Sea Almond Chilled Restaurant & Bar")
Bella_Vista <- subset(Survey, name=="Bella Vista Restaurant")
Kokosnuss <- subset(Survey, name=="Kokosnuss")
Karlssons_Restaurant <- subset(Survey, name=="Karlssons Restaurant Patong")
Papaya <-  subset(Survey, name=="Papaya")
Bite_in <- subset(Survey, name=="Bite in")
Southern_Fried_Rice <- subset(Survey, name=="Southern Fried Rice")
Bamboo_Bar <- subset(Survey, name=="Bamboo Bar")
Catch_Beach_Club <- subset(Survey, name=="Catch Beach Club")
China_Inn_Cafe <- subset(Survey, name=="China Inn Cafe")
Yo_Green <- subset(Survey, name=="Yo Green Restaurant")
Audy_Restaurant <- subset(Survey, name=="Audy Restaurant")

 
#Define a vector based on the Reviews column in the dataframe
#Create text vector and cleaning of data

survey_Sweet_and_sour <- Sweet_and_sour$Review
survey_Yorkshire <- Yorkshire$Review
survey_Mamma_mia <- Mamma_mia$Review
survey_Karon_Cafe <- Karon_Cafe$Review
survey_Mr_Coffee <- Mr_Coffee$Review
survey_Madras_cafe <- Madras_cafe$Review
survey_Gallery_Cafe <- Gallery_Cafe$Review
survey_Leonardo_Davinci <- Leonardo_Davinci$Review
survey_Siam_Smile <- Siam_Smile$Review
survey_White_Orchid <- White_Orchid$Review
survey_Home_Kitchen <- Home_Kitchen$Review
survey_Oiy_Restaurant <- Oiy_Restaurant$Review
survey_Linda_Seafood <- Linda_Seafood$Review
survey_HQ_Beach <- HQ_Beach$Review
survey_Laem_Seafood <- Laem_Seafood$Review
survey_Terrazzo_Bar <- Terrazzo_Bar$Review
survey_Smile_Bar <- Smile_Bar$Review
survey_Ying_Restaurant <- Ying_Restaurant$Review
survey_Sea_Almond <- Sea_Almond$Review
survey_Bella_Vista <- Bella_Vista$Review
survey_Kokosnuss <- Kokosnuss$Review
survey_Karlssons_Restaurant <- Karlssons_Restaurant$Review
survey_Papaya <- Papaya$Review
survey_Bite_in <- Bite_in$Review
survey_Southern_Fried_Rice <- Southern_Fried_Rice$Review
survey_Bamboo_Bar <- Bamboo_Bar$Review
survey_Catch_Beach_Club <- Catch_Beach_Club$Review
survey_China_Inn_Cafe <- China_Inn_Cafe$Review
survey_Yo_Green <- Yo_Green$Review
survey_Audy_Restaurant <- Audy_Restaurant$Review

#convert all to lowercase 

survey_Sweet_and_sour <- tolower(survey_Sweet_and_sour)
survey_Yorkshire <- tolower(survey_Yorkshire)
survey_Mamma_mia <- tolower(survey_Mamma_mia)
survey_Karon_Cafe <- tolower(survey_Karon_Cafe)
survey_Mr_Coffee <- tolower(survey_Mr_Coffee)
survey_Madras_cafe <- tolower(survey_Madras_cafe)
survey_Gallery_Cafe <- tolower(survey_Gallery_Cafe)
survey_Leonardo_Davinci <- tolower(survey_Leonardo_Davinci)
survey_Siam_Smile <- tolower(survey_Siam_Smile)
survey_White_Orchid <- tolower(survey_White_Orchid)
survey_Home_Kitchen <- tolower(survey_Home_Kitchen)
survey_Oiy_Restaurant <- tolower(survey_Oiy_Restaurant)
survey_Linda_Seafood <- tolower(survey_Linda_Seafood)
survey_HQ_Beach <- tolower(survey_HQ_Beach)
survey_Laem_Seafood <- tolower(survey_Laem_Seafood)
survey_Terrazzo_Bar <- tolower(survey_Terrazzo_Bar)
survey_Smile_Bar <- tolower(survey_Smile_Bar)
survey_Ying_Restaurant <- tolower(survey_Ying_Restaurant)
survey_Sea_Almond <- tolower(survey_Sea_Almond)
survey_Bella_Vista <- tolower(survey_Bella_Vista)
survey_Kokosnuss <- tolower(survey_Kokosnuss)
survey_Karlssons_Restaurant <- tolower(survey_Karlssons_Restaurant)
survey_Papaya <- tolower(survey_Papaya)
survey_Bite_in <- tolower(survey_Bite_in)
survey_Southern_Fried_Rice <- tolower(survey_Southern_Fried_Rice)
survey_Bamboo_Bar <- tolower(survey_Bamboo_Bar)
survey_Catch_Beach_Club <- tolower(survey_Catch_Beach_Club)
survey_China_Inn_Cafe <- tolower(survey_China_Inn_Cafe)
survey_Yo_Green <- tolower(survey_Yo_Green)
survey_Audy_Restaurant <- tolower(survey_Audy_Restaurant)

#continue data processing and remove links using gsub function

survey_Sweet_and_sour <- gsub("http\\s+\\s*", "",survey_Sweet_and_sour)
survey_Yorkshire <- gsub("http\\s+\\s*", "", survey_Yorkshire)
survey_Mamma_mia <- gsub("http\\s+\\s*", "", survey_Mamma_mia)
survey_Karon_Cafe <- gsub("http\\s+\\s*", "", survey_Karon_Cafe)
survey_Mr_Coffee<- gsub("http\\s+\\s*", "", survey_Mr_Coffee)
survey_Madras_cafe <- gsub("http\\s+\\s*", "", survey_Madras_cafe)
survey_Gallery_Cafe <- gsub("http\\s+\\s*", "", survey_Gallery_Cafe)
survey_Leonardo_Davinci <- gsub("http\\s+\\s*", "", survey_Leonardo_Davinci)
survey_Siam_Smile <- gsub("http\\s+\\s*", "", survey_Siam_Smile)
survey_White_Orchid <- gsub("http\\s+\\s*", "", survey_White_Orchid)
survey_Home_Kitchen <- gsub("http\\s+\\s*", "", survey_Home_Kitchen)
survey_Oiy_Restaurant <- gsub("http\\s+\\s*", "", survey_Oiy_Restaurant)
survey_Linda_Seafood <- gsub("http\\s+\\s*", "", survey_Linda_Seafood)
survey_HQ_Beach <- gsub("http\\s+\\s*", "", survey_HQ_Beach)
survey_Laem_Seafood <- gsub("http\\s+\\s*", "", survey_Laem_Seafood)
survey_Terrazzo_Bar <- gsub("http\\s+\\s*", "", survey_Terrazzo_Bar)
survey_Smile_Bar <- gsub("http\\s+\\s*", "", survey_Smile_Bar)
survey_Ying_Restaurant <- gsub("http\\s+\\s*", "", survey_Ying_Restaurant)
survey_Sea_Almond <- gsub("http\\s+\\s*", "", survey_Sea_Almond)
survey_Bella_Vista <- gsub("http\\s+\\s*", "", survey_Bella_Vista)
survey_Kokosnuss <- gsub("http\\s+\\s*", "", survey_Kokosnuss)
survey_Karlssons_Restaurant <- gsub("http\\s+\\s*", "", survey_Karlssons_Restaurant)
survey_Papaya <- gsub("http\\s+\\s*", "", survey_Papaya)
survey_Bite_in <- gsub("http\\s+\\s*", "", survey_Bite_in)
survey_Southern_Fried_Rice <- gsub("http\\s+\\s*", "", survey_Southern_Fried_Rice)
survey_Bamboo_Bar <- gsub("http\\s+\\s*", "", survey_Bamboo_Bar)
survey_Catch_Beach_Club <- gsub("http\\s+\\s*", "", survey_Catch_Beach_Club)
survey_China_Inn_Cafe <- gsub("http\\s+\\s*", "", survey_China_Inn_Cafe)
survey_Yo_Green <- gsub("http\\s+\\s*", "",survey_Yo_Green)
survey_Audy_Restaurant <- gsub("http\\s+\\s*", "", survey_Audy_Restaurant)

#Now remove punctuation

survey_Sweet_and_sour <- gsub("[[:punct:]]", "",survey_Sweet_and_sour)
survey_Yorkshire <- gsub("[[:punct:]]", "", survey_Yorkshire)
survey_Mamma_mia <- gsub("[[:punct:]]", "", survey_Mamma_mia)
survey_Karon_Cafe <- gsub("[[:punct:]]", "", survey_Karon_Cafe)
survey_Mr_Coffee<- gsub("[[:punct:]]", "", survey_Mr_Coffee)
survey_Madras_cafe <- gsub("[[:punct:]]", "", survey_Madras_cafe)
survey_Gallery_Cafe <- gsub("[[:punct:]]", "", survey_Gallery_Cafe)
survey_Leonardo_Davinci <- gsub("[[:punct:]]", "", survey_Leonardo_Davinci)
survey_Siam_Smile <- gsub("[[:punct:]]", "", survey_Siam_Smile)
survey_White_Orchid <- gsub("[[:punct:]]", "", survey_White_Orchid)
survey_Home_Kitchen <- gsub("[[:punct:]]", "", survey_Home_Kitchen)
survey_Oiy_Restaurant <- gsub("[[:punct:]]", "", survey_Oiy_Restaurant)
survey_Linda_Seafood <- gsub("[[:punct:]]", "", survey_Linda_Seafood)
survey_HQ_Beach <- gsub("[[:punct:]]", "", survey_HQ_Beach)
survey_Laem_Seafood <- gsub("[[:punct:]]", "", survey_Laem_Seafood)
survey_Terrazzo_Bar <- gsub("[[:punct:]]", "", survey_Terrazzo_Bar)
survey_Smile_Bar <- gsub("[[:punct:]]", "", survey_Smile_Bar)
survey_Ying_Restaurant <- gsub("[[:punct:]]", "", survey_Ying_Restaurant)
survey_Sea_Almond <- gsub("[[:punct:]]", "", survey_Sea_Almond)
survey_Bella_Vista <- gsub("[[:punct:]]", "", survey_Bella_Vista)
survey_Kokosnuss <- gsub("[[:punct:]]", "", survey_Kokosnuss)
survey_Karlssons_Restaurant <- gsub("[[:punct:]]", "", survey_Karlssons_Restaurant)
survey_Papaya <- gsub("[[:punct:]]", "", survey_Papaya)
survey_Bite_in <- gsub("[[:punct:]]", "", survey_Bite_in)
survey_Southern_Fried_Rice <- gsub("[[:punct:]]", "", survey_Southern_Fried_Rice)
survey_Bamboo_Bar <- gsub("[[:punct:]]", "", survey_Bamboo_Bar)
survey_Catch_Beach_Club <- gsub("[[:punct:]]", "", survey_Catch_Beach_Club)
survey_China_Inn_Cafe <- gsub("[[:punct:]]", "", survey_China_Inn_Cafe)
survey_Yo_Green <- gsub("[[:punct:]]", "",survey_Yo_Green)
survey_Audy_Restaurant <- gsub("[[:punct:]]", "", survey_Audy_Restaurant)

#Remove numbers

survey_Sweet_and_sour <- gsub("[[:digit:]]", "",survey_Sweet_and_sour)
survey_Yorkshire <- gsub("[[:digit:]]", "", survey_Yorkshire)
survey_Mamma_mia <- gsub("[[:digit:]]", "", survey_Mamma_mia)
survey_Karon_Cafe <- gsub("[[:digit:]]", "", survey_Karon_Cafe)
survey_Mr_Coffee<- gsub("[[:digit:]]", "", survey_Mr_Coffee)
survey_Madras_cafe <- gsub("[[:digit:]]", "", survey_Madras_cafe)
survey_Gallery_Cafe <- gsub("[[:digit:]]", "", survey_Gallery_Cafe)
survey_Leonardo_Davinci <- gsub("[[:digit:]]", "", survey_Leonardo_Davinci)
survey_Siam_Smile <- gsub("[[:digit:]]", "", survey_Siam_Smile)
survey_White_Orchid <- gsub("[[:digit:]]", "", survey_White_Orchid)
survey_Home_Kitchen <- gsub("[[:digit:]]", "", survey_Home_Kitchen)
survey_Oiy_Restaurant <- gsub("[[:digit:]]", "", survey_Oiy_Restaurant)
survey_Linda_Seafood <- gsub("[[:digit:]]", "", survey_Linda_Seafood)
survey_HQ_Beach <- gsub("[[:digit:]]", "", survey_HQ_Beach)
survey_Laem_Seafood <- gsub("[[:digit:]]", "", survey_Laem_Seafood)
survey_Terrazzo_Bar <- gsub("[[:digit:]]", "", survey_Terrazzo_Bar)
survey_Smile_Bar <- gsub("[[:digit:]]", "", survey_Smile_Bar)
survey_Ying_Restaurant <- gsub("[[:digit:]]", "", survey_Ying_Restaurant)
survey_Sea_Almond <- gsub("[[:digit:]]", "", survey_Sea_Almond)
survey_Bella_Vista <- gsub("[[:digit:]]", "", survey_Bella_Vista)
survey_Kokosnuss <- gsub("[[:digit:]]", "", survey_Kokosnuss)
survey_Karlssons_Restaurant <- gsub("[[:digit:]]", "", survey_Karlssons_Restaurant)
survey_Papaya <- gsub("[[:digit:]]", "", survey_Papaya)
survey_Bite_in <- gsub("[[:digit:]]", "", survey_Bite_in)
survey_Southern_Fried_Rice <- gsub("[[:digit:]]", "", survey_Southern_Fried_Rice)
survey_Bamboo_Bar <- gsub("[[:digit:]]", "", survey_Bamboo_Bar)
survey_Catch_Beach_Club <- gsub("[[:digit:]]", "", survey_Catch_Beach_Club)
survey_China_Inn_Cafe <- gsub("[[:digit:]]", "", survey_China_Inn_Cafe)
survey_Yo_Green <- gsub("[[:digit:]]", "",survey_Yo_Green)
survey_Audy_Restaurant <- gsub("[[:digit:]]", "", survey_Audy_Restaurant)

#Remove blank spaces at the beginning of the Reviews using gsub function(Regular Expressions)

survey_Sweet_and_sour <- gsub("^", "",survey_Sweet_and_sour)
survey_Yorkshire <- gsub("^", "", survey_Yorkshire)
survey_Mamma_mia <- gsub("^", "", survey_Mamma_mia)
survey_Karon_Cafe <- gsub("^", "", survey_Karon_Cafe)
survey_Mr_Coffee<- gsub("^", "", survey_Mr_Coffee)
survey_Madras_cafe <- gsub("^", "", survey_Madras_cafe)
survey_Gallery_Cafe <- gsub("^", "", survey_Gallery_Cafe)
survey_Leonardo_Davinci <- gsub("^", "", survey_Leonardo_Davinci)
survey_Siam_Smile <- gsub("^", "", survey_Siam_Smile)
survey_White_Orchid <- gsub("^", "", survey_White_Orchid)
survey_Home_Kitchen <- gsub("^", "", survey_Home_Kitchen)
survey_Oiy_Restaurant <- gsub("^", "", survey_Oiy_Restaurant)
survey_Linda_Seafood <- gsub("^", "", survey_Linda_Seafood)
survey_HQ_Beach <- gsub("^", "", survey_HQ_Beach)
survey_Laem_Seafood <- gsub("^", "", survey_Laem_Seafood)
survey_Terrazzo_Bar <- gsub("^", "", survey_Terrazzo_Bar)
survey_Smile_Bar <- gsub("^", "", survey_Smile_Bar)
survey_Ying_Restaurant <- gsub("^", "", survey_Ying_Restaurant)
survey_Sea_Almond <- gsub("^","", survey_Sea_Almond)
survey_Bella_Vista <- gsub("^", "", survey_Bella_Vista)
survey_Kokosnuss <- gsub("^", "", survey_Kokosnuss)
survey_Karlssons_Restaurant <- gsub("^", "", survey_Karlssons_Restaurant)
survey_Papaya <- gsub("^", "", survey_Papaya)
survey_Bite_in <- gsub("^", "", survey_Bite_in)
survey_Southern_Fried_Rice <- gsub("^", "", survey_Southern_Fried_Rice)
survey_Bamboo_Bar <- gsub("^", "", survey_Bamboo_Bar)
survey_Catch_Beach_Club <- gsub("^", "", survey_Catch_Beach_Club)
survey_China_Inn_Cafe <- gsub("^", "", survey_China_Inn_Cafe)
survey_Yo_Green <- gsub("^", "",survey_Yo_Green)
survey_Audy_Restaurant <- gsub("^", "", survey_Audy_Restaurant)

#Remove blank spaces at the end of the Reviews

survey_Sweet_and_sour <- gsub("$", "",survey_Sweet_and_sour)
survey_Yorkshire <- gsub("$","", survey_Yorkshire)
survey_Mamma_mia <- gsub("$", "", survey_Mamma_mia)
survey_Karon_Cafe <- gsub("$", "", survey_Karon_Cafe)
survey_Mr_Coffee<- gsub("$", "", survey_Mr_Coffee)
survey_Madras_cafe <- gsub("$", "", survey_Madras_cafe)
survey_Gallery_Cafe <- gsub("$", "", survey_Gallery_Cafe)
survey_Leonardo_Davinci <- gsub("$", "", survey_Leonardo_Davinci)
survey_Siam_Smile <- gsub("$", "", survey_Siam_Smile)
survey_White_Orchid <- gsub("$", "", survey_White_Orchid)
survey_Home_Kitchen <- gsub("$", "", survey_Home_Kitchen)
survey_Oiy_Restaurant <- gsub("$", "", survey_Oiy_Restaurant)
survey_Linda_Seafood <- gsub("$", "", survey_Linda_Seafood)
survey_HQ_Beach <- gsub("$", "", survey_HQ_Beach)
survey_Laem_Seafood <- gsub("$", "", survey_Laem_Seafood)
survey_Terrazzo_Bar <- gsub("$", "", survey_Terrazzo_Bar)
survey_Smile_Bar <- gsub("$", "", survey_Smile_Bar)
survey_Ying_Restaurant <- gsub("$", "", survey_Ying_Restaurant)
survey_Sea_Almond <- gsub("$","", survey_Sea_Almond)
survey_Bella_Vista <- gsub("$", "", survey_Bella_Vista)
survey_Kokosnuss <- gsub("$", "", survey_Kokosnuss)
survey_Karlssons_Restaurant <- gsub("$","", survey_Karlssons_Restaurant)
survey_Papaya <- gsub("$", "", survey_Papaya)
survey_Bite_in <- gsub("$", "", survey_Bite_in)
survey_Southern_Fried_Rice <- gsub("$", "", survey_Southern_Fried_Rice)
survey_Bamboo_Bar <- gsub("$", "", survey_Bamboo_Bar)
survey_Catch_Beach_Club <- gsub("$", "", survey_Catch_Beach_Club)
survey_China_Inn_Cafe <- gsub("$", "", survey_China_Inn_Cafe)
survey_Yo_Green <- gsub("$", "",survey_Yo_Green)
survey_Audy_Restaurant <- gsub("$", "", survey_Audy_Restaurant)

#Remove \n from the texts

survey_Sweet_and_sour <- gsub("[\r\n]", "",survey_Sweet_and_sour)
survey_Yorkshire <- gsub("[\r\n]","", survey_Yorkshire)
survey_Mamma_mia <- gsub("[\r\n]", "", survey_Mamma_mia)
survey_Karon_Cafe <- gsub("[\r\n]", "", survey_Karon_Cafe)
survey_Mr_Coffee<- gsub("[\r\n]", "", survey_Mr_Coffee)
survey_Madras_cafe <- gsub("[\r\n]", "", survey_Madras_cafe)
survey_Gallery_Cafe <- gsub("[\r\n]", "", survey_Gallery_Cafe)
survey_Leonardo_Davinci <- gsub("[\r\n]", "", survey_Leonardo_Davinci)
survey_Siam_Smile <- gsub("[\r\n]", "", survey_Siam_Smile)
survey_White_Orchid <- gsub("[\r\n]", "", survey_White_Orchid)
survey_Home_Kitchen <- gsub("[\r\n]", "", survey_Home_Kitchen)
survey_Oiy_Restaurant <- gsub("[\r\n]", "", survey_Oiy_Restaurant)
survey_Linda_Seafood <- gsub("[\r\n]", "", survey_Linda_Seafood)
survey_HQ_Beach <- gsub("[\r\n]", "", survey_HQ_Beach)
survey_Laem_Seafood <- gsub("[\r\n]", "", survey_Laem_Seafood)
survey_Terrazzo_Bar <- gsub("[\r\n]", "", survey_Terrazzo_Bar)
survey_Smile_Bar <- gsub("[\r\n]", "", survey_Smile_Bar)
survey_Ying_Restaurant <- gsub("[\r\n]", "", survey_Ying_Restaurant)
survey_Sea_Almond <- gsub("[\r\n]","", survey_Sea_Almond)
survey_Bella_Vista <- gsub("[\r\n]", "", survey_Bella_Vista)
survey_Kokosnuss <- gsub("[\r\n]", "", survey_Kokosnuss)
survey_Karlssons_Restaurant <- gsub("[\r\n]","", survey_Karlssons_Restaurant)
survey_Papaya <- gsub("[\r\n]", "", survey_Papaya)
survey_Bite_in <- gsub("[\r\n]", "", survey_Bite_in)
survey_Southern_Fried_Rice <- gsub("[\r\n]", "", survey_Southern_Fried_Rice)
survey_Bamboo_Bar <- gsub("[\r\n]", "", survey_Bamboo_Bar)
survey_Catch_Beach_Club <- gsub("[\r\n]", "", survey_Catch_Beach_Club)
survey_China_Inn_Cafe <- gsub("[\r\n]", "", survey_China_Inn_Cafe)
survey_Yo_Green <- gsub("[\r\n]", "",survey_Yo_Green)
survey_Audy_Restaurant <- gsub("[\r\n]", "", survey_Audy_Restaurant)

#Remove the word "Restaurant" from the Review

survey_Sweet_and_sour <- gsub("restaurant", "",survey_Sweet_and_sour)
survey_Yorkshire <- gsub("restaurant","", survey_Yorkshire)
survey_Mamma_mia <- gsub("restaurant", "", survey_Mamma_mia)
survey_Karon_Cafe <- gsub("restaurant", "", survey_Karon_Cafe)
survey_Mr_Coffee<- gsub("restaurant", "", survey_Mr_Coffee)
survey_Madras_cafe <- gsub("restaurant", "", survey_Madras_cafe)
survey_Gallery_Cafe <- gsub("restaurant", "", survey_Gallery_Cafe)
survey_Leonardo_Davinci <- gsub("restaurant", "", survey_Leonardo_Davinci)
survey_Siam_Smile <- gsub("restaurant", "", survey_Siam_Smile)
survey_White_Orchid <- gsub("restaurant", "", survey_White_Orchid)
survey_Home_Kitchen <- gsub("restaurant", "", survey_Home_Kitchen)
survey_Oiy_Restaurant <- gsub("restaurant", "", survey_Oiy_Restaurant)
survey_Linda_Seafood <- gsub("restaurant", "", survey_Linda_Seafood)
survey_HQ_Beach <- gsub("restaurant", "", survey_HQ_Beach)
survey_Laem_Seafood <- gsub("restaurant", "", survey_Laem_Seafood)
survey_Terrazzo_Bar <- gsub("restaurant", "", survey_Terrazzo_Bar)
survey_Smile_Bar <- gsub("restaurant", "", survey_Smile_Bar)
survey_Ying_Restaurant <- gsub("restaurant", "", survey_Ying_Restaurant)
survey_Sea_Almond <- gsub("restaurant","", survey_Sea_Almond)
survey_Bella_Vista <- gsub("restaurant", "", survey_Bella_Vista)
survey_Kokosnuss <- gsub("restaurant", "", survey_Kokosnuss)
survey_Karlssons_Restaurant <- gsub("restaurant","", survey_Karlssons_Restaurant)
survey_Papaya <- gsub("restaurant", "", survey_Papaya)
survey_Bite_in <- gsub("restaurant", "", survey_Bite_in)
survey_Southern_Fried_Rice <- gsub("restaurant", "", survey_Southern_Fried_Rice)
survey_Bamboo_Bar <- gsub("restaurant", "", survey_Bamboo_Bar)
survey_Catch_Beach_Club <- gsub("restaurant", "", survey_Catch_Beach_Club)
survey_China_Inn_Cafe <- gsub("restaurant", "", survey_China_Inn_Cafe)
survey_Yo_Green <- gsub("restaurant", "",survey_Yo_Green)
survey_Audy_Restaurant <- gsub("restaurant", "", survey_Audy_Restaurant)

#Remove the word "hotel" from the Review

survey_Sweet_and_sour <- gsub("hotel", "",survey_Sweet_and_sour)
survey_Yorkshire <- gsub("hotel","", survey_Yorkshire)
survey_Mamma_mia <- gsub("hotel", "", survey_Mamma_mia)
survey_Karon_Cafe <- gsub("hotel", "", survey_Karon_Cafe)
survey_Mr_Coffee<- gsub("hotel", "", survey_Mr_Coffee)
survey_Madras_cafe <- gsub("hotel", "", survey_Madras_cafe)
survey_Gallery_Cafe <- gsub("hotel", "", survey_Gallery_Cafe)
survey_Leonardo_Davinci <- gsub("hotel", "", survey_Leonardo_Davinci)
survey_Siam_Smile <- gsub("hotel", "", survey_Siam_Smile)
survey_White_Orchid <- gsub("hotel", "", survey_White_Orchid)
survey_Home_Kitchen <- gsub("hotel", "", survey_Home_Kitchen)
survey_Oiy_Restaurant <- gsub("hotel", "", survey_Oiy_Restaurant)
survey_Linda_Seafood <- gsub("hotel", "", survey_Linda_Seafood)
survey_HQ_Beach <- gsub("hotel", "", survey_HQ_Beach)
survey_Laem_Seafood <- gsub("hotel", "", survey_Laem_Seafood)
survey_Terrazzo_Bar <- gsub("hotel", "", survey_Terrazzo_Bar)
survey_Smile_Bar <- gsub("hotel", "", survey_Smile_Bar)
survey_Ying_Restaurant <- gsub("hotel", "", survey_Ying_Restaurant)
survey_Sea_Almond <- gsub("hotel","", survey_Sea_Almond)
survey_Bella_Vista <- gsub("hotel", "", survey_Bella_Vista)
survey_Kokosnuss <- gsub("hotel", "", survey_Kokosnuss)
survey_Karlssons_Restaurant <- gsub("hotel","", survey_Karlssons_Restaurant)
survey_Papaya <- gsub("hotel", "", survey_Papaya)
survey_Bite_in <- gsub("hotel", "", survey_Bite_in)
survey_Southern_Fried_Rice <- gsub("hotel", "", survey_Southern_Fried_Rice)
survey_Bamboo_Bar <- gsub("hotel", "", survey_Bamboo_Bar)
survey_Catch_Beach_Club <- gsub("hotel", "", survey_Catch_Beach_Club)
survey_China_Inn_Cafe <- gsub("hotel", "", survey_China_Inn_Cafe)
survey_Yo_Green <- gsub("hotel", "",survey_Yo_Green)
survey_Audy_Restaurant <- gsub("hotel", "", survey_Audy_Restaurant)

#Remove the word "food" from the Review

survey_Sweet_and_sour <- gsub("food", "",survey_Sweet_and_sour)
survey_Yorkshire <- gsub("food","", survey_Yorkshire)
survey_Mamma_mia <- gsub("food", "", survey_Mamma_mia)
survey_Karon_Cafe <- gsub("food", "", survey_Karon_Cafe)
survey_Mr_Coffee<- gsub("food", "", survey_Mr_Coffee)
survey_Madras_cafe <- gsub("food", "", survey_Madras_cafe)
survey_Gallery_Cafe <- gsub("food", "", survey_Gallery_Cafe)
survey_Leonardo_Davinci <- gsub("food", "", survey_Leonardo_Davinci)
survey_Siam_Smile <- gsub("food", "", survey_Siam_Smile)
survey_White_Orchid <- gsub("food", "", survey_White_Orchid)
survey_Home_Kitchen <- gsub("food", "", survey_Home_Kitchen)
survey_Oiy_Restaurant <- gsub("food", "", survey_Oiy_Restaurant)
survey_Linda_Seafood <- gsub("food", "", survey_Linda_Seafood)
survey_HQ_Beach <- gsub("food", "", survey_HQ_Beach)
survey_Laem_Seafood <- gsub("food", "", survey_Laem_Seafood)
survey_Terrazzo_Bar <- gsub("food", "", survey_Terrazzo_Bar)
survey_Smile_Bar <- gsub("food", "", survey_Smile_Bar)
survey_Ying_Restaurant <- gsub("food", "", survey_Ying_Restaurant)
survey_Sea_Almond <- gsub("food","", survey_Sea_Almond)
survey_Bella_Vista <- gsub("food", "", survey_Bella_Vista)
survey_Kokosnuss <- gsub("food", "", survey_Kokosnuss)
survey_Karlssons_Restaurant <- gsub("food","", survey_Karlssons_Restaurant)
survey_Papaya <- gsub("food", "", survey_Papaya)
survey_Bite_in <- gsub("food", "", survey_Bite_in)
survey_Southern_Fried_Rice <- gsub("food", "", survey_Southern_Fried_Rice)
survey_Bamboo_Bar <- gsub("food", "", survey_Bamboo_Bar)
survey_Catch_Beach_Club <- gsub("food", "", survey_Catch_Beach_Club)
survey_China_Inn_Cafe <- gsub("food", "", survey_China_Inn_Cafe)
survey_Yo_Green <- gsub("food", "",survey_Yo_Green)
survey_Audy_Restaurant <- gsub("food", "", survey_Audy_Restaurant)

#check 5 random vectors to ensure changes and data processing was done properly

head(survey_Gallery_Cafe)
head(survey_Ying_Restaurant)
head(survey_Bite_in)
head(survey_Siam_Smile)
head(survey_White_Orchid)



#Using the corpus function from the tm package, 
#I will create corpus from pre-processed vector(converting the text vectors to corpus)

corpus_Sweet_and_sour <- Corpus(VectorSource(survey_Sweet_and_sour 
))
corpus_Yorkshire  <- Corpus(VectorSource(survey_Yorkshire))
corpus_Mamma_mia  <- Corpus(VectorSource(survey_Mamma_mia
))
corpus_Karon_Cafe  <- Corpus(VectorSource(survey_Karon_Cafe 
))
corpus_Mr_Coffee  <- Corpus(VectorSource(survey_Mr_Coffee
))
corpus_Madras_cafe   <- Corpus(VectorSource(survey_Madras_cafe 
))
corpus_Gallery_Cafe   <- Corpus(VectorSource(survey_Gallery_Cafe 
))
corpus_Leonardo_Davinci  <- Corpus(VectorSource(survey_Leonardo_Davinci 
))
corpus_Siam_Smile   <- Corpus(VectorSource(survey_Siam_Smile 
))
corpus_White_Orchid <- Corpus(VectorSource(survey_White_Orchid 
))
corpus_Home_Kitchen  <- Corpus(VectorSource(survey_Home_Kitchen 
))
corpus_Oiy_Restaurant  <- Corpus(VectorSource(survey_Oiy_Restaurant 
))
corpus_Linda_Seafood  <- Corpus(VectorSource(survey_Linda_Seafood 
))
corpus_HQ_Beach  <- Corpus(VectorSource(survey_HQ_Beach 
))
corpus_Laem_Seafood   <- Corpus(VectorSource(survey_Laem_Seafood 
))
corpus_Terrazzo_Bar   <- Corpus(VectorSource(survey_Terrazzo_Bar 
))
corpus_Smile_Bar   <- Corpus(VectorSource(survey_Smile_Bar 
))
corpus_Ying_Restaurant  <- Corpus(VectorSource(survey_Ying_Restaurant 
))
corpus_Sea_Almond  <- Corpus(VectorSource(survey_Sea_Almond 
))
corpus_Bella_Vista  <- Corpus(VectorSource(survey_Bella_Vista 
))
corpus_Kokosnuss  <- Corpus(VectorSource(survey_Kokosnuss 
))
corpus_Karlssons_Restaurant  <- Corpus(VectorSource(survey_Karlssons_Restaurant 
))
corpus_Papaya   <- Corpus(VectorSource(survey_Papaya 
))
corpus_Bite_in  <- Corpus(VectorSource(survey_Bite_in 
))
corpus_Southern_Fried_Rice  <- Corpus(VectorSource(survey_Southern_Fried_Rice 
))
corpus_Bamboo_Bar   <- Corpus(VectorSource(survey_Bamboo_Bar 
))
corpus_Catch_Beach_Club  <- Corpus(VectorSource(survey_Catch_Beach_Club 
))
corpus_China_Inn_Cafe  <- Corpus(VectorSource(survey_China_Inn_Cafe 
))
corpus_Yo_Green   <- Corpus(VectorSource(survey_Yo_Green 
))
corpus_Audy_Restaurant <- Corpus(VectorSource(survey_Audy_Restaurant 
))

#Inspect 5 random corpus
corpus_Home_Kitchen
corpus_Oiy_Restaurant
corpus_Mamma_mia
corpus_Siam_Smile
corpus_Yo_Green

#Transformation and cleaning up of corpus 
#Clean up corpus by removing stop words, whitespace and stem documents

install.packages("SnowballC")
library(SnowballC)

corpus_Sweet_and_sour <- tm_map(corpus_Sweet_and_sour, removeWords,stopwords("english"))
corpus_Sweet_and_sour <- tm_map(corpus_Sweet_and_sour, stripWhitespace)
stem_corpus_Sweet_and_sour  <- tm_map(corpus_Sweet_and_sour, stemDocument)

corpus_Yorkshire  <- tm_map(corpus_Yorkshire, removeWords,stopwords("english"))
corpus_Yorkshire  <- tm_map(corpus_Yorkshire, stripWhitespace)
stem_corpus_Yorkshire    <- tm_map(corpus_Yorkshire, stemDocument)

corpus_Mamma_mia  <- tm_map(corpus_Mamma_mia, removeWords,stopwords("english"))
corpus_Mamma_mia  <- tm_map(corpus_Mamma_mia, stripWhitespace)
stem_corpus_Mamma_mia    <- tm_map(corpus_Mamma_mia, stemDocument)

corpus_Karon_Cafe  <- tm_map(corpus_Karon_Cafe, removeWords,stopwords("english"))
corpus_Karon_Cafe  <- tm_map(corpus_Karon_Cafe, stripWhitespace)
stem_corpus_Karon_Cafe    <- tm_map(corpus_Karon_Cafe, stemDocument)

corpus_Mr_Coffee  <- tm_map(corpus_Mr_Coffee, removeWords,stopwords("english"))
corpus_Mr_Coffee  <- tm_map(corpus_Mr_Coffee, stripWhitespace)
stem_corpus_Mr_Coffee    <- tm_map(corpus_Mr_Coffee, stemDocument)

corpus_Madras_cafe   <- tm_map(corpus_Madras_cafe, removeWords,stopwords("english"))
corpus_Madras_cafe   <- tm_map(corpus_Madras_cafe, stripWhitespace)
stem_corpus_Madras_cafe     <- tm_map(corpus_Madras_cafe, stemDocument)

corpus_Gallery_Cafe   <- tm_map(corpus_Gallery_Cafe, removeWords,stopwords("english"))
corpus_Gallery_Cafe   <- tm_map(corpus_Gallery_Cafe, stripWhitespace)
stem_corpus_Gallery_Cafe     <- tm_map(corpus_Gallery_Cafe, stemDocument)

corpus_Leonardo_Davinci <- tm_map(corpus_Leonardo_Davinci, removeWords,stopwords("english"))
corpus_Leonardo_Davinci <- tm_map(corpus_Leonardo_Davinci, stripWhitespace)
stem_corpus_Leonardo_Davinci   <- tm_map(corpus_Leonardo_Davinci, stemDocument)

corpus_Siam_Smile   <- tm_map(corpus_Siam_Smile, removeWords,stopwords("english"))
corpus_Siam_Smile   <- tm_map(corpus_Siam_Smile, stripWhitespace)
stem_corpus_Siam_Smile     <- tm_map(corpus_Siam_Smile, stemDocument)

corpus_White_Orchid <- tm_map(corpus_White_Orchid, removeWords,stopwords("english"))
corpus_White_Orchid <- tm_map(corpus_White_Orchid, stripWhitespace)
stem_corpus_White_Orchid   <- tm_map(corpus_White_Orchid, stemDocument)

corpus_Home_Kitchen  <- tm_map(corpus_Home_Kitchen, removeWords,stopwords("english"))
corpus_Home_Kitchen  <- tm_map(corpus_Home_Kitchen, stripWhitespace)
stem_corpus_Home_Kitchen    <- tm_map(corpus_Home_Kitchen, stemDocument)

corpus_Oiy_Restaurant   <- tm_map(corpus_Oiy_Restaurant, removeWords,stopwords("english"))
corpus_Oiy_Restaurant   <- tm_map(corpus_Oiy_Restaurant, stripWhitespace)
stem_corpus_Oiy_Restaurant     <- tm_map(corpus_Oiy_Restaurant, stemDocument)

corpus_Linda_Seafood  <- tm_map(corpus_Linda_Seafood, removeWords,stopwords("english"))
corpus_Linda_Seafood  <- tm_map(corpus_Linda_Seafood, stripWhitespace)
stem_corpus_Linda_Seafood    <- tm_map(corpus_Linda_Seafood, stemDocument)

corpus_HQ_Beach  <- tm_map(corpus_HQ_Beach, removeWords,stopwords("english"))
corpus_HQ_Beach  <- tm_map(corpus_HQ_Beach, stripWhitespace)
stem_corpus_HQ_Beach    <- tm_map(corpus_HQ_Beach, stemDocument)

corpus_Laem_Seafood   <- tm_map(corpus_Laem_Seafood, removeWords,stopwords("english"))
corpus_Laem_Seafood   <- tm_map(corpus_Laem_Seafood, stripWhitespace)
stem_corpus_Laem_Seafood     <- tm_map(corpus_Laem_Seafood, stemDocument)

corpus_Terrazzo_Bar   <- tm_map(corpus_Terrazzo_Bar, removeWords,stopwords("english"))
corpus_Terrazzo_Bar   <- tm_map(corpus_Terrazzo_Bar, stripWhitespace)
stem_corpus_Terrazzo_Bar     <- tm_map(corpus_Terrazzo_Bar, stemDocument)

corpus_Smile_Bar  <- tm_map(corpus_Smile_Bar, removeWords,stopwords("english"))
corpus_Smile_Bar  <- tm_map(corpus_Smile_Bar, stripWhitespace)
stem_corpus_Smile_Bar    <- tm_map(corpus_Smile_Bar, stemDocument)

corpus_Ying_Restaurant  <- tm_map(corpus_Ying_Restaurant, removeWords,stopwords("english"))
corpus_Ying_Restaurant  <- tm_map(corpus_Ying_Restaurant, stripWhitespace)
stem_corpus_Ying_Restaurant    <- tm_map(corpus_Ying_Restaurant, stemDocument)

corpus_Sea_Almond  <- tm_map(corpus_Sea_Almond, removeWords,stopwords("english"))
corpus_Sea_Almond  <- tm_map(corpus_Sea_Almond, stripWhitespace)
stem_corpus_Sea_Almond    <- tm_map(corpus_Sea_Almond, stemDocument)

corpus_Bella_Vista <- tm_map(corpus_Bella_Vista, removeWords,stopwords("english"))
corpus_Bella_Vista <- tm_map(corpus_Bella_Vista, stripWhitespace)
stem_corpus_Bella_Vista   <- tm_map(corpus_Bella_Vista, stemDocument)

corpus_Kokosnuss  <- tm_map(corpus_Kokosnuss, removeWords,stopwords("english"))
corpus_Kokosnuss  <- tm_map(corpus_Kokosnuss, stripWhitespace)
stem_corpus_Kokosnuss    <- tm_map(corpus_Kokosnuss, stemDocument)

corpus_Karlssons_Restaurant <- tm_map(corpus_Karlssons_Restaurant, removeWords,stopwords("english"))
corpus_Karlssons_Restaurant <- tm_map(corpus_Karlssons_Restaurant, stripWhitespace)
stem_corpus_Karlssons_Restaurant   <- tm_map(corpus_Karlssons_Restaurant, stemDocument)

corpus_Papaya   <- tm_map(corpus_Papaya, removeWords,stopwords("english"))
corpus_Papaya   <- tm_map(corpus_Papaya, stripWhitespace)
stem_corpus_Papaya     <- tm_map(corpus_Papaya, stemDocument)

corpus_Bite_in  <- tm_map(corpus_Bite_in, removeWords,stopwords("english"))
corpus_Bite_in  <- tm_map(corpus_Bite_in, stripWhitespace)
stem_corpus_Bite_in    <- tm_map(corpus_Bite_in, stemDocument)

corpus_Southern_Fried_Rice  <- tm_map(corpus_Southern_Fried_Rice, removeWords,stopwords("english"))
corpus_Southern_Fried_Rice  <- tm_map(corpus_Southern_Fried_Rice, stripWhitespace)
stem_corpus_Southern_Fried_Rice   <- tm_map(corpus_Southern_Fried_Rice, stemDocument)

corpus_Bamboo_Bar   <- tm_map(corpus_Bamboo_Bar, removeWords,stopwords("english"))
corpus_Bamboo_Bar   <- tm_map(corpus_Bamboo_Bar, stripWhitespace)
stem_corpus_Bamboo_Bar     <- tm_map(corpus_Bamboo_Bar, stemDocument)

corpus_Catch_Beach_Club  <- tm_map(corpus_Catch_Beach_Club, removeWords,stopwords("english"))
corpus_Catch_Beach_Club  <- tm_map(corpus_Catch_Beach_Club, stripWhitespace)
stem_corpus_Catch_Beach_Club    <- tm_map(corpus_Catch_Beach_Club, stemDocument)

corpus_China_Inn_Cafe  <- tm_map(corpus_China_Inn_Cafe, removeWords,stopwords("english"))
corpus_China_Inn_Cafe  <- tm_map(corpus_China_Inn_Cafe, stripWhitespace)
stem_corpus_China_Inn_Cafe    <- tm_map(corpus_China_Inn_Cafe, stemDocument)

corpus_Yo_Green  <- tm_map(corpus_Yo_Green, removeWords,stopwords("english"))
corpus_Yo_Green  <- tm_map(corpus_Yo_Green, stripWhitespace)
stem_corpus_Yo_Green    <- tm_map(corpus_Yo_Green, stemDocument)

corpus_Audy_Restaurant <- tm_map(corpus_Audy_Restaurant, removeWords,stopwords("english"))
corpus_Audy_Restaurant <- tm_map(corpus_Audy_Restaurant, stripWhitespace)
stem_corpus_Audy_Restaurant   <- tm_map(corpus_Audy_Restaurant, stemDocument)
 
#After the pre-processing, load positive and negative lexicon data

positive_lexicon <- read.csv("positive-lexicon.txt", header = F)
negative_lexicon <- read.csv("negative-lexicon.txt", header = F)

#Data exploration on the newly created dataframes

head(positive_lexicon)
tail(positive_lexicon)


head(negative_lexicon)
tail(negative_lexicon)

#Create function for sentiment analysis
#generate Wordclouds
#define the variable based on the length of the stemmed corpus

sentiment <- function(stem_corpus)
{
  wordcloud(stem_corpus,
            min.freq = 3,
            colors=brewer.pal(8, "Dark2"),
            random.color = TRUE,
            max.words = 100)
  
  total_pos_count <- 0
  total_neg_count <- 0
  pos_count_vector <- c()
  neg_count_vector <- c()
  
  size <- length(stem_corpus)
  
  for(i in 1:size)
  {
    corpus_words <- list(strsplit(stem_corpus[[i]]$content, split=" "))
    
    pos_count <- length(intersect(unlist(corpus_words), unlist(positive_lexicon)))
  
    neg_count <- length(intersect(unlist(corpus_words), unlist(negative_lexicon)))
    
    total_pos_count <- total_pos_count + pos_count
    total_neg_count <- total_neg_count + neg_count
    
  
  }
  
  total_count <- total_pos_count + total_neg_count
  pos_percent <- (total_pos_count/total_count)*100
  neg_percent <- (total_neg_count/total_count)*100
  
  df <- data.frame(Review_words=c("Positive", "Negative"),
                   Count=c(total_pos_count, total_neg_count))
  
  print(df)
  
  overall_positive_percentage <- paste("Percentage of Positive Surveys:",
                                       round(pos_percent,2), "%")
  
  return(overall_positive_percentage)
}

sentiment(stem_corpus_Sweet_and_sour)

sentiment(stem_corpus_Yorkshire)

sentiment(stem_corpus_Mamma_mia)

sentiment(stem_corpus_Karon_Cafe)

sentiment(stem_corpus_Mr_Coffee)

sentiment(stem_corpus_Madras_cafe)

sentiment(stem_corpus_Gallery_Cafe)

sentiment(stem_corpus_Leonardo_Davinci)

sentiment(stem_corpus_Siam_Smile)

sentiment(stem_corpus_White_Orchid)

sentiment(stem_corpus_Home_Kitchen)

sentiment(stem_corpus_Oiy_Restaurant)

sentiment(stem_corpus_Linda_Seafood)

sentiment(stem_corpus_HQ_Beach)

sentiment(stem_corpus_Laem_Seafood)

sentiment(stem_corpus_Terrazzo_Bar)

sentiment(stem_corpus_Smile_Bar)

sentiment(stem_corpus_Ying_Restaurant)

sentiment(stem_corpus_Sea_Almond)

sentiment(stem_corpus_Bella_Vista)

sentiment(stem_corpus_Kokosnuss)

sentiment(stem_corpus_Karlssons_Restaurant)

sentiment(stem_corpus_Papaya)

sentiment(stem_corpus_Bite_in)

sentiment(stem_corpus_Southern_Fried_Rice)

sentiment(stem_corpus_Bamboo_Bar)

sentiment(stem_corpus_Catch_Beach_Club)

sentiment(stem_corpus_China_Inn_Cafe)

sentiment(stem_corpus_Yo_Green)

sentiment(stem_corpus_Audy_Restaurant)




