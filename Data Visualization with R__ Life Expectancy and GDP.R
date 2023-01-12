library(tidyverse)
library(gapminder)


data("gapminder")
View(gapminder)



# Rename the data set
life_gdp <- gapminder
life_gdp



#Import necessary librbaries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)





#What variables are included in this dataset?
colnames(life_gdp)





#What countries are included in this data?
unique(life_gdp$continent)




#What is the time frame of this dataset
range(life_gdp$Yeat)




#Create a dataframe called countries so the data for each country can be graphed seperately.
Countries <- split(life_gdp, life_gdp$country)





#Changes in life expectacny between 1952 and 2007 in all these countries
gdp_all <- ggplot(data = life_gdp, aes(x = year, y = gdpPercap, color = continent)) + labs(title = "GDP Per Capita of All Countries from 1952 to 2007") + geom_line() + theme_economist() +  scale_color_brewer(palette = "Accent")
gdp_all





##Changes in life expectacny between 1952 and 2007 in Bangladesh
gdp_time_bangladesh <- ggplot(data=Countries$Bangladesh, aes(x = year, y = gdpPercap)) + labs(title = "GDP of Bangladesh from 1952 to 2007") + geom_line(color = "red") + theme_economist()
gdp_time_bangladesh






#Changes in life expectacny between 1952 and 2007 in Zimbabwe
gdp_time_zimbabwe <- ggplot(data=Countries$Zimbabwe, aes(x = year, y = gdpPercap)) + labs(title = "GDP of Zimbabwe from 1952 to 2007") + geom_line(color = "red") + theme_economist()
gdp_time_zimbabwe







#Changes in life expectacny between 1952 and 2007 in the United States
gdp_time_usa <- ggplot(data=Countries$`United States`, aes(x = year, y = gdpPercap)) + labs(title = "GDP of the United States from 1952 to 2007") + geom_line(color = "red") + theme_economist()
gdp_time_usa






#How has GDP changed in China compared to the United States between 1952 and 2007?

us_china_df <- life_gdp %>%
  filter(country == "United States" | country == "China")
gdp_time_usa_vs_china <- ggplot(data = us_china_df, aes(x = year, y = gdpPercap, color = country)) + labs(title = "GDP of China vs. the United States") + geom_line() + theme_economist()
gdp_time_usa_vs_china







#Changes in average life expectancy from 1952 to 2007 in Bangladesh

life_time_bangladesh <- ggplot(data=Countries$Bangladesh, aes(x = year, y = lifeExp)) + labs(title = "Life Expectancy of Bangladesh from 1952 to 2007", y = "Life Expectancy (years)") + geom_line(color = "red") + theme_economist()
life_time_bangladesh








#What is the average life expectancy in these countries?

aggregate_average_life <- aggregate(lifeExp ~ country, life_gdp, mean)
aggregate_average_life









#Is there a correlation between GDP and life expectancy of a country?**

life_vs_gdp_bangladesh_smooth <- ggplot(data=Countries$Bangladesh, aes(x=lifeExp, y=gdpPercap)) + labs(title = "Life Expectancy versus GDP in Bangladesh", x = "Life Expectancy") + geom_point() + geom_smooth(method = lm) + geom_smooth(se = FALSE, color = "red") + theme_economist()
life_vs_gdp_bangladesh_smooth










bangladesh_lm <- lm(gdpPercap ~ lifeExp, data = Countries$Bangladesh)
summary(bangladesh_lm)
summary(bangladesh_lm)$r.squared





standard_res_bangladesh <- rstandard(bangladesh_lm)
bangladesh_final_data <- cbind(Countries$Bangladesh, standard_res_bangladesh)
bangladesh_final_data[order(-standard_res_bangladesh),]
plot(bangladesh_final_data$lifeExp, standard_res_bangladesh, ylab='Standardized Residuals', xlab='Life_Expectancy', abline(0, 0))




#None of the standardized residuals exceed an absolute value of 3. Therefore, outliers are not a conern for this model.



