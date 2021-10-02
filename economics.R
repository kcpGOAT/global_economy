library(tidyverse)
library(modelr)
library(ggthemes)

# CALIFORNIA UNEMPLOYMENT AND MINIMUM WAGE
dataset_CA <- tibble(
  year = c(1980:2020), 
  mw = mw_ca$STTMINWGCA[1:41], 
  unemployment = ca_unemployment$LAUST060000000000003A
)

ggplot(dataset_CA, aes(year)) +
  geom_line(aes(y = mw), color = "blue") +
  geom_line(aes(y = unemployment), color = "red")

summary(lm(unemployment ~ mw, data = dataset_CA))

ggplot(dataset_CA, aes(mw, unemployment)) +
  geom_point() +
  geom_smooth(method = "lm")

# GDP AND POPULATION (1)
population$POPTOTUSA647NWDB <- population$POPTOTUSA647NWDB / 1000000
gdp$GDPA <- gdp$GDPA / 1000

econ_data1 <- tibble(
  year = c(1960:2020), 
  gdp = gdp$GDPA, 
  population = population$POPTOTUSA647NWDB[1:61]
)

econ_model1 <- lm(gdp ~ population, data = econ_data1)
summary(econ_model1)
econ_data1 <- econ_data1 %>%
  add_predictions(econ_model1) %>%
  add_residuals(econ_model1)

ggplot(econ_data1, aes(year)) +
  geom_line(aes(y = gdp), color = "blue") +
  geom_line(aes(y = population), color = "red")

ggplot(econ_data1, aes(population, gdp)) +
  geom_point(color = "red") +
  geom_line(aes(y = pred))

ggplot(econ_data1, aes(year, resid)) +
  geom_point() +
  geom_ref_line(h = 0)

ggplot(econ_data1, aes(year, resid)) +
  geom_bar(stat = "identity") 
  
# GDP AND POPULATION (2)
econ_data2 <- econ_data1
econ_model2 <- lm(gdp ~ I(1.012 ^ population + 1), data = econ_data2)
summary(econ_model2)

econ_data2 <- econ_data2 %>%
  add_predictions(econ_model2) %>%
  add_residuals(econ_model2)

ggplot(econ_data2, aes(population, gdp)) +
  geom_point(color = "red") +
  geom_line(aes(y = pred)) +
  theme_light()

ggplot(econ_data2, aes(population, gdp)) +
  geom_point(color = "red") +
  geom_smooth(method = "loess") +
  theme_light()

ggplot(econ_data2, aes(year, resid)) +
  geom_point() +
  geom_ref_line(h = 0)

ggplot(econ_data2, aes(year, resid)) +
  geom_bar(stat = "identity") +
  theme_light()

# COMPARING ECONOMIES: 1990-2019
pwt100_modern <- pwt100[pwt100$year >= 1990, ]
pwt100_modern <- pwt100_modern[pwt100_modern$pop >= 1, ]

# Medium-sized countries, 2012
pwt_2012 <- pwt100_modern[pwt100_modern$year == 2012, ]
pwt_2012 <- pwt_2012[pwt_2012$pop < 250, ]
pwt_2012$rgdpo <- pwt_2012$rgdpo/1000
pwt_2012 <- pwt_2012[!is.na(pwt_2012$country), ]

pwt_2012_mod <- lm(rgdpo ~ pop, data = pwt_2012)
summary(pwt_2012_mod)
pwt_2012 <- pwt_2012 %>%
  add_predictions(pwt_2012_mod) %>%
  add_residuals(pwt_2012_mod)

ggplot(pwt_2012, aes(pop, rgdpo)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  theme_stata() +
  labs(x = "Population in millions", 
       y = "Real GDP in billions of 2017 US$") +
  theme(text = element_text(face = "bold"), 
        axis.title.y = element_text(vjust = 5), 
        axis.text.y = element_text(hjust = 0.5), 
        axis.title.x = element_text(vjust = -2), 
        axis.text.x = element_text(vjust = 1.1), 
        panel.grid.major = element_blank())

best_performing2012 <- pwt_2012 %>%
  arrange(desc(resid))
best_performing2012 <- head(best_performing2012, 10)
ggplot(best_performing2012, aes(reorder(country, -resid), resid)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.6) +
  theme_light() +
  scale_y_continuous(breaks = pretty(0:3000, n = 7)) +
  labs(x = NULL, 
       y = "Residual", 
       title = "Ten biggest economies among medium-sized countries in 2012 relative to GDP ~ population", 
       caption = "Source: Penn World Table version 10.0") +
  theme(panel.grid.major = element_blank())


## OTHERS

# World GDP over the years
world_gdp_byyear <- pwt100[!is.na(pwt100$rgdpo), ]
world_gdp_byyear <- world_gdp_byyear %>%
  group_by(year) %>%
  summarize(world_gdp = sum(rgdpo, na.rm = TRUE)/1000000)

ggplot(world_gdp_byyear, aes(year, world_gdp)) +
  geom_line(size = 1.25) +
  geom_area(aes(y = world_gdp), fill = "gray") +
  labs(x = "Year", 
       y = "Trillions of 2017 US$",
       title = "Real World GDP: 1950-2019", 
       caption = "Source: Penn World Table version 10.0") +
  scale_y_continuous(breaks = pretty(0:120, n = 7)) +
  theme_stata() +
  theme(axis.title.y = element_text(vjust = 6),
        axis.text.y = element_text(hjust = 0.5), 
        axis.title.x = element_text(vjust = -2), 
        plot.caption = element_text(hjust = -0.05, vjust = -4), 
        text = element_text(face = "bold"))

# Working hours and GDP - 2017
working_GDP_2017 <- pwt100[!is.na(pwt100$avh), ]
working_GDP_2017 <- working_GDP_2017[!is.na(working_GDP_2017$rgdpo), ]
working_GDP_2017 <- working_GDP_2017 %>%
  filter(year == 2017) %>%
  select(country, avh, rgdpo)

workGDP2017_model <- lm(rgdpo ~ avh, data = working_GDP_2017)
summary(workGDP2017_model)
plot(rgdpo ~ avh, data = working_GDP_2017, 
     xlab = "Average hours worked", ylab = "GDP")
abline(workGDP2017_model)

