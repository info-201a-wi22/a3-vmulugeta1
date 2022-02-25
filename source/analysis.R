library(tidyverse)

incarcerations_dataset <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## **Five Values of Interest**

### 1. *What is the ratio of black to white jail populations? What is the ratio of latinx to white jail populations?* 
specific_interest_dataset <- incarcerations_dataset %>%
  summarize(state, year, county_name, total_pop, black_jail_pop, white_jail_pop, 
            aapi_jail_pop, latinx_jail_pop, other_race_jail_pop,total_jail_pop)

### California Ratio of Black/White Jail Population
prop_ca_black_white <- specific_interest_dataset %>%
  filter(state == "CA") %>%
  filter(year == max(year)) %>%
  mutate(black_white_prop_ca = black_jail_pop / white_jail_pop) %>%
  filter(black_white_prop_ca == max(black_white_prop_ca, na.rm = TRUE)) %>%
  pull(black_white_prop_ca)

prop_ca_black_white2 <- specific_interest_dataset %>%
  filter(state == "CA") %>%
  mutate(black_white_prop_ca = black_jail_pop / white_jail_pop) %>%
  filter(black_white_prop_ca == max(black_white_prop_ca, na.rm = TRUE)) %>%
  pull(black_white_prop_ca)

### California Ratio of Latinx/White Jail Population 
prop_ca_latinx_white <- specific_interest_dataset %>%
  filter(state == "CA") %>%
  filter(year == max(year)) %>%
  mutate(latinx_white_prop_ca = latinx_jail_pop / white_jail_pop) %>%
  filter(latinx_white_prop_ca == max(latinx_white_prop_ca, na.rm = TRUE)) %>%
  pull(latinx_white_prop_ca)

prop_ca_latinx_white2 <- specific_interest_dataset %>%
  filter(state == "CA") %>%
  mutate(latinx_white_prop_ca = latinx_jail_pop / white_jail_pop) %>%
  filter(latinx_white_prop_ca == max(latinx_white_prop_ca, na.rm = TRUE)) %>%
  pull(latinx_white_prop_ca)

## Oregon Ratio of Black/White Jail Population
prop_or_black_white <- specific_interest_dataset %>%
  filter(state == "OR") %>%
  filter(year == max(year)) %>%
  mutate(black_white_prop_or = black_jail_pop / white_jail_pop) %>%
  filter(black_white_prop_or == max(black_white_prop_or, na.rm = TRUE)) %>%
  pull(black_white_prop_or)

prop_or_black_white2 <- specific_interest_dataset %>%
  filter(state == "OR") %>%
  mutate(black_white_prop_or = black_jail_pop / white_jail_pop) %>%
  filter(black_white_prop_or == max(black_white_prop_or, na.rm = TRUE)) %>%
  pull(black_white_prop_or)

## Oregon Ratio of Latinx/White Jail Population
prop_or_latinx_white <- specific_interest_dataset %>%
  filter(state == "OR") %>%
  filter(year == max(year)) %>%
  mutate(latinx_white_prop_or = latinx_jail_pop / white_jail_pop) %>%
  filter(latinx_white_prop_or == max(latinx_white_prop_or, na.rm = TRUE)) %>%
  pull(latinx_white_prop_or)

prop_or_latinx_white2 <- specific_interest_dataset %>%
  filter(state == "OR") %>%
  mutate(latinx_white_prop_or = latinx_jail_pop / white_jail_pop) %>%
  filter(latinx_white_prop_or == max(latinx_white_prop_or, na.rm = TRUE)) %>%
  pull(latinx_white_prop_or)

## **Insights**

After computing this data for both **Oregon** and **California**, it was 
unsurprising to see that for the most recent dates of incarcerations, which was 
in *2018*, the proportion of black/white and latinx/white in California did not
have a huge gap, (3.4 v.s. 19.8), this is not shocking because latinx and black
populations are amongst the most profiled races by law enforcement, and face
much harsher punishment for crimes due to this. However, I was shocked at how 
much higher the proportion was for latinx/white, which had a propotion over 5 
times higher than the black/white. The gap for the overall ratio is much smaller, 
with the `prop_ca_black_white` being roughly 2.8 and `prop_ca_latinx_white` being
roughly 5.3. It is unfortuante to see that the level of incarceration has increased
over the years, as it is evident that criminal justice reform is necessary. On
the other hand, when looking at Oregon, for *2018* incarcerations, there was not
a huge gap between the proportion of black/white and latinx/white, (0.81 v.s. 2),
this was not shocking to me since Oregon is much smaller in population in comparison
to California, therefore having less of a black and latinx community. It was relieving
to see that there was not a huge increase in incarceration when compared to the overall
proportion for Oregon, (0.81 VS. 1.04). This also raises the question on if racial disparities
in the justice system are more present in highly populated areas of people of color. 

### 2. *What is the proportion of BIPOC in Los Angeles County jails in 2010? What is the proportion of BIPOC in King County jails in 2010?*

los_angeles_county_BIPOC_white <- specific_interest_dataset %>%
  filter(county_name == "Los Angeles County") %>%
  filter(state == "CA") %>%
  filter(year == "2010") %>%
  mutate(white_minus_total = (total_jail_pop - white_jail_pop)) %>%
  mutate(prop_BIPOC_total = (white_minus_total / total_jail_pop))

king_county_BIPOC_white <- specific_interest_dataset %>%
  filter(county_name == "King County") %>%
  filter(state == "WA") %>%
  filter(year == "2010") %>%
  mutate(white_minus_total = (total_jail_pop - white_jail_pop)) %>%
  mutate(prop_BIPOC_total = (white_minus_total / total_jail_pop))

### 3. *What county has the highest population of black prisoners? What county has the highest population of black males in jail? What county has the highest population of black females in jail?*

### Highest Black Jail Population
black_highest <- incarcerations_dataset %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  pull(black_jail_pop)

black_highest_county <- incarcerations_dataset %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)

black_male_prison_number <- incarcerations_dataset %>%
  filter(black_male_prison_adm == max(black_male_prison_adm, na.rm = TRUE)) %>%
  pull(black_male_prison_adm)

black_male_prison_location <- incarcerations_dataset %>%
  filter(black_male_prison_adm == max(black_male_prison_adm, na.rm = TRUE)) %>%
  pull(county_name)

black_female_prison_number <- incarcerations_dataset %>%
  filter(black_female_prison_adm == max(black_female_prison_adm, na.rm = TRUE)) %>%
  pull(black_female_prison_adm)

black_female_prison_location <- incarcerations_dataset %>%
  filter(black_female_prison_adm == max(black_female_prison_adm, na.rm = TRUE)) %>%
  pull(county_name)

## 4. *Where is the highest amount of males incarcerated? Where is the highest amount of females incarcerated? What are these numbers?* 

max_male_adult_jail_pop <- incarcerations_dataset %>%
  filter(male_adult_jail_pop == max(male_adult_jail_pop, na.rm = TRUE)) %>%
  pull(state)

max_female_adult_jail_pop <- incarcerations_dataset %>%
  filter(female_adult_jail_pop == max(female_adult_jail_pop, na.rm = TRUE)) %>%
  pull(state)

max_male_adult_jail_pop_number <- incarcerations_dataset %>%
  filter(male_adult_jail_pop == max(male_adult_jail_pop, na.rm = TRUE)) %>%
  pull(male_adult_jail_pop)

max_female_adult_jail_pop_number <- incarcerations_dataset %>%
  filter(female_adult_jail_pop == max(female_adult_jail_pop, na.rm = TRUE)) %>%
  pull(female_adult_jail_pop)



## **Graph #1** _Black Jail Incarcerations by Year_


library(ggplot2)
library(tidyverse)
black_by_year <- incarcerations_dataset %>%
  group_by(year) %>%
  summarize(black_jail_population = sum(black_jail_pop, na.rm = TRUE))

black_bar_chart3 <- ggplot(data = black_by_year) +
  geom_col(mapping = aes(x = year, y = black_jail_population, fill = year)) +
  labs(x = "Year", y = "Black Population", title = "Black Jail Incarceration Rates by the Year") +
  scale_fill_gradient(low = "#0000FF", high = "#FF0000")
coord_flip()


## **Graph #2** _Incarceration Rates of BIPOC in Comparison to White_

colors <- c("White Jail Population" = "lightblue", "Black Jail Population" = "pink")
jail_pop_by_race <- ggplot(data = specific_interest_dataset) +
  geom_line(aes(x = year, y = white_jail_pop, color = "White Jail Population"), size = 1) + 
  geom_line(aes(x = year, y = black_jail_pop, color = "Black Jail Population"), size = 1) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Jail Population of Black People vs White People", y = "Jail Population",
       x = "Years", color = "Legend") +
  scale_color_manual(values = colors)

colors <- c("Latinx Jail Population" = "orange", "White Jail Population" = "darkpurple")
jail_pop_by_race2 <- ggplot(data = specific_interest_dataset) +
  geom_line(aes(x = year, y = latinx_jail_pop, color = "Latinx Jail Population"), size = 1) + 
  geom_line(aes(x = year, y = white_jail_pop, color = "White Jail Population"), size = 1) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Jail Population of Latinx vs White People", y = "Jail Population",
       x = "Years", color = "Legend") +
  scale_color_manual(values = colors)



