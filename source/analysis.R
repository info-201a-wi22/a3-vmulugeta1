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

colors <- c("Latinx Jail Population" = "orange", "White Jail Population" = "purple")
jail_pop_by_race2 <- ggplot(data = specific_interest_dataset) +
  geom_line(aes(x = year, y = latinx_jail_pop, color = "Latinx Jail Population"), size = 1) + 
  geom_line(aes(x = year, y = white_jail_pop, color = "White Jail Population"), size = 1) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Jail Population of Latinx vs White People", y = "Jail Population",
       x = "Years", color = "Legend") +
  scale_color_manual(values = colors)

### **U.S. Trends Map:** Black Jail Population in Washington

library(tidyverse)
library(ggplot2)
library(maps)
library(mapproj)
library(patchwork)

counties_wa <- incarcerations_dataset %>%
  filter(year == max(year)) 


wa_shape <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by="polyname")

map_data <- wa_shape %>%
  left_join(counties_wa, by="fips") %>%
  filter(state == "WA")

blank_theme_wa <- theme_bw() + 
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )
incarcerations_black_map_wa <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop_rate), 
    color="blue", size = 0.3
  ) + 
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$black_jail_pop_rate)), na.value = "white",
                        low="blue", high="red") +
  blank_theme_wa +
  ggtitle("Black Jail Population in Washington")


#### **U.S. Trends Map:** Black Jail Population in California

counties_ca <- incarcerations_dataset %>%
  filter(year == max(year)) 


ca_shape <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by="polyname")

map_data_ca <- ca_shape %>%
  left_join(counties_ca, by="fips") %>%
  filter(state == "CA")

blank_theme_ca <- theme_bw() + 
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )
incarceration_black_map_ca <- ggplot(map_data_ca) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop_rate), 
    color="blue", size = 0.3
  ) + 
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$black_jail_pop_rate)), na.value = "white",
                        low="blue", high="red") +
  blank_theme_ca +
  ggtitle("Black Jail Population in California")


