## Load Libraries

```{r}
library(readr)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
```

## Load Data

```{r}
jan23 <- read_csv("/Users/aaronrice/Downloads/DS495/24 Class/Mass Data/2023/jan.csv")
feb23 <- read_csv("/Users/aaronrice/Downloads/DS495/24 Class/Mass Data/2023/feb.csv")
mar23 <- read_csv("/Users/aaronrice/Downloads/DS495/24 Class/Mass Data/2023/mar.csv")
apr23 <- read_csv("/Users/aaronrice/Downloads/DS495/24 Class/Mass Data/2023/apr.csv")
may23 <- read_csv("/Users/aaronrice/Downloads/DS495/24 Class/Mass Data/2023/may.csv")
jun23 <- read_csv("/Users/aaronrice/Downloads/DS495/24 Class/Mass Data/2023/jun.csv")
jul23 <- read_csv("/Users/aaronrice/Downloads/DS495/24 Class/Mass Data/2023/jul.csv")
aug23 <- read_csv("/Users/aaronrice/Downloads/DS495/24 Class/Mass Data/2023/aug.csv")
sep23 <- read_csv("/Users/aaronrice/Downloads/DS495/24 Class/Mass Data/2023/sep.csv")
oct23 <- read_csv("/Users/aaronrice/Downloads/DS495/24 Class/Mass Data/2023/oct.csv")
nov23 <- read_csv("/Users/aaronrice/Downloads/DS495/24 Class/Mass Data/2023/nov.csv")
dec23 <- read_csv("/Users/aaronrice/Downloads/DS495/24 Class/Mass Data/2023/dec.csv")
```

### Combine Data into Single Dataframes

```{r}
df23 <- bind_rows(jan23, feb23, mar23, apr23, may23, jun23, jul23, aug23, sep23, oct23, nov23, dec23)
```

### Load Contents Table

```{r}
airline_id <- read.csv("/Users/aaronrice/Downloads/DS495/24 Class/Mass Data/Contents/L_AIRLINE_ID.csv")
```

### Change character type in dataframe columns

```{r}
df23$OP_CARRIER_AIRLINE_ID <- as.character(df23$OP_CARRIER_AIRLINE_ID)
airline_id$Code <- as.character(airline_id$Code)
```

### Replace airline code with name of the airline & add a month name & weekday column

```{r}
df23 <- df23 %>%
  left_join(airline_id, by = c("OP_CARRIER_AIRLINE_ID" = "Code")) %>%
  mutate(OP_CARRIER_AIRLINE_ID = ifelse(is.na(Description), category, Description)) %>%
  select(-Description)

df23 <- df23 %>% 
  mutate(MONTH_NAME = case_when(
    MONTH == 1 ~ "january",
    MONTH == 2 ~ "february",
    MONTH == 3 ~ "march",
    MONTH == 4 ~ "april",
    MONTH == 5 ~ "may",
    MONTH == 6 ~ "june",
    MONTH == 7 ~ "july",
    MONTH == 8 ~ "august",
    MONTH == 9 ~ "september",
    MONTH == 10 ~ "october",
    MONTH == 11 ~ "november",
    MONTH == 12 ~ "december",
  ))

df23 <- df23 %>% 
  mutate(DAY_WEEK_NAME = case_when(
    DAY_OF_WEEK == 1 ~ "monday",
    DAY_OF_WEEK == 2 ~ "tuesday",
    DAY_OF_WEEK == 3 ~ "wednesday",
    DAY_OF_WEEK == 4 ~ "thursday",
    DAY_OF_WEEK == 5 ~ "friday",
    DAY_OF_WEEK == 6 ~ "saturday",
    DAY_OF_WEEK == 7 ~ "sunday",
    DAY_OF_WEEK == 9 ~ "unknown",
  ))
```

##### Check what airlines are included in the dataset \< data exploration

```{r}
sort(unique(df23$OP_CARRIER_AIRLINE_ID))
```

### Sort data into separate dataframes based on airline and separate oceanic and interisland flights

```{r}
alaska <- df23[df23$OP_CARRIER_AIRLINE_ID == "Alaska Airlines Inc.: AS", ]

american <- df23[df23$OP_CARRIER_AIRLINE_ID == "American Airlines Inc.: AA", ]

delta <- df23[df23$OP_CARRIER_AIRLINE_ID == "Delta Airlines Inc.: DL", ]

hawaiian <- df23[df23$OP_CARRIER_AIRLINE_ID == "Hawaiian Airlines Inc.: HA", ]

southwest <- df23[df23$OP_CARRIER_AIRLINE_ID == "Southwest Airlines Inc.: WN", ]

united <- df23[df23$OP_CARRIER_AIRLINE_ID == "United Airlines Inc.: UA", ]

oceanic_flights_from_hawaii <- df23[df23$ORIGIN_STATE_ABR == "HI" & df23$DEST_STATE_ABR != "HI", ]

interisland_flights <- df23[df23$ORIGIN_STATE_ABR == "HI" & df23$DEST_STATE_ABR == "HI", ]
```

```{r}
ggplot(df23, aes(x = MONTH_NAME, y = DEP_DELAY, color = OP_CARRIER_AIRLINE_ID)) +
  geom_violin() +
  labs(title = "Total Monthly Departure Delays (minutes) by Airline",
       x = "Month",
       y = "Delay (minutes)")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

#### Create dataframes calculating the delay means grouped by airline for different circumstances such as each month, day of week, interisland and oceanic, and types of delays based on provided data

```{r}
day_mean_delays <- df23 %>%
  group_by(DAY_WEEK_NAME, OP_CARRIER_AIRLINE_ID) %>%
  summarize(mean_delay = mean(DEP_DELAY_NEW, na.rm = TRUE), .groups = "drop")

month_mean_delays <- df23 %>%
  group_by(MONTH_NAME, OP_CARRIER_AIRLINE_ID) %>%
  summarize(mean_delay = mean(DEP_DELAY_NEW, na.rm = TRUE), .groups = "drop")

year_mean_delays <- df23 %>%
  group_by(OP_CARRIER_AIRLINE_ID) %>%
  summarize(mean_delay = mean(DEP_DELAY_NEW, na.rm = TRUE), .groups = "drop")

oceanic_delays <- oceanic_flights_from_hawaii %>% 
  group_by(OP_CARRIER_AIRLINE_ID) %>% 
  summarize(mean_delay = mean(DEP_DELAY_NEW, na.rm = TRUE), .groups = "drop")

interisland_delays <- interisland_flights %>% 
  group_by(OP_CARRIER_AIRLINE_ID) %>% 
  summarize(mean_delay = mean(DEP_DELAY_NEW, na.rm = TRUE), .groups = "drop")


            
types_of_delay_averages <- df23 %>%
  group_by(OP_CARRIER_AIRLINE_ID) %>%
  summarize(
    CARRIER_DELAY = mean(CARRIER_DELAY, na.rm = TRUE, .groups = "drop"),
    WEATHER_DELAY = mean(WEATHER_DELAY, na.rm = TRUE, .groups = "drop"),
    NAS_DELAY = mean(NAS_DELAY, na.rm = TRUE, .groups = "drop"),
    SECURITY_DELAY = mean(SECURITY_DELAY, na.rm = TRUE, .groups = "drop"),
    LATE_AIRCRAFT_DELAY = mean(LATE_AIRCRAFT_DELAY, na.rm = TRUE, .groups = "drop")
)
```

#### change days/months to factors to reorder in plots

```{r}
day_mean_delays$DAY_WEEK_NAME <- factor(day_mean_delays$DAY_WEEK_NAME, 
                         levels = c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"))

month_mean_delays$MONTH_NAME <- factor(month_mean_delays$MONTH_NAME, 
                         levels = c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"))
```

#### Create different ggplots and learn about the data visually

```{r}
ggplot(day_mean_delays, aes(x = DAY_WEEK_NAME, y = mean_delay, fill = OP_CARRIER_AIRLINE_ID)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Day of Week Departure Delays by Airline",
       x = "Day of the Week",
       y = "Mean Delay (minutes)",
       fill = "Airline") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```{r}
ggplot(month_mean_delays, aes(x = MONTH_NAME, y = mean_delay, fill = OP_CARRIER_AIRLINE_ID, group = OP_CARRIER_AIRLINE_ID)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Monthly Departure Delays by Airline",
       x = "Month",
       y = "Mean Delay (minutes)",
       color = "Airline") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(month_mean_delays, aes(x = MONTH_NAME, y = OP_CARRIER_AIRLINE_ID, fill = mean_delay)) +
  geom_tile() +
  labs(title = "Average Delay Heatmap by Month and Airline (2023)",
       x = "Month",
       y = "Airline",
       fill = "Minutes") +
  scale_fill_gradient(low = "lightblue", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

test

```{r}
ggplot(year_mean_delays, aes(x = mean_delay, y = OP_CARRIER_AIRLINE_ID)) + 
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  scale_y_discrete(limits = rev( year_mean_delays$OP_CARRIER_AIRLINE_ID)) +  
  labs(title = "Average Delay per Airline (2023)", 
       x = "Average Delay (Minutes)", 
       y = "Airline") +
  theme_minimal()

ggplot(interisland_delays, aes(x = mean_delay, y = OP_CARRIER_AIRLINE_ID)) + 
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  scale_y_discrete(limits = rev( interisland_delays$OP_CARRIER_AIRLINE_ID)) +  
  labs(title = "Interisland Delays (2023)", 
       x = "Average Delay (Minutes)", 
       y = "Airline") +
  theme_minimal()

ggplot(oceanic_delays, aes(x = mean_delay, y = OP_CARRIER_AIRLINE_ID)) + 
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  scale_y_discrete(limits = rev( oceanic_delays$OP_CARRIER_AIRLINE_ID)) +  
  labs(title = "Oceanic Delays per Airline (2023)", 
       x = "Average Delay (Minutes)", 
       y = "Airline") +
  theme_minimal()
```

```{r}
ggplot(day_mean_delays, aes(x = DAY_WEEK_NAME, y = mean_delay, color = OP_CARRIER_AIRLINE_ID, group = OP_CARRIER_AIRLINE_ID)) +
  geom_line() +
  geom_point() +
  labs(title = "2023 Mean Departure Delays by Airline and Day of the Week",
       x = "Day of the Week",
       y = "Mean Delay (minutes)",
       color = "Airline") +
    facet_grid(~OP_CARRIER_AIRLINE_ID) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```{r}
ggplot(day_mean_delays, aes(x = DAY_WEEK_NAME, y = mean_delay, group = OP_CARRIER_AIRLINE_ID, color = OP_CARRIER_AIRLINE_ID)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Mean Delays Trend Across Days for Airlines (2023)",
       x = "Day of the Week", y = "Mean Delay (minutes)", color = "Airline") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = "Dark2")
```

testing stuff

```{r}
df_transposed <- data.frame(
  OP_CARRIER_AIRLINE_ID = c(
    "Alaska Airlines Inc.: AS", 
    "American Airlines Inc.: AA", 
    "Delta Air Lines Inc.: DL", 
    "Hawaiian Airlines Inc.: HA", 
    "Southwest Airlines Co.: WN", 
    "United Air Lines Inc.: UA"
  ),
  CARRIER_DELAY = c(14.06481, 42.46123, 41.52086, 25.39735, 15.81637, 34.14489),
  WEATHER_DELAY = c(1.5363757, 1.6163102, 0.9116348, 1.1126701, 0.2287040, 1.0889205),
  NAS_DELAY = c(14.108025, 14.676471, 11.852234, 2.529433, 10.276434, 9.809375),
  SECURITY_DELAY = c(0.17791005, 0.37366310, 0.09180167, 0.17586243, 0.28306265, 0.00000000),
  LATE_AIRCRAFT_DELAY = c(19.27998, 24.26136, 21.60088, 18.55586, 16.90835, 28.78949)
)


df_transposed <- df_transposed %>%
  pivot_longer(cols = starts_with("CARRIER_DELAY"):starts_with("LATE_AIRCRAFT_DELAY"), 
               names_to = "Delay_Type", 
               values_to = "Delay_Value")

ggplot(df_transposed, aes(x = OP_CARRIER_AIRLINE_ID, y = Delay_Value, fill = Delay_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "2023 Delays by Airline and Delay Type",
       x = "Airlines", 
       y = "Delay (Minutes)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "top")
```

interisland flights and delays plot

```{r}

interisland_flightsdelays <- interisland_flights %>%
    group_by(OP_CARRIER_AIRLINE_ID) %>%
    summarise(
        total_flights = n(),
        delayed_flights = sum(DEP_DELAY > 10, na.rm = TRUE) ,
        .groups = "drop"
    )


ggplot(interisland_flightsdelays) +
    geom_bar(aes(x = OP_CARRIER_AIRLINE_ID, y = total_flights), stat = "identity", fill = "cornflowerblue", width = 0.5) +
    geom_col(aes(x = OP_CARRIER_AIRLINE_ID, y = delayed_flights), fill = "coral1", width = 0.5, position = position_nudge(x = 0.0)) +
   geom_text(aes(x = OP_CARRIER_AIRLINE_ID, y = total_flights, label = total_flights), vjust = 2.0, color = "black", size = 3.5) +
    geom_text(aes(x = OP_CARRIER_AIRLINE_ID, y = delayed_flights, label = delayed_flights), vjust = 1.5, color = "black", size = 3.5, position = position_nudge(x = 0.0)) +
    labs(title = "Total Interisland Flights and >10 Minute Delays (2023)",
         x = "Airline",
         y = "Number of Flights") +
    theme_minimal() 
```
