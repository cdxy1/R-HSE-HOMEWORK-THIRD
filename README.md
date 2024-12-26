# Дублирую работу в обычном readme для Вашего удобства!!!

---
title: "Minkin_DZ"
author: "Minkin Artem"
date: "2024-12-25"
---

*Здесь я делаю все импорты*

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(readr)
library(curl)
library(httr)
library(jsonlite)
```

*Данные беру тут:*

```{r}
heroes <- read_csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/refs/heads/master/data/heroes_information.csv", na = c("NA", "-", "-99", ""))
```

### *Задание №1:*

```{r}
selected_heroes <- heroes %>%
  select(name, Alignment, Race, Publisher)
print(selected_heroes)
```

### *Задание №2:*

```{r}
filtered_heroes <- heroes %>%
  filter(Height > 100 & Weight < 50)
print(filtered_heroes)
```

### *Задание №3:*

```{r}
unique_eye_color_heroes <- heroes %>%
  group_by(`Eye color`) %>%
  filter(n() == 1) %>%
  ungroup()

print(unique_eye_color_heroes)
```

### *Задание №4:*

```{r}
mean_height_by_eye_color <- heroes %>%
  group_by(`Eye color`) %>%
  summarise(
    mean_height = mean(Height, na.rm = TRUE),
    n = n()                                     
  ) %>%
  arrange(desc(mean_height))                    

print(mean_height_by_eye_color)
```

### *Задание №5.1:*

```{r}
my_mode <- function(x) {
  x <- na.omit(x)
  
  if (length(x) == 0) {
    return(NA)
  }
  
  freq_table <- table(x)
  
  max_freq <- max(freq_table)
  
  modes <- names(freq_table[freq_table == max_freq])
  
  return(paste(modes, collapse = " / "))
}

print(my_mode(c("red", "yellow", "red", "green", "yellow"))) 
print(my_mode(c("red", "yellow", "red")))                   
print(my_mode(c(NA, NA)))                                     
```

### *Задание №5.2:*

```{r}
result <- heroes %>%
  group_by(Race) %>%
  summarise(
    max_height = max(Height, na.rm = TRUE),
    n = n(),
    mode_skin_color = my_mode(`Skin color`)
  ) %>%
  arrange(desc(n))
print(result)
```

### *Задание 6:*

```{r}
response <- GET("https://superheroapi.com/api/ВашТокенПожалуйста/644/powerstats/")

if (status_code(response) == 200) {
  data <- fromJSON(content(response, as = "text"))
  
  superman_stats <- tibble(
    name = data$name,
    intelligence = data$intelligence,
    strength = data$strength,
    speed = data$speed,
    durability = data$durability,
    power = data$power,
    combat = data$combat
  )
  
  print(superman_stats)
} else {
  stop("Ошибка при получении данных: ", status_code(response))
}
```

### *Задание 7:*

*Не нашел в документации этого апи где у него роут, который отдает всех героев, поэтому просто запихнул в цикл. Еще я тестил на 150 запросах, так как не знаю есть ли у этого апи ограничения, но код работает!!*

```{r}
all_powers <- list()

for (id in 1:731) {
  url <- paste0("https://superheroapi.com/api/ВашТокенПожалуйста/", id, "/powerstats")
  
  response <- GET(url)
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, as = "text"))
    
    if (!is.null(data)) {
      super_stats <- tibble(
        name = data$name,
        intelligence = data$intelligence,
        strength = data$strength,
        speed = data$speed,
        durability = data$durability,
        power = data$power,
        combat = data$combat
      )
      
      all_powers[[id]] <- super_stats
    } else {
      message(paste("Нет данных для ID:", id))
    }
  } else {
    message(paste("Ошибка при получении данных для ID:", id, "Статус:", status_code(response)))
  }
  
}

all_powers <- bind_rows(all_powers)

print(all_powers)


# Сделал 7.b
duplicates <- all_powers %>%
  count(name) %>%
  filter(n > 1)

print(duplicates)

# Сделал 7.c
all_powers_without_dupes <- all_powers %>%
  distinct(name, .keep_all = TRUE)

print(all_powers_without_dupes)

# Сделал 7.d
missing_in_all_powers <- anti_join(heroes, all_powers_without_dupes, by = "name")

print(missing_in_all_powers)

# Сделал 7.e
combined_data <- left_join(heroes, all_powers_without_dupes, by = "name")

print(combined_data)
```