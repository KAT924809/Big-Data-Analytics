library(ggplot2)
library(dplyr)
library(reshape2)
chess_games <- read.csv("C:/Users/kat92/Downloads/chess_games.csv/chess_games.csv")
str(chess_games)

#Aggregate average elo by opening and game result
heatmap_data <- chess_games %>%
  group_by(Opening, Result) %>%
  summarise(
    AvgWhiteElo = mean(WhiteElo, na.rm = TRUE),
    AvgBlackElo = mean(BlackElo, na.rm = TRUE)
  ) %>%
  ungroup()

#filter is applied so that the heat map is much more readable hence
##1st 20 openings with respective win rate.
top_openings <- chess_games %>%
  count(Opening, sort = TRUE) %>%
  head(20) %>%
  pull(Opening)

heatmap_data_filtered <- heatmap_data %>%
  filter(Opening %in% top_openings)

#heat map generation
ggplot(heatmap_data_filtered, aes(x = Result, y = reorder(Opening, AvgWhiteElo), fill = AvgWhiteElo)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +  
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
  labs(title = "Heatmap: Avg White Elo by Result and Opening", 
       x = "Game Result", y = "Opening", fill = "Avg White Elo") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
    axis.text.y = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")  
  )


chess_games <- chess_games %>%
  mutate(EloDifference = WhiteElo - BlackElo)
##here the elodifference is calculated 
## it is used here.
ggplot(chess_games, aes(x = Result, y = EloDifference, fill = Result)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  labs(title = "Boxplot of Elo Difference by Game Result", 
       x = "Game Result", y = "Elo Difference") +
  theme_minimal() +
  theme(legend.position = "none")


win_rate_data <- chess_games %>%
  mutate(Winner = case_when(
    Result == "1-0" ~ "White",
    Result == "0-1" ~ "Black",
    TRUE ~ "Draw"
  )) %>%
  group_by(Opening, Winner) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  filter(Opening %in% top_openings)


ggplot(win_rate_data, aes(x = Opening, y = Count, fill = Winner)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Win Rates by Opening", x = "Opening", y = "Number of Games", fill = "Winner") +
  theme_minimal()
##CODE USED FOR bar-graph plotting

opening_counts <- chess_games %>%
  count(Opening, sort = TRUE) %>%
  head(10)  ## top 10

##this is the bar graph
ggplot(opening_counts, aes(x = reorder(Opening, n), y = n)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() + ##so that it is much more readable as previous graph was'not 
  labs(title = "Top 10 Most Popular Openings", x = "Opening", y = "Number of Games") +
  theme_minimal()

##trendline 
ggplot(chess_games, aes(x = WhiteElo, y = BlackElo, color = Result)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +  
  labs(title = "Scatter Plot of White Elo vs Black Elo", 
       x = "White Elo", y = "Black Elo") +
  theme_minimal()

black_wins <- chess_games %>%
  filter(Result == "0-1") %>%
  count(Opening, sort = TRUE) %>%
  head(10) 
##here we are filtering the moves done by opponent black and the result
ggplot(black_wins, aes(x = reorder(Opening, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  labs(title = "Top 10 Openings Leading to Black Wins", 
       x = "Opening", y = "Number of Wins") +
  theme_minimal()
white_openings <- chess_games %>%
  count(Opening, sort = TRUE) %>%
  head(10)  # Top 10 openings chosen by White

# Plotting the bar chart
ggplot(white_openings, aes(x = reorder(Opening, n), y = n)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Top 10 Most Frequent Openings Chosen by White", 
       x = "Opening", y = "Number of Games") +
  theme_minimal()

