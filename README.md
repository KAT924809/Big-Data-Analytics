library(ggplot2)
library(dplyr)
library(reshape2)

chess_games <- read.csv("C:/Users/kat92/Downloads/chess_games.csv/chess_games.csv")
str(chess_games)

ggplot2: Used for creating visualizations.
dplyr: Used for data manipulation (filtering, grouping, etc.).
reshape2: Helps reshape data for plotting (though not directly used here).
read.csv(): Reads the dataset into chess_games.
str(): Displays the structure of the dataset (columns, types).
___________________________________________________________________________________
heatmap_data <- chess_games %>%
  group_by(Opening, Result) %>%
  summarise(
    AvgWhiteElo = mean(WhiteElo, na.rm = TRUE),
    AvgBlackElo = mean(BlackElo, na.rm = TRUE)
  ) %>%
  ungroup()
group_by(): Groups the data by Opening and Result.
summarise(): Calculates the average White and Black Elo for each group.
na.rm = TRUE: Ignores missing values.
ungroup(): Removes the grouping to make it easier to work with the result.
___________________________________________________________________________________
top_openings <- chess_games %>%
  count(Opening, sort = TRUE) %>%
  head(20) %>%
  pull(Opening)
heatmap_data_filtered <- heatmap_data %>%
filter(Opening %in% top_openings)

count(): Counts the occurrences of each opening and sorts them.
head(20): Selects the top 20 most frequent openings.
pull(): Extracts the Opening column as a vector.
filter(): Filters the heatmap data to include only the top 20 openings.
__________________________________________________________________________________
ggplot(heatmap_data_filtered, aes(x = Result, y = reorder(Opening, AvgWhiteElo), fill = AvgWhiteElo)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +  
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
  labs(title = "Heatmap: Avg White Elo by Result and Opening", x = "Game Result", y = "Opening", fill = "Avg White Elo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), axis.text.y = element_text(size = 8), plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

geom_tile(): Creates a heatmap with tiles representing combinations of Result and Opening.
scale_fill_gradient(): Adds a color gradient for average Elo.
reorder(): Orders openings by average White Elo for readability.
theme(): Adjusts axis text size and rotation to avoid clutter.
___________________________________________________________________________________
chess_games <- chess_games %>%
  mutate(EloDifference = WhiteElo - BlackElo)

ggplot(chess_games, aes(x = Result, y = EloDifference, fill = Result)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  labs(title = "Boxplot of Elo Difference by Game Result", x = "Game Result", y = "Elo Difference") +
  theme_minimal() +
  theme(legend.position = "none")

mutate(): Adds a new column, EloDifference (difference between White and Black Elo).
geom_boxplot(): Creates a boxplot to visualize the distribution of Elo differences by game result.
Outliers: Marked with red dots to identify unusual values.
theme(): Hides the legend since the fill is redundant.
___________________________________________________________________________________
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
case_when(): Assigns a winner based on the result (White, Black, or Draw).
group_by(): Groups data by Opening and Winner.
summarise(): Counts the number of wins for each group.
filter(): Keeps only the top 20 openings.
___________________________________________________________________________________

ggplot(win_rate_data, aes(x = Opening, y = Count, fill = Winner)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Win Rates by Opening", x = "Opening", y = "Number of Games", fill = "Winner") +
  theme_minimal()
geom_bar(): Creates a grouped bar plot with wins by opening and winner.
position = "dodge": Places bars side by side.
coord_flip(): Flips axes for readability.
___________________________________________________________________________________
opening_counts <- chess_games %>%
  count(Opening, sort = TRUE) %>%
  head(10)

ggplot(opening_counts, aes(x = reorder(Opening, n), y = n)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +
  labs(title = "Top 10 Most Popular Openings", x = "Opening", y = "Number of Games") +
  theme_minimal()
Counts the top 10 openings and plots them as a bar chart.
coord_flip() improves readability by making openings vertical.

___________________________________________________________________________________
ggplot(chess_games, aes(x = WhiteElo, y = BlackElo, color = Result)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +  
  labs(title = "Scatter Plot of White Elo vs Black Elo", x = "White Elo", y = "Black Elo") +
  theme_minimal()

geom_point(): Creates a scatter plot of White Elo vs Black Elo.
geom_smooth(): Adds a trendline using linear regression.
___________________________________________________________________________________
black_wins <- chess_games %>%
  filter(Result == "0-1") %>%
  count(Opening, sort = TRUE) %>%
  head(10)

ggplot(black_wins, aes(x = reorder(Opening, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  labs(title = "Top 10 Openings Leading to Black Wins", x = "Opening", y = "Number of Wins") +
  theme_minimal()
Filters games where Black won and counts the top 10 openings used.
___________________________________________________________________________________
white_openings <- chess_games %>%
  count(Opening, sort = TRUE) %>%
  head(10)

ggplot(white_openings, aes(x = reorder(Opening, n), y = n)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Top 10 Most Frequent Openings Chosen by White", x = "Opening", y = "Number of Games") +
  theme_minimal()
Counts the most frequently chosen openings by White.
______________________________________________________________________________________________________

PROJECT BY
KSHITIJ TRIPATHI
NISHANT MISHRA
DEVRAJ DESHMUKH
ROHAN MENON







