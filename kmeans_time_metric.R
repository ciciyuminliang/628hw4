#title: "Time metric"


#KMEANS to get time metric
rm(list=ls())
library(dplyr)
library(ggplot2)


data <- read.csv("E:/UWM/628/hw4/seed9999_no_kmeans.csv")

# get（EpisodeID、PodcastID、Duration）
data_filtered <- data %>%
  select(Episode.ID, Podcast.ID, `Duration..minutes.`) %>%
  rename(Duration = `Duration..minutes.`)

# missing value?
data_filtered <- na.omit(data_filtered)

# kmeans k=3
set.seed(123)  
kmeans_result <- kmeans(data_filtered$Duration, centers = 5)


data_filtered$EpisodeCategory <- kmeans_result$cluster

# separate
centers <- data.frame(Cluster = 1:5, Center = kmeans_result$centers)
centers <- centers[order(centers$Center), ]
length_map <- setNames(c("Short","Short-Medium", "Medium","Medium-Long", "Long"), centers$Cluster)
data_filtered$EpisodeCategory <- length_map[data_filtered$EpisodeCategory]

# podcast level
podcast_summary <- data_filtered %>%
  group_by(Podcast.ID) %>%
  summarise(
    Short = mean(EpisodeCategory == "Short"),
    Short_Medium = mean(EpisodeCategory == "Short-Medium"),
    Medium = mean(EpisodeCategory == "Medium"),
    Medium_Long = mean(EpisodeCategory == "Medium-Long"),
    Long = mean(EpisodeCategory == "Long")
  )

# final classification（finalCategory）
# （PodcastCategory）


podcast_summary <- podcast_summary %>%
  mutate(PodcastCategory = case_when(
    Short > Short_Medium & Short > Medium & Short > Medium_Long & Short > Long ~ "Short",
    Short_Medium > Short & Short_Medium > Medium & Short_Medium > Medium_Long & Short_Medium > Long ~ "Short-Medium",
    Medium > Short & Medium > Short_Medium & Medium > Medium_Long & Medium > Long ~ "Medium",
    Medium_Long > Short & Medium_Long > Short_Medium & Medium_Long > Medium & Medium_Long > Long ~ "Medium-Long",
    Long > Short & Long > Short_Medium & Long > Medium & Long > Medium_Long ~ "Long",
    TRUE ~ "Mixed"  )) %>%
  rowwise() %>%
  mutate(PodcastCategory = if_else(
    PodcastCategory == "Mixed",
    # mixed combine into 5 category
    c("Short", "Short-Medium", "Medium", "Medium-Long", "Long")[which.min(c(
      abs(Short - max(Short, Short_Medium, Medium, Medium_Long, Long)),
      abs(Short_Medium - max(Short, Short_Medium, Medium, Medium_Long, Long)),
      abs(Medium - max(Short, Short_Medium, Medium, Medium_Long, Long)),
      abs(Medium_Long - max(Short, Short_Medium, Medium, Medium_Long, Long)),
      abs(Long - max(Short, Short_Medium, Medium, Medium_Long, Long))
    ))],
    PodcastCategory
  )) %>%
  ungroup()

# combine
data_with_categories <- data %>%
  left_join(data_filtered %>% select(Episode.ID, EpisodeCategory), by = "Episode.ID") %>%
  left_join(podcast_summary %>% select(Podcast.ID, PodcastCategory), by = "Podcast.ID")


#write.csv(data_with_categories, "E:/UWM/628/hw4/seed9999_no_kmeans有时长合理版.csv", row.names = FALSE)


print(head(data_with_categories))



# kmeans dots print
print(kmeans_result$centers)

# order by centers
centers <- sort(kmeans_result$centers)

# print boundary
boundaries <- (centers[-length(centers)] + centers[-1]) / 2
print(boundaries) 


# episode level
ggplot(data_with_categories, aes(x = Duration..minutes., fill = EpisodeCategory)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Distribution of Podcast Episode Durations",
       x = "Duration (minutes)", y = "Count") +
  scale_fill_manual(values = c("Short" = "#1f77b4", 
                               "Short-Medium" = "#76c7c0", 
                               "Medium" = "#ff7f0e", 
                               "Medium-Long" = "red", 
                               "Long" = "#2ca02c")) +
  theme_minimal()


# pocast level
ggplot(podcast_summary, aes(x = PodcastCategory, fill = PodcastCategory)) +
  geom_bar(alpha = 0.7) +
  labs(title = "Distribution of Podcast Final Categories",
       x = "Final Category (Short / Short-Medium / Medium / Medium-Long / Long)",
       y = "Count of Podcasts") +
  scale_fill_manual(values = c("Short" = "#1f77b4", 
                               "Short-Medium" = "#76c7c0", 
                               "Medium" = "#ff7f0e", 
                               "Medium-Long" = "red", 
                               "Long" = "#2ca02c")) +
  theme_minimal()
