
rm(list=ls())
library(shiny)
library(ggplot2)
library(dplyr)
library(grid)  
library(png)   
library(gridExtra)  
setwd("E:/UWM/628/hw4shiny")

data <- read.csv("seed9999_no_kmeans有时长合理版.csv")
# delete GroupIndex and rename
data <- data %>%
  select(-GroupIndex) %>%   # drop some 
  rename(
    PodcastTopic = Mode,              #  Mode -> PodcastTopic
    #PodcastCategory = finalCategory#,  #  finalCategory -> PodcastCategory
    #EpisodeCategory = LengthCategory, #  LengthCategory -> EpisodeCategory
    EpisodeTopic = topic              #  topic -> EpisodeTopic
  )
# category is correct
data$EpisodeCategory_encoded <- as.numeric(factor(data$EpisodeCategory))
data$PodcastCategory_encoded <- as.numeric(factor(data$PodcastCategory))

# encode
episode_mapping <- unique(data.frame(
  EpisodeCategory = data$EpisodeCategory,
  EpisodeCategory_encoded = data$EpisodeCategory_encoded
))


cat("EpisodeCategory Mapping:\n")
print(episode_mapping)

# Shiny UI
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: rgba(179, 229, 252, 0.2);  
        background-size: cover;
        background-attachment: fixed;
      }
      .sidebar {
        background-color: #ffffff; 
        border-radius: 10px;      
        padding: 10px;
        box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.1); 
      }
      .mainpanel {
        background-color: rgba(255, 255, 255, 0.8); 
      }
    "))
  ),
  titlePanel("Podcast Episode Distribution and Dynamic Clustering"),
  sidebarLayout(
    sidebarPanel(
      width = 3,  
      selectizeInput(
        "selected_podcasts",
        "Select one or more podcasts:",
        choices = unique(data$Podcast.Name),
        multiple = TRUE,
        options = list(
          placeholder = "Type to select podcasts..."#,
          #maxOptions = 10
        )
      ),
      radioButtons(
        "color_by", "Color by:",
        choices = list("Podcast Name" = "Podcast.Name", "Clustering Result" = "cluster"),
        selected = "Podcast.Name"
      ),
      actionButton("show_plot", "Show Comparison"),
      uiOutput("podcast_images"),  
      tags$br(),
      tags$hr(),
      tags$h5("Need Help?"),
      tags$p("If you have any questions, feel free to contact:"),
      tags$p(tags$a(href = "mailto:myu259@wisc.edu", "myu259@wisc.edu")),
      tags$p(tags$a(href = "mailto:mzhao246@wisc.edu", "mzhao246@wisc.edu"))
    ),
    mainPanel(
      plotOutput("comparison_plot", height = "700px", width = "100%"),  
      tags$pre(textOutput("summary_text"))
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  # 定义类别及其描述数据框
  category_annotations <- data.frame(
    EpisodeTopic = c(1, 2, 3, 4, 5, 6, 7, 8),  # 对应 EpisodeTopic 的值
    Description = c(
      "Category 1: Story\nHot words: stories",
      "Category 2: Politics\nHot words: trump",
      "Category 3: Crime\nHot words: murder, crime",
      "Category 4: Health\nHot words: sleep",
      "Category 5: Non-English\nHot words: vida, redes",
      "Category 6: Business\nHot words: business",
      "Category 7: Sport\nHot words: football, nba",
      "Category 8: Comedy\nHot words: comedy"
    )
  )
  
  # 更新选择输入
  observe({
    updateSelectizeInput(
      session,
      "selected_podcasts",
      choices = unique(data$Podcast.Name),
      server = TRUE
    )
  })
  
  observeEvent(input$show_plot, {
    if (is.null(input$selected_podcasts) || length(input$selected_podcasts) < 1) {
      showNotification("Please select at least one podcast!", type = "error")
      return()
    }
    
    selected_data <- data %>% filter(Podcast.Name %in% input$selected_podcasts)
    
    k <- min(length(input$selected_podcasts), nrow(selected_data))
    if (k <= 1) k <- 1 
    
    # 数据矩阵
    matrix_data <- selected_data %>%
      select(EpisodeTopic, EpisodeCategory_encoded) %>%
      as.matrix()
    matrix_data <- scale(matrix_data)  
    
    # KMeans 聚类
    set.seed(123)
    kmeans_model <- kmeans(matrix_data, centers = k)
    selected_data$cluster <- as.factor(kmeans_model$cluster)
    
    # 绘图部分
    output$comparison_plot <- renderPlot({
      ggplot(selected_data, aes(x = EpisodeTopic, y = EpisodeCategory_encoded, color = !!sym(input$color_by))) +
        geom_jitter(size = 3, alpha = 0.7, width = 0.2, height = 0.2) +
        scale_y_continuous(
          breaks = episode_mapping$EpisodeCategory_encoded,  
          labels = episode_mapping$EpisodeCategory           
        ) +
        labs(
          title = "Selected Podcasts' Theme and Duration Distribution",
          x = "Episode Topic",
          y = "Podcast Category",
          color = ifelse(input$color_by == "Podcast.Name", "Podcast Name", "Cluster")
        ) +
        theme_minimal() +
        # 添加类别注释
        geom_text(
          data = category_annotations,
          aes(
            x = EpisodeTopic, 
            y = max(selected_data$EpisodeCategory_encoded) + 1,  # 调整 y 位置到数据点上方
            label = Description
          ),
          inherit.aes = FALSE,  # 不继承全局 aes 映射
          size = 3.5,           # 字体大小
          hjust = 0.5,          # 水平居中
          vjust = 0,            # 垂直对齐方式
          color = "black"       # 文本颜色
        )
    })
    
    # Summary Text
    output$summary_text <- renderText({
      paste(
        "Selected podcasts:", paste(input$selected_podcasts, collapse = ", "), "\n",
        "Total episodes:", nrow(selected_data), "\n",
        "Episode distribution per podcast:", "\n",
        paste(sapply(input$selected_podcasts, function(p) {
          paste(p, ":", nrow(filter(selected_data, Podcast.Name == p)))
        }), collapse = "\n"), "\n",
        "Dynamic clustering groups:", k
      )
    })
    
    # Podcast images
    output$podcast_images <- renderUI({
      if (length(input$selected_podcasts) == 0) {
        return(NULL)
      }
      images <- lapply(input$selected_podcasts, function(podcast) {
        img_url <- data %>%
          filter(Podcast.Name == podcast) %>%
          slice(1) %>%
          pull(Image.URL)  # 获取第一个 URL
        
        if (!is.null(img_url) && !is.na(img_url)) {
          tags$div(
            tags$img(src = img_url, height = "150px", style = "margin: 5px;"),
            style = "display: inline-block; text-align: center; margin: 10px;"
          )
        } else {
          NULL  # 没有 URL 则跳过
        }
      })
      do.call(tagList, images)  # 合并图片
    })
  })
}


shinyApp(ui = ui, server = server)

