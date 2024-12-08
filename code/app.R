#这个是最后一班
rm(list=ls())
library(shiny)
library(ggplot2)
library(dplyr)
library(grid)  # 用于添加背景图片
library(png)   # 用于读取背景图片
library(gridExtra)  # 用于布局图片

# 加载数据
data <- read.csv("seed9999_no_kmeans有时长合理版.csv")
# 删除 GroupIndex 并重命名列
data <- data %>%
  select(-GroupIndex) %>%   # 删除 GroupIndex 列
  rename(
    PodcastTopic = Mode,              # 重命名 Mode -> PodcastTopic
    #PodcastCategory = finalCategory#,  # 重命名 finalCategory -> PodcastCategory
    #EpisodeCategory = LengthCategory, # 重命名 LengthCategory -> EpisodeCategory
    EpisodeTopic = topic              # 重命名 topic -> EpisodeTopic
  )
# 确保数据类型正确
data$EpisodeCategory_encoded <- as.numeric(factor(data$EpisodeCategory))
data$PodcastCategory_encoded <- as.numeric(factor(data$PodcastCategory))

# 提取编码与实际分类的映射
episode_mapping <- unique(data.frame(
  EpisodeCategory = data$EpisodeCategory,
  EpisodeCategory_encoded = data$EpisodeCategory_encoded
))

# 打印映射信息（可选）
cat("EpisodeCategory Mapping:\n")
print(episode_mapping)

# Shiny UI
ui <- fluidPage(
  # 添加全局背景样式
  tags$head(
    tags$style(HTML("
      body {
        background-color: rgba(179, 229, 252, 0.2);  /* 替换为实际背景图片路径 */
        background-size: cover;
        background-attachment: fixed;
      }
      .sidebar {
        background-color: #ffffff; /* 左侧栏背景为白色 */
        border-radius: 10px;      /* 添加圆角 */
        padding: 10px;
        box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.1); /* 添加轻微阴影 */
      }
      .mainpanel {
        background-color: rgba(255, 255, 255, 0.8); /* 半透明背景 */
      }
    "))
  ),
  titlePanel("Podcast Episode Distribution and Dynamic Clustering"),
  sidebarLayout(
    sidebarPanel(
      width = 3,  # 减小 sidebar 宽度
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
      uiOutput("podcast_images"),  # 用于动态显示图片
      tags$br(),
      tags$hr(),
      tags$h5("Need Help?"),
      tags$p("If you have any questions, feel free to contact:"),
      tags$p(tags$a(href = "mailto:myu259@wisc.edu", "myu259@wisc.edu")),
      tags$p(tags$a(href = "mailto:mzhao246@wisc.edu", "mzhao246@wisc.edu"))
    ),
    mainPanel(
      plotOutput("comparison_plot", height = "700px", width = "100%"),  # 放大主图
      tags$pre(textOutput("summary_text"))
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  # 动态更新选择项
  observe({
    updateSelectizeInput(
      session,
      "selected_podcasts",
      choices = unique(data$Podcast.Name),
      server = TRUE
    )
  })
  
  # 绘制对比图
  observeEvent(input$show_plot, {
    if (is.null(input$selected_podcasts) || length(input$selected_podcasts) < 1) {
      showNotification("Please select at least one podcast!", type = "error")
      return()
    }
    
    # 筛选所选播客的数据
    selected_data <- data %>% filter(Podcast.Name %in% input$selected_podcasts)
    
    # 动态调整聚类数
    k <- min(length(input$selected_podcasts), nrow(selected_data))
    if (k <= 1) k <- 1  # 至少分为 1 类
    
    # 构造特征矩阵
    matrix_data <- selected_data %>%
      select(EpisodeTopic, EpisodeCategory_encoded) %>%
      as.matrix()
    matrix_data <- scale(matrix_data)  # 标准化数据
    
    # 动态聚类
    set.seed(123)
    kmeans_model <- kmeans(matrix_data, centers = k)
    selected_data$cluster <- as.factor(kmeans_model$cluster)
    
    # 绘制散点图
    output$comparison_plot <- renderPlot({
      ggplot(selected_data, aes(x = EpisodeTopic, y = EpisodeCategory_encoded, color = !!sym(input$color_by))) +
        geom_jitter(size = 3, alpha = 0.7, width = 0.2, height = 0.2) +
        scale_y_continuous(
          breaks = episode_mapping$EpisodeCategory_encoded,  # 编码值
          labels = episode_mapping$EpisodeCategory           # 实际分类
        ) +
        labs(
          title = "Selected Podcasts' Theme and Duration Distribution",
          x = "Episode Topic",
          y = "Podcast Category",
          color = ifelse(input$color_by == "Podcast.Name", "Podcast Name", "Cluster")
        ) +
        theme_minimal()
    })
    
    # 显示文字摘要
    output$summary_text <- renderText({
      paste(
        "Selected podcasts:", paste(input$selected_podcasts, collapse = ", "),"\n",
        "Total episodes:", nrow(selected_data),"\n",
        "Episode distribution per podcast:","\n",
        paste(sapply(input$selected_podcasts, function(p) {
          paste(p, ":", nrow(filter(selected_data, Podcast.Name == p)))
        }), collapse = "\n"),"\n",
        "Dynamic clustering groups:", k
      )
    })
    
    # 显示每个播客的第一个图片
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
          NULL  # 如果没有 URL 则跳过
        }
      })
      do.call(tagList, images)  # 合并所有图片
    })
  })
}

# 运行 Shiny App
shinyApp(ui = ui, server = server)

