library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(plotly)

# 读取数据
data <- read_csv("gross-enrollment-ratio-2013-2016.csv")

# 清理数据，将 'NR' 和 '@' 替换为 0
data <- data %>%
  mutate(
    Primary_Total = as.numeric(gsub("NR|@", "0", Primary_Total)),
    Upper_Primary_Total = as.numeric(gsub("NR|@", "0", Upper_Primary_Total)),
    Secondary_Total = as.numeric(gsub("NR|@", "0", Secondary_Total)),
    Higher_Secondary_Total = as.numeric(gsub("NR|@", "0", Higher_Secondary_Total))
  ) %>%
  # 计算总和列
  mutate(Total = Primary_Total + Upper_Primary_Total + Secondary_Total + Higher_Secondary_Total)

# 读取印度边界数据，使用 'sf' 格式
india_map <- ne_states(country = "India", returnclass = "sf")

# 合并数据：确保通过共同的字段进行合并，假设 'State_UT' 和 'name_en' 是共同字段
india_map <- india_map %>%
  left_join(data, by = c("name_en" = "State_UT"))

# 创建列名到友好名称的映射
category_names <- list(
  "Primary_Total" = "Primary Education",
  "Upper_Primary_Total" = "Upper Primary Education",
  "Secondary_Total" = "Secondary Education",
  "Higher_Secondary_Total" = "Higher Secondary Education",
  "Total" = "Total Enrollment"
)

# 将 "Year" 格式化为起始年份
data <- data %>%
  mutate(Year = as.numeric(substr(Year, 1, 4)))

# 定义 UI
ui <- fluidPage(
  titlePanel("Education Data by State and Union Territories"),
  
  # 使用 leaflet 作为地图主界面，确保地图高度为 100%
  div(style = "height: 90vh; position: relative;",  # 使用 100% 视窗高度
      leafletOutput("map", height = "90vh", width = "100vw"),  # 地图高度和宽度设置为视口高度和宽度
      
      # 调整缩放按钮和下拉框的位置，使它们并排显示
      div(style = "position: absolute; top: 10px; left: 10px; display: flex; background-color: transparent;",  # 背景设为透明
          
          # 缩放按钮
          div(style = "background-color: transparent; padding: 10px; margin-right: 10px;",  # 去掉背景颜色
              tags$div(class = "leaflet-control-zoom")
          ),
          
          # 下拉框
          div(style = "background-color: rgba(255, 255, 255, 0.7); padding: 10px; border-radius: 10px; width: 200px;",  # 仅为下拉框添加白色背景
              selectInput("category", "Select Education Level:", choices = names(category_names), selected = "Primary_Total")
          )
      ),
      
      # 将折线图放置到地图的右下角，并扩大面积
      div(style = "position: absolute; bottom: 10px; right: 10px; width: 30%; height: 40%; background-color: rgba(255, 255, 255, 0.5); padding: 10px; border-radius: 10px;",
          plotlyOutput("linePlot", height = "100%", width = "100%")  # 调整高度和宽度
      )
  )
)

# 定义 Server
server <- function(input, output, session) {
  
  # 动态生成地图，初始视角设为印度
  output$map <- renderLeaflet({
    category_selected <- input$category  # 根据筛选框选择的类别展示
    friendly_name <- category_names[[category_selected]]  # 友好名称映射
    
    pal <- colorNumeric(palette = "RdPu", domain = india_map[[category_selected]], na.color = "transparent")
    
    leaflet(india_map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(india_map[[category_selected]]),  # 根据数据填充颜色
        weight = 1,
        color = "white",  # 边界颜色设为白色
        fillOpacity = 0.7,  # 设置透明度为 0.7
        label = ~paste("State: ", name_en),  # 显示英文州名
        popup = ~paste("State: ", name_en, "<br>", friendly_name, ": ", india_map[[category_selected]]),  # 使用友好名称
        layerId = ~name_en  # 为每个州设置唯一的ID
      ) %>%
      addLegend(
        pal = pal, 
        values = india_map[[category_selected]], 
        opacity = 0.7, 
        title = paste(friendly_name, "[Units]"),  # 图例显示友好名称
        position = "bottomleft"
      ) %>%
      setView(lng = 78.9629, lat = 20.5937, zoom = 5)
  })
  
  # 数据准备：动态根据点击的州或显示全国数据
  plot_data <- reactive({
    if (is.null(input$map_shape_click$id)) {
      data_summary <- data %>%
        group_by(Year) %>%
        summarise(
          Primary_Total = sum(Primary_Total, na.rm = TRUE),
          Upper_Primary_Total = sum(Upper_Primary_Total, na.rm = TRUE),
          Secondary_Total = sum(Secondary_Total, na.rm = TRUE),
          Higher_Secondary_Total = sum(Higher_Secondary_Total, na.rm = TRUE)
        )
      return(data_summary)
    } else {
      clicked_state <- input$map_shape_click$id
      data %>%
        filter(State_UT == clicked_state)
    }
  })
  
  # 动态生成折线图，并为每个点选择不同的形状，且图例字体较小
  output$linePlot <- renderPlotly({
    plot_df <- plot_data()
    
    # 确保选择的列名存在
    selected_category <- input$category
    selected_category_name <- category_names[[selected_category]]  # 获取友好名称
    
    gg <- ggplot(plot_df, aes(x = as.factor(Year))) +
      geom_line(aes_string(y = selected_category, group = 1, color = paste0("'", selected_category_name, "'")), size = 0.8) +  # 减小线条粗细
      geom_point(aes_string(y = selected_category, color = paste0("'", selected_category_name, "'")), size = 2) +  # 减小点的大小
      
      # 添加趋势线
      geom_smooth(aes_string(y = selected_category), method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 0.6) +
      
      # 设置标题和坐标轴的标签大小
      labs(title = paste(selected_category_name, "Enrollment Trend"),
           x = "Year",
           y = "Total Enrollment") +
      
      # 设置图例、标题和坐标轴文字大小
      theme(
        plot.title = element_text(size = 8, face = "bold", hjust = 0.5),  # 减小标题大小并居中
        axis.title.x = element_text(size = 9, face = "bold"),  # 加粗X轴标题
        axis.title.y = element_text(size = 9, face = "bold"),  # 加粗Y轴标题
        axis.text.x = element_text(size = 8, face = "bold"),  # 加粗X轴文字
        axis.text.y = element_text(size = 8, face = "bold"),  # 加粗Y轴文字
        legend.position = "bottom",  # 图例放在底部
        legend.text = element_text(size = 6),  # 减小图例文字大小
        legend.title = element_blank(),  # 去除图例标题
        legend.key.size = unit(0.2, "cm")  # 减小图例中的标记大小
      )
    
    ggplotly(gg) %>% layout(
      title = list(
        text = paste(selected_category_name, "Enrollment Trend"),  # 修改标题文本
        font = list(size = 10, family = "Arial", color = "black", weight = "bold"),  # 设置标题为加粗
        y = 0.9  # 标题位置调整
      ),
      legend = list(
        orientation = 'h', 
        y = -0.4  # 将图例向下移动两个单位（负数值表示向下移动）
      ),
      autosize = TRUE  # 自适应大小
    )
  })
}

# 启动应用
shinyApp(ui = ui, server = server)
