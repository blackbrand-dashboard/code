library(rsconnect)
library(shinydashboard)
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)

# Read data
state_summary_metrics <- read_csv("success_summary_metrics_state.csv")
metro_summary_metrics <- read_csv("success_summary_metrics_metro.csv")
state_names <- unique(state_summary_metrics$NAME)
metro_names <- unique(metro_summary_metrics$NAME)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Black Entrepreneurship Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Key Paper Insights", tabName = "key_paper_insights", icon = icon("file-text")),
      menuItem("Black Business Trend Tracker", tabName = "metric_trend_tracker", icon = icon("area-chart")),
      menuItem("Full Dashboard", tabName = "original_dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "key_paper_insights",
        tabsetPanel(
          tabPanel("Paper 1",
                   h3("Building Supportive Ecosystems For Black-owned US Businesses"),
                   h4("McKinsey & Company"),
                   HTML('<a href="https://www.mckinsey.com/industries/public-sector/our-insights/building-supportive-ecosystems-for-black-owned-us-businesses" target="_blank" style="color: #0066cc; text-decoration: underline;">Read Full Paper</a>'),
                   p("The article explores the challenges and opportunities for Black-owned businesses in the US, especially in the context of the COVID-19 pandemic and the racial wealth gap. Some of the key findings and recommendations from the report are:
"),
                   HTML("<ul>
                         <li>The article argues that building supportive ecosystems for Black-owned businesses can help them overcome structural barriers, such as access to capital, markets, talent, and networks, and achieve revenue parity with their white-owned counterparts.
</li>
                         <li>The article estimates that achieving revenue parity between Black- and white-owned businesses could add $290 billion in business equity and $190 billion in GDP annually.
</li>
                         <li>The article also suggests that providing additional liquidity to Black-owned SMBs during the pandemic could preserve up to 815,000 jobs and reduce the racial unemployment gap by 10 percent.
</li>
                        <li>The article highlights some examples of successful ecosystems that support Black-owned businesses, such as Atlanta, Georgia; Durham, North Carolina; and Detroit, Michigan. It also identifies some best practices and recommendations for ecosystem builders, such as governments, corporations, philanthropies, and community organizations.
</li>
                                            </ul>"),
                   img(src='pic.jpg', width = "700px", height = "400px"),
          ),
          tabPanel("Paper 2",
                   h3("The Tapestry of Black Business Ownership In America"),
                   h4("Untapped Opportunities For Success"),
                   HTML('<a href="https://aeoworks.org/wp-content/uploads/2019/03/AEO_Black_Owned_Business_Report_02_16_17_FOR_WEB-1.pdf" target="_blank" style="color: #0066cc; text-decoration: underline;">Read Full Paper</a>'),
                   p("The report highlights the strong entrepreneurial spirit among Black Americans, which has spurred the creation of numerous Black-owned businesses. However, it also notes that Black-owned businesses on the whole lag behind the average U.S. firm in terms of size and revenue. The report identifies three persistent barriers that impede the establishment and growth of Black-owned firms:
"),
                   HTML("<ul>
                         <li>Wealth Gap: Fewer assets and less disposable income to invest in business</li>
                         <li>Credit Gap: Decreased access to formal credit and high denial rates</li>
                         <li>Trust Gap: Institutional bias that Blacks have experienced, inhibiting them from actions such as applying to financial institutions for more capital, joining networks, creating valuable partnerships, and more</li>
                                            </ul>"),
                   p("It is stated that of the Black business owners surveyed, 48 percent reported not trusting institutions that provide business education and training, and 45 percent reported not trusting institutions that finance businesses. Moreover, 29 percent of respondents reported deciding not to approach lenders or investors for capital, even when their business needed it. The report’s authors calculated that if Black-owned firms were able to reach employment parity with all privately-held U.S. firms, 600,000 new jobs could be created and $55 billion would be added to the U.S. economy. This could also reduce unemployment in the Black community down to 5 percent.
"),
                   img(src='pic1.png', width = "700px", height = "400px"),
          ),
          
          tabPanel("Paper 3",
                   h3("Equitable Growth In Hampton Roads"),
                   HTML('<a href="https://github.com/blackbrand-dashboard/article/blob/main/ar3.pdf" target="_blank" style="color: #0066cc; text-decoration: underline;">Read Full Paper</a>'),
                   p("The paper argues that Hampton Roads has the potential to become a hub for equitable growth and innovation, but it needs to address some key issues, such as:"),
                   HTML("<ul>
                         <li>Diversifying its economy beyond defense and tourism, which are vulnerable to federal budget cuts and environmental shocks.</li>
                         <li>Investing in human capital and workforce development, especially for low-income and minority residents who face barriers to education and employment opportunities.</li>
                         <li>Fostering a culture of entrepreneurship and civic engagement, by supporting local startups, nonprofits, and community organizations that can create social impact and economic value.</li>
                         <li>Leveraging its unique assets and strengths, such as its strategic location, rich history, diverse population, and military presence.</li>
                         <li>Bridging gaps in educational attainment by investing in human capital and workforce development, with a focus on overcoming barriers for low-income and minority residents.</li>
                             </ul>"),
                   p("The paper also provides some recommendations and best practices for policymakers, business leaders, philanthropists, and community members who want to promote equitable growth in Hampton Roads. Some of these include:"),
                   HTML("<ul>
                       <li>Creating a regional economic development authority that can coordinate and align the efforts of different stakeholders and jurisdictions.</li>
                       <li>Establishing a regional innovation fund that can provide seed capital and mentorship to entrepreneurs and innovators from underrepresented backgrounds.</li>
                       <li>Developing a regional talent pipeline that can connect local students and workers with high-demand skills and industries.</li>
                      <li>Building a regional identity and brand that can attract and retain talent, investment, and visitors.</li>
                            </ul>"),
                   img(src='pic2.png', width = "700px", height = "400px"),
          )
        )
      ),
      tabItem(
        tabName = "metric_trend_tracker",
        sidebarLayout(
          sidebarPanel(
            radioButtons("selectionType", "Choose your selection type:",
                         choices = c("Select by State" = "state", 
                                     "Select by Metropolitan Area" = "metro")),
            uiOutput("dynamicSelectInput"),
            selectInput("metrics", "Select Metrics:", 
                        choices = c("Total Average Annual Pay (Total)" = "Total_Avg_Annual_Pay_Total",
                                    "Total Average Annual Pay (Black Business)" = "Total_Avg_Annual_Pay_Black_Business",
                                    "Total Average Employees (Total)" = "Total_Avg_Employees_Total",
                                    "Total Average Employees (Black Business)" = "Total_Avg_Employees_Black_Business",
                                    "Total Sum of Firms (Total)" = "Total_Sum_of_Firms_Total",
                                    "Total Sum of Firms (Black Business)" = "Total_Sum_of_Firms_Black_Business",
                                    "Average Pay Annual Per Employee (Total)" = "Pay_Annual_Per_Employee_Total",
                                    "Average Pay Annual Per Employee (Black Business)" = "Pay_Annual_Per_Employee_Black_Business",
                                    "Percent of Total Average Annual Pay (Black Business)" = "Percent_Total_Avg_Annual_Pay_BB",
                                    "Percent of Total Average Employees (Black Business)" = "Percent_Total_Avg_Employees_BB",
                                    "Percent of Total Sum of Firms (Black Business)" = "Percent_Total_Sum_of_Firms_BB")),
          ),
          mainPanel(
            tabPanel("Plot and Table", 
                     plotOutput("plot"),
                     dataTableOutput("table")
            )
          )
        )
      ),
      tabItem(
        tabName = "original_dashboard",
        HTML('<a href="https://dspgtools.shinyapps.io/dspg21hampton_roads/" target="_blank" style="color: #0066cc; text-decoration: underline;">Visit Full Dashboard</a>')
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Mapping of metrics to titles
  metric_titles <- c(
    "Total_Avg_Annual_Pay_Total" = "Total Average Annual Pay (Total)",
    "Total_Avg_Annual_Pay_Black_Business" = "Total Average Annual Pay (Black Business)",
    "Total_Avg_Employees_Total" = "Total Average Employees (Total)",
    "Total_Avg_Employees_Black_Business" = "Total Average Employees (Black Business)",
    "Total_Sum_of_Firms_Total" = "Total Sum of Firms (Total)",
    "Total_Sum_of_Firms_Black_Business" = "Total Sum of Firms (Black Business)",
    "Pay_Annual_Per_Employee_Total" = "Pay Annual Per Employee (Total)",
    "Pay_Annual_Per_Employee_Black_Business" = "Pay Annual Per Employee (Black Business)",
    "Percent_Total_Avg_Annual_Pay_BB" = "Percent of Total Average Annual Pay made up by Black Buisness",
    "Percent_Total_Avg_Employees_BB" = "Percent of Total Average Employees made up by Black Buisness",
    "Percent_Total_Sum_of_Firms_BB" = "Percent of Total Sum of Firms made up by Black Buisness"
  )
  
  
  
  # Dynamic UI for state or metro selection
  output$dynamicSelectInput <- renderUI({
    if(input$selectionType == "state") {
      selectInput("state", "Select State:", choices = state_names)
    } else {
      selectInput("metroArea", "Select Metropolitan Area:", choices = metro_names)
    }
  })
  
  # Reactive expression to filter data based on selection
  filtered_data <- reactive({
    if(input$selectionType == "state") {
      selected_state <- input$state
      state_summary_metrics %>% filter(NAME == selected_state)
    } else {
      selected_metro <- input$metroArea
      metro_summary_metrics %>% filter(NAME == selected_metro)
    }
  })
  
  output$plot <- renderPlot({
    req(input$metrics)  # Ensure that a metric is selected
    data_to_plot <- filtered_data()
    
    selected_metric <- input$metrics
    
    # Define y-axis labels based on the selected metric
    y_label <- switch(selected_metric,
                      "Total_Avg_Annual_Pay_Total" = "Average Annual payroll ($1,000) per a firm",
                      "Total_Avg_Annual_Pay_Black_Business" = "Average Annual payroll ($1,000) per a firm",
                      "Total_Avg_Employees_Total" = "Average Employee per a firm",
                      "Total_Avg_Employees_Black_Business" = "Average Employee per a firm",
                      "Total_Sum_of_Firms_Total" = "Number of firms",
                      "Total_Sum_of_Firms_Black_Business" = "Number of firms",
                      "Pay_Annual_Per_Employee_Total" = "Average take home pay for an employee",
                      "Pay_Annual_Per_Employee_Black_Business" = "Average take home pay for an employee",
                      "Percent_Total_Avg_Annual_Pay_BB" = "Percent Black Business in Total Statistic",
                      "Percent_Total_Avg_Employees_BB" = "Percent Black Business in Total Statistic",
                      "Percent_Total_Sum_of_Firms_BB" = "Percent Black Business in Total Statistic")
    
    
    title <- metric_titles[selected_metric]  # Get the title from the mapping
    if(selected_metric %in% c("Pay_Annual_Per_Employee_Total", "Pay_Annual_Per_Employee_Black_Business")) {
      data_to_plot <- data_to_plot %>%
        mutate(Pay_Annual_Per_Employee_Total = Total_Avg_Annual_Pay_Total / Total_Avg_Employees_Total,
               Pay_Annual_Per_Employee_Black_Business = Total_Avg_Annual_Pay_Black_Business / Total_Avg_Employees_Black_Business)
      
    }
    # Handle new percentage metrics
    if(selected_metric == "Percent_Total_Avg_Annual_Pay_BB") {
      data_to_plot <- data_to_plot %>%
        mutate(Percent_Total_Avg_Annual_Pay_BB = (Total_Avg_Annual_Pay_Black_Business / Total_Avg_Annual_Pay_Total) * 100)
    } else if(selected_metric == "Percent_Total_Avg_Employees_BB") {
      data_to_plot <- data_to_plot %>%
        mutate(Percent_Total_Avg_Employees_BB = (Total_Avg_Employees_Black_Business / Total_Avg_Employees_Total) * 100)
    } else if(selected_metric == "Percent_Total_Sum_of_Firms_BB") {
      data_to_plot <- data_to_plot %>%
        mutate(Percent_Total_Sum_of_Firms_BB = (Total_Sum_of_Firms_Black_Business / Total_Sum_of_Firms_Total) * 100)
    }
    
    # # Check if rate of change is selected
    # if(input$rateOfChange) {
    #   # Calculate rate of change
    #   data_to_plot <- data_to_plot %>%
    #     arrange(Year) %>%
    #     mutate(RateOfChange = c(NA, diff(get(selected_metric))))
    #   
    #   plot_title <- paste(title, "- Rate of Change")
    #   y_label <- "Rate of Change"
    #   plot_metric <- "RateOfChange"
    # } else {
    #   plot_title <- title
    #   y_label <- selected_metric
    #   plot_metric <- selected_metric
    # }
    selected_area <- if(input$selectionType == "state") input$state else input$metroArea
    plot_title <- paste(title, "\nin", selected_area)
    
    # Calculate the slope (difference) between consecutive points
    data_to_plot <- data_to_plot %>%
      arrange(Year) %>%
      mutate(Slope = c(NA, diff(as.numeric(get(selected_metric)))),
             SlopeType = ifelse(Slope > 0, "Positive", ifelse(Slope < 0, "Negative", "Flat")))
    
    # Shift the SlopeType column up by one row
    data_to_plot$SlopeType <- c(data_to_plot$SlopeType[-1], NA)
    
    # Create the plot with trend line
    p <- ggplot(data_to_plot, aes_string(x = "Year", y = selected_metric, group = "1")) +
      geom_line(aes(color = SlopeType)) +  # Color lines based on slope type
      scale_color_manual(values = c("Positive" = "green", "Negative" = "red", "Flat" = "blue")) +
      labs(title = plot_title, x = "Year", y = y_label) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),  # Center the title
            plot.margin = margin(10, 10, 10, 10),  # Adjust margins if needed
            legend.position = "bottom")  # Position the legend at the bottom
    
    
    # Custom function to format y-axis as percentages
    percent_format <- function(x) {
      paste0(format(x, nsmall = 1), "%")
    }
    
    # Apply custom percent format to y-axis for percentage metrics
    if(selected_metric %in% c("Percent_Total_Avg_Annual_Pay_BB", "Percent_Total_Avg_Employees_BB", "Percent_Total_Sum_of_Firms_BB")) {
      p <- p + scale_y_continuous(labels = percent_format)
    }
    
    
    
    
    # Render the plot
    p
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
