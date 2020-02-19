library(ggplot2)
library(plotly)
library(reshape2)
library(yaml)
library(dplyr)
library(gridExtra)
library(data.table)

democracymatrix = fread("website_data_v2.csv")

# Names of countries and regions
choices = list(regions=democracymatrix %>% 
                 pull(country) %>%
                 unique() %>% 
                 .[grepl('[[:digit:]]',.) == T] %>% 
                 as.character,
               countries = 
                 democracymatrix %>% 
                 pull(country) %>%
                 unique() %>% 
                 .[grepl('[[:digit:]]',.) == F] %>% 
                 as.character
               )



server = function(input, output) {

  #15-Field-Matrix
  vis_15_Felder <- function(country_sel, year_used, radio) {
    
    low = "red"
    mid = "yellow"
    high="green"
    midpoint = 0.5
    limit=c(0,1)
    
    if (radio == "Core Measurement") {
      variables_sel = "_core"
    }
    if (radio == "Context Measurement") {
      variables_sel = "_context"
    }
    if (radio == "Trade-Off Measurement") {
      variables_sel = "_trade_off"
      low = "#58ACFA"
      mid = "#0040FF"
      high= "#0101DF"
      midpoint= 0.8
      limit = c(0.4,1)
    }
    
    plot_heat <- democracymatrix %>%
      filter(country == country_sel, year == year_used) %>%
      select_at(vars(ends_with(variables_sel)))
    
    if (dim(plot_heat)[1] == 0)  {
      plot_heat[1,1] = country_sel
    }
    
    
    df_grid = data.frame(Dim = c(rep(c("Freedom", "Equality", "Control"), 5),rep("Institution", 5), c("Freedom", "Equality", "Control", "Institution")),
                         Int = c(rep(c("Procedurs\n of \nDecision", "Intermediate\n Sphere", "Communication", "Rights", "Rules\n settlement"), each=3), c("Procedurs\n of \nDecision", "Intermediate\n Sphere", "Communication", "Rights", "Rules\n settlement", rep("Dimension",4)))
    )
    
    df_grid$merge = paste(df_grid$Dim, df_grid$Int)
    
    df_grid$value[df_grid$merge == "Freedom Procedurs\n of \nDecision"] =  plot_heat$decision_freedom
    df_grid$value[df_grid$merge == "Equality Procedurs\n of \nDecision"] =  plot_heat$decision_equality
    df_grid$value[df_grid$merge == "Control Procedurs\n of \nDecision"] =  plot_heat$decision_control
    
    df_grid$value[df_grid$merge == "Freedom Intermediate\n Sphere"] =  plot_heat$intermediate_freedom
    df_grid$value[df_grid$merge == "Equality Intermediate\n Sphere"] =  plot_heat$intermediate_equality
    df_grid$value[df_grid$merge == "Control Intermediate\n Sphere"] =  plot_heat$intermediate_control
    
    df_grid$value[df_grid$merge == "Freedom Communication"] =  plot_heat$communication_freedom
    df_grid$value[df_grid$merge == "Equality Communication"] =  plot_heat$communication_equality
    df_grid$value[df_grid$merge == "Control Communication"] =  plot_heat$communication_control
    
    df_grid$value[df_grid$merge == "Freedom Rights"] =  plot_heat$rights_freedom
    df_grid$value[df_grid$merge == "Equality Rights"] =  plot_heat$rights_equality
    df_grid$value[df_grid$merge == "Control Rights"] =  plot_heat$rights_control
    
    df_grid$value[df_grid$merge == "Freedom Rules\n settlement"] =  plot_heat$rule_settlement_freedom
    df_grid$value[df_grid$merge == "Equality Rules\n settlement"] =  plot_heat$rule_settlement_equality
    df_grid$value[df_grid$merge == "Control Rules\n settlement"] =  plot_heat$rule_settlement_control
    
    df_grid$value[df_grid$merge == "Institution Procedurs\n of \nDecision"] =  plot_heat$decision_inst_index
    df_grid$value[df_grid$merge == "Institution Intermediate\n Sphere"] =  plot_heat$intermediate_inst_index
    df_grid$value[df_grid$merge == "Institution Communication"] =  plot_heat$communication_inst_index
    df_grid$value[df_grid$merge == "Institution Rights"] =  plot_heat$rights_inst_index
    df_grid$value[df_grid$merge == "Institution Rules\n settlement"] =  plot_heat$rule_settlement_inst_index
    
    df_grid$value[df_grid$merge == "Freedom Dimension"] =  plot_heat$freedom_dim_index
    df_grid$value[df_grid$merge == "Equality Dimension"] =  plot_heat$equality_dim_index
    df_grid$value[df_grid$merge == "Control Dimension"] =  plot_heat$control_dim_index
    df_grid$value[df_grid$merge == "Institution Dimension"] =  plot_heat$total_index
    
    yaxis <- factor(c("Dimension", "Rules\n settlement", "Rights", "Communication", "Intermediate\n Sphere", "Procedurs\n of \nDecision"), levels=c("Procedurs\n of \nDecision", "Intermediate\n Sphere", "Communication", "Rights", "Rules\n settlement", "Dimension"))
    xaxis <- factor(c("Freedom", "Equality", "Control", "Institution"), levels=c("Freedom", "Equality", "Control", "Institution"))
    
    plot_heat_ma = ggplot(data = df_grid, aes(x = Dim, y = Int, fill=value)) +  
      geom_raster ()  + 
      geom_text(aes(Dim, Int, label = round(value,2)), color = "black", size = 4, hjust=0.5, vjust=0.5) + 
      scale_y_discrete(limits=yaxis, expand = c(0, 0)) +  scale_x_discrete(limits=xaxis, expand = c(0, 0), position = "top") +  
      ggtitle(paste(country_sel, year_used)) + theme(legend.position = "none", axis.title = element_blank(), plot.title = element_text(size=10, hjust=0.5), axis.text.x = element_text(face="bold"), axis.text.y = element_text(face="bold")) + 
      geom_rect(aes(xmin=0.5, xmax=4.5, ymin=0.5, ymax=1.5), fill=NA, color="black", size=1.5) + geom_rect(aes(xmin=3.5, xmax=4.5, ymin=0.5, ymax=6.5), fill=NA, color="black", size=1.5) + scale_fill_gradient2(low = low, high = high, mid=mid,
                                                                                                                                                                                                                 midpoint = midpoint, limit = limit, space = "Lab", name="DQ")
    return(plot_heat_ma)
    
  }
  
  # radarplot
  get_radar_plot = function(country_sel, year_used, radio, plottype) {
    if (radio == "Core Measurement") {
      variables_sel = "_core"
    }
    if (radio == "Context Measurement") {
      variables_sel = "_context"
    }
    if (radio == "Trade-Off Measurement") {
      variables_sel = "_trade_off"
    }
    
    plot_radar <- democracymatrix %>%
      subset(country %in% country_sel) %>%
      filter(year == year_used)  %>%
      select_at(vars(country, ends_with(variables_sel)))
    
    if (dim(plot_radar)[1] == 0)  {
      plot_radar[1,1] = country_sel
    }
    
    sub_country_names = unique(plot_radar$country) 
    
    
    df_grid = data.frame(Dim = c(rep(c("Freedom", "Equality", "Control"), 5),rep("Institution", 5), c("Freedom", "Equality", "Control", "Institution")),
                         Int = c(rep(c("Procedurs\n of \nDecision", "Intermediate\n Sphere", "Communication", "Rights", "Rules\n settlement"), each=3), c("Procedurs\n of \nDecision", "Intermediate\n Sphere", "Communication", "Rights", "Rules\n settlement", rep("Dimension",4)))
    )
    
    df_grid$merge = paste(df_grid$Dim, df_grid$Int)
    
    for (i in 1:length(sub_country_names)) {
      i = i + 3
      df_grid[df_grid$merge == "Freedom Procedurs\n of \nDecision", i] =  plot_radar$decision_freedom[i-3]
      df_grid[df_grid$merge == "Equality Procedurs\n of \nDecision", i] =  plot_radar$decision_equality[i-3]
      df_grid[df_grid$merge == "Control Procedurs\n of \nDecision", i] =  plot_radar$decision_control[i-3]
      
      df_grid[df_grid$merge == "Freedom Intermediate\n Sphere", i] =  plot_radar$intermediate_freedom[i-3]
      df_grid[df_grid$merge == "Equality Intermediate\n Sphere", i] =  plot_radar$intermediate_equality[i-3]
      df_grid[df_grid$merge == "Control Intermediate\n Sphere", i] =  plot_radar$intermediate_control[i-3]
      
      df_grid[df_grid$merge == "Freedom Communication", i] =  plot_radar$communication_freedom[i-3]
      df_grid[df_grid$merge == "Equality Communication", i] =  plot_radar$communication_equality[i-3]
      df_grid[df_grid$merge == "Control Communication", i] =  plot_radar$communication_control[i-3]
      
      df_grid[df_grid$merge == "Freedom Rights", i] =  plot_radar$rights_freedom[i-3]
      df_grid[df_grid$merge == "Equality Rights", i] =  plot_radar$rights_equality[i-3]
      df_grid[df_grid$merge == "Control Rights", i] =  plot_radar$rights_control[i-3]
      
      df_grid[df_grid$merge == "Freedom Rules\n settlement", i] =  plot_radar$rule_settlement_freedom[i-3]
      df_grid[df_grid$merge == "Equality Rules\n settlement", i] =  plot_radar$rule_settlement_equality[i-3]
      df_grid[df_grid$merge == "Control Rules\n settlement", i] =  plot_radar$rule_settlement_control[i-3]
      
      df_grid[df_grid$merge == "Institution Procedurs\n of \nDecision", i] =  plot_radar$decision_inst_index[i-3]
      df_grid[df_grid$merge == "Institution Intermediate\n Sphere", i] =  plot_radar$intermediate_inst_index[i-3]
      df_grid[df_grid$merge == "Institution Communication", i] =  plot_radar$communication_inst_index[i-3]
      df_grid[df_grid$merge == "Institution Rights", i] =  plot_radar$rights_inst_index[i-3]
      df_grid[df_grid$merge == "Institution Rules\n settlement", i] =  plot_radar$rule_settlement_inst_index[i-3]
      
      df_grid[df_grid$merge == "Freedom Dimension", i] =  plot_radar$freedom_dim_index[i-3]
      df_grid[df_grid$merge == "Equality Dimension", i] =  plot_radar$equality_dim_index[i-3]
      df_grid[df_grid$merge == "Control Dimension", i] =  plot_radar$control_dim_index[i-3]
      df_grid[df_grid$merge == "Institution Dimension", i] =  plot_radar$total_index[i-3]
    }
    
    
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,1)
      )
    )
    margin = list(t = 50)
    legend = list(orientation = 'h')
    
    # reorder data
    if (plottype == "fields") {
      df_grid = df_grid %>%
        filter(Int != "Dimension",
               Dim != "Institution")
      df_grid = df_grid[order(15:1),]
      df_grid = df_grid[order(df_grid$Dim),]  
      showlegend = T
    } else {
      df_grid = df_grid %>%
        filter(Int == "Dimension",
               Dim != "Institution")
      showlegend = T
    }
    
    plotly_radar <- plot_ly(
      type = 'scatterpolar',
      r = df_grid[,4],
      theta = paste(df_grid$Int, df_grid$Dim, sep="\n"),
      fill = 'toself',
      name = paste(sub_country_names[1], year_used, paste("\n",radio, sep=""))
    ) %>% 
      layout(polar=polar, showlegend= showlegend, margin = margin, legend=legend)
    
    
    if (length(sub_country_names) > 1) {
      for (i in 2:length(sub_country_names)) {
        i = i +3
        plotly_radar = plotly_radar %>% add_trace(
          r = df_grid[,i],
          theta = paste(df_grid$Int, df_grid$Dim, sep="\n"),
          name = paste(sub_country_names[i-3], year_used, paste("\n",radio, sep=""))
        )
      }     
    }
    
    return(plotly_radar)
  }
  
  
  # countygraph
  countrygraph = function(country_sel, variables) {
    years_complete = data.frame(country = country_sel, year = seq(1900, max(democracymatrix$year)))
    
    country_data = subset(democracymatrix, country == country_sel, select=c("country", "year", variables))
    country_data_filled = merge(country_data, years_complete, by=c("country", "year"), all.y=T)
    country_data_melt = melt(country_data_filled, id.vars = c("country", "year"))
    
    myplot_country = ggplot(country_data_melt, aes(x=year, y=value, col=variable)) + geom_line(size=1.2) + theme_bw() +
      ylab("value") + ylim(0,1) + ggtitle(paste("Country graph", " ", country_sel)) + scale_x_continuous(breaks=seq(1900,2010,10)) + 
      theme(axis.text.x = element_text(angle=90))
    return(myplot_country)
  }
  
  country_sel = c("Germany", "France")
  variable = "total_index_core"
# variable graph ----
  variablegraph = function(country_sel, variable) { 
    years_complete = data.frame(country = country_sel, year = rep(seq(1900, max(democracymatrix$year)),each=length(country_sel)))
    
    variable_data = democracymatrix %>% 
      filter(country %in% country_sel) %>% 
      select(country, year, variable = variable)
    variable_data_filled = merge(variable_data, years_complete, by=c("country", "year"), all.y=T)
    
    myplot_var = ggplot(variable_data_filled, aes(x=year, y=variable, col=country)) + geom_line(size=1.2) + theme_bw() +
      ylab("value") + ylim(0,1) + ggtitle(paste("Variable Graph", " ", variable)) + scale_x_continuous(breaks=seq(1900,2010,10)) + 
      theme(axis.text.x = element_text(angle=90))
    return(myplot_var)
  }
  
  #reactive
  values <- reactiveValues()
  
  observeEvent(input$country15,{
    values$country <- input$country15
    values$year <- input$year15
    values$radio <- input$radio15
  })
  observeEvent(input$year15,{
    values$country <- input$country15
    values$year <- input$year15
    values$radio <- input$radio15
  })  
  observeEvent(input$radio15,{
    values$country <- input$country15
    values$year <- input$year15
    values$radio <- input$radio15
  })
  
  observeEvent(input$country16,{
    values$country16 <- input$country16
  })
  observeEvent(input$year16,{
    values$year16 <- input$year16
  })  
  observeEvent(input$radio16,{
    values$radio16 <- input$radio16
  })
  
  observeEvent(input$country17,{
    values$country17 <- input$country17
  })
  observeEvent(input$year17,{
    values$year17 <- input$year17
  })  
  observeEvent(input$radio17,{
    values$radio17 <- input$radio17
  })
  
  observeEvent(input$country_radar,{
    values$country_radar <- input$country_radar
  })
  observeEvent(input$year_radar,{
    values$year_radar <- input$year_radar
  })
  observeEvent(input$radio_radar,{
    values$radio_radar <- input$radio_radar
  })
  
  observeEvent(input$country_cg,{
    values$country_cg <- input$country_cg
  })
  observeEvent(input$variable_cg,{
    values$variable_cg <- input$variable_cg
  })
  
  observeEvent(input$country_vg,{
    values$country_vg <- input$country_vg
  })
  observeEvent(input$variable_vg,{
    values$variable_vg <- input$variable_vg
  })
  
  # heatmaps
  output$heat15 = renderPlotly({
    req(c(values$country, values$year, values$radio), cancelOutput = T)
    
    
    ggplotly(vis_15_Felder(values$country, values$year, values$radio)) %>%
      layout(xaxis = list(side ="top"))
  })
  
  output$class_output15 = renderText({
    req(c(values$country, values$year, values$radio), cancelOutput = T)
    
    if (values$radio == "Core Measurement") {
      variables_sel = "_core"
    }
    if (values$radio == "Context Measurement") {
      variables_sel = "_context"
    }
    if (values$radio == "Trade-Off Measurement") {
      variables_sel = "_trade_off"
    }
    
    classification = democracymatrix %>%
      filter(country==values$country, year == values$year)  %>%
      select_at(vars(ends_with(variables_sel))) %>%
      select_at(vars(classification = matches("classification"))) %>% 
      pull(classification)
    
    classification[classification == 1] = "Autocracy"
    classification[classification == 2] = "Hybrid Regime"
    classification[classification == 3] = "Deficient Democracy"
    classification[classification == 4] = "Working Democracy"
    
    return(paste(values$country, values$year, ":", classification))
  })
  
  output$heat16 = renderPlot({
    req(c(values$country16, values$year16, values$radio16), cancelOutput = T)
    
    country1 = vis_15_Felder(values$country16, values$year16, values$radio16)
    
    country2 = vis_15_Felder(values$country17, values$year17, values$radio17)
    
    gridExtra::grid.arrange(country1, country2, nrow=1)

  })
  
  output$class_output16 = renderText({
    req(c(values$country16, values$year16, values$radio16), cancelOutput = T)
    
    
    if (values$radio16 == "Core Measurement") {
      variables_sel = "_core"
    }
    if (values$radio16 == "Context Measurement") {
      variables_sel = "_context"
    }
    if (values$radio16 == "Trade-Off Measurement") {
      variables_sel = "_trade_off"
    }
    
    classification = democracymatrix %>%
      filter(country==values$country16, year == values$year16)  %>%
      select_at(vars(ends_with(variables_sel))) %>%
      select_at(vars(classification = matches("classification")))
    return(paste(values$country16, values$year16, ":",classification$classification))
  
  })
  
  output$class_output17 = renderText({
    req(c(values$country17, values$year17, values$radio17), cancelOutput = T)
    
    
    if (values$radio17 == "Core Measurement") {
      variables_sel = "_core"
    }
    if (values$radio17 == "Context Measurement") {
      variables_sel = "_context"
    }
    if (values$radio17 == "Trade-Off Measurement") {
      variables_sel = "_trade_off"
    }
    
    classification17 = democracymatrix %>%
      filter(country==values$country17, year == values$year17)  %>%
      select_at(vars(ends_with(variables_sel))) %>%
      select_at(vars(classification = matches("classification")))
    return(paste(values$country17, values$year17, ":", classification17$classification))
  })
  
  
  # radar
  output$radargraph = renderPlotly(
    get_radar_plot(values$country_radar, values$year_radar,  values$radio_radar, "fields")
  )
  output$radargraph_dim = renderPlotly(
    get_radar_plot(values$country_radar, values$year_radar,  values$radio_radar, "dim")
  )
  
  
  # graphs
  output$countrygraph = renderPlotly(
    ggplotly(countrygraph(values$country_cg, values$variable_cg))
  )
  
  output$variablegraph = renderPlotly(
    ggplotly(variablegraph(values$country_vg, values$variable_vg))
  )
  
  # dynamic ui
  
  output$SecondCountry <- renderUI({
    selectInput("country16", "Select Country", choices=c("Choose countries"="", choices),
                selected = values$country, multiple = F)
  })
  output$SecondYear <- renderUI({
    selectInput("year16", "Select Year", choices=c("Choose year"="", unique(democracymatrix$year)),
                selected = values$year, multiple = F)
  })
  output$SecondRadio <- renderUI({
    radioButtons("radio16", "Select Type of Measurment", 
                 choices = c("Core Measurement", "Context Measurement", "Trade-Off Measurement"), 
                 selected = values$radio)
    
  })
  
}
