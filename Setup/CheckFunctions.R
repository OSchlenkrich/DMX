# Diagnostic Plots


# Plot with Number of NAs per variable ----
NA_plot = function(data, name_data, var_selection = NULL) {
  data = data %>% 
    select(-country_name, -year) 
  
  if (is.null(var_selection) == F ){
    data = data %>% 
      select_at(vars(matches(var_selection)))
    }
  
  na_col = sapply(data, function(x) sum(is.na(x)))
  na_col_plot = na_col %>%
    melt() %>%
    mutate("variable" = rownames(.),
           variable= fct_reorder(variable, value, .desc = T)) 
  
  NA_plot = ggplot(na_col_plot, aes(x=variable, y=value)) + 
    geom_bar(stat="identity") + 
    theme_bw() + 
    ggtitle(paste(name_data, "- Missings for each Variable")) +
    theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5))
  
  return(NA_plot)
}

# Plot with Number of Coders per variable ----
Coders_plot = function(data, name_data) {
  Nr_coder_df = data %>% 
    select_at(vars(ends_with("_nr"))) %>%
    na.omit() %>% 
    melt(id.vars=NULL)
  
  ggplot(Nr_coder_df, aes(x=as.factor(value), fill=as.factor(value))) + 
    geom_bar(stat="count", position = "dodge") + 
    theme_bw() + 
    scale_x_discrete(name = "Number of Coders", labels=c("1","2","3","4","5 or \nmore")) + 
    scale_fill_manual("legend", values = c("1" = "red", "2" = "red", "3" = "red", "4" = "darkgreen", "5" = "darkgreen")) + 
    facet_wrap(variable ~ .) + 
    ggtitle(paste(name_data, "- Number of Coders for each Variable")) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
}

# Density Plot for each Variable ----
Density_plot = function(data, name_data, var_selection = NULL) {
  plot_data = data %>% 
    select_at(vars(ends_with("facto"), ends_with("core"))) 
  
  if (is.null(var_selection) == F ){
    plot_data = data %>% 
      select_at(vars(matches(var_selection)))
  }
  
  plot_data = plot_data %>% 
    melt(id.vars = NULL)
  
  ggplot(plot_data, aes(x=value, fill=variable)) + 
    geom_density() + 
    facet_wrap(~variable) + 
    theme_bw() + 
    ggtitle(paste(name_data, "- Density for each Variable")) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
}

# Boxplot for each Variable ----
Box_plot = function(data, name_data) {
  plot_data = data %>% 
    select_at(vars(ends_with("facto"), ends_with("core"))) %>% 
    melt(id.vars = NULL)
  
  ggplot(plot_data, aes(y=value, x=variable, fill=variable)) + 
    geom_boxplot() + 
    xlab("") + 
    theme_bw() + 
    ggtitle(paste(name_data, "- Density for each Variable")) +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90), legend.position = "none")

}


# Plot Sample Countries ----
Plot_Countries = function(dataset) {
  
  sample_countries = c("Germany", "Italy", "Sweden","United States of America", 
                       "United Kingdom", "Poland", "Russia", "Mexico", "Colombia", 
                       "South Africa", "Democratic Republic of Congo", "China",
                       "Tunisia", "Syria", "Mexico", "Netherlands")
  
  plot_data = dataset %>%
    filter(country_name %in% sample_countries) %>%
    select_at(vars(country_name, year,  ends_with("core"))) %>%
    melt(id.vars=c("country_name", "year"))
  
  ggplot(plot_data, aes(x=year, y=value, col=variable)) + 
    geom_line(size=1) + 
    theme_bw() + 
    facet_wrap(country_name~.) + 
    ylim(0,1) +
    scale_x_continuous(name="", breaks=seq(1900,2020,10)) +  
    theme(axis.text.x = element_text(angle=90, size=7), strip.text.x = element_text(size = 8), 
          legend.position = "bottom", legend.title = element_blank())
  
}

# Plot Regions ----
Plot_Regions = function(dataset) {
  
  source("Setup/Regions/create_regions_data.R")

  plot_data = dataset %>%
    left_join(dem_matrix_regions, by="country_name") %>% 
    group_by(regions, year) %>%
    summarise_at(vars(ends_with("core")), mean, na.rm=T) %>%
    melt(id.vars=c("regions", "year"))
  ggplot(plot_data, aes(x=year, y=value, col=variable)) + 
    geom_line(size=1) + 
    theme_bw() + 
    facet_wrap(~regions) + 
    theme(axis.text.x = element_text(angle=90)) + 
    ylim(0,1) +
    scale_x_continuous(name="", breaks=seq(1900,2020,10)) +  
    theme(axis.text.x = element_text(angle=90, size=7), strip.text.x = element_text(size = 8),
          legend.position = "bottom", legend.title = element_blank())
}


# Table Summary ----
table_sum = function(dataset, var_selection = NULL, rounding = F) {
  dataset = dataset %>% 
    select_at(vars(-country_name, -year,-matches("_nr"), -matches("class"))) 
  
  if (is.null(var_selection) == F ){
    dataset = dataset %>% 
      select_at(vars(matches(var_selection)))
  }
  
  dataset = dataset %>% 
    melt(id.vars=NULL)
  
  table_data = dataset %>%
    group_by(variable) %>%
    summarise_all(funs(mean, min, max), na.rm=T) %>%
    na.omit()
  
  if (rounding == T) {
    table_data = table_data %>%  
    mutate_if(is.numeric, funs(round(.,3))) 
  }
  
  return(flextable(table_data))  
}

# 15 Field Matrix as Heat Map ----

create_plot = function(country, plot_data, variables) {

  vis_15_Felder <- function(country, year_used, complete_data, variables) {
    
      plot_heat <- complete_data %>%
        filter(country_name == country, year == year_used)
      
      if (dim(plot_heat)[1] == 0)  {
        plot_heat[1,1] = country
      }
      
      
      plot_heat <- plot_heat %>%
        select(-country_name, -year) %>%
        select_at(vars(ends_with(variables), -matches("class"))) %>%
        t() %>%
        data.table() %>%
        rename(DQ = colnames(.[1]))  %>%        
        mutate(Dim = c(rep(c("F", "G", "K"), 5), c("F", "G", "K"),rep("Inst", 6)),
               Int = c(rep(c("EV", "IV", "ME", "RG", "RS"), each=3), rep("Dim",3), c("EV", "IV", "ME", "RG", "RS", "Dim"))
        )
      
      yaxis <- factor(c("Dim", "RS", "RG", "ME", "IV", "EV"), levels=c("EV", "IV", "ME", "RG", "RS", "Dim"))
      xaxis <- factor(c("F", "G", "K", "Inst"), levels=c("F", "G", "K", "Inst"))
      
      ggplot(data = plot_heat, aes(x = Dim, y = Int, fill=DQ)) +  
        geom_raster () + 
        scale_fill_gradient2(low = "red", high = "green", mid="yellow",
                                              midpoint = 0.6, limit = c(0,1), space = "Lab", name="DQ") + 
        geom_text(aes(Dim, Int, label = round(DQ,2)), color = "black", size = 4, hjust=0.5, vjust=0.5) + 
        scale_y_discrete(limits=yaxis, expand = c(0, 0)) +  
        scale_x_discrete(limits=xaxis, expand = c(0, 0), position = "top") +  
        ggtitle(paste(year_used)) + 
        theme(legend.position = "none", axis.title = element_blank(), 
              plot.title = element_text(size=10, hjust=0.5)) + 
        geom_rect(aes(xmin=0.5, xmax=4.5, ymin=0.5, ymax=1.5), fill=NA, color="black", size=1.5) + 
        geom_rect(aes(xmin=3.5, xmax=4.5, ymin=0.5, ymax=6.5), fill=NA, color="black", size=1.5)
    }
  
  sequenz = seq(1900, 2010, 10)
  
  if (any(grepl("_corr", names(plot_data))) == T) {
    plot_title = "With Corruption"
  } else {
    plot_title = NULL 
  }
  
  plotlist = list()
  for (i in 1:length(sequenz)) {
    plotlist[[i]] = vis_15_Felder(country, sequenz[i], plot_data, variables)
  }
  do.call("grid.arrange", c(plotlist, ncol=4, top=paste(country, plot_title)))
}
