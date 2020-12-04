library(shiny)
library(dplyr)
library(plotly)
library(tidyr)
library(rsconnect)

co2_emissions <- read.csv("co2-emissions.csv")
continents <- read.csv("continents2.csv")
co2_emissions_conts <- merge(co2_emissions, continents, by.x = "iso_code", by.y = "alpha.3", all.x = TRUE, all.y = TRUE)

#2018 Global CO2 emmissions
golbal_co2_emissions_2018 <- co2_emissions_conts %>% select(iso_code, country, year, co2, region, sub.region, region.code, sub.region.code)
golbal_co2_emissions_2018 <- golbal_co2_emissions_2018 %>% filter(year == '2018' & iso_code != '' & region != 'NA')
golbal_co2_emissions_2018$total <- sum(golbal_co2_emissions_2018$co2)
golbal_co2_emissions_2018$percentage <- round((golbal_co2_emissions_2018$co2 / golbal_co2_emissions_2018$total) * 100,2)
golbal_co2_emissions_2018$percentage <- paste(golbal_co2_emissions_2018$percentage, "%")

#Setting Regions
regions <- golbal_co2_emissions_2018 %>% select(region, co2)
regions <- aggregate(x = regions$co2, by = list(regions$region), FUN = sum)
regions$lables <- regions$Group.1
regions$parents <- ""
regions$sub.region <- ""
regions$countries <- ""
regions$x <- round(regions$x / 1000, 2)
regions$regions <- regions$Group.1
regions <- regions %>% rename("ids" = Group.1,
                              "values" = x)
regions$total <- sum(regions$values)
regions$percentage <- round((regions$values / regions$total) * 100,2)

#Setting Sub Regions
sub_regions <- golbal_co2_emissions_2018 %>% select(sub.region, co2, region)
sub_regions <- aggregate(x = sub_regions$co2, by = list(sub_regions$sub.region, sub_regions$region), FUN = sum)
sub_regions$ids <- sub_regions$Group.1
sub_regions$regions <- sub_regions$Group.2
sub_regions$sub.region <- sub_regions$Group.1
sub_regions$countries <- ""
sub_regions$x <- round(sub_regions$x / 1000, 2)
sub_regions <- sub_regions %>% rename("parents" = Group.2, 
                                      "values" = x,
                                      "lables" = Group.1)
sub_regions$total <- sum(sub_regions$values)
sub_regions$percentage <- round((sub_regions$values / sub_regions$total) * 100,2)

plotly_treepmap <- bind_rows(regions, sub_regions)

#Setting Countries
countries <- golbal_co2_emissions_2018 %>% select(country, co2, sub.region, region)
countries <- aggregate(x = countries$co2, by = list(countries$country, countries$sub.region, countries$region), FUN = sum)
countries$ids <- countries$Group.1
countries$sub.region <- countries$Group.2
countries$countries <- countries$Group.1
countries$x <- round(countries$x / 1000, 2)
countries <- countries %>% rename("parents" = Group.2, 
                                  "values" = x,
                                  "lables" = Group.1,
                                  "regions" = Group.3)
countries$total <- sum(countries$values)
countries$percentage <- round((countries$values / countries$total) * 100,2)

plotly_treepmap <- bind_rows(plotly_treepmap, countries)

#Annual Global CO2 Share
golbal_co2_emissions_2018_top <- golbal_co2_emissions_2018 %>% top_n(4, co2)
top_countries <- c(golbal_co2_emissions_2018_top$country, "United Kingdom")
annual_share_co2_emission <- co2_emissions_conts %>% select(country, share_global_co2, year, population, region)
annual_share_co2_emission <- annual_share_co2_emission %>% filter(year %in% c(1960:2018))
annual_share_co2_emission <- annual_share_co2_emission %>% filter(country %in% top_countries)
annual_share_co2_emission <- annual_share_co2_emission %>% filter(region != "NA")
annual_share_co2_emission <- annual_share_co2_emission %>% filter(share_global_co2 != "NA")
annual_share_co2_emission$share_global_co2 <- round(annual_share_co2_emission$share_global_co2,0)
annual_share_co2_emission <- annual_share_co2_emission %>% filter(share_global_co2 > 0)
annual_share_co2_emission <-  annual_share_co2_emission %>% arrange(desc(year))
annual_share_co2_emission$frame_year <- annual_share_co2_emission$year

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

annual_share_co2_emission <- annual_share_co2_emission %>% accumulate_by(~year)


#CO2 emissions are used to produce
golbal_co2_emissions_details <- co2_emissions_conts %>% select(country, year, cement_co2, coal_co2, flaring_co2, gas_co2, oil_co2)
golbal_co2_emissions_details <- golbal_co2_emissions_details %>% filter(year != 'NA' & country != 'NA')
golbal_co2_emissions_details <- golbal_co2_emissions_details %>% filter(year != '' & country != '')
golbal_co2_emissions_details <- golbal_co2_emissions_details %>% filter(country %in% top_countries)
golbal_co2_emissions_details <- golbal_co2_emissions_details %>% filter(year %in% c(1960:2018))
golbal_co2_emissions_details <-  golbal_co2_emissions_details %>% arrange(desc(year))

# china_golbal_co2_emissions_details <- golbal_co2_emissions_details %>% filter(country == 'China')
# india_golbal_co2_emissions_details <- golbal_co2_emissions_details %>% filter(country == 'India')
# russia_golbal_co2_emissions_details <- golbal_co2_emissions_details %>% filter(country == 'Russia')
# uk_golbal_co2_emissions_details <- golbal_co2_emissions_details %>% filter(country == 'United Kingdom')
# us_golbal_co2_emissions_details <- golbal_co2_emissions_details %>% filter(country == 'United States')

  

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  h1("Who emits the most CO2?"),
  HTML("<p><ul><li>CO2—also known as greenhouse gases—has become a major concern as climate change becomes a bigger issue.</li>
<li>The biggest culprit of CO2 emissions for these countries is burning coal, producing gas and oil.</li>
       <li>We will analyse who emits more co2 globally and also what are biggest culprits for co2 emission in each country.</li></ul></p>"),
  hr(),
  h2("Who emits the most CO2 each year?"),
  HTML("<p><ul><li>In the treemap visualization, the annual CO2 emissions of 2108 are shown by country, and aggregated by region & sub-region</li>
       <li>Each inner rectangle represents a country, which are then nested and colored by region and sub-region.</li>
       <li>The size of each rectangle corresponds to its annual CO2 emissions in 2018</li>
       <li>The emissions shown here relate to the country where CO2 is produced (i.e.production-based CO2) , not to where the goods and services that generate emissions are finally consumed.</li>
       <li>Asia is by far the largest emitter, accounting for 53% of global emissions followed by Americas and Europe.</li>
       <li>China is, by a significant margin, Asia’s and the world’s largest emitter: it emits nearly 10 billion tonnes each year, more than one-quarter of global emissions.</li>
       <li>North America – dominated by the USA – is the second largest regional emitter at 18% of global emissions. It’s followed closely by Europe with 17%.</li>
       <li>Africa and South America are both fairly small emitters: accounting for 3-4% of global emissions each.</li></ul></p>"),
  plotlyOutput("distPlot"),
  hr(),
  h2("How did CO2 emissions change over time?"),
  HTML("<p><ul><li>The distribution of emissions has changed significantly over time.</li>
       <li>The UK was the largest Co2 emitter until 1888, when it was overtaken by the US in 1900's and in the recent year from 2000 China has become the world’s largest emitters.</li>
       <li>UK was the first country to industrialize, contributed to massive improvements in living standards for much of its population.</li>
       <li>Many of the world’s largest emitters today are in Asia. However, Asia’s rapid rise in emissions has only occurred in very recent decades. This too has been a by-product of massive improvements in living standards.</li>
       <li>I have considered top four CO2 emitters in recent year (China, United States, India, Russia and United Kingdom (one of the 28 EU countries) and analysed how co2 emissions changed over years.</li>
       <li>The plot shows co2 emissions from 1960 to 2018, the plot shows United States was the largest co2 emitter and suddenly in 2000's because of massive improvements to living standards.</li></ul></p>"),
  plotlyOutput("distPlot1"),
  hr(),
  h2("How are countries emitting CO2?"),
  HTML("<p><ul><li>The below graph show the CO2 emission for each country to produce Gas, Oil, Flaring, Cement and buring Coal. </li>
       <li>Buring coal for generation of power in most countries contribiutes to most CO2 emissions and next comes production of oil & gas.</li>
       <li>The major culprit of co2 emissions in China is buring coal and producing cement</li>
       <li>The major culprit of co2 emissions in US is producing gas and oil cement</li></ul></p>"),
  plotlyOutput("distPlot2"),
  # plotlyOutput("distPlot3"),
  # plotlyOutput("distPlot4"),
  # plotlyOutput("distPlot5"),
  # plotlyOutput("distPlot6"),
  hr(),
  h2("Conclusion"),
  HTML("<p>Whilst all countries must work collectively, action from the very top emitters will be essential to reduce cabron footproint. China, the USA, India, Russia and 28 countries of the EU account for more than half of global emissions. Without commitment from these largest emitters, the world will not come close to meeting its global targets.</p>"),
  hr(),
  h2("Data Source"),
  a(href="https://github.com/owid/co2-data", "co2 data")
)


# Define server logic required to draw  ----
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    plot_ly(
      type="treemap",
      labels =  plotly_treepmap$lables,
      parents = plotly_treepmap$parents,
      values = plotly_treepmap$values,
      text = paste("<b>Continent</b> = ",plotly_treepmap$regions,
                   "<br><b>Sub Regions</b> = ",plotly_treepmap$sub.region,
                   "<br><b>Country</b> = ", plotly_treepmap$countries,
                   "<br><b>CO2 emissions (billion tons)</b> = ", plotly_treepmap$values,
                   "<br><b>2018 global co2 emission %</b> = ", plotly_treepmap$percentage),
      textinfo = "text",
      hoverinfo = "text",
      colors = "RdYlBu") %>%
      layout( title = list(text = "Who emitted most global CO2 emissions in the world in 2018", xanchor = 'right'))
    
  })
  
  output$distPlot1 <- renderPlotly({
    plot_ly(
      data = annual_share_co2_emission,
      x = ~year,
      y = ~share_global_co2,
      split = ~country,
      frame = ~frame,
      type = 'scatter',
      mode = 'lines',
      line = list(simplyfy = F)
    ) %>% 
      layout( title = list(text = "Global annual CO2 emissions share", xanchor = 'right'),
              yaxis = list (title = "Annual CO2 Share %", autotick = FALSE,
                            tick0 = 0,
                            dtick = 5,
                            ticklen = 30),
              xaxis = list(title = "Year", tickangle = 45)) %>%
      animation_slider(currentvalue = list(prefix = "Year ")) %>%
      animation_opts(frame = 100,transition = 0,redraw = FALSE)
  })
  
  output$distPlot2 <- renderPlotly({
    ##
    p11 <- plot_ly(data = golbal_co2_emissions_details) %>%
      add_lines(x=~year, y=~cement_co2, color = ~country, visible = "TRUE", legendgroup=~country)%>%
      layout(annotations = list(list(x = 0 , y = 1.2, text = "Co2 Emissions from Cement", showarrow = F, xref='paper', yref='paper', font = list(size = 12, color = 'blue'))),
             yaxis = list(title = "million tons"), xaxis = list(title = "Year"))
    
    p12 <- plot_ly(data = golbal_co2_emissions_details) %>%
      add_lines(x=~year, y=~coal_co2, color = ~country,visible = "TRUE", legendgroup=~country, showlegend = FALSE) %>%
      layout(annotations = list(list(x = 0 , y = 1.3, text = "Co2 Emissions from Coal", showarrow = F, xref='paper', yref='paper', font = list(size = 12, color = 'blue'))),
             yaxis = list(title = "million tons"), xaxis = list(title = "Year"))
    
    p13 <- plot_ly(data = golbal_co2_emissions_details) %>%
      add_lines(x=~year, y=~flaring_co2, color = ~country,visible = "TRUE", legendgroup=~country, showlegend = FALSE)%>%
      layout(annotations = list(list(x = 0 , y = 1.2, text = "Co2 Emissions from Flaring", showarrow = F, xref='paper', yref='paper', font = list(size = 12, color = 'blue'))),
             yaxis = list(title = "million tons"), xaxis = list(title = "Year"))
    
    p14 <- plot_ly(data = golbal_co2_emissions_details) %>%
      add_lines(x=~year, y=~gas_co2, color = ~country,visible = "TRUE", legendgroup=~country, showlegend = FALSE)%>%
      layout(annotations = list(list(x = 0 , y = 2, text = "Co2 Emissions from Gas", showarrow = F, xref='paper', yref='paper', font = list(size = 12, color = 'blue'))),
             yaxis = list(title = "million tons"), xaxis = list(title = "Year"))
    
    p15 <- plot_ly(data = golbal_co2_emissions_details) %>%
      add_lines(x=~year, y=~oil_co2, color = ~country,visible = "TRUE", legendgroup=~country, showlegend = FALSE)%>%
      layout(annotations = list(list(x = 0 , y = 1.2, text = "Co2 Emissions from Oil", showarrow = F, xref='paper', yref='paper', font = list(size = 12, color = 'blue'))),
             yaxis = list(title = "million tons"), xaxis = list(title = "Year"))
    
    subplot(p11, p12, p13, p14, p15, nrows = 3, margin = 0.1, titleY = TRUE, shareX = TRUE, titleX = TRUE)
    
  })
  
  # output$distPlot2 <- renderPlotly({
  #   china_plot <- plot_ly(data = china_golbal_co2_emissions_details, x=~year, y=~coal_co2, name = "Coal", type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#F5FF8D') %>%
  #     add_trace(y = ~cement_co2, name = 'Cement', fillcolor = '#50CB86') %>%
  #     add_trace(y = ~flaring_co2, name = 'Flaring', fillcolor = '#4C74C9') %>%
  #     add_trace(y = ~oil_co2, name = 'Oil', fillcolor = '#700961') %>%
  #     add_trace(y = ~gas_co2, name = 'Gas', fillcolor = '#312F44')
  #   
  #   china_plot <- china_plot %>% 
  #     layout(yaxis = list(title = "% of CO2 emissions for Production each year"), 
  #            xaxis = list(title = "Year"), 
  #            title = list(text = "China's % of CO2 emissions each year for production", xanchor = 'right'))
  # })
  
  # output$distPlot3 <- renderPlotly({
  #   india_plot <- plot_ly(data = india_golbal_co2_emissions_details, x=~year, y=~coal_co2, name = "Coal", type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#F5FF8D') %>%
  #     add_trace(y = ~cement_co2, name = 'Cement', fillcolor = '#50CB86') %>%
  #     add_trace(y = ~flaring_co2, name = 'Flaring', fillcolor = '#4C74C9') %>%
  #     add_trace(y = ~oil_co2, name = 'Oil', fillcolor = '#700961') %>%
  #     add_trace(y = ~gas_co2, name = 'Gas', fillcolor = '#312F44')
  #   
  #   india_plot <- india_plot %>% 
  #     layout(yaxis = list(title = "% CO2 emissions to produce"), 
  #            xaxis = list(title = "Year"), 
  #            title = list(text = "India's % of CO2 emissions each year for production", xanchor = 'right'))
  # })
  # 
  # output$distPlot4 <- renderPlotly({
  #   russia_plot <- plot_ly(data = russia_golbal_co2_emissions_details, x=~year, y=~coal_co2, name = "Coal", type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#F5FF8D') %>%
  #     add_trace(y = ~cement_co2, name = 'Cement', fillcolor = '#50CB86') %>%
  #     add_trace(y = ~flaring_co2, name = 'Flaring', fillcolor = '#4C74C9') %>%
  #     add_trace(y = ~oil_co2, name = 'Oil', fillcolor = '#700961') %>%
  #     add_trace(y = ~gas_co2, name = 'Gas', fillcolor = '#312F44')
  #   
  #   russia_plot <- russia_plot %>% 
  #     layout(yaxis = list(title = "% CO2 emissions to produce"), 
  #            xaxis = list(title = "Year"), 
  #            title = list(text = "Russia's % of CO2 emissions each year for production", xanchor = 'right'))
  # })
  # 
  # output$distPlot5 <- renderPlotly({
  #   uk_plot <- plot_ly(data = uk_golbal_co2_emissions_details, x=~year, y=~coal_co2, name = "Coal", type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#F5FF8D') %>%
  #     add_trace(y = ~cement_co2, name = 'Cement', fillcolor = '#50CB86') %>%
  #     add_trace(y = ~flaring_co2, name = 'Flaring', fillcolor = '#4C74C9') %>%
  #     add_trace(y = ~oil_co2, name = 'Oil', fillcolor = '#700961') %>%
  #     add_trace(y = ~gas_co2, name = 'Gas', fillcolor = '#312F44')
  #   
  #   uk_plot <- uk_plot %>% 
  #     layout(yaxis = list(title = "% CO2 emissions to produce"), 
  #            xaxis = list(title = "Year"), 
  #            title = list(text = "United Kingdom's % of CO2 emissions each year for production", xanchor = 'right'))
  # })
  # 
  # output$distPlot6 <- renderPlotly({
  #   us_plot <- plot_ly(data = us_golbal_co2_emissions_details, x=~year, y=~coal_co2, name = "Coal", type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#F5FF8D') %>%
  #     add_trace(y = ~cement_co2, name = 'Cement', fillcolor = '#50CB86') %>%
  #     add_trace(y = ~flaring_co2, name = 'Flaring', fillcolor = '#4C74C9') %>%
  #     add_trace(y = ~oil_co2, name = 'Oil', fillcolor = '#700961') %>%
  #     add_trace(y = ~gas_co2, name = 'Gas', fillcolor = '#312F44')
  #   
  #   us_plot <- us_plot %>% 
  #     layout(yaxis = list(title = "% CO2 emissions to produce"), 
  #            xaxis = list(title = "Year"), 
  #            title = list(text = "United States's % of CO2 emissions each year for production", xanchor = 'right'))
  # })
    
}
shinyApp(ui, server)