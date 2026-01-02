#----------------------------------------------------------------------------------------------------------
# PROJECT TITLE: EU Trade Dependency Analysis: Trade Dependencies and Economic Resilience 
#                Across Global Partners (2015-2024)
#
# DESCRIPTION:   Examining trade dependencies, Brexit impacts, and economic resilience across
#                Europe's top 4 economies and 10 global partners (2015-2024) using UN Comtrade data
#
# AUTHOR:        Chukwudi Samuel Ogbuta    
#
# DATE CREATED:  21-11-2025
#
# LAST MODIFIED: 30-12-2025
#
# INSTITUTION:   Université Marie et Louis Pasteur
#
# COURSE:        Data Analysis and Softwares
#----------------------------------------------------------------------------------------------------------

#---------------------------------------SECTION 1: LOAD PACKAGES & DATA IMPORT
#Function to only install and load packages not already installed and loaded
use_packages <- function(packages){
  for (package in packages){
    
    #Install uninstalled packages for this script
    if(!requireNamespace(package)){
      install.packages(package)}
    
    #Load unloaded libraries into the environment
    if(!package %in% (.packages())){
      library(package, character.only = TRUE)} #character.only=TRUE allows R to read value stored in package variable
  }
}

#List of required packages
packages <- c("tidyverse", "readxl", "gridExtra", "flextable")

#Call function
use_packages(packages)

#Confirmation to see loaded packages
print(.packages())

#Ensure large numbers are not represented as exponential in table viewer
options(scipen = 999)

#Import Data
trade <- read_excel("C:/Users/Ogbuta/OneDrive/Masters/First Year/First Semester/R Project/TradeData.xlsx")

#---------------------------------------SECTION 1: END


#---------------------------------------SECTION 2: DATA CLEANING AND STANDARDIZATION
#Overview of data [rows, columns, column types]
glimpse(trade)

#Reduce to key variables for analysis
trade <- trade %>% 
  select(reporterISO, partnerISO, refYear, cmdCode, flowDesc, primaryValue) %>% 
  set_names("reporter_country", "partner_country", "year", "product_code",
           "trade_flow", "trade_value_usd") %>% 
  mutate(product = case_when(product_code=="08" ~"Fruits", #map product code to product name
                             product_code=="10" ~"Cereals",
                             product_code=="26" ~"Minerals",
                             product_code=="27" ~"Fuels",
                             product_code=="30" ~"Pharma",
                             product_code=="39" ~"Plastics",
                             product_code=="63" ~"Textiles",
                             product_code=="84" ~"Machinery",
                             product_code=="85" ~"Electronics",
                             product_code=="87" ~"Vehicles")) %>% 
  relocate(product, .before=product_code) %>% 
  select(-product_code)

#Check for Null Values in data-set
anyNA(trade)

#Check for duplicated records
anyDuplicated(trade)

#Quick summary of data
summary(trade)

#Distribution of trade values plot - using log x scale to see distribution better
#NOTE: Plot is set to function for easy reuse here
hist_plot <- function(df, subtitle){
  
  plot = ggplot(df, aes(x=trade_value_usd)) +
    geom_histogram(bins = 50, fill = "tomato2", color = "white")+
    scale_x_log10(labels = scales::comma)+
    labs(x= "Trade Value (USD, log scale)",
         y = "Frequency",
         title="Distribution of Trade Value (USD)",
         subtitle = subtitle)+
    theme_minimal()+
    theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"))
  
  return(plot)
}

#Create and store plot for all trades in variable full_distribution 
full_distribution <- hist_plot(trade, "All Trades")

#Filter out records with trade value < 1000 [Keep meaningful trade relationships]
clean_trade <- trade %>% 
  filter(trade_value_usd>=1000)

#Create and store plot for trades>=1,000USD in variable filtered_distribution 
filtered_distribution <- hist_plot(clean_trade, "Trades >= 1000 USD")

#Stack plot to compare side by side
grid.arrange(full_distribution, filtered_distribution, ncol=1)

#---------------------------------------SECTION 2: END


#---------------------------------------SECTION 3: DESCRIPTIVE ANALYSIS AND VISUALIZATION
#Question1: Which EU countries are most dependent on specific partners for critical products?
dependency <- clean_trade %>% 
  filter(trade_flow=='Import') %>%  #filter for just imports
  group_by(reporter_country, product, partner_country) %>% 
  summarise(total_value = sum(trade_value_usd)) %>% #Derive total worth of import by EU country, product, and partner country
  mutate(share = total_value/sum(total_value), #Calculate each partner's % share of product imports
         share = round(share*100,2)) %>%  #Convert to percentage and round
  ungroup()



#Q1.1 Top 20 highest dependencies overall
top_dependencies <- dependency %>%
  arrange(desc(share)) %>% #Sort shares from highest to lowest
  head(20) %>% #Keep top 20 records - showing highest dependency
  mutate(label = paste0(reporter_country, ", ", product, " - ", partner_country)) #create label to show EU country, product, and partner country

#Q1.2 Countries with most high dependencies (>60% from one partner)
high_risk <- dependency %>%
  filter(share > 60)

#Q1.3 Each Product's highest concentration
product_risk <- dependency %>% 
  group_by(product) %>% 
  arrange(desc(share)) %>% 
  slice(1) #keep highest dependency percentage per product

#Create clean table export for report - Q1.3
product_risk_table <- product_risk %>%
  select(reporter_country, product, partner_country, share) %>%
  arrange(desc(share)) %>%
  rename("EU Country" = reporter_country,
         "Product" = product,
         "Main Partner" = partner_country,
         "Share (%)" = share)

#Export to Working Directory as Image
product_risk_table %>%
  flextable() %>%
  theme_vanilla() %>%
  autofit() %>%
  save_as_image("product_risk_table.png")

#Question1: Plots Area
#Top 20 Import Dependencies - Q1.1 Plot
ggplot(top_dependencies, aes(x = reorder(label, share), y = share)) + #x=reorder(label,share) reorder bars from highest to lowest share
  geom_col(fill = "tomato2") +
  geom_text(aes(label = paste0(share, "%")), hjust = -0.1, size = 3) + #include data labels outside each bar
  coord_flip() + #Flip to horizontal bar chart 
  labs(x = "Importer, Product - Exporter", 
       y = "Import Share (%)",
       title = "Top 20 Import Dependencies",
       subtitle = "Partner share of product imports (2015-2024)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic")) +
  ylim(0, max(top_dependencies$share) * 1.1)  #Improves space for the text on y axis


#Countries with most high dependencies - Q1.2 Plot
ggplot(high_risk, aes(x = product, y = reporter_country, fill = share)) +
  geom_tile(color = "white", size = 0.5) + #create heat-map plot with share as fill
  geom_text(aes(label = paste0(partner_country, "\n", share, "%")), size = 3) +
  scale_fill_gradient(low = "wheat1", high = "tomato2", name = "Share (%)") + #set color range
  labs(x = NULL, 
       y = NULL,
       title = "High-Risk Dependencies by Country and Product",
       subtitle = "Partner country and share shown (>60% import share)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9, face = "bold"),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"))


#Products with highest concentration - Q1.3 plot
ggplot(product_risk, aes(x = reorder(product, share), y = share)) +
  geom_col(fill = "turquoise") +
  geom_text(aes(label = paste0(share, "%")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(x = NULL, 
       y = "Highest Partner Share (%)",
       title = "Supply Chain Concentration by Product",
       subtitle = "Maximum dependency on single partner (2015-2024)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic")) +
  ylim(0, max(product_risk$share) * 1.1)


#----------------------------
#Question2: BREXIT Impact Analysis
#Q2.1 How did EU imports from UK change post-2020?
brexit <- clean_trade %>% 
  filter(partner_country=="GBR") %>% #keep only Great Britain records
  group_by(year, trade_flow) %>% 
  summarise(total_trade_usd = sum(trade_value_usd)) %>% # calculate total trade value across all products
  arrange(trade_flow, year) %>% 
  ungroup() %>% 
  group_by(trade_flow) %>% #group by trade flow again to calculate % change in import and export
  mutate(previous_trade_value=lag(total_trade_usd),
         percentage_change = ((total_trade_usd-previous_trade_value)/previous_trade_value)*100,
         percentage_change = round(percentage_change,2))
  

#Q2.2 Did EU countries replace UK imports with USA/China/others?
substitution <- clean_trade %>% 
  filter(trade_flow=="Import" & year %in% c(2019, 2023)) %>% #just before and some time after brexit
  group_by(product, partner_country, year) %>% 
  summarise(total_value_usd = sum(trade_value_usd)) %>% # calculate total imports value per product for each country per year
  pivot_wider(names_from = year, values_from = total_value_usd) %>%  #restructure dataframe to easily compare yearly values side by side
  mutate(change = `2023` - `2019`,
         percentage_change = (change/`2019`)*100,
         percentage_change = round(percentage_change, 2)) %>%  #calculate % change from 2019 to 2023
  group_by(product) %>% 
  filter(any(partner_country=="GBR" & percentage_change<0)) %>% #group by product and filter where GBR has a negative change to see who else in the group gained from this negative change
  ungroup() %>% 
  select(product, partner_country, percentage_change) 


#Q2.3 Which products saw biggest drops in UK sourcing?
gbr_drops <- substitution %>% 
  filter(partner_country=="GBR" & percentage_change<0) %>% 
  arrange(percentage_change)

#Question2: Plots Area
#How did EU imports from UK change post-2020? - Q2.1
ggplot(brexit, aes(x = year, y = total_trade_usd, color = trade_flow)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2020,  #include vertical line on year 2020 to indicate BREXIT event
             linetype = "dashed", 
             color = "darkorchid3") +
  annotate("text", #add "brexit" label to vertical line
           x = 2020, 
           y = max(brexit$total_trade_usd, na.rm = TRUE), 
           label = "Brexit", 
           vjust = -0.5, 
           hjust = -0.1,
           size = 3.5) +
  labs(x = "Year", 
       y = "Total Trade (USD)",
       title = "Brexit Impact on EU-UK Trade",
       subtitle = "Percentage change in trade volume (2015-2024)",
       color = "Trade Flow") +
  scale_x_continuous(breaks = 2015:2024)+ #provide clean breaks on x axis
  scale_color_manual(values = c("Export" = "tomato2", "Import" = "turquoise")) + #set line colors
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"))

#Did EU countries replace UK imports with USA/China/others? - Q2.2 (Export as table)
substitution_table <- substitution %>% 
  arrange(product, desc(percentage_change)) %>%
  rename("Product" = product,
         "Partner" = partner_country,
         "Percentage Change (2019-2023)" = percentage_change) %>% 
  filter(Product %in% c("Electronics", "Fruits")) #keep just 2 products to keep table shorter
  
substitution_table %>% #Export table and color code values for easy reading
  flextable() %>%
  theme_vanilla() %>%
  color(j = "Percentage Change (2019-2023)", 
        color = ifelse(substitution_table$`Percentage Change (2019-2023)` > 0, "green4", "darkred")) %>%
  bold(j = "Percentage Change (2019-2023)", bold = TRUE) %>%
  autofit() %>%
  save_as_image("substitution_table.png")

#Which products saw biggest drops in UK sourcing? - Q2.3
ggplot(gbr_drops, aes(x = reorder(product, percentage_change), y = percentage_change)) +
  geom_col(fill = "darkred") +
  geom_text(aes(label = paste0(percentage_change, "%")), vjust = -0.1, color = "white", size = 3.5) +
  labs(x = NULL, 
       y = "Change (%)",
       title = "Products with Largest UK Import Declines",
       subtitle = "EU import reduction from UK (2019-2023)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9, face="bold"),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"))

#---------------------------------------SECTION 3: END


