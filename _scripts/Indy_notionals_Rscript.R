### Creating 2014 Indyref vote notionals for 2024 UK Parliamentary Constituencies ###

# This is the R code to calculated weighted averages of 2014 indyref vote for 2024 UK Parliamentary Constituencies (UKPC)
    # 2014 Indyref results are currently only available at the Local Authority (LA) level 
    # Most UKPC in Scotland are spread across multiple Local Authorities 
    # For each constituency, I bring together different LA scores by averaging them 
    # These averages are weighted for 
        # the population from the UKPC living in each LA 
        # turnout in each LA 

## Loading packages -------------------------------------------------------
library(dplyr)
library(haven)
library(tidyverse)
library(readxl)

## Loading data -----------------------------------------------------------

    # 1) Independence data: 2014 vote and turnout 
      
      # this file has independence results at the LA level (+ turnout)
          # Download here: https://find.data.gov.scot/datasets/32636
    
            indyref <- read_excel("/Users/user/Downloads/2014 Independence results at the Local Authority level.xls") %>%
              mutate(
                Council = case_when( # Making sure names match the lookup dataset 
                  Council == "Aberdeen" ~ "Aberdeen City",
                  Council == "Dundee" ~ "Dundee City",
                  Council == "Edinburgh" ~ "City of Edinburgh",
                  Council == "Elian Siar" ~ "Na h-Eileanan Siar",
                  Council == "Eilean Siar" ~ "Na h-Eileanan Siar",  
                  Council == "Glasgow" ~ "Glasgow City",
                  TRUE ~ Council
                )
              ) %>%
              rename(LA_name = Council)
                
            
    # 2) Population data: 2014 census estimates 
        
      # this file has population estimates for 2014 at the data zone level 
          # Download here: https://webarchive.nrscotland.gov.uk/20210313183007mp_/https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape2015/correctedtabs2012-13-14/2014-sape-persons-cor.xlsx
            
              population <- read_excel("/Users/user/Downloads/2014 population estimates at the Data Zone level.xlsx") %>%
                slice(-(1:5)) %>%
                select(-c(2, 4), 
                       Datazone = 'Table 1a: Estimated population by sex, single year of age and 2011 Data Zone area: 30 June 2014') %>%
                mutate(across(-1, as.numeric)) %>%
                mutate(Adult_pop = .[[2]] - rowSums(.[, 3:18])) # Calculating adult population
              
    
    # 3) Lookup file with LA, UKPCs and datazones
        
        # this file has DZ, LAs, and 2024 Constituencies 
                # Download here: https://statistics.gov.scot/data/data-zone-lookup
        
                  lookup <- read.csv("/Users/user/Downloads/Lookup Data Zone to other geographies.csv") %>%
                    select(
                      Datazone = DZ2011_Code, 
                      LA = LA_Code,
                      LA_name = LA_Name,
                      Constituency = UKPC_Code,
                      Constituency_name = UKPC_Name)
    
                  
## Merging all three ------------------------------------------------------
    
      # Merging 
        merged_data <- left_join(indyref, lookup, by = "LA_name")
        merged_data <- left_join(merged_data, population, by = "Datazone")
        
        merged_data <- merged_data %>%
          select(
            Datazone, 
            LA, 
            LA_name, 
            Constituency, 
            Constituency_name, 
            Adult_pop, 
            Turnout, 
            YesPercent, 
            NoPercent,
            Yes, 
            No, 
            WinDiff 
          )
    
## Calculating indyref at the constituency level ------------------------------
    
      # In the dataset we now have independence & turnout at the datazone level 

      # But most constituencies will have more than 1 datazone (and therefore, indy values)
        yes_by_constituency_unique <- merged_data %>%
          group_by(Constituency) %>%
          summarize(yes_by_constituency_unique = n_distinct(YesPercent, na.rm = TRUE))
        print(yes_by_constituency_unique)
    
      # To deal with this, I calculate a weighted average of Yes support taking into account
          # the population in each data zone 
          # the turnout for each area 
        
      # I make a weight with population * turnout 
        # The population is at the data zone level - sadly not the turnout (AL level)
        merged_data <- merged_data %>%
          mutate(Turnout = Turnout/100) %>% 
          mutate(merged_weight = Adult_pop*Turnout)
           
      # I then create that are weighted for pop. and turnout
        notional_data <- merged_data %>%
          group_by(Constituency, Constituency_name) %>%
          summarize(
            Yes_Notional = round(sum(YesPercent * merged_weight) / sum(merged_weight), 2), 
            No_Notional = round(sum(NoPercent * merged_weight) / sum(merged_weight), 2)
          ) %>%
          mutate(
            WinDiff = round(No_Notional - Yes_Notional, 2)
          ) %>%
          rename(
            ONSConstID = Constituency
          )
      
        # Remove the last NA row 
        notional_data <- notional_data %>%
          filter(!is.na(Constituency_name))
        
        print(notional_data)
        
  ## Downloading CSV -----------------
        
        # File path to save the CSV
        file_path <- "/Users/user/Desktop/Indy2014_ConstituencyEstimates.csv"
        
        # Use the write.csv() function to export the data
        write.csv(notional_data, file = file_path, row.names = FALSE)
        
        
        
