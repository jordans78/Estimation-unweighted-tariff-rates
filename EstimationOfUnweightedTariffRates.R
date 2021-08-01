#Estimation of unweighted tariff rates#########################################################################################
#
#                                              TARIFF CODE UNION 2013-2019
#
#
#
#


# (I.) Data and Libraries  -------------------------------------------------

        library(dplyr)
        library(stringr)
        library(stringi)
        library(magrittr)
        library(tidyverse)


# (II.) Data cleaning and preparation of tariff codes -----------------------------

                # Data set
                    UNION_DATA_SIMULATION<-UNION_DATA_BASE
                    #str(UNION_DATA_SIMULATION)
                # Remove all punctuation (see ?regex), removes more than just brackes
                    UNION_DATA_SIMULATION$Ten_digits_1  <- gsub("[[:punct:]]", "", UNION_DATA_SIMULATION$Ten_digits)
                    
                # Make "NA" strings to real 'NA values'
                    UNION_DATA_SIMULATION$Ten_digits_1[UNION_DATA_SIMULATION$Ten_digits == "NA"] <- NA
                
                # Insert spaces
                    UNION_DATA_SIMULATION$Ten_digits_1 <-
                      trimws(
                      gsub("^([0-9]{4})([0-9]{2})?([0-9]{2})?([0-9]{2})?$", 
                           "\\1 \\2 \\3 \\4", UNION_DATA_SIMULATION$Ten_digits_1)
                            )
                
                    UNION_DATA_SIMULATION1<-UNION_DATA_SIMULATION %>% 
                    mutate(Ten_digits_1 = case_when(
                      str_length(Ten_digits) == 3 ~ paste0("0", Ten_digits),
                      TRUE ~ Ten_digits_1
                    ))

                # View(UNION_DATA_SIMULATION1)

              UNION_DATA_SIMULATION2<-mutate(UNION_DATA_SIMULATION1,
                                      CHAPTER=substr(UNION_DATA_SIMULATION1$Ten_digits_1,1,2),
                                      FOUR_DIGIT=substr(UNION_DATA_SIMULATION1$Ten_digits_1,0,4),
                                      SIX_DIGIT=substr(UNION_DATA_SIMULATION1$Ten_digits_1,1,7),
                                      NEW_10_DIGITS=if_else(str_count(Ten_digits, "[0-9]") == 10,
                                                         str_replace_all(Ten_digits, fixed(" "), ""),
                                                         Ten_digits)
                                      )
     
              
# (III.) Extraction tariff rate's separately by columns(three columns) ---------------------------

                #Preparation simulation sample for 2017 for estimation of TE'S
                SIMULATION_SAMPLE<-UNION_DATA_SIMULATION2%>%
                  dplyr:: filter(Y==2017)
                
                  
                
                FINAL_TEST<-SIMULATION_SAMPLE %>%
                  dplyr:: select(Customs_rates)%>%
                  dplyr:: mutate(Customs_rates = str_replace_all(Customs_rates, ",", "."), 
                         RATE = str_extract(Customs_rates, "^[0-9]+(?=\\+|$)|^[0-9.]+$"), 
                         SPECIFIC_RATE = str_extract(Customs_rates, "\\d+\\.\\d+(?=\\s)"), 
                         MAXIMUM_RATE = str_extract(Customs_rates, "(?<=max\\.)\\d+")) %>% 
                  dplyr::select(2:4) %>% 
                  dplyr::mutate_all(as.numeric)  
                
                #View(FINAL_TEST)

              str(BASE_TARIFF_SIMULATION)
              BASE_TARIFF_SIMULATION<-cbind(SIMULATION_SAMPLE,FINAL_TEST)
              BASE_TARIFF_SIMULATION1<-BASE_TARIFF_SIMULATION%>%
                select(Ten_digits,Customs_rates,RATE,SPECIFIC_RATE,MAXIMUM_RATE)
              
              BASE_TARIFF_SIMULATION2<-tibble(BASE_TARIFF_SIMULATION1)
              head(BASE_TARIFF_SIMULATION2)
              str(BASE_TARIFF_SIMULATION2)

              
              BASE_TARIFF_SIMULATION2<-BASE_TARIFF_SIMULATION1%>%
                filter(Ten_digits %>% str_remove_all("\\s") %>% str_length %>% `==`(10)) %>%   #ova vazi za kod od 10 cifri
                replace(is.na(.), 0)
              
     

              FINAL_SIMULATION1<-mutate(BASE_TARIFF_SIMULATION2,MEAN_CALCULATION=ifelse(MAXIMUM_RATE>=RATE,MAXIMUM_RATE,RATE),
                                        CHAPTER=substr(BASE_TARIFF_SIMULATION2$Ten_digits,1,2),
                                        FOUR_DIGIT=substr(BASE_TARIFF_SIMULATION2$Ten_digits,1,4),
                                        SIX_DIGIT=substr(BASE_TARIFF_SIMULATION2$Ten_digits,1,7)
                                        )
              
            # Merging datasets
              
              MERGING_SAMPLE<-SIMULATION_SAMPLE%>%
                      select(Ten_digits,NEW_10_DIGITS )
              FINAL_SIMULATION2<-left_join(FINAL_SIMULATION1,MERGING_SAMPLE,by = c("Ten_digits"))
              FINAL_SIMULATION3<-left_join(FINAL_SIMULATION2,PRODUCTS_NAMES,by = c("CHAPTER"))
              View(FINAL_SIMULATION3)
              
        
 