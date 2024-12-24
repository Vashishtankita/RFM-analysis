## Install Packages (if needed)
library(dplyr)
set.seed(1)
## Read in ankita data
ankita <- read.csv("C:/Users/ankuv/Downloads/retail_ankita.csv") 
ankita
getOption("max.print")
## How many levels for each
groups <- 5 ## This will use quintiles to sort and give 125 tota groups
## Run ankita Analysis with Independent Sort
ankita$recency_score_indep <- ntile(ankita$recency_days*-1, groups) 	
ankita$frequency_score_indep <- ntile(ankita$number_of_orders, groups) 	
ankita$monetary_score_indep <- ntile(ankita$revenue, groups) 	
ankita$ankita_score_indep <- paste(ankita$recency_score_indep*100 + ankita$frequency_score_indep * 10 + ankita$monetary_score_indep)
ankita
## Run ankita Analysis with Sequential Sort
ankita$recency_score_seq <- ntile(ankita$recency_days*-1, groups)
r_groups <- NULL; rf_groups <- NULL; temp <- NULL ## Initialize empty matrices
for (r in 1:groups) {
  r_groups[[r]] <- filter(ankita, ankita$recency_score_seq == r)
  r_groups[[r]]$frequency_score_seq <- ntile(r_groups[[r]]$number_of_orders, groups)
  for (m in 1:groups) {
    rf_groups[[m]] <- filter(r_groups[[r]], r_groups[[r]]$frequency_score_seq == m) 					
    rf_groups[[m]]$monetary_score_seq <- ntile(rf_groups[[m]]$revenue, groups)
    temp <- bind_rows(temp, rf_groups[[m]])
  }
}
ankita_result <- temp[order(temp$customer_id),]
ankita_result$ankita_score_seq <- paste(ankita_result$recency_score_seq*100 + ankita_result$frequency_score_seq * 10 + ankita_result$monetary_score_seq)
## Export ankita Results with Independent and Sequential Sort 	
write.csv(ankita_result, file = "C:/Users/ankuv/Downloads/retail_ankita_RESULTS.csv", row.names = FALSE)
