#' Function to "clean"/standardise LGA names for easy matching
#' @param df dataframe with attribute 'LGA'
clean_lga <-function(df){
  
  df %>%   mutate(LGA=stri_trans_totitle(tolower(LGA)),
                  LGA=str_remove(LGA,"City of"),
                  LGA=str_remove(LGA,"Shire of"),
                  LGA=str_remove(LGA,"Shire"),
                  LGA=str_remove(LGA,"City"),
                  LGA=str_remove(LGA,"Rural"),
                  LGA=str_remove(LGA,"Borough of"),
                  LGA=str_remove(LGA,"Borough"),
                  LGA=str_remove(LGA,"Town Council"),
                  LGA=str_remove(LGA,"Regional Council"),
                  LGA=str_remove(LGA,"Community Government"),
                  LGA=str_remove(LGA,"Council"),
                  LGA=str_remove(LGA,"Corporation"),
                  LGA=str_remove(LGA,"Of"),
                  LGA=str_remove(LGA,","),
                  LGA=str_remove(LGA,"\\(Unincorporated\\)"),
                  LGA=str_remove(LGA,"Unincorporated"),
                  LGA=str_remove(LGA,"Un-Incorporated"),
                  LGA=str_remove(LGA,"Area"),
                  LGA=str_remove(LGA,"\\("),
                  LGA=str_remove(LGA,"\\)"),
                  LGA=str_remove(LGA,"\\(Uninc\\)"),
                  LGA=str_remove(LGA,"Uninc"),
                  LGA=str_remove(LGA,"Region"),
                  LGA=str_remove(LGA,"Municipality"),
                  LGA=str_remove(LGA,"Municipal"),
                  LGA=str_remove(LGA,"\\(East Arm\\)"),
                  LGA=str_remove(LGA,"The The Of"),
                  LGA=str_remove(LGA,"The Of"),
                  LGA=str_squish(LGA),
                  LGA=str_trim(LGA),
                  LGA=str_replace(LGA,"Un-Incorporated (Nhulunbuy) Area","Nhulunbuy"),
                  LGA=str_replace(LGA,"George","George Town"),
                  LGA=str_replace(LGA,"Mid-Western al","Mid-Western"),
                  LGA=str_replace(LGA,"The The Of Hornsby","Hornsby"),   
                  LGA=str_replace(LGA,"Bathurst al","Bathurst"),                  
                  LGA=str_replace(LGA,"Queanbeyan-Palerang","Queanbeyan–Palerang"),
                  LGA=str_replace(LGA,"The The Of Kiama","Kiama"),
                  LGA=str_replace(LGA,"The Of Sydney","Sydney"),
                  LGA=if_else(str_length(LGA)==0,"Unincorporated",LGA)
                  )

}




