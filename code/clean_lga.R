
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
                  LGA=str_remove(LGA,"Region"),
                  LGA=str_remove(LGA,"Municipality"),
                  LGA=str_remove(LGA,"\\(East Arm\\)"),
                  LGA=str_squish(LGA),
                  LGA=str_trim(LGA),
                  LGA=str_replace(LGA,"Un-Incorporated (Nhulunbuy) Area","Nhulunbuy"))
  
}



