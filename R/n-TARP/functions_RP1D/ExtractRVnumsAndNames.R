# 2017.09.20. extracted from C20_

ExtractRVnumsAndNames <- function(RP_names){ 
  for(i in 1:length(RP_names)) 
  {
    #store name of current random projection
    curRP_name <- RP_names[i]
    
    #store number of current random variable being used
    if(i==1)
    {
      #create the data frames during first iteration
      RV_nums = data.frame("RV" = regmatches(x = curRP_name, regexpr(pattern = "\\d+$", text =  curRP_name)))
      RV_names = data.frame("RV"= paste0("RV_", regmatches(x = curRP_name, 
                                                              regexpr(pattern = "\\d+$", text =  curRP_name))))
    }else
    {
      #add rows during remaining iterations
      RV_nums <- add_row(RV_nums, "RV"= paste0(regmatches(x = curRP_name, 
                                                          regexpr(pattern = "\\d+$", text =  curRP_name))))
      RV_names <- add_row(RV_names, 
                             "RV"= paste0("RV_",regmatches(x = curRP_name, 
                                                           regexpr(pattern = "\\d+$", text =  curRP_name))))
    }

  }
  
  #return both variables in a list (to be extracted after the function call)
    #extract using two lines like the following:
    # var1 <- returnedList$var1
    # var2 <- returnedList$var2
  return(list("nums" = RV_nums, "names" = RV_names))
}
