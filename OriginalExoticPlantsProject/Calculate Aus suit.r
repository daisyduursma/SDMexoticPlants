
rm(list = ls())

dat<-read.csv("f:\\Current_Projects\\exotic_plants\\outputs\\species_threat_assessment_20_03_2013.csv")


  with(dat, {
    
AUS.current_suitability<- ((ACT.current_suitability*ACT_gridcell_pres)+
                          (NSW.current_suitability*NSW_gridcell_pres)+
                          (NT.current_suitability*NT_gridcell_pres)+
                          (QLD.current_suitability*QLD_gridcell_pres)+
                          (SA.current_suitability*SA_gridcell_pres)+
                          (TAS.current_suitability*TAS_gridcell_pres)+
                          (VIC.current_suitability*VIC_gridcell_pres)+
                          (WA.current_suitability*WA_gridcell_pres))/AUS_gridcell_pres



AUS.rcp_45_2035_suitability<- ((ACT.rcp_45_2035_suitability*ACT_gridcell_pres)+
                              (NSW.rcp_45_2035_suitability*NSW_gridcell_pres)+
                              (NT.rcp_45_2035_suitability*NT_gridcell_pres)+
                              (QLD.rcp_45_2035_suitability*QLD_gridcell_pres)+
                              (SA.rcp_45_2035_suitability*SA_gridcell_pres)+
                              (TAS.rcp_45_2035_suitability*TAS_gridcell_pres)+
                              (VIC.rcp_45_2035_suitability*VIC_gridcell_pres)+
                              (WA.rcp_45_2035_suitability*WA_gridcell_pres))/AUS_gridcell_pres



AUS.rcp_85_2035_suitability<- ((ACT.rcp_85_2035_suitability*ACT_gridcell_pres)+
                              (NSW.rcp_85_2035_suitability*NSW_gridcell_pres)+
                              (NT.rcp_85_2035_suitability*NT_gridcell_pres)+
                              (QLD.rcp_85_2035_suitability*QLD_gridcell_pres)+
                              (SA.rcp_85_2035_suitability*SA_gridcell_pres)+
                              (TAS.rcp_85_2035_suitability*TAS_gridcell_pres)+
                              (VIC.rcp_85_2035_suitability*VIC_gridcell_pres)+
                              (WA.rcp_85_2035_suitability*WA_gridcell_pres))/AUS_gridcell_pres





AUS.rcp_45_2065_suitability<- ((ACT.rcp_45_2065_suitability*ACT_gridcell_pres)+
                              (NSW.rcp_45_2065_suitability*NSW_gridcell_pres)+
                              (NT.rcp_45_2065_suitability*NT_gridcell_pres)+
                              (QLD.rcp_45_2065_suitability*QLD_gridcell_pres)+
                              (SA.rcp_45_2065_suitability*SA_gridcell_pres)+
                              (TAS.rcp_45_2065_suitability*TAS_gridcell_pres)+
                              (VIC.rcp_45_2065_suitability*VIC_gridcell_pres)+
                              (WA.rcp_45_2065_suitability*WA_gridcell_pres))/AUS_gridcell_pres



AUS.rcp_85_2065_suitability<- ((ACT.rcp_85_2065_suitability*ACT_gridcell_pres)+
                              (NSW.rcp_85_2065_suitability*NSW_gridcell_pres)+
                              (NT.rcp_85_2065_suitability*NT_gridcell_pres)+
                              (QLD.rcp_85_2065_suitability*QLD_gridcell_pres)+
                              (SA.rcp_85_2065_suitability*SA_gridcell_pres)+
                              (TAS.rcp_85_2065_suitability*TAS_gridcell_pres)+
                              (VIC.rcp_85_2065_suitability*VIC_gridcell_pres)+
                              (WA.rcp_85_2065_suitability*WA_gridcell_pres))/AUS_gridcell_pres

})








write.csv(dat,"f:\\Current_Projects\\exotic_plants\\outputs\\species_threat_assessment_18_04_2013.csv")






