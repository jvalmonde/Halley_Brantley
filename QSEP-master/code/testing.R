misMatch_ID <- adult_covar_ID[which(!(adult_covar_ID %in% adult_mod_ID))]

misMatch <- 
  adult_covar %>%
  filter(Indv_Sys_Id %in% misMatch_ID) %>%
  inner_join(spend, by = c("Indv_Sys_Id", "FamilyID", "Year"), 
             suffix = c(".member", ".spend")) 

table(misMatch$sick_child_ct)

misMatch2 <- 
  misMatch %>% 
  select(Indv_Sys_Id, FamilyID, ends_with(".member"), ends_with(".spend"), child_ct) %>%
  mutate(Trauma_diff = Trauma_Flag.member - Trauma_Flag.spend, 
         T1D_diff = T1D_Flag.member - T1D_Flag.spend,
         Asthma_diff = Asthma_Flag.member - Asthma_Flag.spend, 
         CA_diff = CA_Flag.member - CA_Flag.spend, 
         Cerebral_diff = Cerebral_Flag.member - Cerebral_Flag.spend, 
         ASD_diff = ASD_Flag.member - ASD_Flag.spend) %>%
  distinct() %>%
  arrange(FamilyID) %>%
  select(Indv_Sys_Id, FamilyID, 
         starts_with("T1D"), 
         starts_with("CA"), 
         starts_with("Trauma"),
         starts_with("ASD"),
         starts_with("Asthma"),
         starts_with("Cerebral"),
         child_ct
  )

misMatch2$numDiff <- misMatch2 %>% select(ends_with("diff")) %>%
  apply(., 1, function(x) sum(x != 0))

table(misMatch2$Trauma_diff)

spend %>% filter(FamilyID == "100137398240760144") %>%
  select(Indv_Sys_Id, FamilyID, ends_with("Flag")) %>%
  arrange(Indv_Sys_Id) %>% distinct()
  

misMatch2 %>% filter(T1D_diff == 1)


members %>% filter(Indv_Sys_Id == "388801909") %>% 
  select(Year, Zip, FamilyID, ends_with("Flag"))

members %>% filter(Indv_Sys_Id == "226354281") %>% 
  select(Year, Zip, FamilyID, ends_with("Flag"))

members %>% filter(FamilyID == "100000000764667625")