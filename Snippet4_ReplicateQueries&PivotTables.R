
# WORK IN PROGRESS !!
# STILL DEBUGGING

# download nuSEDS from https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6
# then extract and save in the Data/Large folder and provide the path below
# also download aopen-data-portaljanuary-2019conservation_unit_system_sites.csv 
# Note: contents of the Data/Large folder are not being tracked on github.

# create output folder if it doesn't exist
if(!dir.exists("OUTPUT")){dir.create("OUTPUT")}

# load required packages
library(tidyverse)

# load data
data.file.use <- "DATA/Large/NUSEDS_20190117.csv"  # doesn't have CU Names, need to match based on below
cu.match.file <-"DATA/Large/aopen-data-portaljanuary-2019conservation_unit_system_sites.csv"

raw.data <- read.csv(data.file.use, stringsAsFactors = FALSE)
names(raw.data)
pop.info <- read.csv(cu.match.file, stringsAsFactors = FALSE)
names(pop.info)

cu.matches <- pop.info %>% dplyr::select(POP_ID,FULL_CU_IN,CU_NAME,CU_ACRO)

# merge the CU Name into the raw.data
merged.data <- dplyr::left_join(raw.data, cu.matches, by="POP_ID")


# Totals by CU - Version 1 = All records

allrecords.sums <- merged.data %>% dplyr::select(SPECIES, CU_NAME,FULL_CU_IN, ANALYSIS_YR,  MAX_ESTIMATE,
                           NATURAL_ADULT_SPAWNERS, ADULT_BROODSTOCK_REMOVALS, TOTAL_RETURN_TO_RIVER) %>% 
                              dplyr::group_by(SPECIES, CU_NAME,FULL_CU_IN,ANALYSIS_YR) %>%
                                  dplyr::summarise_all(funs(sum),na.rm=TRUE)

allrecords.counts <-merged.data %>% dplyr::select(SPECIES, CU_NAME,FULL_CU_IN, ANALYSIS_YR) %>% 
                                                dplyr::group_by(SPECIES, CU_NAME,FULL_CU_IN,ANALYSIS_YR) %>%
                                                   dplyr::summarise(numSites=n()) 

allrecords.out <- left_join(allrecords.counts, allrecords.sums, by = c("SPECIES", "CU_NAME","FULL_CU_IN", "ANALYSIS_YR"))

write.csv(allrecords.out,"OUTPUT/nuSEDS_CUTotals_AllRecords.csv")





# Totals by CU - Version 2 = High Quality Records Only

sort(unique(merged.data$ESTIMATE_CLASSIFICATION))

est.class.keep <- c("TRUE ABUNDANCE (TYPE-1)","TRUE ABUNDANCE (TYPE-2)","RELATIVE ABUNDANCE (TYPE-3)",
                    "RELATIVE ABUNDANCE (TYPE-4)")



filtered.sums <- merged.data %>%  dplyr::filter(ESTIMATE_CLASSIFICATION %in% est.class.keep)  %>% 
                          dplyr::select(SPECIES, CU_NAME,FULL_CU_IN, ANALYSIS_YR,  MAX_ESTIMATE,
                                              NATURAL_ADULT_SPAWNERS, ADULT_BROODSTOCK_REMOVALS, TOTAL_RETURN_TO_RIVER) %>% 
                                                  dplyr::group_by(SPECIES, CU_NAME,FULL_CU_IN,ANALYSIS_YR) %>%
                                                       dplyr::summarise_all(funs(sum),na.rm=TRUE)



filtered.counts <-merged.data %>% dplyr::filter(ESTIMATE_CLASSIFICATION %in% est.class.keep)  %>% 
                         dplyr::select(SPECIES, CU_NAME,FULL_CU_IN, ANALYSIS_YR ) %>% 
                      dplyr::group_by(SPECIES, CU_NAME,FULL_CU_IN,ANALYSIS_YR) %>%
                  dplyr::summarise(numSites=n()) 

filtered.out <- left_join(allrecords.counts, allrecords.sums, by = c("SPECIES", "CU_NAME","FULL_CU_IN", "ANALYSIS_YR"))

write.csv(filtered.out,"OUTPUT/nuSEDS_CUTotals_FilteredRecords.csv")



# Record counts by CU
# using only those with a numeric value for Nat Spn 
# (otherwise get lots of zeros from records that only have one of the other variables)

allrecords.summary <- allrecords.out %>% dplyr::select(SPECIES, CU_NAME,FULL_CU_IN, ANALYSIS_YR,numSites,NATURAL_ADULT_SPAWNERS ) %>%
                        dplyr::filter(!is.na(NATURAL_ADULT_SPAWNERS)) %>%
                        dplyr::group_by(SPECIES, CU_NAME,FULL_CU_IN)  %>%
                        dplyr::summarise(all.numYears = n(),
                                         all.firstYear = min(ANALYSIS_YR),
                                         all.lastYear = max(ANALYSIS_YR),
                                         all.minSites = min(numSites),all.maxSites = max(numSites),
                                         all.minSpn = min(NATURAL_ADULT_SPAWNERS,na.rm=TRUE),
                                         all.medSpn = median(NATURAL_ADULT_SPAWNERS,na.rm=TRUE),
                                         all.maxSpn = max(NATURAL_ADULT_SPAWNERS,na.rm=TRUE)
                                        )



filtered.summary <- filtered.out %>% dplyr::select(SPECIES, CU_NAME,FULL_CU_IN, ANALYSIS_YR,numSites,NATURAL_ADULT_SPAWNERS ) %>%
  dplyr::filter(!is.na(NATURAL_ADULT_SPAWNERS)) %>%
  dplyr::group_by(SPECIES, CU_NAME,FULL_CU_IN)  %>%
  dplyr::summarise(all.numYears = n(),
                   all.firstYear = min(ANALYSIS_YR),
                   all.lastYear = max(ANALYSIS_YR),
                   all.minSites = min(numSites),all.maxSites = max(numSites),
                   all.minSpn = min(NATURAL_ADULT_SPAWNERS,na.rm=TRUE),
                   all.medSpn = median(NATURAL_ADULT_SPAWNERS,na.rm=TRUE),
                   all.maxSpn = max(NATURAL_ADULT_SPAWNERS,na.rm=TRUE)
  )
 











 






