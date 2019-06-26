
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


# custom functions

safe.sum <- function(x){z <- x[!is.na(x)]; ifelse(length(z), sum(z), NA)}
# as per https://stat.ethz.ch/pipermail/r-help/2002-April/020796.html

safe.log <- function(x){ x.fix <- x; x.fix[x.fix==0] <- NA; return(log(x.fix)) }


simple.plot <- function(x,y,xlab="X",ylab="Y",quantiles=TRUE,lims=NULL){
  
  num.pts <- sum(!is.na(y))
 
  if(num.pts>0){ 
  
  if(is.null(lims)){xlim <- range(x,na.rm=TRUE); ylim <- range(c(0,y), na.rm=TRUE) }
  if(!is.null(lims)){xlim <- lims[c(1,2)]; ylim <- lims[c(3,4)] }
  

    
    plot(x,y,bty="n",xlab=xlab,ylab=ylab,pch=19, col="darkblue", type="o",ylim=ylim, xlim=xlim)
    if(quantiles){abline(h=quantile(y,probs=c(0.25,0.5),na.rm=TRUE),col=c("red","green"),lty=c(1,2))}
  }
  
  if(num.pts==0){empty.plot()}
  
}

empty.plot <- function(x="NA"){plot(1:5,1:5,type="n",bty="n",axes=FALSE,xlab="",ylab = ""); text(3,2.5,x)}



# load data
data.file.use <- "DATA/Large/NUSEDS_20190117.csv"  # doesn't have CU Names, need to match based on below
cu.match.file <-"DATA/Large/aopen-data-portaljanuary-2019conservation_unit_system_sites.csv"

raw.data <- read.csv(data.file.use, stringsAsFactors = FALSE)
names(raw.data)
pop.info <- read.csv(cu.match.file, stringsAsFactors = FALSE)
names(pop.info)

cu.matches <- pop.info %>% dplyr::select(POP_ID,FULL_CU_IN,CU_NAME,CU_ACRO,AREA)

# merge the CU Name into the raw.data
merged.data <- dplyr::left_join(raw.data, cu.matches, by="POP_ID")
names(merged.data)[dim(merged.data)[2]] <- "CU_AREA"


# Totals by CU - Version 1 = All records

allrecords.sums <- merged.data %>% dplyr::select(SPECIES, CU_AREA, CU_NAME,FULL_CU_IN, ANALYSIS_YR,  MAX_ESTIMATE,
                           NATURAL_ADULT_SPAWNERS, ADULT_BROODSTOCK_REMOVALS, TOTAL_RETURN_TO_RIVER) %>% 
                              dplyr::group_by(SPECIES, CU_AREA,CU_NAME,FULL_CU_IN,ANALYSIS_YR) %>%
                                  dplyr::summarise_all(funs(safe.sum))

allrecords.counts <-merged.data %>% dplyr::filter(!is.na(NATURAL_ADULT_SPAWNERS)) %>% 
                          dplyr::select(SPECIES, CU_AREA, CU_NAME,FULL_CU_IN, ANALYSIS_YR) %>% 
                                                dplyr::group_by(SPECIES, CU_AREA, CU_NAME,FULL_CU_IN,ANALYSIS_YR) %>%
                                                   dplyr::summarise(numSites=n() )

allrecords.out <- left_join(allrecords.counts, allrecords.sums, by = c("SPECIES", "CU_AREA", "CU_NAME","FULL_CU_IN", "ANALYSIS_YR"))

write.csv(allrecords.out,"OUTPUT/nuSEDS_CUTotals_AllRecords.csv",row.names = FALSE)





# Totals by CU - Version 2 = High Quality Records Only

sort(unique(merged.data$ESTIMATE_CLASSIFICATION))

est.class.keep <- c("TRUE ABUNDANCE (TYPE-1)","TRUE ABUNDANCE (TYPE-2)","RELATIVE ABUNDANCE (TYPE-3)",
                    "RELATIVE ABUNDANCE (TYPE-4)")


filtered.records <- merged.data %>%  dplyr::filter(ESTIMATE_CLASSIFICATION %in% est.class.keep)


filtered.sums <- filtered.records  %>% dplyr::select(SPECIES, CU_AREA, CU_NAME,FULL_CU_IN, ANALYSIS_YR,  MAX_ESTIMATE,
                                NATURAL_ADULT_SPAWNERS, ADULT_BROODSTOCK_REMOVALS, TOTAL_RETURN_TO_RIVER) %>% 
                                                  dplyr::group_by(SPECIES, CU_AREA, CU_NAME,FULL_CU_IN,ANALYSIS_YR) %>%
                                                       dplyr::summarise_all(funs(safe.sum))


filtered.counts <- filtered.records %>%  dplyr::filter(!is.na(NATURAL_ADULT_SPAWNERS)) %>% 
                         dplyr::select(SPECIES, CU_AREA, CU_NAME,FULL_CU_IN, ANALYSIS_YR ) %>% 
                      dplyr::group_by(SPECIES, CU_AREA,  CU_NAME,FULL_CU_IN,ANALYSIS_YR) %>%
                  dplyr::summarise(numSites=sum(!is.na(NATURAL_ADULT_SPAWNERS))) # only count sites with Nat Spn Data

filtered.out <- left_join(filtered.counts, filtered.sums, by = c("SPECIES", "CU_AREA", "CU_NAME","FULL_CU_IN", "ANALYSIS_YR"))

write.csv(filtered.out,"OUTPUT/nuSEDS_CUTotals_FilteredRecords.csv",row.names = FALSE)



# Record counts by CU
# using only those with a numeric value for Nat Spn 
# (otherwise get lots of zeros from records that only have one of the other variables)

allrecords.summary <- allrecords.out %>% dplyr::select(SPECIES, CU_AREA, CU_NAME,FULL_CU_IN, ANALYSIS_YR,numSites,NATURAL_ADULT_SPAWNERS ) %>%
                        dplyr::filter(!is.na(NATURAL_ADULT_SPAWNERS)) %>%
                        dplyr::group_by(SPECIES, CU_AREA, CU_NAME,FULL_CU_IN)  %>%
                        dplyr::summarise(all.numYears = n(),
                                         all.firstYear = min(ANALYSIS_YR),
                                         all.lastYear = max(ANALYSIS_YR),
                                         all.minSites = min(numSites),all.maxSites = max(numSites),
                                         all.minSpn = min(NATURAL_ADULT_SPAWNERS,na.rm=TRUE),
                                         all.medSpn = median(NATURAL_ADULT_SPAWNERS,na.rm=TRUE),
                                         all.maxSpn = max(NATURAL_ADULT_SPAWNERS,na.rm=TRUE)
                                        )



filtered.summary <- filtered.out %>% dplyr::select(SPECIES, CU_AREA, CU_NAME,FULL_CU_IN, ANALYSIS_YR,numSites,NATURAL_ADULT_SPAWNERS ) %>%
  dplyr::filter(!is.na(NATURAL_ADULT_SPAWNERS)) %>%
  dplyr::group_by(SPECIES, CU_AREA, CU_NAME,FULL_CU_IN)  %>%
  dplyr::summarise(HQ.numYears = n(),
                   HQ.firstYear = min(ANALYSIS_YR),
                   HQ.lastYear = max(ANALYSIS_YR),
                   HQ.minSites = min(numSites),all.maxSites = max(numSites),
                   HQ.minSpn = min(NATURAL_ADULT_SPAWNERS,na.rm=TRUE),
                   HQ.medSpn = median(NATURAL_ADULT_SPAWNERS,na.rm=TRUE),
                   HQ.maxSpn = max(NATURAL_ADULT_SPAWNERS,na.rm=TRUE)
                 )
 
merged.summary <- left_join(allrecords.summary,filtered.summary,by=c("SPECIES", "CU_AREA", "CU_NAME","FULL_CU_IN"))


write.csv(merged.summary,"OUTPUT/nuSEDS_SummaryofRecordsByCU.csv",row.names = FALSE)


# DIAGNOSTIC PLOTS

# diagnostic plots by species, in alphebetical order of CUs

species.list <- sort(unique(allrecords.summary$SPECIES))

for(species.plot in species.list ){
  
pdf(paste0("OUTPUT/PrelimPlots_",species.plot,".pdf"),height = 11, width = 8.5)  
  
  
cu.list <- sort(unique(allrecords.summary$FULL_CU_IN[allrecords.summary$SPECIES==species.plot]))
  
print("-------------")
print(species.plot)
print(cu.list)

for(cu.plot in cu.list) {
print("---")
print(paste("plotting",cu.plot)  )
  
all.idx <- allrecords.out$FULL_CU_IN == cu.plot
all.idx[is.na(all.idx)] <- FALSE
filtered.idx <- filtered.out$FULL_CU_IN== cu.plot
filtered.idx[is.na(filtered.idx)] <- FALSE

allrecords.sub <- allrecords.out[all.idx,]
filtered.sub <- filtered.out[filtered.idx,]

cu.name <- unique(allrecords.sub$CU_NAME)


yr.lims <- range(pretty(allrecords.sub$ANALYSIS_YR ,na.rm=TRUE))
val.lims <- range(pretty(c(0,allrecords.sub$NATURAL_ADULT_SPAWNERS ,na.rm=TRUE)))
log.lims <- range(pretty(safe.log(c(1,allrecords.sub$NATURAL_ADULT_SPAWNERS)) ,na.rm=TRUE))
num.lims <- range(pretty(c(0,allrecords.sub$numSites ,na.rm=TRUE)))

par(mfrow=c(3,2))

simple.plot(allrecords.sub$ANALYSIS_YR,allrecords.sub$NATURAL_ADULT_SPAWNERS,lims = c(yr.lims, val.lims))
title(main = "Natural Adult Spawners - All Records")

simple.plot(filtered.sub$ANALYSIS_YR,filtered.sub$NATURAL_ADULT_SPAWNERS,lims = c(yr.lims, val.lims))
title(main = "Natural Adult Spawners - High Quality Records")

simple.plot(allrecords.sub$ANALYSIS_YR,safe.log(allrecords.sub$NATURAL_ADULT_SPAWNERS),lims = c(yr.lims, log.lims))
title(main = "Log Natural Adult Spawners - All Records")

simple.plot(filtered.sub$ANALYSIS_YR,safe.log(filtered.sub$NATURAL_ADULT_SPAWNERS),lims = c(yr.lims, log.lims))
title(main = "Log Natural Adult Spawners - High Quality Records")


simple.plot(allrecords.sub$ANALYSIS_YR,allrecords.sub$numSites,quantiles = FALSE,lims = c(yr.lims, num.lims))
title(main = "Num Sites - All Records")

simple.plot(filtered.sub$ANALYSIS_YR,filtered.sub$numSites,quantiles = FALSE,lims = c(yr.lims, num.lims))
title(main = "Num Sites - High Quality Records")

title(main=paste(species.plot,cu.plot,cu.name, sep=" - "), outer=TRUE,line=-1)  


}  # end looping through CU
  
dev.off()
  
} # end looping through species



