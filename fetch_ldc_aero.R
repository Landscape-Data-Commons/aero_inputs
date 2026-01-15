library(tidyverse)
library(trex)

username = "wheelerbrandi6@gmail.com"
password = "Pot@to555"

my_token <- trex::get_ldc_token(username = username, password = password)

# You need to add the argument for the token to your code.
# If you also provide username and password, then it'll automatically
# grab a new token when the current one expires. When that happens, just
# rerun the above code because it's just a stopgap to prevent a single
# call of the function from failing if it makes multiple queries
# and runs out of token time.


# we go through a flow of selecting the data of interest by removing the project
# keys we don't want, and then from there selecting the data
# that has data from the previous method retrieval such as gap or height (ie is not NA or duplicated)
# start by retrieving header since it requires the least effort to retrieve
header <- trex::fetch_ldc(data_type = "header",
                    token = my_token,
                    username = username,
                    password = password)

# this is the names of the ProjectKeys that you do not want to get the data for
# BLM_AIM is just very large, and we don't run AERO for NWERN sites since 
# hflux is measured there

target <- c("BLM_AIM","NWERN_Moab", "NWERN_Lordsburg", "NWERN_HAFB",
            "NWERN_Mandan", "NWERN_TwinValley","NWERN_JER", "NWERN_CPER",
            "NWERN_Akron", "NWERN_ElReno", "NWERN_Morton", "NWERN_Pullman",
            "NWERN_SLV","NWERN_RedHills")

`%notin%` <- Negate(`%in%`)

headeroi <- header[header$ProjectKey %notin% target,]

pkeys <- headeroi$PrimaryKey


gap <- trex::fetch_ldc(data_type = "gap", token = my_token, keys = pkeys , key_type = "PrimaryKey",)

#write.csv(gap, "C:/yourfoldername/gap_nwern.csv") # you can write the outputs here
# to save the data you've retrieved thus far - recommended for BLM_AIM because of how long
# it takes to retrieve the data in the first place

target_data <- gap[!duplicated(gap$PrimaryKey),]

target <- target_data$PrimaryKey

height <- trex::fetch_ldc(data_type = "height", keys = target , key_type = "PrimaryKey", token = my_token)

#write.csv(height, "C:/yourfoldername/height_where_gap.csv")

target_data <- height[!duplicated(height$PrimaryKey),]
target <- target_data$PrimaryKey

lpi <- trex::fetch_ldc(data_type = "lpi", keys = target , key_type = "PrimaryKey", token = my_token)

#write.csv(lpi, "C:/yourfoldername/lpi_where_gapheight.csv")

target_data <- lpi[!duplicated(lpi$PrimaryKey),]
target <- target_data$PrimaryKey

ind <- trex::fetch_ldc(data_type = "indicators", keys = target , key_type = "PrimaryKey", token = my_token)
ind <- ind[!duplicated(ind$PrimaryKey),]

geoind <- ind[!is.na(ind$BareSoil),]

# need every plot with bare soil assignment as well
target <- geoind$PrimaryKey

header_aero <- header %>% dplyr::filter(PrimaryKey %in% target)

gap <- gap %>% dplyr::filter(PrimaryKey %in% target)

height <- height %>% dplyr::filter(PrimaryKey %in% target)

lpi <- lpi %>% dplyr::filter(PrimaryKey %in% target)


write.csv(gap, "C:/yourfoldername/Tall/gap_tall.csv")
write.csv(height, "C:/yourfoldername/Tall/height_tall.csv")
write.csv(lpi, "C:/yourfoldername/Tall/lpi_tall.csv")
write.csv(geoind, "C:/Users/bwheeler.ACN/Documents/LDC_Data/Processing/projects_todo/AERO/geoIndicators.csv")
write.csv(header_aero, "C:/yourfoldername/Tall/header.csv")

