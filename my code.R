#9/17/2026
library(tidyverse)
#1. Read in parrotfish.csv data set. This data set represents the density of 13 parrotfish species observed by divers at each sample (PRIMARY_SAMPLE_UNIT) in 2021. This might be a typical way you would be given some data.
parrotfish <- read_csv("data/parrotfish.csv")

#2. Using the pivot_longer function, reshape the data frame structure to look like….
lng_pa <- parrotfish |> 
  pivot_longer(cols = "CRY ROSE":"SPA VIRI",
                 names_to = "SPECIES_CD",
               values_to = "density")

#3. Using your new dataframe created from #2, what is the mean number of parrotfish in each STRAT? Think through the problem first to develop your steps to find a solution.
#PSU is one diver pair (sum densities by each PSU first)
strat_avg <- lng_pa |> 
  group_by(YEAR, REGION, STRAT, PROT, PRIMARY_SAMPLE_UNIT) |> 
  summarize(sumDen = sum(density)) |> 
  ungroup() |> #using a function
  group_by(STRAT) |> 
  summarize(
    pa_mean = mean(sumDen))

strat_avg <- lng_pa |> 
  group_by(YEAR, REGION, STRAT, PROT, PRIMARY_SAMPLE_UNIT) |> 
  summarize(sumDen = sum(density), .groups = "drop") |> #using a parameter 
  group_by(STRAT) |> 
  summarize(
    pa_mean = mean(sumDen))

#4. Using the pivot_wider function, reshape the dataframe structure created from #2 to look like the original parrotfish.csv you imported. However, instead of SPECIES_CD as each column name, I want them to be Scientific Name. Hint: you will need the taxonomic.csv data set.
tax <- read_csv("data/taxonomic.csv")

lng_pa_sciname <- lng_pa |> 
  left_join(tax |> select(SPECIES_CD, SCINAME), by = "SPECIES_CD")

wide_pa <- lng_pa_sciname |> 
  select(-SPECIES_CD) |> 
  pivot_wider(names_from = SCINAME,
              values_from = density)

#5. The parrotfish.csv data set from #2 has a STRAT field that represents a habitat component and a depth category component. Separate the STRAT field into two columns “HABITAT” and “DEPTHCAT” while retaining the original STRAT column.
strat_sep <- lng_pa |> 
  separate(col = STRAT, into = c("HABITAT", "DEPTHCAT"), sep = "_", remove = FALSE)
 
#6. The parrotfish.csv data set from #2 has two fields YEAR and PRIMARY_SAMPLE_UNIT. Our NCRMP data reuses the PRIMARY_SAMPLE_UNIT numbers every year. Create a new data frame that concatenates the YEAR and PRIMARY_SAMPLE_UNIT fields into field called “ID” while retaining the original fields.
# Knowing that we reuse numbers for PRIMARY_SAMPLE_UNITS every year, what would happen if you had a dataset with multiple years of data and were asked to group_by() on the PRIMARY_SAMPLE_UNIT field but did not first concatenate the YEAR and PRIMARY_SAMPLE_UNIT fields?.
yr_psu <- lng_pa |> 
  unite(col = "ID", c("YEAR", "PRIMARY_SAMPLE_UNIT"), sep = "_", remove = FALSE)
#If you did not concatenate before grouping by the psu, you would get values for each psu across all the years. Therefore, you would not get a true understading of the data every year. 

#7. The depth class (embedded in the strat) represents depths shallower or greater than or equal to 12 meters. On average, do we observe higher parrotfish species richness in the shallower or deeper depths?
#is there a way to check work when looking at species richness?
#number of different species in that area (number of spp for deep & number of spp for shallow)
# right answers: d 3.61 s 3.56

# find the number of spp per psu
unique_spp_pres <- strat_sep |>  
  group_by(YEAR, REGION, DEPTHCAT,PRIMARY_SAMPLE_UNIT) |> 
  summarize(sum_pres = sum(density > 0), .groups = "drop") 
# density > 0 is taking the density column and seeing if there is a value greater than zero then it is present = TRUE = 1, if it is 0 then it is not present = FALSE = 0
# summed up all of the TRUEs / 1s per psu 
#can use if_else(0,0,1) to alter the data to make anything not 0 to be 1

spp_richness <- unique_spp_pres |> 
  group_by(DEPTHCAT) |> #avg the number of distinct species present by each depth cat
  summarize(
    spp_mean = mean(sum_pres))

