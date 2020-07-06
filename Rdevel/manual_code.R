

# Back Matter - not run officially but needed before hand for the nice stuff

## create dem file
```
## create dem file

dem_long_name= dem_long_name$value
dem_units= dem_units$value

save(dem_frame, dem_long_name,dem_units,
     file = "../gtopo30_016_Cheyenne.RData")

```

## Produce Variable List


```
# Produce Variable List


data_file_location = str_c("~/GitHub/LOCA_TREWARTHA_KOEPPEN/local_climatologies/LOCA_THORNTHWAITE_ALL_ENSEMBLES_CLIM_",
                      period_length,
                      "-yr.nc", 
                      sep = "")


thornthwaite_variable_list = nc_atts(data_file_location) %>% 
  select( c("variable","name","value") )  %>%
  rename(attribute = "name") %>%
  spread(key = attribute, value = value) %>%
  select("variable", "long_name", "units") %>%
  unnest(cols = c(units,long_name)) %>%
  arrange(variable)

thornthwaite_variable_dims = nc_vars(data_file_location) %>%
  rename(variable = "name") %>%
  select(c("variable","ndims"))  %>%
  unnest(cols = c(ndims))

thornthwaite_variable_list = left_join(thornthwaite_variable_list,thornthwaite_variable_dims) %>%
  filter((ndims >= 6) & (!is.na(ndims))) %>%
  select("variable", "long_name", "units") %>%
  arrange(variable)

thornthwaite_variable_list 
save(thornthwaite_variable_list, 
     file = "../Thornthwaite_Budget_Parameters.RData")
```
## Produce Period List

```
# Produce Period List
splitted= str_split(Period , pattern="-", simplify = TRUE)


period_length="10"
period_file_location = str_c("~/GitHub/LOCA_CHEYENNE_THORNTHWAITE_WEB/Cheyenne_climatology_years_",
                             period_length,
                             "-yr.RData",
                             sep="")

load(file = period_file_location)



climatology_period_info = climatology_period_info %>% 
  mutate(string_rcp45 = ifelse(test  = (end_year < 2006),
                               yes =        "Historical",
                               no  =        "RCP 4.5"))
                                             
historical_climatology_period_info = climatology_period_info %>% filter(string_rcp45 == "Historical")

future_climatology_period_info = climatology_period_info %>% filter(string_rcp45 == "RCP 4.5")

save(climatology_period_info, historical_climatology_period_info, future_climatology_period_info,
     file = period_file_location)
```

