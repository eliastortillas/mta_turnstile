Analyzing MTA Turnstile data
================
April 2020

This markdown is to explain how to analyze turnstile data from the MTA
website in R.

Data can be found here: <http://web.mta.info/developers/turnstile.html>.

``` r
library(tidyverse)
library(readxl)

mta <- read_csv("/Users/eliasguerra/interactives/mta_turnstile/data/mta_example14st.csv") 
# This is just a shortened file. You can find the full one in file /data. 
```

Begin by loading the necessary libraries and reading in the data. I’m
using the tidyverse packages for most of the analysis so that code looks
different from base R. Next I’m going to clean up the data a bit.

Let’s take a look at the whole data frame.

``` r
str(mta)
```

    ## tibble [5,669 × 11] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ C/A     : chr [1:5669] "A033" "A033" "A033" "A033" ...
    ##  $ UNIT    : chr [1:5669] "R170" "R170" "R170" "R170" ...
    ##  $ SCP     : chr [1:5669] "'02-00-00" "'02-00-00" "'02-00-00" "'02-00-00" ...
    ##  $ STATION : chr [1:5669] "14 ST-UNION SQ" "14 ST-UNION SQ" "14 ST-UNION SQ" "14 ST-UNION SQ" ...
    ##  $ LINENAME: chr [1:5669] "LNQR456W" "LNQR456W" "LNQR456W" "LNQR456W" ...
    ##  $ DIVISION: chr [1:5669] "BMT" "BMT" "BMT" "BMT" ...
    ##  $ DATE    : POSIXct[1:5669], format: "2020-03-27" "2020-03-27" ...
    ##  $ TIME    : POSIXct[1:5669], format: "1899-12-31 01:00:00" "1899-12-31 05:00:00" ...
    ##  $ DESC    : chr [1:5669] "REGULAR" "REGULAR" "REGULAR" "REGULAR" ...
    ##  $ ENTRIES : num [1:5669] 337556 337556 337558 337563 337578 ...
    ##  $ EXITS   : num [1:5669] 1029327 1029331 1029381 1029418 1029468 ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   `C/A` = col_character(),
    ##   ..   UNIT = col_character(),
    ##   ..   SCP = col_character(),
    ##   ..   STATION = col_character(),
    ##   ..   LINENAME = col_character(),
    ##   ..   DIVISION = col_character(),
    ##   ..   DATE = col_datetime(format = ""),
    ##   ..   TIME = col_datetime(format = ""),
    ##   ..   DESC = col_character(),
    ##   ..   ENTRIES = col_double(),
    ##   ..   EXITS = col_double()
    ##   .. )

We’re going to need to reformat the date.

``` r
mta$DATE[2]
```

    ## [1] "2020-03-27 UTC"

``` r
mta_date <- as.character.Date(mta$DATE) %>% str_sub(1,10)
mta_time <- as.character(mta$TIME) %>% str_sub(12,21)
mta_dt <- paste(mta_date,"T",mta_time, sep = "") %>% lubridate::ymd_hms()

mta$date_time <- mta_dt
mta$just_date <- mta_date
mta$just_time <- mta_time

mta$date_time[2]
```

    ## [1] "2020-03-27 05:00:00 UTC"

New look, same great taste.

When you look at “ENTRIES” you see that they’re not starting from 0 so
we’re going to fix
    that.

``` r
head(mta$ENTRIES, 12)
```

    ##  [1]  337556  337556  337558  337563  337578  337594 2545124 2545124
    ##  [9] 2545130 2545136 2545150 2545161

``` r
# Create new column "new.entries" and "new.exits" 
# calculated from cumulative "ENTRIES"/"EXITS" for each station
new_entries <- vector(length = nrow(mta))
new_exits <- vector(length = nrow(mta))
new_scp <- vector(length = nrow(mta)) # new_scp indicates device ID (dif turnstile?)
for (i in 1:nrow(mta)) {
  new_entries[i] <- mta$ENTRIES[i+1] - mta$ENTRIES[i]
  new_exits[i] <- mta$EXITS[i+1] - mta$EXITS[i]
  new_scp[i] <- mta$SCP[i+1] != mta$SCP[i]
}

mta$new_entries <- new_entries # Not the same as "ENTRIES" 
mta$new_exits <- new_exits
new_scp[1] <- TRUE
mta$new_scp <- new_scp

head(mta, 2)
```

    ## # A tibble: 2 x 17
    ##   `C/A` UNIT  SCP   STATION LINENAME DIVISION DATE               
    ##   <chr> <chr> <chr> <chr>   <chr>    <chr>    <dttm>             
    ## 1 A033  R170  '02-… 14 ST-… LNQR456W BMT      2020-03-27 00:00:00
    ## 2 A033  R170  '02-… 14 ST-… LNQR456W BMT      2020-03-27 00:00:00
    ## # … with 10 more variables: TIME <dttm>, DESC <chr>, ENTRIES <dbl>,
    ## #   EXITS <dbl>, date_time <dttm>, just_date <chr>, just_time <chr>,
    ## #   new_entries <dbl>, new_exits <dbl>, new_scp <lgl>

``` r
mta_cleanr <- mta %>%
  filter(new_scp == F) %>% 
  select(date_time,STATION, LINENAME, SCP, ENTRIES, new_entries, new_exits, just_date, just_time)
head(mta_cleanr, 10)
```

    ## # A tibble: 10 x 9
    ##    date_time           STATION LINENAME SCP   ENTRIES new_entries new_exits
    ##    <dttm>              <chr>   <chr>    <chr>   <dbl>       <dbl>     <dbl>
    ##  1 2020-03-27 05:00:00 14 ST-… LNQR456W '02-…  3.38e5           2        50
    ##  2 2020-03-27 09:00:00 14 ST-… LNQR456W '02-…  3.38e5           5        37
    ##  3 2020-03-27 13:00:00 14 ST-… LNQR456W '02-…  3.38e5          15        50
    ##  4 2020-03-27 17:00:00 14 ST-… LNQR456W '02-…  3.38e5          16        21
    ##  5 2020-03-27 01:00:00 14 ST-… LNQR456W '02-…  2.55e6           0         3
    ##  6 2020-03-27 05:00:00 14 ST-… LNQR456W '02-…  2.55e6           6        67
    ##  7 2020-03-27 09:00:00 14 ST-… LNQR456W '02-…  2.55e6           6        32
    ##  8 2020-03-27 13:00:00 14 ST-… LNQR456W '02-…  2.55e6          14        34
    ##  9 2020-03-27 17:00:00 14 ST-… LNQR456W '02-…  2.55e6          11        13
    ## 10 2020-03-27 01:00:00 14 ST-… LNQR456W '02-…  1.51e7           0         3
    ## # … with 2 more variables: just_date <chr>, just_time <chr>

``` r
#This will take the total rides per day and the average rides every four hours. 
mta_perday <-
  mta_cleanr %>%
  group_by(STATION, just_date) %>%
  summarise(avg_entries = mean(new_entries), # avg is four each unit of time data was collected, four hours
            total_entries = sum(new_entries),
            avg_exits = mean(new_exits),
            total_exits = sum(new_exits)) %>%
  filter(just_date != "1899-12-31") # This date gets included for some reason
head(mta_perday, 10)
```

    ## # A tibble: 10 x 6
    ## # Groups:   STATION [1]
    ##    STATION        just_date avg_entries total_entries avg_exits total_exits
    ##    <chr>          <chr>           <dbl>         <dbl>     <dbl>       <dbl>
    ##  1 14 ST-UNION SQ 2020-02-…        212.         28555      217.       29266
    ##  2 14 ST-UNION SQ 2020-02-…        162.         21871      156.       21021
    ##  3 14 ST-UNION SQ 2020-02-…        397.         53584      420.       56657
    ##  4 14 ST-UNION SQ 2020-02-…        398.         53719      429.       57897
    ##  5 14 ST-UNION SQ 2020-02-…        410.         55324      444.       59991
    ##  6 14 ST-UNION SQ 2020-02-…        403.         54424      439.       59274
    ##  7 14 ST-UNION SQ 2020-02-…        396.         53439      431.       58242
    ##  8 14 ST-UNION SQ 2020-02-…        203.         27424      212.       28566
    ##  9 14 ST-UNION SQ 2020-03-…        153.         20696      153.       20614
    ## 10 14 ST-UNION SQ 2020-03-…        391.         52761      415.       56080

We have a our data in a form we can use now.

``` r
head(mta_cleanr) #This is the cleaned up data
```

    ## # A tibble: 6 x 9
    ##   date_time           STATION LINENAME SCP   ENTRIES new_entries new_exits
    ##   <dttm>              <chr>   <chr>    <chr>   <dbl>       <dbl>     <dbl>
    ## 1 2020-03-27 05:00:00 14 ST-… LNQR456W '02-…  337556           2        50
    ## 2 2020-03-27 09:00:00 14 ST-… LNQR456W '02-…  337558           5        37
    ## 3 2020-03-27 13:00:00 14 ST-… LNQR456W '02-…  337563          15        50
    ## 4 2020-03-27 17:00:00 14 ST-… LNQR456W '02-…  337578          16        21
    ## 5 2020-03-27 01:00:00 14 ST-… LNQR456W '02-… 2545124           0         3
    ## 6 2020-03-27 05:00:00 14 ST-… LNQR456W '02-… 2545124           6        67
    ## # … with 2 more variables: just_date <chr>, just_time <chr>

Time to graph it.

``` r
mta_perday %>% filter(STATION == "14 ST-UNION SQ") %>%
  mutate(month_day = str_sub(just_date, 6,10)) %>%
  ggplot(aes(x = month_day, y = total_entries)) +
  geom_bar(stat = "identity") +
  ylab("Daily turnstiles entries") + xlab("Date") + 
  ggtitle("Total daily turnstile entries at 14-Street Union Sq.", ) +
  coord_flip() 
```

![](turnstile_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Now we’re going to combine the the income-geolocated data from Sam.

``` r
data_files <- list.files("~/interactives/mta_turnstile/data")
setwd("~/interactives/mta_turnstile/data")
geo_income <- read_csv(data_files[1])
```

    ## Parsed with column specification:
    ## cols(
    ##   the_geom = col_character(),
    ##   lat = col_double(),
    ##   long = col_double(),
    ##   station_name = col_character(),
    ##   borough = col_character(),
    ##   train_lines = col_character(),
    ##   service = col_character(),
    ##   unit = col_character(),
    ##   `c-a` = col_character(),
    ##   ct_median_income_2018_ACS = col_character(),
    ##   ct_shape_leng = col_double(),
    ##   ct_shape_area = col_double()
    ## )

Let’s compare the two data sets we’re working with rn.

``` r
head(select(geo_income, -the_geom)) #left out one column for mapping
```

    ## # A tibble: 6 x 11
    ##     lat  long station_name borough train_lines service unit  `c-a`
    ##   <dbl> <dbl> <chr>        <chr>   <chr>       <chr>   <chr> <chr>
    ## 1  40.5 -74.2 ELTINGVILLE… Staten… Z           SRT     X002  R470 
    ## 2  40.6 -74.1 TOMPKINSVIL… Staten… 1           SRT     S102  R165 
    ## 3  40.6 -74.1 ST. GEORGE   Staten… 1           SRT     S101  R070 
    ## 4  40.6 -74.1 ST. GEORGE   Staten… 1           SRT     S101A R070 
    ## 5  40.6 -74.0 BAY RIDGE-9… Brookl… R           BMT     C027  R216 
    ## 6  40.6 -74.0 BAY RIDGE-9… Brookl… R           BMT     C028  R216 
    ## # … with 3 more variables: ct_median_income_2018_ACS <chr>,
    ## #   ct_shape_leng <dbl>, ct_shape_area <dbl>

``` r
head(mta_perday)
```

    ## # A tibble: 6 x 6
    ## # Groups:   STATION [1]
    ##   STATION        just_date  avg_entries total_entries avg_exits total_exits
    ##   <chr>          <chr>            <dbl>         <dbl>     <dbl>       <dbl>
    ## 1 14 ST-UNION SQ 2020-02-22        212.         28555      217.       29266
    ## 2 14 ST-UNION SQ 2020-02-23        162.         21871      156.       21021
    ## 3 14 ST-UNION SQ 2020-02-24        397.         53584      420.       56657
    ## 4 14 ST-UNION SQ 2020-02-25        398.         53719      429.       57897
    ## 5 14 ST-UNION SQ 2020-02-26        410.         55324      444.       59991
    ## 6 14 ST-UNION SQ 2020-02-27        403.         54424      439.       59274

We;re going to want to combine the data by station name so let’s compare
the names in both data
frames.

``` r
mta_stations <- mta_perday$STATION %>% unique %>% data_frame(station_names=.) 
```

    ## Warning: `data_frame()` is deprecated as of tibble 1.1.0.
    ## Please use `tibble()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
geo_station <- geo_income$station_name %>% unique %>% data_frame(station_names=.)
overlapping_stations <- c(mta_stations$station_names, geo_station$station_names)
```

It’s really bad:

``` r
unique(overlapping_stations) %>% sort()
```

    ##   [1] "1 AVE"           "103 ST"          "103 ST-CORONA"  
    ##   [4] "104 ST"          "110 ST"          "110 ST-CATHEDRL"
    ##   [7] "110 ST-CPN"      "111 ST"          "116 ST"         
    ##  [10] "116 ST-COLUMBIA" "121 ST"          "125 ST"         
    ##  [13] "135 ST"          "137 ST-CITY COL" "138 ST-3 AVE"   
    ##  [16] "138 ST-GR CONC"  "14 ST"           "14 ST-6 AVE"    
    ##  [19] "14 ST-UNION SQ"  "145 ST"          "148 ST-LENOX"   
    ##  [22] "149 ST-3 AVE"    "149 ST-GR CONC"  "14TH STREET"    
    ##  [25] "15 ST-PROSPECT"  "155 ST"          "157 ST"         
    ##  [28] "161 ST-YANKEE"   "163 ST-AMSTERDM" "167 ST"         
    ##  [31] "168 ST-BROADWAY" "169 ST"          "170 ST"         
    ##  [34] "174 ST"          "174-175 ST"      "175 ST"         
    ##  [37] "176 ST"          "18 AVE"          "18 ST"          
    ##  [40] "181 ST"          "182-183 ST"      "183 ST"         
    ##  [43] "190 ST"          "191 ST"          "2 AVE"          
    ##  [46] "2 BDWY CUST SRV" "20 AVE"          "207 ST"         
    ##  [49] "21 ST"           "215 ST"          "219 ST"         
    ##  [52] "22 AVE-BAY PKY"  "225 ST"          "23 ST"          
    ##  [55] "23 ST-5 AVE"     "23 ST-6 AVE"     "231 ST"         
    ##  [58] "233 ST"          "238 ST"          "242 ST"         
    ##  [61] "25 AVE"          "25 ST"           "28 ST"          
    ##  [64] "28 ST-BROADWAY"  "3 AVE"           "33 ST"          
    ##  [67] "33 ST/RAWSON ST" "34 ST-HERALD SQ" "34 ST-PENN STA" 
    ##  [70] "36 ST"           "4 AVE"           "40 ST-LOWERY ST"
    ##  [73] "42 ST-BRYANT PK" "42 ST-GRD CNTRL" "42 ST-PA BUS TE"
    ##  [76] "42 ST-TIMES SQ"  "45 ST"           "46 ST"          
    ##  [79] "46 ST-BLISS ST"  "47-50 ST-ROCK"   "49 ST-7 AVE"    
    ##  [82] "5 AVE-53 ST"     "5 AVE-59 ST"     "5 AVE-BRYANT PK"
    ##  [85] "50 ST"           "51 ST"           "52 ST-LINCOLN"  
    ##  [88] "53 ST"           "55 ST"           "57 ST"          
    ##  [91] "57 ST-7 AVE"     "59 ST"           "59 ST-COLUMBUS" 
    ##  [94] "6 AVE"           "61 ST/WOODSIDE"  "63 DR-REGO PARK"
    ##  [97] "65 ST"           "66 ST-LINCOLN"   "67 AVE"         
    ## [100] "68ST-HUNTER COL" "69 ST-FISK AVE"  "7 AV-PARK SLOPE"
    ## [103] "7 AVE"           "7 AVE-53 ST"     "71 ST"          
    ## [106] "72 ST"           "74 ST-BROADWAY"  "75 AVE"         
    ## [109] "77 ST"           "79 ST"           "8 AVE"          
    ## [112] "8 ST-B'WAY NYU"  "81 ST-MUSEUM"    "82 ST-JACKSON H"
    ## [115] "86 ST"           "9 AVE"           "9 ST"           
    ## [118] "90 ST-ELMHURST"  "96 ST"           "ALABAMA AVE"    
    ## [121] "ALLERTON AVE"    "AQUEDUCT TRACK"  "AQUEDUCT-N CNDT"
    ## [124] "ASTOR PLACE"     "ATLANTIC AVE"    "AVE H"          
    ## [127] "AVE I"           "AVE J"           "AVE M"          
    ## [130] "AVE N"           "AVE P"           "AVE U"          
    ## [133] "AVE X"           "BAY 50 ST"       "BAY PARKWAY"    
    ## [136] "BAY PKY-22 AVE"  "BAY RIDGE AVE"   "BAY RIDGE-95 ST"
    ## [139] "BAYCHESTER AVE"  "BEACH 105 ST"    "BEACH 25 ST"    
    ## [142] "BEACH 36 ST"     "BEACH 44 ST"     "BEACH 60 ST"    
    ## [145] "BEACH 67 ST"     "BEACH 90 ST"     "BEACH 98 ST"    
    ## [148] "BEDFORD AVE"     "BEDFORD PARK BL" "BEDFORD/NOSTRAN"
    ## [151] "BEEBE-39 AVE"    "BERGEN ST"       "BEVERLEY ROAD"  
    ## [154] "BEVERLY ROAD"    "BLEECKER ST"     "BOROUGH HALL/CT"
    ## [157] "BOTANIC GARDEN"  "BOWERY"          "BOWLING GREEN"  
    ## [160] "BOYD-88 ST"      "BRIGHTON BEACH"  "BROAD CHANNEL"  
    ## [163] "BROAD ST"        "BROADWAY"        "BROADWAY-31 ST" 
    ## [166] "BROADWAY-ENY"    "BROADWAY/LAFAY"  "BRONX PARK EAST"
    ## [169] "BROOK AVE"       "BROOKLYN BRIDGE" "BUHRE AVE"      
    ## [172] "BURKE AVE"       "BURNSIDE AVE"    "BUSHWICK AVE"   
    ## [175] "CANAL ST"        "CARROLL ST"      "CASTLE HILL AVE"
    ## [178] "CATHEDRL-110 ST" "CENTRAL AVE"     "CHAMBERS ST"    
    ## [181] "CHAUNCEY ST"     "CHRISTOPHER ST"  "CHURCH AVE"     
    ## [184] "CLARK ST"        "CLASSON AVE"     "CLEVELAND ST"   
    ## [187] "CLINTON-WASH AV" "CORTELYOU ROAD"  "CORTLANDT ST"   
    ## [190] "COURT SQ"        "COURT SQ-23 ST"  "CRESCENT ST"    
    ## [193] "CROWN HTS-UTICA" "CYPRESS AVE"     "CYPRESS HILLS"  
    ## [196] "DEKALB AVE"      "DELANCEY ST"     "DITMARS BL-31 S"
    ## [199] "DITMAS AVE"      "DYCKMAN ST"      "DYCKMAN-200 ST" 
    ## [202] "DYRE AVE"        "E 143 ST"        "E 149 ST"       
    ## [205] "E 177 ST-PARKCH" "E 180 ST"        "E TREMONT AVE"  
    ## [208] "EAST 105 ST"     "EAST BROADWAY"   "EASTERN PKWY"   
    ## [211] "ELDER AVE"       "ELDERTS LANE"    "ELMHURST AVE"   
    ## [214] "ELTINGVILLE PK"  "ESSEX ST"        "EUCLID AVE"     
    ## [217] "FAR ROCKAWAY"    "FLATBUSH AVE"    "FLUSHING AVE"   
    ## [220] "FORDHAM ROAD"    "FOREST AVE"      "FOREST HILLS-71"
    ## [223] "FOREST PARKWAY"  "FRANKLIN AVE"    "FRANKLIN ST"    
    ## [226] "FREEMAN ST"      "FRESH POND ROAD" "FT HAMILTON PKY"
    ## [229] "FULTON ST"       "GATES AVE"       "GRAHAM AVE"     
    ## [232] "GRAND ARMY PLAZ" "GRAND AV-NEWTON" "GRAND ST"       
    ## [235] "GRAND-30 AVE"    "GRANT AVE"       "GREENPOINT AVE" 
    ## [238] "GREENWOOD-111"   "GUN HILL ROAD"   "HALSEY ST"      
    ## [241] "HEWES ST"        "HIGH ST"         "HOUSTON ST"     
    ## [244] "HOYT ST"         "HOYT ST-ASTORIA" "HOYT/SCHERMER"  
    ## [247] "HUDSON-80 ST"    "HUNTERS PT AVE"  "HUNTS POINT AVE"
    ## [250] "INTERVALE-163"   "INWOOD-207 ST"   "JACKSON AVE"    
    ## [253] "JAMAICA CENTER"  "JAMAICA-179 ST"  "JAMAICA-VAN WYC"
    ## [256] "JAY ST-METROTEC" "JEFFERSON ST"    "JUNCTION BLVD"  
    ## [259] "JUNIUS ST"       "KINGS HIGHWAY"   "KINGSBRIDGE RD" 
    ## [262] "KINGSTON AVE"    "KINGSTON-THROOP" "KNICKERBOCKER"  
    ## [265] "KOSCIUSZKO ST"   "LAFAYETTE AVE"   "LEFFERTS BLVD"  
    ## [268] "LEXINGTON AVE"   "LEXINGTON-53 ST" "LGA AIRPORT CTB"
    ## [271] "LIBERTY AVE"     "LIVONIA AVE"     "LONGWOOD AVE"   
    ## [274] "LORIMER ST"      "MAIN ST"         "MARBLE HILL-225"
    ## [277] "MARCY AVE"       "METROPOLITAN AV" "METS-WILLETS PT"
    ## [280] "MIDDLETOWN ROAD" "MONTROSE AVE"    "MORGAN AVE"     
    ## [283] "MORRIS PARK"     "MORRISON AVE"    "MOSHOLU PARKWAY"
    ## [286] "MT EDEN AVE"     "MURRAY ST-B'WAY" "MYRTLE AVE"     
    ## [289] "MYRTLE-WILLOUGH" "NASSAU AV"       "NECK ROAD"      
    ## [292] "NEPTUNE AVE"     "NEREID AVE"      "NEVINS ST"      
    ## [295] "NEW LOTS AVE"    "NEW UTRECHT AVE" "NEWKIRK AVE"    
    ## [298] "NORTHERN BLVD"   "NORWOOD AVE"     "NORWOOD-205 ST" 
    ## [301] "NOSTRAND AVE"    "OCEAN PARKWAY"   "ORCHARD BEACH"  
    ## [304] "OXFORD-104 ST"   "PACIFIC ST"      "PARK PLACE"     
    ## [307] "PARKSIDE AVE"    "PARSONS BLVD"    "PATH WTC"       
    ## [310] "PATH WTC 2"      "PELHAM BAY PARK" "PELHAM PARKWAY" 
    ## [313] "PENNSYLVANIA AV" "PRESIDENT ST"    "PRINCE ST-B'WAY"
    ## [316] "PROSPECT AVE"    "PROSPECT PARK"   "QUEENS PLAZA"   
    ## [319] "QUEENSBORO PLZ"  "RALPH AVE"       "RECTOR ST"      
    ## [322] "RIT-MANHATTAN"   "RIT-ROOSEVELT"   "ROCKAWAY AVE"   
    ## [325] "ROCKAWAY BLVD"   "ROCKAWAY PK 116" "ROCKAWAY PKY"   
    ## [328] "ROOSEVELT AVE"   "ROOSEVELT IS"    "SARATOGA AVE"   
    ## [331] "SENECA AVE"      "SHEEPSHEAD BAY"  "SHEPHERD AVE"   
    ## [334] "SIMPSON ST"      "SMITH-9 ST"      "SOUTH FERRY"    
    ## [337] "SPRING ST"       "ST LAWRENCE AVE" "ST. GEORGE"     
    ## [340] "STEINWAY ST"     "STERLING ST"     "STILLWELL AVE"  
    ## [343] "SUTPHIN BLVD"    "SUTTER AVE"      "THIRTY THIRD ST"
    ## [346] "TOMPKINSVILLE"   "TREMONT AVE"     "TWENTY THIRD ST"
    ## [349] "UNION ST"        "UNION TPK-KEW G" "UTICA AVE"      
    ## [352] "VAN ALSTON-21ST" "VAN SICLEN AVE"  "VAN WYCK BLVD"  
    ## [355] "VERNON/JACKSON"  "W 4 ST-WASH SQ"  "W 8 ST-AQUARIUM"
    ## [358] "WAKEFIELD-241"   "WALL ST"         "WASHINGTON-36 A"
    ## [361] "WESTCHESTER SQ"  "WHITEHALL ST"    "WHITLOCK AVE"   
    ## [364] "WILSON AVE"      "WINTHROP ST"     "WOODHAVEN BLVD" 
    ## [367] "WOODLAWN ROAD"   "WORLD TRADE CTR" "YORK ST"        
    ## [370] "ZEREGA AVE"
