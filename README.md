Analyzing MTA Turnstile data
================
April 2020

This markdown is to explain how to analyze turnstile data from the MTA
website in R.

Data can be found here: <http://web.mta.info/developers/turnstile.html>.

``` r
library(tidyverse)
library(readxl)

mta <- read_csv("~/interactives/mta_turnstile/data/mta_example14st.csv") 
# This is just a shortened file only including 14th Street Union Square. You can find the full one in  /data. 
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

I need to add an explanation of what I’m doing ^here.

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
# write_csv(mta_cleanr, "~/interactives/mta_turnstile/data/export/mta_clean.csv")
# I did this on my computer with the whole file. i'll use this later. 
```

We have a our data in a form we can use now.

``` r
#overwriting the above file to access the whole file (it's big af)
mta_cleanr <- read_csv("~/interactives/mta_turnstile/data/export/mta_clean.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   date_time = col_datetime(format = ""),
    ##   STATION = col_character(),
    ##   LINENAME = col_character(),
    ##   SCP = col_character(),
    ##   ENTRIES = col_double(),
    ##   new_entries = col_double(),
    ##   new_exits = col_double(),
    ##   just_date = col_date(format = ""),
    ##   just_time = col_time(format = "")
    ## )

``` r
mta_cleanr
```

    ## # A tibble: 910,221 x 9
    ##    date_time           STATION LINENAME SCP   ENTRIES new_entries new_exits
    ##    <dttm>              <chr>   <chr>    <chr>   <dbl>       <dbl>     <dbl>
    ##  1 2020-03-27 04:00:00 59 ST   NQR456W  '02-… 7412671           9        21
    ##  2 2020-03-27 08:00:00 59 ST   NQR456W  '02-… 7412680          19        24
    ##  3 2020-03-27 12:00:00 59 ST   NQR456W  '02-… 7412699          49        10
    ##  4 2020-03-27 16:00:00 59 ST   NQR456W  '02-… 7412748          59         9
    ##  5 2020-03-27 00:00:00 59 ST   NQR456W  '02-… 6588992           1         2
    ##  6 2020-03-27 04:00:00 59 ST   NQR456W  '02-… 6588993           4        16
    ##  7 2020-03-27 08:00:00 59 ST   NQR456W  '02-… 6588997           7         8
    ##  8 2020-03-27 12:00:00 59 ST   NQR456W  '02-… 6589004          46         8
    ##  9 2020-03-27 16:00:00 59 ST   NQR456W  '02-… 6589050          46         2
    ## 10 2020-03-27 00:00:00 59 ST   NQR456W  '02-… 1376224           0         1
    ## # … with 910,211 more rows, and 2 more variables: just_date <date>,
    ## #   just_time <time>

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
    ##    STATION just_date  avg_entries total_entries avg_exits total_exits
    ##    <chr>   <date>           <dbl>         <dbl>     <dbl>       <dbl>
    ##  1 1 AV    2020-02-22       113.           6015     148.         7832
    ##  2 1 AV    2020-02-23        28.1          4273      38.5        5846
    ##  3 1 AV    2020-02-24       267.          14697     372.        20436
    ##  4 1 AV    2020-02-25       283.          15553     389.        21402
    ##  5 1 AV    2020-02-26       294.          16167     375.        20627
    ##  6 1 AV    2020-02-27       299.          16453     403.        22173
    ##  7 1 AV    2020-02-28       309.          16985     415.        22799
    ##  8 1 AV    2020-02-29       109.           6000     146.         8005
    ##  9 1 AV    2020-03-01        77.0          4234     104.         5713
    ## 10 1 AV    2020-03-02       270.          14854     356.        19593

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

Now we’re going to combine the the income-geolocated data from Sam. Also
going to read in the full cleaned up data, mta\_cleanr. (I just did this
to save time compiling on my
computer)

``` r
geo_income <- read_csv("~/interactives/mta_turnstile/data/2018-med-income-ACS_by_subway-station - 2018-med-income-ACS_by_subway-station.csv")
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
    ##   STATION just_date  avg_entries total_entries avg_exits total_exits
    ##   <chr>   <date>           <dbl>         <dbl>     <dbl>       <dbl>
    ## 1 1 AV    2020-02-22       113.           6015     148.         7832
    ## 2 1 AV    2020-02-23        28.1          4273      38.5        5846
    ## 3 1 AV    2020-02-24       267.          14697     372.        20436
    ## 4 1 AV    2020-02-25       283.          15553     389.        21402
    ## 5 1 AV    2020-02-26       294.          16167     375.        20627
    ## 6 1 AV    2020-02-27       299.          16453     403.        22173

We;re going to want to combine the data by station name so let’s compare
the names in both data
frames.

``` r
mta_stations <- mta_cleanr$STATION %>% unique %>% tibble(station_names=.) 
geo_station <- geo_income$station_name %>% unique %>% tibble(station_names=.)
both_station_names <- c(mta_stations$station_names, geo_station$station_names) 
sort(both_station_names)
```

    ##   [1] "1"               "1 AV"            "1 AVE"          
    ##   [4] "103 ST"          "103 ST"          "103 ST-CORONA"  
    ##   [7] "104 ST"          "104 ST"          "110 ST"         
    ##  [10] "110 ST-CATHEDRL" "110 ST-CPN"      "111 ST"         
    ##  [13] "111 ST"          "116 ST"          "116 ST"         
    ##  [16] "116 ST-COLUMBIA" "121 ST"          "121 ST"         
    ##  [19] "123"             "1237ACENQRS"     "1237ACENQRSW"   
    ##  [22] "123ACE"          "123FLM"          "125 ST"         
    ##  [25] "125 ST"          "135 ST"          "135 ST"         
    ##  [28] "137 ST-CITY COL" "138 ST-3 AVE"    "138 ST-GR CONC" 
    ##  [31] "14 ST"           "14 ST"           "14 ST-6 AVE"    
    ##  [34] "14 ST-UNION SQ"  "14 ST-UNION SQ"  "145 ST"         
    ##  [37] "145 ST"          "148 ST-LENOX"    "149 ST-3 AVE"   
    ##  [40] "149 ST-GR CONC"  "14TH STREET"     "14TH STREET"    
    ##  [43] "15 ST-PROSPECT"  "15 ST-PROSPECT"  "155 ST"         
    ##  [46] "155 ST"          "157 ST"          "161 ST-YANKEE"  
    ##  [49] "161/YANKEE STAD" "163 ST-AMSTERDM" "163 ST-AMSTERDM"
    ##  [52] "167 ST"          "167 ST"          "168 ST"         
    ##  [55] "168 ST-BROADWAY" "169 ST"          "169 ST"         
    ##  [58] "170 ST"          "170 ST"          "174 ST"         
    ##  [61] "174-175 ST"      "174-175 STS"     "175 ST"         
    ##  [64] "175 ST"          "176 ST"          "18 AV"          
    ##  [67] "18 AVE"          "18 ST"           "181 ST"         
    ##  [70] "181 ST"          "182-183 ST"      "182-183 STS"    
    ##  [73] "183 ST"          "190 ST"          "190 ST"         
    ##  [76] "191 ST"          "1ABCD"           "1AC"            
    ##  [79] "1RW"             "2"               "2 AV"           
    ##  [82] "2 AVE"           "2 BDWY CUST SRV" "20 AV"          
    ##  [85] "20 AVE"          "207 ST"          "21 ST"          
    ##  [88] "21 ST"           "21 ST-QNSBRIDGE" "215 ST"         
    ##  [91] "219 ST"          "22 AVE-BAY PKY"  "225 ST"         
    ##  [94] "23"              "23 ST"           "23 ST"          
    ##  [97] "23 ST-5 AVE"     "23 ST-6 AVE"     "231 ST"         
    ## [100] "233 ST"          "2345"            "2345ACJZ"       
    ## [103] "2345BDNQR"       "2345R"           "2345S"          
    ## [106] "238 ST"          "23ACE"           "242 ST"         
    ## [109] "245"             "25"              "25 AV"          
    ## [112] "25 AVE"          "25 ST"           "25 ST"          
    ## [115] "28 ST"           "28 ST"           "28 ST-BROADWAY" 
    ## [118] "3"               "3 AV"            "3 AVE"          
    ## [121] "33 ST"           "33 ST"           "33 ST/RAWSON ST"
    ## [124] "34"              "34 ST-HERALD SQ" "34 ST-HERALD SQ"
    ## [127] "34 ST-PENN STA"  "34 ST-PENN STA"  "36 ST"          
    ## [130] "36 ST"           "4"               "4 AV-9 ST"      
    ## [133] "4 AVE"           "40 ST-LOWERY ST" "42 ST-BRYANT PK"
    ## [136] "42 ST-BRYANT PK" "42 ST-GRD CNTRL" "42 ST-PA BUS TE"
    ## [139] "42 ST-PORT AUTH" "42 ST-TIMES SQ"  "45"             
    ## [142] "45 ST"           "45 ST"           "456"            
    ## [145] "4567S"           "456JZ"           "456LNQRW"       
    ## [148] "456NQRW"         "46 ST"           "46 ST"          
    ## [151] "46 ST-BLISS ST"  "47-50 ST-ROCK"   "47-50 STS ROCK" 
    ## [154] "49 ST"           "49 ST-7 AVE"     "4AV-9 ST"       
    ## [157] "4BD"             "5"               "5 AV/53 ST"     
    ## [160] "5 AV/59 ST"      "5 AVE-53 ST"     "5 AVE-59 ST"    
    ## [163] "5 AVE-BRYANT PK" "50 ST"           "50 ST"          
    ## [166] "51 ST"           "51 ST"           "52 ST-LINCOLN"  
    ## [169] "53 ST"           "53 ST"           "55 ST"          
    ## [172] "55 ST"           "57 ST"           "57 ST"          
    ## [175] "57 ST-7 AV"      "57 ST-7 AVE"     "59 ST"          
    ## [178] "59 ST"           "59 ST COLUMBUS"  "59 ST-COLUMBUS" 
    ## [181] "6"               "6 AV"            "6 AVE"          
    ## [184] "61 ST/WOODSIDE"  "63 DR-REGO PARK" "63 DR-REGO PARK"
    ## [187] "65 ST"           "65 ST"           "66 ST-LINCOLN"  
    ## [190] "66 ST-LINCOLN"   "67 AV"           "67 AVE"         
    ## [193] "68ST-HUNTER COL" "69 ST-FISK AVE"  "7"              
    ## [196] "7 AV"            "7 AV-PARK SLOPE" "7 AVE"          
    ## [199] "7 AVE-53 ST"     "71 ST"           "71 ST"          
    ## [202] "72 ST"           "72 ST"           "72 ST-2 AVE"    
    ## [205] "74 ST-BROADWAY"  "75 AV"           "75 AVE"         
    ## [208] "75 ST-ELDERTS"   "77 ST"           "77 ST"          
    ## [211] "79 ST"           "79 ST"           "7BDFM"          
    ## [214] "7EFMR"           "7NQW"            "8 AV"           
    ## [217] "8 AVE"           "8 ST-B'WAY NYU"  "8 ST-NYU"       
    ## [220] "80 ST"           "81 ST-MUSEUM"    "81 ST-MUSEUM"   
    ## [223] "82 ST-JACKSON H" "85 ST-FOREST PK" "86 ST"          
    ## [226] "86 ST"           "86 ST-2 AVE"     "88 ST"          
    ## [229] "9 AV"            "9 AVE"           "9 ST"           
    ## [232] "90 ST-ELMHURST"  "96 ST"           "96 ST"          
    ## [235] "96 ST-2 AVE"     "9TH STREET"      "ALABAMA AV"     
    ## [238] "ALABAMA AVE"     "ALLERTON AVE"    "AQUEDUCT N.COND"
    ## [241] "AQUEDUCT RACETR" "AQUEDUCT TRACK"  "AQUEDUCT-N CNDT"
    ## [244] "ASTOR PLACE"     "ATL AV-BARCLAY"  "ATLANTIC AV"    
    ## [247] "ATLANTIC AVE"    "AVE H"           "AVE I"          
    ## [250] "AVE J"           "AVE M"           "AVE N"          
    ## [253] "AVE P"           "AVE U"           "AVE X"          
    ## [256] "AVENUE H"        "AVENUE I"        "AVENUE J"       
    ## [259] "AVENUE M"        "AVENUE N"        "AVENUE P"       
    ## [262] "AVENUE U"        "AVENUE X"        "B'WAY-LAFAYETTE"
    ## [265] "BAY 50 ST"       "BAY 50 ST"       "BAY PARKWAY"    
    ## [268] "BAY PKWY"        "BAY PKY-22 AVE"  "BAY RIDGE AV"   
    ## [271] "BAY RIDGE AVE"   "BAY RIDGE-95 ST" "BAY RIDGE-95 ST"
    ## [274] "BAYCHESTER AVE"  "BDNQR2345"       "BEACH 105 ST"   
    ## [277] "BEACH 105 ST"    "BEACH 25 ST"     "BEACH 25 ST"    
    ## [280] "BEACH 36 ST"     "BEACH 36 ST"     "BEACH 44 ST"    
    ## [283] "BEACH 44 ST"     "BEACH 60 ST"     "BEACH 60 ST"    
    ## [286] "BEACH 67 ST"     "BEACH 67 ST"     "BEACH 90 ST"    
    ## [289] "BEACH 90 ST"     "BEACH 98 ST"     "BEACH 98 ST"    
    ## [292] "BEDFORD AV"      "BEDFORD AVE"     "BEDFORD PARK BL"
    ## [295] "BEDFORD PK BLVD" "BEDFORD-NOSTRAN" "BEDFORD/NOSTRAN"
    ## [298] "BEEBE-39 AVE"    "BERGEN ST"       "BERGEN ST"      
    ## [301] "BEVERLEY ROAD"   "BEVERLEY ROAD"   "BEVERLY ROAD"   
    ## [304] "BLEECKER ST"     "BLEECKER ST"     "BOROUGH HALL"   
    ## [307] "BOROUGH HALL/CT" "BOTANIC GARDEN"  "BOTANIC GARDEN" 
    ## [310] "BOWERY"          "BOWERY"          "BOWLING GREEN"  
    ## [313] "BOWLING GREEN"   "BOYD-88 ST"      "BRIARWOOD"      
    ## [316] "BRIGHTON BEACH"  "BRIGHTON BEACH"  "BROAD CHANNEL"  
    ## [319] "BROAD CHANNEL"   "BROAD ST"        "BROAD ST"       
    ## [322] "BROADWAY"        "BROADWAY"        "BROADWAY JCT"   
    ## [325] "BROADWAY-31 ST"  "BROADWAY-ENY"    "BROADWAY/LAFAY" 
    ## [328] "BRONX PARK EAST" "BROOK AVE"       "BROOKLYN BRIDGE"
    ## [331] "BROOKLYN BRIDGE" "BUHRE AVE"       "BURKE AVE"      
    ## [334] "BURNSIDE AVE"    "BUSHWICK AV"     "BUSHWICK AVE"   
    ## [337] "CANAL ST"        "CANAL ST"        "CANARSIE-ROCKAW"
    ## [340] "CARROLL ST"      "CARROLL ST"      "CASTLE HILL AVE"
    ## [343] "CATHEDRAL PKWY"  "CATHEDRL-110 ST" "CENTRAL AV"     
    ## [346] "CENTRAL AVE"     "CHAMBERS ST"     "CHAMBERS ST"    
    ## [349] "CHAUNCEY ST"     "CHAUNCEY ST"     "CHRISTOPHER ST" 
    ## [352] "CHRISTOPHER ST"  "CHURCH AV"       "CHURCH AVE"     
    ## [355] "CITY / BUS"      "CITY HALL"       "CLARK ST"       
    ## [358] "CLASSON AV"      "CLASSON AVE"     "CLEVELAND ST"   
    ## [361] "CLEVELAND ST"    "CLINTON-WASH AV" "CLINTON-WASH AV"
    ## [364] "CONEY IS-STILLW" "CORTELYOU RD"    "CORTELYOU ROAD" 
    ## [367] "CORTLANDT ST"    "CORTLANDT ST"    "COURT SQ"       
    ## [370] "COURT SQ"        "COURT SQ-23 ST"  "COURT SQ-23 ST" 
    ## [373] "CRESCENT ST"     "CRESCENT ST"     "CROWN HTS-UTICA"
    ## [376] "CYPRESS AVE"     "CYPRESS HILLS"   "CYPRESS HILLS"  
    ## [379] "DEKALB AV"       "DEKALB AVE"      "DELANCEY ST"    
    ## [382] "DELANCEY/ESSEX"  "DITMARS BL-31 S" "DITMAS AV"      
    ## [385] "DITMAS AVE"      "DYCKMAN ST"      "DYCKMAN ST"     
    ## [388] "DYCKMAN-200 ST"  "DYRE AVE"        "E 143 ST"       
    ## [391] "E 149 ST"        "E 177 ST-PARKCH" "E 180 ST"       
    ## [394] "E TREMONT AVE"   "EAST 105 ST"     "EAST 105 ST"    
    ## [397] "EAST BROADWAY"   "EAST BROADWAY"   "EASTERN PKWY"   
    ## [400] "ELDER AVE"       "ELDERTS LANE"    "ELMHURST AV"    
    ## [403] "ELMHURST AVE"    "ELTINGVILLE PK"  "ESSEX ST"       
    ## [406] "EUCLID AV"       "EUCLID AVE"      "EXCHANGE PLACE" 
    ## [409] "FAR ROCKAWAY"    "FAR ROCKAWAY"    "FLATBUSH AVE"   
    ## [412] "FLUSHING AV"     "FLUSHING AVE"    "FORDHAM RD"     
    ## [415] "FORDHAM ROAD"    "FOREST AVE"      "FOREST AVE"     
    ## [418] "FOREST HILLS 71" "FOREST HILLS-71" "FOREST PARKWAY" 
    ## [421] "FRANKLIN AV"     "FRANKLIN AVE"    "FRANKLIN ST"    
    ## [424] "FREEMAN ST"      "FRESH POND RD"   "FRESH POND ROAD"
    ## [427] "FT HAMILTON PKY" "FT HAMILTON PKY" "FULTON ST"      
    ## [430] "FULTON ST"       "GATES AV"        "GATES AVE"      
    ## [433] "GRAHAM AV"       "GRAHAM AVE"      "GRAND ARMY PLAZ"
    ## [436] "GRAND AV-NEWTON" "GRAND ST"        "GRAND ST"       
    ## [439] "GRAND-30 AVE"    "GRAND-NEWTOWN"   "GRANT AV"       
    ## [442] "GRANT AVE"       "GRD CNTRL-42 ST" "GREENPOINT AV"  
    ## [445] "GREENPOINT AVE"  "GREENWOOD-111"   "GROVE STREET"   
    ## [448] "GUN HILL ROAD"   "HALSEY ST"       "HALSEY ST"      
    ## [451] "HARRISON"        "HEWES ST"        "HEWES ST"       
    ## [454] "HIGH ST"         "HIGH ST"         "HOUSTON ST"     
    ## [457] "HOWARD BCH JFK"  "HOYT ST"         "HOYT ST-ASTORIA"
    ## [460] "HOYT-SCHER"      "HOYT/SCHERMER"   "HUDSON-80 ST"   
    ## [463] "HUNTERS PT AVE"  "HUNTS POINT AVE" "INTERVALE-163"  
    ## [466] "INWOOD-207 ST"   "INWOOD-207 ST"   "JACKSON AVE"    
    ## [469] "JAMAICA 179 ST"  "JAMAICA CENTER"  "JAMAICA CENTER" 
    ## [472] "JAMAICA VAN WK"  "JAMAICA-179 ST"  "JAMAICA-VAN WYC"
    ## [475] "JAY ST-METROTEC" "JAY ST-METROTEC" "JEFFERSON ST"   
    ## [478] "JEFFERSON ST"    "JFK JAMAICA CT1" "JKSN HT-ROOSVLT"
    ## [481] "JOURNAL SQUARE"  "JUNCTION BLVD"   "JUNIUS ST"      
    ## [484] "KEW GARDENS"     "KINGS HIGHWAY"   "KINGS HWY"      
    ## [487] "KINGSBRIDGE RD"  "KINGSBRIDGE RD"  "KINGSTON AVE"   
    ## [490] "KINGSTON-THROOP" "KINGSTON-THROOP" "KNICKERBOCKER"  
    ## [493] "KNICKERBOCKER"   "KOSCIUSZKO ST"   "KOSCIUSZKO ST"  
    ## [496] "LACKAWANNA"      "LAFAYETTE AV"    "LAFAYETTE AVE"  
    ## [499] "LEFFERTS BLVD"   "LEXINGTON AV/53" "LEXINGTON AV/63"
    ## [502] "LEXINGTON AVE"   "LEXINGTON-53 ST" "LGA AIRPORT CTB"
    ## [505] "LIBERTY AV"      "LIBERTY AVE"     "LIVONIA AV"     
    ## [508] "LIVONIA AVE"     "LONGWOOD AVE"    "LORIMER ST"     
    ## [511] "LORIMER ST"      "MAIN ST"         "MARBLE HILL-225"
    ## [514] "MARCY AV"        "MARCY AVE"       "METROPOLITAN AV"
    ## [517] "METROPOLITAN AV" "METS-WILLETS PT" "METS-WILLETS PT"
    ## [520] "MIDDLETOWN ROAD" "MONTROSE AV"     "MONTROSE AVE"   
    ## [523] "MORGAN AV"       "MORGAN AVE"      "MORRIS PARK"    
    ## [526] "MORRISON AVE"    "MOSHOLU PARKWAY" "MT EDEN AVE"    
    ## [529] "MURRAY ST-B'WAY" "MYRTLE AV"       "MYRTLE AVE"     
    ## [532] "MYRTLE-WILLOUGH" "MYRTLE-WILLOUGH" "MYRTLE-WYCKOFF" 
    ## [535] "NASSAU AV"       "NASSAU AV"       "NECK RD"        
    ## [538] "NECK ROAD"       "NEPTUNE AV"      "NEPTUNE AVE"    
    ## [541] "NEREID AVE"      "NEVINS ST"       "NEW LOTS"       
    ## [544] "NEW LOTS AVE"    "NEW UTRECHT AV"  "NEW UTRECHT AVE"
    ## [547] "NEWARK BM BW"    "NEWARK C"        "NEWARK HM HE"   
    ## [550] "NEWARK HW BMEBE" "NEWKIRK AVE"     "NEWKIRK PLAZA"  
    ## [553] "NORTHERN BLVD"   "NORTHERN BLVD"   "NORWOOD 205 ST" 
    ## [556] "NORWOOD AV"      "NORWOOD AVE"     "NORWOOD-205 ST" 
    ## [559] "NOSTRAND AV"     "NOSTRAND AVE"    "NQW"            
    ## [562] "OCEAN PARKWAY"   "OCEAN PKWY"      "ORCHARD BEACH"  
    ## [565] "OXFORD-104 ST"   "OZONE PK LEFFRT" "PACIFIC ST"     
    ## [568] "PARK PLACE"      "PARK PLACE"      "PARKSIDE AV"    
    ## [571] "PARKSIDE AVE"    "PARSONS BLVD"    "PARSONS BLVD"   
    ## [574] "PATH NEW WTC"    "PATH WTC"        "PATH WTC 2"     
    ## [577] "PATH WTC 2"      "PAVONIA/NEWPORT" "PELHAM BAY PARK"
    ## [580] "PELHAM PARKWAY"  "PENNSYLVANIA AV" "PRESIDENT ST"   
    ## [583] "PRINCE ST"       "PRINCE ST-B'WAY" "PROSPECT AV"    
    ## [586] "PROSPECT AVE"    "PROSPECT PARK"   "PROSPECT PARK"  
    ## [589] "QUEENS PLAZA"    "QUEENS PLAZA"    "QUEENSBORO PLZ" 
    ## [592] "RALPH AV"        "RALPH AVE"       "RECTOR ST"      
    ## [595] "RECTOR ST"       "RIT-MANHATTAN"   "RIT-MANHATTAN"  
    ## [598] "RIT-ROOSEVELT"   "RIT-ROOSEVELT"   "ROCKAWAY AV"    
    ## [601] "ROCKAWAY AVE"    "ROCKAWAY BLVD"   "ROCKAWAY BLVD"  
    ## [604] "ROCKAWAY PARK B" "ROCKAWAY PK 116" "ROCKAWAY PKY"   
    ## [607] "ROOSEVELT AVE"   "ROOSEVELT IS"    "ROOSEVELT ISLND"
    ## [610] "SARATOGA AVE"    "SENECA AVE"      "SENECA AVE"     
    ## [613] "SHEEPSHEAD BAY"  "SHEEPSHEAD BAY"  "SHEPHERD AV"    
    ## [616] "SHEPHERD AVE"    "SIMPSON ST"      "SMITH-9 ST"     
    ## [619] "SMITH-9 ST"      "SOUTH FERRY"     "SPRING ST"      
    ## [622] "SPRING ST"       "ST LAWRENCE AVE" "ST. GEORGE"     
    ## [625] "ST. GEORGE"      "STEINWAY ST"     "STEINWAY ST"    
    ## [628] "STERLING ST"     "STILLWELL AVE"   "SUTPHIN BLVD"   
    ## [631] "SUTPHIN BLVD"    "SUTPHIN-ARCHER"  "SUTTER AV"      
    ## [634] "SUTTER AVE"      "THIRTY ST"       "THIRTY THIRD ST"
    ## [637] "THIRTY THIRD ST" "TIMES SQ-42 ST"  "TOMPKINSVILLE"  
    ## [640] "TOMPKINSVILLE"   "TREMONT AV"      "TREMONT AVE"    
    ## [643] "TWENTY THIRD ST" "TWENTY THIRD ST" "UNION ST"       
    ## [646] "UNION ST"        "UNION TPK-KEW G" "UTICA AV"       
    ## [649] "UTICA AVE"       "VAN ALSTON-21ST" "VAN SICLEN AV"  
    ## [652] "VAN SICLEN AVE"  "VAN SICLEN AVE"  "VAN WYCK BLVD"  
    ## [655] "VERNON/JACKSON"  "W 4 ST-WASH SQ"  "W 4 ST-WASH SQ" 
    ## [658] "W 8 ST-AQUARIUM" "W 8 ST-AQUARIUM" "WAKEFIELD-241"  
    ## [661] "WALL ST"         "WALL ST"         "WASHINGTON-36 A"
    ## [664] "WESTCHESTER SQ"  "WHITEHALL S-FRY" "WHITEHALL ST"   
    ## [667] "WHITLOCK AVE"    "WILSON AV"       "WILSON AVE"     
    ## [670] "WINTHROP ST"     "WOODHAVEN BLVD"  "WOODHAVEN BLVD" 
    ## [673] "WOODLAWN ROAD"   "WORLD TRADE CTR" "WORLD TRADE CTR"
    ## [676] "WTC-CORTLANDT"   "YORK ST"         "YORK ST"        
    ## [679] "ZEREGA AVE"

Most of them look the same but a lot are different. Let’s look at
stations that include “42” for 42nd street and we can see the
discrepincies.

``` r
both_station_names[str_which(both_station_names, "42")]
```

    ## [1] "TIMES SQ-42 ST"  "42 ST-PORT AUTH" "42 ST-BRYANT PK" "GRD CNTRL-42 ST"
    ## [5] "42 ST-PA BUS TE" "42 ST-TIMES SQ"  "42 ST-BRYANT PK" "42 ST-GRD CNTRL"
    ## [9] "242 ST"

``` r
both_station_names[str_which(both_station_names, "42")] %>% sort
```

    ## [1] "242 ST"          "42 ST-BRYANT PK" "42 ST-BRYANT PK" "42 ST-GRD CNTRL"
    ## [5] "42 ST-PA BUS TE" "42 ST-PORT AUTH" "42 ST-TIMES SQ"  "GRD CNTRL-42 ST"
    ## [9] "TIMES SQ-42 ST"
