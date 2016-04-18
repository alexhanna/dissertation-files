library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(datasets)
library(ggmap)
library(maps)

speedRevGeo <- function(df) {
    ## reverse geocode df entries
    df$city <- NA
    df$state <- NA
    for (i in 0:nrow(df)){
        lon <- df[i,]$GP8
        lat <- df[i,]$GP7
        if (length(lon) == 0 & length(lat) == 0) {
            next
        }

        if (!is.na(lon) & !is.na(lat)) {
            tryCatch({
                g <- revgeocode( c(lon, lat), output = "more" )
            }, error = function(e) {
                print(e)
                next
            })

            if(is.atomic(g)) {
                next
            }

            locality <- g$locality
            sublocality <- g$sublocality_level_1
            state <- g$administrative_area_level_1
            city <- NULL

            if(!is.null(locality)) {
                city <- locality
            } else if (!is.null(sublocality)) {
                city <- sublocality
            } else {
                ## we can get by without a city
                city <- ''
            }

            ## but we cannot get by without a state
            if(is.null(state)) {
                next
            }

            df[i,]$city <- as.character(city)
            df[i,]$state <- as.character(state)
        }
    }
    df
}

my_theme <- theme(
    axis.text.x     = element_text(size = 12),
    axis.text.y     = element_text(size = 12, hjust = 0),
    axis.title.x    = element_text(size = 14, color = "#666666"),
    axis.title.y    = element_text(size = 14, color = "#666666"),
    strip.text.x    = element_text(size = 14)
)

doca  <- read.csv("../data/doca-full-geocoded_2016-04-08.csv", stringsAsFactors = FALSE)
speed <- read.csv("../data/speed-revgeocoded_2016-04-14.csv", stringsAsFactors = FALSE)

##### DoCA pre-processing
## remove ethnic conflict
doca <- doca %>% filter(targ1 != 7)
#doca <- doca %>% filter(targ2 != 7)

## remove lawsuits
# act1,97
# form1,18
doca <- doca %>% filter(act1 != 97, form1 != 18)
#doca <- doca %>% filter(act2 != 97, form2 != 18)

###### SPEED pre-processing
## only "political expression"
# speed <- speed %>% filter(EV_TYPE == 1)

## filter SPEED events to US
# speed <- speed %>% filter(country == 'United States')

## loading the violent stuff
#speed.violent <- read.csv("../input/SPEED/ssp_public.csv", stringsAsFactors = FALSE)

#US, riots
#speed.violent <- speed.violent %>% filter(country == 'United States', EV_TYPE == 2, ATK_TYPE == 1)

## afam
# speed.violent.afam <- speed.violent %>% filter(INI_SGRP1 %in% c('African American', 'African', 'Black', 'African Descent') | str_detect(INI_PGRP1, allcrm.regex))

## make dates
## use event days if available
doca$year  <- ifelse(!is.na(doca$evyy) & doca$evyy != '', as.integer(doca$evyy), as.integer(doca$rptyy))
doca$month <- ifelse(!is.na(doca$evmm) & doca$evmm != '', as.integer(doca$evmm), as.integer(doca$rptmm))
doca$day   <- ifelse(!is.na(doca$evdd) & doca$evdd != '', as.integer(doca$evdd), as.integer(doca$rptdd))

## make dates
doca$date  <- ymd(sprintf("%d-%02d-%02d", doca$year, doca$month, doca$day))
doca$mdate <- ymd(sprintf("%d-%02d-%02d", doca$year, doca$month, 1))
doca <- doca %>% filter(!is.na(date))
doca <- doca %>% filter(!is.na(mdate))

speed$mdate <- ymd(sprintf("%d-%02d-%02d", speed$year, speed$month, 1))
speed$date <- ymd(sprintf("%d-%02d-%02d", speed$year, speed$month, speed$day))
speed <- speed %>% filter(!is.na(date))
speed <- speed %>% filter(!is.na(mdate))

## only AfAm activities and those orgs which have involvement of the movement
allcrm.regex <- regex("(congress\\s+.+\\s+racial\\s+equality|core|national\\s+association\\s+.+\\s+advancement\\s+of\\s+colored\\s+.+|naacp|southern\\s+christian\\s+leadership.+|sclc|slcc|student\\s+non(\\-)*violent\\s+coordinating\\s+.+|sncc|black\\s+panther.+)", ignore_case = TRUE)
doca.afam <- doca %>% filter(igrp1c1 == 401 | igrp1c2 == 401 | igrp2c1 == 401 | igrp2c2 == 401 | igrp3c1 == 401 | igrp3c2 == 401 | str_detect(smoname1, allcrm.regex))
#speed.afam <- speed %>% filter(NGOV_I1 %in% c(1, 2, 24, 25))
speed.afam <- speed %>% filter(INI_SGRP1 %in% c('African American', 'African', 'Black', 'African Descent') | str_detect(INI_PGRP1, allcrm.regex))

## group by month and year
doca.m <- doca %>% group_by(mdate) %>% summarise(count = n())
doca.y <- doca %>% group_by(year) %>% summarise(count = n())
doca.afam.m <- doca.afam %>% group_by(mdate) %>% summarise(count = n())
doca.afam.y <- doca.afam %>% group_by(year) %>% summarise(count = n())

speed.m <- speed %>% group_by(mdate) %>% summarise(count = n())
speed.y <- speed %>% group_by(year) %>% summarise(count = n())
speed.afam.m <- speed.afam %>% group_by(mdate) %>% summarise(count = n())
speed.afam.y <- speed.afam %>% group_by(year) %>% summarise(count = n())

## month correlations
doca.speed.m <- merge(doca.m, speed.m, by = "mdate", suffixes = c(".doca", ".speed"))
doca.speed.m <- doca.speed.m[complete.cases(doca.speed.m),]
doca.speed.afam.m <- merge(doca.afam.m, speed.afam.m, by = "mdate", suffixes = c(".doca", ".speed"))
doca.speed.afam.m <- doca.speed.afam.m[complete.cases(doca.speed.afam.m),]

## year correlations
doca.speed.y <- merge(doca.y, speed.y, by = "year", suffixes = c(".doca", ".speed"))
doca.speed.y <- doca.speed.y[complete.cases(doca.speed.y),]
doca.speed.afam.y <- merge(doca.afam.y, speed.afam.y, by = "year", suffixes = c(".doca", ".speed"))
doca.speed.afam.y <- doca.speed.afam.y[complete.cases(doca.speed.afam.y),]

## sixties filtering
doca.speed.m.sixties <- doca.speed.m %>% filter(mdate >= ymd('1960-01-01'), mdate <= ymd('1970-01-01'))
doca.speed.y.sixties <- doca.speed.y %>% filter(year >= 1960, year <= 1970)
doca.speed.afam.m.sixties <- doca.speed.afam.m %>% filter(mdate >= ymd('1960-01-01'), mdate <= ymd('1970-01-01'))
doca.speed.afam.y.sixties <- doca.speed.afam.y %>% filter(year >= 1960, year <= 1970)

## CORRELATIONS
c.m        <- cor.test(doca.speed.m$count.doca, doca.speed.m$count.speed)
c.afam.m   <- cor.test(doca.speed.afam.m$count.doca, doca.speed.afam.m$count.speed)
c.y        <- cor.test(doca.speed.y$count.doca, doca.speed.y$count.speed)
c.afam.y   <- cor.test(doca.speed.afam.y$count.doca, doca.speed.afam.y$count.speed)
c.six.m    <- cor.test(doca.speed.m.sixties$count.doca, doca.speed.m.sixties$count.speed)
c.six.af.m <- cor.test(doca.speed.afam.m.sixties$count.doca, doca.speed.afam.m.sixties$count.speed)
c.six.y    <- cor.test(doca.speed.y.sixties$count.doca, doca.speed.y.sixties$count.speed)
c.six.af.y <- cor.test(doca.speed.afam.y.sixties$count.doca, doca.speed.afam.y.sixties$count.speed)

makeStars <- function(x) {
    pv <- x$p.value
    if (pv < 0.001) {
        '***'
    } else if (pv < 0.01) {
        '**'
    } else if (pv < 0.05) {
        '*'
    } else {
        ''
    }
}

cors <- list(c.m, c.afam.m, c.y, c.afam.y, c.six.m, c.six.af.m, c.six.y, c.six.af.y)
vals <- unlist(lapply(cors, function(x) sprintf("%0.2f", round(x$estimate, 2))))
pval <- unlist(lapply(cors, makeStars))

cor.tex <- paste0("
\\begin{tabular}{lrrrr}
\\toprule
{} & \\multicolumn{2}{c}{1960 - 1995} & \\multicolumn{2}{c}{1960 - 1970} \\\\
{} &   Monthly &   Yearly             & Monthly & Yearly \\\\
\\midrule
All events & ",                 vals[[1]], pval[[1]], " & ", vals[[3]], pval[[3]], " & ", vals[[5]], pval[[5]], " & ", vals[[7]], pval[[7]], "\\\\\n",
"Movement-initiatied events  & ", vals[[2]], pval[[2]], " & ", vals[[4]], pval[[4]], " & ", vals[[6]], pval[[6]], " & ", vals[[8]], pval[[8]], "\\\\\n",
"\\bottomrule
\\end{tabular}")
write(cor.tex, file = "../output/doca-speed-correlations_2016-04-14.tex")

### PLOTTING
doca.m$dataset <- "DoCA" 
speed.m$dataset <- "SPEED"
doca.afam.m$dataset <- "DoCA" 
speed.afam.m$dataset <- "SPEED"

doca.y$dataset <- "DoCA"
speed.y$dataset <- "SPEED"
doca.afam.y$dataset <- "DoCA"
speed.afam.y$dataset <- "SPEED"

## unit
plot.m <- rbind(doca.m, speed.m)
plot.y <- rbind(doca.y, speed.y)
plot.afam.m <- rbind(doca.afam.m, speed.afam.m)
plot.afam.y <- rbind(doca.afam.y, speed.afam.y)

plot.y$mdate <- ymd(sprintf("%d-%02d-%02d", plot.y$year, 1, 1))
plot.y$year <- NULL
plot.afam.y$mdate <- ymd(sprintf("%d-%02d-%02d", plot.afam.y$year, 1, 1))
plot.afam.y$year <- NULL

plot.m$unit <- 'month'
plot.y$unit <- 'year'
plot.afam.m$unit <- 'month'
plot.afam.y$unit <- 'year'

plot.all <- rbind(plot.m, plot.y)
plot.afam.all <- rbind(plot.afam.m, plot.afam.y)

plot.all <- plot.all %>% filter(mdate >= ymd('1960-01-01'), mdate <= max(doca.m$mdate))
plot.afam.all <- plot.afam.all %>% filter(mdate >= ymd('1960-01-01'), mdate <= max(doca.afam.m$mdate))

#plot.joined <- merge(plot.afam.all, plot.all, by = c('mdate', 'dataset', 'unit'), suffixes = c(".afam", ".all"))
#plot.joined$proportion <- plot.joined$count.afam / plot.joined$count.all
plot.all$base <- 'All events'
plot.afam.all$base <- 'Movement-initiated events'
plot.joined <- rbind(plot.all, plot.afam.all)

## plot all
p <- ggplot(plot.joined %>% filter(unit == 'year'), aes(mdate, count, color = dataset))
p <- p + geom_line()
p <- p + scale_colour_brewer(palette = "Set1")
p <- p + facet_wrap(~ base, ncol = 1, scales = "free_y") 
p <- p + theme_bw() + theme(
    legend.text     = element_text(size = 14),
    legend.title    = element_blank(),
    axis.text.x     = element_text(size = 14),
    axis.text.y     = element_text(size = 14, hjust = 0),
    axis.title.x    = element_text(size = 14, color = "#666666"),
    axis.title.y    = element_text(size = 14, color = "#666666"), 
    strip.text.x    = element_text(size = 14))
p <- p + xlab("Year") + ylab("Event Count") 
ggsave(p, file = "../output/speed-doca-yearly-afam-all_2016-04-13.pdf", width = 12, height = 6)

## plot the 60s
p <- ggplot(plot.joined %>% filter(mdate >= ymd('1960-01-01'), mdate <= ymd('1970-01-01'), unit == 'month'), 
    aes(mdate, count, color = dataset))
p <- p + geom_line()
p <- p + facet_wrap(~ base, ncol = 1, scales = "free_y") 
p <- p + scale_colour_brewer(palette = "Set1")
p <- p + theme_bw() + theme(
    legend.text     = element_text(size = 14),
    legend.title    = element_blank(),
    axis.text.x     = element_text(size = 14),
    axis.text.y     = element_text(size = 14, hjust = 0),
    axis.title.x    = element_text(size = 14, color = "#666666"),
    axis.title.y    = element_text(size = 14, color = "#666666"),
    strip.text.x    = element_text(size = 14))
p <- p + xlab("Year") + ylab("Event Count") 
ggsave(p, file = "../output/speed-doca-monthly-sixties-afam-all_2016-04-13.pdf", width = 12, height = 6)

## Get 1961 - 70 period
doca.afam.six <- doca.afam %>% filter(year >= 1961, year <= 1970)
speed.afam.six <- speed.afam %>% filter(year >= 1961, year <= 1970)

doca.afam.six$dataset <- 'DoCA'
speed.afam.six$dataset <- 'SPEED'

## note before/after 1965
doca.afam.six$after65 <- 1
doca.afam.six[doca.afam.six$year <= 1965,]$after65 <- 0

speed.afam.six$after65 <- 1
speed.afam.six[speed.afam.six$year <= 1965,]$after65 <- 0

## ORGS
core.regex <- regex("(congress\\s+.+\\s+racial\\s+equality|core)", ignore_case = TRUE)
naacp.regex <- regex("(national\\s+association\\s+.+\\s+advancement\\s+of\\s+colored\\s+.+|naacp)", ignore_case = TRUE)
sclc.regex <- regex("(southern\\s+christian\\s+leadership.+|sclc|slcc)", ignore_case = TRUE)
sncc.regex <- regex("(student\\s+non(\\-)*violent\\s+coordinating\\s+.+|sncc)", ignore_case = TRUE)

doca.afam.six$smoclean <- NA
doca.afam.six[doca.afam.six$smoname1 %in% c('', '.'),]$smoclean <- NA
doca.afam.six[str_detect(doca.afam.six$smoname1, regex('[a-z]+', ignore_case = TRUE)),]$smoclean <- 'Other'
doca.afam.six[str_detect(doca.afam.six$smoname1, core.regex),]$smoclean <- 'CORE'
doca.afam.six[str_detect(doca.afam.six$smoname1, naacp.regex),]$smoclean <- 'NAACP'
doca.afam.six[str_detect(doca.afam.six$smoname1, sclc.regex),]$smoclean <- 'SCLC'
doca.afam.six[str_detect(doca.afam.six$smoname1, sncc.regex),]$smoclean <- 'SNCC'    

speed.afam.six$smoclean <- NA
speed.afam.six[speed.afam.six$INI_PGRP1 %in% c('', '.'),]$smoclean <- NA
speed.afam.six[str_detect(speed.afam.six$INI_PGRP1, regex('[a-z]+', ignore_case = TRUE)),]$smoclean <- 'Other'
speed.afam.six[str_detect(speed.afam.six$INI_PGRP1, core.regex),]$smoclean <- 'CORE'
speed.afam.six[str_detect(speed.afam.six$INI_PGRP1, naacp.regex),]$smoclean <- 'NAACP'
speed.afam.six[str_detect(speed.afam.six$INI_PGRP1, sclc.regex),]$smoclean <- 'SCLC'
speed.afam.six[str_detect(speed.afam.six$INI_PGRP1, sncc.regex),]$smoclean <- 'SNCC'  

doca.org <- data.frame(table(doca.afam.six$smoclean, doca.afam.six$after65)) %>% spread(Var2, Freq)
speed.org <- data.frame(table(speed.afam.six$smoclean, speed.afam.six$after65)) %>% spread(Var2, Freq)

names(doca.org) <- c('val', 'pre', 'post')
names(speed.org) <- c('val', 'pre', 'post')

## percentage of all events which have orgs
table(is.na(doca.afam.six$smoclean))[1] / nrow(doca.afam.six)
table(doca.afam.six$smoclean != 'Other')[1] / nrow(doca.afam.six)
table(is.na(speed.afam.six$smoclean))[1] / nrow(speed.afam.six)
table(speed.afam.six$smoclean != 'Other')[1] / nrow(speed.afam.six)

## generate percentages, order orgs
doca.org$pre_p <- (doca.org$pre / sum(doca.org$pre))*100
doca.org$post_p <- (doca.org$post / sum(doca.org$post))*100
doca.org.out <- doca.org[c(2, 1, 4, 5, 3), c(1, 2, 4, 3, 5)]

speed.org$pre_p <- (speed.org$pre / sum(speed.org$pre))*100
speed.org$post_p <- (speed.org$post / sum(speed.org$post))*100
speed.org.out <- speed.org[c(2, 1, 4, 5, 3), c(1, 2, 4, 3, 5)]

## make one table and arrange it
df.org <- merge(doca.org.out, speed.org.out, by = 'val')
df.org <- df.org[c(2, 1, 4, 5, 3),]

## output to LaTeX
addtorow <- list()
addtorow$pos <- list(0, 0, 0, 0, 0)
addtorow$command <- c("\\toprule \n",
        "{} & \\multicolumn{4}{c}{DoCA} & \\multicolumn{4}{c}{SPEED} \\\\\n",
        "{} & \\multicolumn{2}{c}{1961-65} & \\multicolumn{2}{c}{1966-70} & \\multicolumn{2}{c}{1961-65} & \\multicolumn{2}{c}{1966-70} \\\\\n",
        "\\midrule \n", 
        "Organization & N & \\% & N & \\% & N & \\% & N & \\% \\\\\n")
# print(xtable(doca.org.out), file = "../output/doca-org_2016-04-13.tex", 
#     include.rownames = FALSE, include.colnames = FALSE,
#     booktabs = TRUE, add.to.row = addtorow, only.contents = TRUE)
# print(xtable(speed.org.out), file = "../output/speed-org_2016-04-13.tex", 
#     include.rownames = FALSE, include.colnames = FALSE,
#     booktabs = TRUE, add.to.row = addtorow, only.contents = TRUE)

print(xtable(df.org), file = "../output/doca-speed-org_2016-04-13.tex", 
    include.rownames = FALSE, include.colnames = FALSE,
    booktabs = TRUE, add.to.row = addtorow, only.contents = TRUE)

## LOCATION
## create a dictionary
state_dict <- state.name
names(state_dict) <- state.abb
state_dict[['DC']] <- 'District of Columbia'

## DoCA location
doca.afam.six$state1_name <- sapply(doca.afam.six$state1, function(x) tolower(state_dict[x]), USE.NAMES = FALSE)

## set up states -- long/lat information for all states
states <- map_data("state") 

## get state event counts
doca.loc <- data.frame(table(doca.afam.six$state1_name, doca.afam.six$after65)) %>% spread(Var2, Freq)
names(doca.loc) <- c('region', 'pre', 'post')

## append empty states
doca.loc <- rbind(doca.loc, data.frame(region = setdiff(states$region, doca.afam.six$state1_name), pre = 0, post = 0))

doca.loc$pre_p <- doca.loc$pre / sum(doca.loc$pre)
doca.loc$post_p <- doca.loc$post / sum(doca.loc$post)
doca.loc$dataset <- 'DoCA'
doca.loc$variable <- 'Location'
doca.loc$diff <- doca.loc$post_p - doca.loc$pre_p

## SPEED location
speed.afam.six$state <- tolower(speed.afam.six$state)

## get rid of Potwin garbage for this
speed.tmp <- speed.afam.six[speed.afam.six$city != 'Potwin',]

## get state event counts
speed.loc <- data.frame(table(speed.tmp$state, speed.tmp$after65)) %>% spread(Var2, Freq)
names(speed.loc) <- c('region', 'pre', 'post')

## append empty states
speed.loc <- rbind(speed.loc, data.frame(region = setdiff(states$region, speed.afam.six$state1_name), pre = 0, post = 0))

speed.loc$pre_p <- speed.loc$pre / sum(speed.loc$pre)
speed.loc$post_p <- speed.loc$post / sum(speed.loc$post)
speed.loc$dataset <- 'SPEED'
speed.loc$variable <- 'Location'
speed.loc$diff <- speed.loc$post_p - speed.loc$pre_p

## plotting maps
plot.doca.geo <- merge(states, doca.loc, by = "region")
p <- ggplot(plot.doca.geo, aes(x=long, y=lat, group=group, fill=diff*100)) 
p <- p + geom_polygon(color="black", size = 0.2)
p <- p + coord_map("mercator") + guides(fill=guide_legend(title="% Change"))
p <- p + scale_fill_gradient2()
p <- p + theme_bw() + xlab("") + ylab("") + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks  = element_blank(), 
    axis.ticks  = element_blank(),
    axis.line   = element_blank(),
    panel.background = element_blank(),
    panel.border     = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background  = element_blank(),
    legend.justification=c(1,0),
    legend.position=c(0.9,0),
    legend.text     = element_text(size = 12)
)
ggsave(p, file = "../output/doca-map-pre-post-diff_2016-04-12.pdf", width = 12, height = 6)

plot.speed.geo <- merge(states, speed.loc, by = "region")
p <- ggplot(plot.speed.geo, aes(x=long, y=lat, group=group, fill=diff*100)) 
p <- p + geom_polygon(color="black", size = 0.2)
p <- p + coord_map("mercator") + guides(fill=guide_legend(title="% Change")) 
p <- p + scale_fill_gradient2()
p <- p + theme_bw() + xlab("") + ylab("") + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks  = element_blank(), 
    axis.ticks  = element_blank(),
    axis.line   = element_blank(),
    panel.background = element_blank(),
    panel.border     = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background  = element_blank(),
    legend.justification=c(1,0),
    legend.position=c(0.9,0),
    legend.text     = element_text(size = 12)
)
ggsave(p, file = "../output/speed-map-pre-post-diff_2016-04-12.pdf", width = 12, height = 6)

## Location tables
state.data <- data.frame(state = tolower(state.name), region = state.region)
state.data <- rbind(
    state.data,
    cbind(state = "district of columbia", region = "Border States")
    )

## add factors
levels(state.data$region) <- c(levels(state.data$region), 'Deep South', 'Middle South', 'Border States', 'Other South')

## McAdam's categories
state.data[state.data$state %in% c('alabama', 'georgia', 'louisiana', 'mississippi', 'south carolina'),]$region <- 'Deep South'
state.data[state.data$state %in% c('arkansas', 'florida', 'north carolina', 'tennessee', 'texas', 'virginia'),]$region <- 'Middle South'
state.data[state.data$state %in% c('kentucky', 'maryland', 'missouri', 'oklahoma', 'west virginia', 'district of columbia'),]$region <- 'Border States'
state.data[state.data$state %in% c('delaware'),]$region <- 'Other South'

## merge
doca.loc$state <- doca.loc$region
doca.loc$region <- NULL

## summarise by region
doca.loc.out <- merge(doca.loc, state.data, by = "state") %>% group_by(region) %>% 
    summarise(pre_sum = as.integer(sum(pre)), post_sum = as.integer(sum(post)), 
        prep_sum = sum(pre_p)*100, postp_sum = sum(post_p)*100)

## reorder and rename columns
doca.loc.out <- doca.loc.out[, c(1, 2, 4, 3, 5)]

speed.loc$state <- speed.loc$region
speed.loc$region <- NULL
speed.loc.out <- merge(speed.loc, state.data, by = "state") %>% group_by(region) %>% 
    summarise(pre_sum = as.integer(sum(pre)), post_sum = as.integer(sum(post)), 
        prep_sum = sum(pre_p)*100, postp_sum = sum(post_p)*100)
speed.loc.out <- speed.loc.out[, c(1, 2, 4, 3, 5)]

## output to LaTeX
# addtorow <- list()
# addtorow$pos <- list(0, 0, 0)
# addtorow$command <- c("\\toprule \n",
#         "{} & \\multicolumn{2}{c}{1961-65} & \\multicolumn{2}{c}{1966-70} \\\\\n", 
#         "Region & N & \\% & N & \\% \\\\\n")
# print(xtable(doca.loc.out), file = "../output/doca-loc_2016-04-13.tex", 
#     include.rownames = FALSE, include.colnames = FALSE,
#     booktabs = TRUE, add.to.row = addtorow, only.contents = TRUE)
# print(xtable(speed.loc.out), file = "../output/speed-loc_2016-04-13.tex", 
#     include.rownames = FALSE, include.colnames = FALSE,
#     booktabs = TRUE, add.to.row = addtorow, only.contents = TRUE)

## one big table
df.loc <- merge(doca.loc.out, speed.loc.out, by = 'region')

addtorow <- list()
addtorow$pos <- list(0, 0, 0, 0, 0)
addtorow$command <- c("\\toprule \n",
        "{} & \\multicolumn{4}{c}{DoCA} & \\multicolumn{4}{c}{SPEED} \\\\\n",
        "{} & \\multicolumn{2}{c}{1961-65} & \\multicolumn{2}{c}{1966-70} & \\multicolumn{2}{c}{1961-65} & \\multicolumn{2}{c}{1966-70} \\\\\n",
        "\\midrule \n", 
        "Region & N & \\% & N & \\% & N & \\% & N & \\% \\\\\n")

print(xtable(df.loc), file = "../output/doca-speed-loc_2016-04-13.tex", 
    include.rownames = FALSE, include.colnames = FALSE,
    booktabs = TRUE, add.to.row = addtorow, only.contents = TRUE)

## CLAIMS
doca.claim <- data.frame(table(doca.afam.six$claim1, doca.afam.six$after65)) %>% spread(Var2, Freq)
names(doca.claim) <- c('val', 'pre', 'post')

## generate percentages, order claims
doca.claim$pre_p <- (doca.claim$pre / sum(doca.claim$pre))*100
doca.claim$post_p <- (doca.claim$post / sum(doca.claim$post))*100

## filter on African-American claims and misc social issues
doca.claim.out <- doca.claim %>% filter(str_detect(val, '^15') | str_detect(val, '^13'), pre_p >= 0.1) %>%
    arrange(desc(pre_p))

## create a code dictionary
m <- matrix(c(
'1318', 'Tenants Rights Movement',
'1331', 'Housing Issues Other',
'1333', 'Poverty/hunger, general',
'1334', 'State surveillance/Prosecution of protesters',
'1336', 'Political figure',
'1338', 'Government policy',
'1342', 'Education',
'1500', 'African American Civil Rights, general',
'1501', 'Any Desegregation Claim',
'1502', 'Pro-Voting Rights/Political Power',
'1505', 'Anti-Discrimination in Housing or Employment',
'1506', 'Affirmative Action for African Americans',
'1509', 'Black Pride, entrepreneurship, separatist',
'1517', 'Reduction of Black Poverty',
'1518', 'Anti-Police Brutality/Harassment',
'1519', 'More positive media depictions'), nrow = 2)
claims_dict <- data.frame(t(m), stringsAsFactors = FALSE)
names(claims_dict) <- c('code', 'description')

doca.claim.out$description <- NA
doca.claim.out$description <- sapply(doca.claim.out$val, function(x) claims_dict[claims_dict$code == x,]$description)
doca.claim.out$val <- NULL
doca.claim.out <- doca.claim.out[, c(5, 1, 3, 2, 4)]

## get rid of anything which does not had > 1% in either period
doca.claim.out <- doca.claim.out %>% filter(pre_p > 1 | post_p > 1)

## LaTeX
addtorow <- list()
addtorow$pos <- list(0, 0, 0)
addtorow$command <- c("\\toprule \n",
        "{} & \\multicolumn{2}{c}{1961-65} & \\multicolumn{2}{c}{1966-70} \\\\\n", 
        "Claim & N & \\% & N & \\% \\\\\n")
print(xtable(doca.claim.out), file = "../output/doca-claim_2016-04-13.tex", 
    include.rownames = FALSE, include.colnames = FALSE,
    booktabs = TRUE, add.to.row = addtorow, only.contents = TRUE)

## SPEED
## gather relevant claims-types variables
speed.claim <- speed.afam.six %>% 
    select(eventid, SC_ANIMOSITY, ANTI_GOV_SENTMNTS, CLASS_CONFLICT, 
        POL_DESIRES, RETAIN_POWER, ECO_SCARCITY, PERS_SECURITY, after65) %>%
    gather(eventid, after65)
names(speed.claim) <- c('eventid', 'after65', 'variable', 'value')
speed.claim.out <- speed.claim %>% group_by(after65, variable) %>% summarise(count = sum(value)) %>% spread(after65, count)
names(speed.claim.out) <- c('val', 'pre', 'post')

m <- matrix(c(
'SC_ANIMOSITY', 'Socio-cultural animosities',
'ANTI_GOV_SENTMNTS', 'Anti-government sentiments',
'CLASS_CONFLICT', 'Class-based conflict',
'POL_DESIRES', 'Desire for political rights',
'RETAIN_POWER', 'Desire to retain political power',
'ECO_SCARCITY', 'Ecological resource scarcities',
'PERS_SECURITY', 'Desire for personal security'
), nrow = 2)
speed.claims_dict <- data.frame(t(m), stringsAsFactors = FALSE)
names(speed.claims_dict) <- c('code', 'description')

speed.claim.out$description <- NA
speed.claim.out$description <- sapply(speed.claim.out$val, function(x) speed.claims_dict[speed.claims_dict$code == x,]$description)
speed.claim.out$val <- NULL

## denominator here is out of all events
speed.claim.out$pre_p <- (speed.claim.out$pre / nrow(speed.afam.six %>% filter(after65 == 0)))*100
speed.claim.out$post_p <- (speed.claim.out$post / nrow(speed.afam.six %>% filter(after65 == 1)))*100
speed.claim.out <- speed.claim.out[, c(3, 1, 4, 2, 5)] %>% arrange(desc(pre_p))

## LaTeX
addtorow <- list()
addtorow$pos <- list(0, 0, 0)
addtorow$command <- c("\\toprule \n",
        "{} & \\multicolumn{2}{c}{1961-65} & \\multicolumn{2}{c}{1966-70} \\\\\n", 
        "Claim & N & \\% & N & \\% \\\\\n")
print(xtable(speed.claim.out), file = "../output/speed-claim_2016-04-13.tex", 
    include.rownames = FALSE, include.colnames = FALSE,
    booktabs = TRUE, add.to.row = addtorow, only.contents = TRUE)

##### FORM

## redefining this to include 1960 because of sit-ins
doca.afam.six1 <- doca.afam %>% filter(year >= 1960, year <= 1970)
speed.afam.six1 <- speed.afam %>% filter(year >= 1960, year <= 1970)

## DoCA
doca.form_dict <- data.frame(t(
        matrix(c(
            1,   'Rally/demonstration',
            2,   'March',
            4,   'Picket',
            5,   'Civil disobedience',
            12,  'Riot, melee, mob violence',
            14,  'Boycott'), 
        nrow = 2)
    ), stringsAsFactors = FALSE)
names(doca.form_dict) <- c('code', 'description')

doca.act_dict <- data.frame(t(
        matrix(c(
            20,  'Picketing',
            22,  'Marching',
            52,  'Sit-in',
            62,  'Damaging property'), 
        nrow = 2)
    ), stringsAsFactors = FALSE)
names(doca.act_dict) <- c('code', 'description')

## plot the over time graphs
## trying to distinguish between the major peaks in McAdam 1982 + 1983
doca.form.m <- doca.afam.six1 %>% filter(form1 %in% doca.form_dict$code) %>% 
    group_by(mdate, form1) %>% summarise(count = n())
doca.form.m$description <- NA
doca.form.m$description <- sapply(doca.form.m$form1, function(x) doca.form_dict[doca.form_dict$code == x,]$description)

doca.act.m <- doca.afam.six1 %>% filter(act1 %in% doca.act_dict$code) %>% 
    group_by(mdate, act1) %>% summarise(count = n())
doca.act.m$description <- NA
doca.act.m$description <- sapply(doca.act.m$act1, function(x) doca.act_dict[doca.act_dict$code == x,]$description)

p <- ggplot(doca.form.m, aes(mdate, count)) + geom_bar(stat='identity') 
p <- p + facet_wrap( ~ description, ncol = 3)
p <- p + theme_bw() + my_theme
p <- p + xlab("Date") + ylab("Event Count")
ggsave(p, file = '../output/doca-form_2016-04-13.pdf', width = 12, height = 6)

p <- ggplot(doca.act.m, aes(mdate, count)) + geom_bar(stat='identity') 
p <- p + facet_wrap( ~ description, ncol = 4)
p <- p + theme_bw() + my_theme
p <- p + xlab("Date") + ylab("Event Count")
ggsave(p, file = '../output/doca-act_2016-04-13.pdf', width = 12, height = 3)

## SPEED
speed.pe_dict <- data.frame(t(
    matrix(c(
    7,  'Demonstration/March',
    9,  'Symbolic act',
    12, 'Riot or brawl'), 
    nrow = 2)
    ), stringsAsFactors = FALSE)
names(speed.pe_dict) <- c('code', 'description')

speed.sym_dict <- data.frame(t(
    matrix(c(
    1,  'Passive Resistance',
    3,  'Boycott',
    4,  'Block Egress/Regress'), nrow = 2)
    ), stringsAsFactors = FALSE)
names(speed.sym_dict) <- c('code', 'description')

## gather by month
speed.pe.m <- speed.afam.six1 %>% filter(PE_TYPE %in% speed.pe_dict$code) %>% 
    group_by(mdate, PE_TYPE) %>% summarise(count = n())

## get riots
speed.atk.m <- speed.afam.six1 %>% filter(ATK_TYPE == 1) %>% 
    group_by(mdate, ATK_TYPE) %>% summarise(count = n())

## recode for riot
speed.atk.m[speed.atk.m$ATK_TYPE == 1,]$ATK_TYPE <- 12

## change column names to merge
names(speed.atk.m) <- names(speed.pe.m)

## merge riots in
speed.pe.m <- rbind(speed.pe.m, speed.atk.m)

speed.pe.m$description <- NA
speed.pe.m$description <- sapply(speed.pe.m$PE_TYPE, function(x) speed.pe_dict[speed.pe_dict$code == x,]$description)

speed.sym.m <- speed.afam.six1 %>% filter(SYM_TYPE %in% speed.sym_dict$code) %>% 
    group_by(mdate, SYM_TYPE) %>% summarise(count = n())
speed.sym.m$description <- NA
speed.sym.m$description <- sapply(speed.sym.m$SYM_TYPE, function(x) speed.sym_dict[speed.sym_dict$code == x,]$description)

p <- ggplot(speed.pe.m, aes(mdate, count)) + geom_bar(stat='identity') 
p <- p + facet_wrap( ~ description, ncol = 3)
p <- p + theme_bw() + my_theme
p <- p + xlab("Date") + ylab("Event Count")
ggsave(p, file = '../output/speed-pe_2016-04-13.pdf', width = 12, height = 3)

p <- ggplot(speed.sym.m, aes(mdate, count)) + geom_bar(stat='identity') 
p <- p + facet_wrap( ~ description, ncol = 3) 
p <- p + theme_bw() + my_theme + scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10))
p <- p + xlab("Date") + ylab("Event Count")
ggsave(p, file = '../output/speed-sym_2016-04-13.pdf', width = 12, height = 3)

##### SIZE

## max events -- 
head(doca.afam.six1[order(-doca.afam.six1$particex),]$particex)
head(doca.afam.six1[order(-doca.afam.six1$particex),])
head(speed.afam.six1[order(-speed.afam.six1$N_OF_INI_P),]$N_OF_INI_P)
head(speed.afam.six1[order(-speed.afam.six1$N_OF_INI_P),])

## construct participant lists, remove NAs
doca.part <- doca.afam.six1 %>% 
    select(date, particex) %>% 
    filter(!is.na(particex)) %>% 
    arrange(date) %>%
    group_by(date) %>%
    summarise(n_participants = sum(particex)) %>%
    mutate(cumsum_part = cumsum(n_participants))

speed.part <- speed.afam.six1 %>% 
    select(date, N_OF_INI_P) %>% 
    filter(!is.na(N_OF_INI_P)) %>% 
    arrange(date) %>%
    group_by(date) %>%
    summarise(n_participants = sum(N_OF_INI_P)) %>%
    mutate(cumsum_part = cumsum(n_participants))

doca.part$dataset <- 'DoCA'
speed.part$dataset <- 'SPEED'

## test correlation
join.part <- merge(doca.part, speed.part, by = 'date', suffix = c('.doca', '.speed'))
cor.part <- cor.test(join.part$cumsum_part.doca, join.part$cumsum_part.speed)

## join all for plotting
all.part <- rbind(doca.part, speed.part)

## density/rug plots
p <- ggplot(all.part, # %>% filter(n_participants < limit, n_participants < limit),
    aes(n_participants, fill = dataset, color = dataset))
p <- p + geom_density(alpha = 0.4) + geom_rug(alpha = 0.4) 
p <- p + scale_x_log10()
p <- p + theme_bw() + scale_colour_brewer(palette = 'Set1') + my_theme 
p <- p + theme(
    legend.title = element_blank(),
    legend.justification=c(1,0),
    legend.position=c(0.9,0.5)
)
p <- p + xlab('Number of Participants (logged)') + ylab('Density')
ggsave(p, file = "../output/doca-speed-density-size_2016-04-13.pdf", width = 6, height = 6)

## plot per day estimate. remove entries over limit
p <- ggplot(join.part, # %>% filter(n_participants.doca < limit, n_participants.speed < limit), 
    aes(n_participants.doca, n_participants.speed)) 
p <- p + geom_point(alpha = 0.4, position = "jitter", size = 3) 
p <- p + geom_abline(slope = 1, intercept = 0, color = 'red', linetype = "dashed", alpha = 0.7, size = 1.2) ## perfect agreement
p <- p + geom_smooth(method = "lm", se = FALSE)
#p <- p + xlim(-0.1, limit) + ylim(-0.1, limit)
p <- p + scale_x_log10() + scale_y_log10() 
p <- p + theme_bw() + my_theme
p <- p + xlab('Number of Participants, DoCA (logged)') + ylab('Number of Participants, SPEED (logged)')
ggsave(p, file = "../output/doca-speed-scatterplot-size_2016-04-13.pdf", width = 6, height = 6)

## plot cumulative progress
p <- ggplot(all.part, aes(date, cumsum_part, color = dataset))
p <- p + geom_line() 
p <- p + theme_bw() + scale_colour_brewer(palette = "Set1") + my_theme
p <- p + xlab("Date") + ylab("Cumulative participants")
ggsave(p, file = "../output/doca-speed-cumsum-size_2016-04-13.pdf", width = 12, height = 3)


