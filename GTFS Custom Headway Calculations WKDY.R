library(tidytransit)
library(data.table)
library(lubridate)
library(tidyverse)

gtfs <- read_gtfs("R:/Google/google_transit/2003/2003_google_transit.zip")


summary(gtfs)

####Initial Preparations####

gtfs <- set_servicepattern(gtfs)
calendar <- gtfs$calendar


gtfs$trips <- gtfs$trips %>%
  left_join(gtfs$routes, by = "route_id")

gtfs$trips$route_id <- gtfs$trips$route_short_name

trips <- gtfs$trips
stops <- gtfs$stops
stoptimes <- gtfs$stop_times

Combined <- calendar %>%
  left_join(trips, by = "service_id") %>%
  left_join(stoptimes, by = "trip_id") %>%
  ungroup()

Combined$events_and_status[is.na(Combined$events_and_status)] <- ""

####END Initial Preparations####

####Service_id filtering by service day####
service_ids <- gtfs$trips$service_id

#WKDY service_ids (All ons)
WKDYservice_id <- ifelse((calendar$operating_days != "smuwtfa" & calendar$operating_days != "a" & calendar$operating_days != "s") & (grepl("on", calendar$events_and_status) | (is.na(calendar$events_and_status))), calendar$service_id, NA)
WKDYservice_id <- WKDYservice_id[complete.cases(WKDYservice_id)]

#Sat service_ids
SATservice_id <- ifelse(calendar$operating_days == "a", calendar$service_id, NA)
SATservice_id <- SATservice_id[complete.cases(SATservice_id)]

#Sun service_ids
SUNservice_id <- ifelse(calendar$operating_days == "s", calendar$service_id, NA)
SUNservice_id <- SUNservice_id[complete.cases(SUNservice_id)]

#HOL service_ids
HOLservice_id <- ifelse(calendar$operating_days == "smuwtfa", calendar$service_id, NA)
HOLservice_id <- HOLservice_id[complete.cases(HOLservice_id)]




#####END Service_id filtering by service day####

####Create service day frames to prepare for calculations####



WKDYFrame <- Combined %>%
  filter(service_id %in% WKDYservice_id) %>%
  select(route_id, direction_id, departure_time, stop_id, events_and_status) %>%
  arrange(route_id, direction_id, stop_id, departure_time) %>%
  distinct()

WKDYFrame <- na.omit(WKDYFrame)

WKDYFrame$departure_time_sec <- period_to_seconds(hms(WKDYFrame$departure_time))

####END Create service day frames to prepare for calculations####

####Lead the departure time and perform calculations####

WKDYFrame <- WKDYFrame %>%
  arrange(route_id, direction_id, stop_id, departure_time_sec) %>%
  group_by(route_id, direction_id, stop_id) %>%
  mutate(secondsstagger = lead(departure_time_sec)) %>%
  ungroup()

WKDYFrame$headway <- (WKDYFrame$secondsstagger - WKDYFrame$departure_time_sec)/60
WKDYFrame <- na.omit(WKDYFrame)

####END Lead the departure time and perform calculations####

####Create Original Unfiltered Frame (Filtered frame compensates for express routes and 1 trip routes)####

OrigWKDYFrame <- WKDYFrame
WKDYFrame <- WKDYFrame %>% filter(WKDYFrame$headway < 240)

####END Create Original Unfiltered Frame (Filtered frame compensates for express routes and 1 trip routes)####

####Cutting into Service Periods####

EMWKDYFrame <- WKDYFrame %>% filter(WKDYFrame$departure_time_sec < 21600)
AMWKDYFrame <- WKDYFrame %>% filter(WKDYFrame$departure_time_sec < 32400 & WKDYFrame$departure_time_sec > 21600)
BASEWKDYFrame <- WKDYFrame %>% filter(WKDYFrame$departure_time_sec < 50400 & WKDYFrame$departure_time_sec > 32400)
PMWKDYFrame <- WKDYFrame %>% filter(WKDYFrame$departure_time_sec < 64800 & WKDYFrame$departure_time_sec > 50400)
NTWKDYFrame <- WKDYFrame %>% filter(WKDYFrame$departure_time_sec < 100800 & WKDYFrame$departure_time_sec < 100800)

####END Cutting into Service Periods####

####Tabulations By Route and Stop####
#Early Morning
WKDY_RSEM <- EMWKDYFrame %>% 
  group_by(route_id, stop_id, direction_id, events_and_status) %>%
  summarize(Median_Headway = median(headway), Mean_Headway = mean(headway), Departures = n())

WKDY_RSEM$Median_Headway <- round(WKDY_RSEM$Median_Headway, 0)
WKDY_RSEM$Mean_Headway <- round(WKDY_RSEM$Mean_Headway, 0)

#AM Period
WKDY_RSAM <- AMWKDYFrame %>% 
  group_by(route_id, stop_id, direction_id, events_and_status) %>%
  summarize(Median_Headway = median(headway), Mean_Headway = mean(headway), Departures = n())

WKDY_RSAM$Median_Headway <- round(WKDY_RSAM$Median_Headway, 0)
WKDY_RSAM$Mean_Headway <- round(WKDY_RSAM$Mean_Headway, 0)

#Base Period
WKDY_RSBASE <- BASEWKDYFrame %>% 
  group_by(route_id, stop_id, direction_id, events_and_status) %>%
  summarize(Median_Headway = median(headway), Mean_Headway = mean(headway), Departures = n())

WKDY_RSBASE$Median_Headway <- round(WKDY_RSBASE$Median_Headway, 0)
WKDY_RSBASE$Mean_Headway <- round(WKDY_RSBASE$Mean_Headway, 0)

#PM Period
WKDY_RSPM <- PMWKDYFrame %>% 
  group_by(route_id, stop_id, direction_id, events_and_status) %>%
  summarize(Median_Headway = median(headway), Mean_Headway = mean(headway), Departures = n())

WKDY_RSPM$Median_Headway <- round(WKDY_RSPM$Median_Headway, 0)
WKDY_RSPM$Mean_Headway <- round(WKDY_RSPM$Mean_Headway, 0)

#NT Period
WKDY_RSNT <- NTWKDYFrame %>% 
  group_by(route_id, stop_id, direction_id, events_and_status) %>%
  summarize(Median_Headway = median(headway), Mean_Headway = mean(headway), Departures = n())

WKDY_RSNT$Median_Headway <- round(WKDY_RSNT$Median_Headway, 0)
WKDY_RSNT$Mean_Headway <- round(WKDY_RSNT$Mean_Headway, 0)

####END Tabulations By Route and Stop####

####Tabulations By Route####
#Early Morning
WKDY_REM <- EMWKDYFrame %>% 
  group_by(route_id) %>%
  summarize(Median_Headway = median(headway), Mean_Headway = mean(headway), st_dev_headways = sd(headway), Departures = n(), stop_count = n_distinct(stop_id))

WKDY_REM$Median_Headway <- round(WKDY_REM$Median_Headway, 0)
WKDY_REM$Mean_Headway <- round(WKDY_REM$Mean_Headway, 0)
WKDY_REM$st_dev_headways <- round(WKDY_REM$st_dev_headways, 1)

#AM Period
WKDY_RAM <- AMWKDYFrame %>% 
  group_by(route_id) %>%
  summarize(Median_Headway = median(headway), Mean_Headway = mean(headway), st_dev_headways = sd(headway), Departures = n(), stop_count = n_distinct(stop_id))

WKDY_RAM$Median_Headway <- round(WKDY_RAM$Median_Headway, 0)
WKDY_RAM$Mean_Headway <- round(WKDY_RAM$Mean_Headway, 0)
WKDY_RAM$st_dev_headways <- round(WKDY_RAM$st_dev_headways, 1)

#Base Period
WKDY_RBASE <- BASEWKDYFrame %>% 
  group_by(route_id) %>%
  summarize(Median_Headway = median(headway), Mean_Headway = mean(headway), st_dev_headways = sd(headway), Departures = n(), stop_count = n_distinct(stop_id))

WKDY_RBASE$Median_Headway <- round(WKDY_RBASE$Median_Headway, 0)
WKDY_RBASE$Mean_Headway <- round(WKDY_RBASE$Mean_Headway, 0)
WKDY_RBASE$st_dev_headways <- round(WKDY_RBASE$st_dev_headways, 1)

#PM Period
WKDY_RPM <- PMWKDYFrame %>% 
  group_by(route_id) %>%
  summarize(Median_Headway = median(headway), Mean_Headway = mean(headway), st_dev_headways = sd(headway), Departures = n(), stop_count = n_distinct(stop_id))

WKDY_RPM$Median_Headway <- round(WKDY_RPM$Median_Headway, 0)
WKDY_RPM$Mean_Headway <- round(WKDY_RPM$Mean_Headway, 0)
WKDY_RPM$st_dev_headways <- round(WKDY_RPM$st_dev_headways, 1)

#NT Period
WKDY_RNT <- NTWKDYFrame %>% 
  group_by(route_id) %>%
  summarize(Median_Headway = median(headway), Mean_Headway = mean(headway), st_dev_headways = sd(headway), Departures = n(), stop_count = n_distinct(stop_id))

WKDY_RNT$Median_Headway <- round(WKDY_RNT$Median_Headway, 0)
WKDY_RNT$Mean_Headway <- round(WKDY_RNT$Mean_Headway, 0)
WKDY_RNT$st_dev_headways <- round(WKDY_RNT$st_dev_headways, 1)

####END Tabulations By Route####

####StopMap Plots####
hnl_stops_sf <- stops_as_sf(stops)

#WKDY EM StopMap
WKDY_EMStopMap <- WKDY_RSEM %>%
  left_join(stops, by = "stop_id")

WKDY_EMStopMap_sf <- hnl_stops_sf %>%
  right_join(WKDY_EMStopMap, by = "stop_id")

WKDY_EMStopMap_sf %>% 
  ggplot() + 
  geom_sf(aes(color=Mean_Headway)) +
  scale_color_gradient(low="blue", high="red") +
  ggtitle("WKDY Early Morning (1st trip - 6AM)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WKDY_EMStopMap.pdf", width = 11, height = 8.5)

#WKDY AM StopMap
WKDY_AMStopMap <- WKDY_RSAM %>%
  left_join(stops, by = "stop_id")

WKDY_AMStopMap_sf <- hnl_stops_sf %>%
  right_join(WKDY_AMStopMap, by = "stop_id")

WKDY_AMStopMap_sf %>% 
  ggplot() + 
  geom_sf(aes(color=Mean_Headway)) +
  scale_color_gradient(low="blue", high="red") +
  ggtitle("WKDY AM Peak (6AM - 9AM)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WKDY_AMStopMap.pdf", width = 11, height = 8.5)

#WKDY BASE StopMap
WKDY_BASEStopMap <- WKDY_RSBASE %>%
  left_join(stops, by = "stop_id")

WKDY_BASEStopMap_sf <- hnl_stops_sf %>%
  right_join(WKDY_BASEStopMap, by = "stop_id")

WKDY_BASEStopMap_sf %>% 
  ggplot() + 
  geom_sf(aes(color=Mean_Headway)) +
  scale_color_gradient(low="blue", high="red") +
  ggtitle("WKDY BASE Period (9AM - 2PM)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WKDY_BASEStopMap.pdf", width = 11, height = 8.5)

#WKDY PM Peak StopMap
WKDY_PMStopMap <- WKDY_RSPM %>%
  left_join(stops, by = "stop_id")

WKDY_PMStopMap_sf <- hnl_stops_sf %>%
  right_join(WKDY_PMStopMap, by = "stop_id")

WKDY_PMStopMap_sf %>% 
  ggplot() + 
  geom_sf(aes(color=Mean_Headway)) +
  scale_color_gradient(low="blue", high="red") +
  ggtitle("WKDY PM Peak (2PM - 6PM)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WKDY_PMStopMap.pdf", width = 11, height = 8.5)

#WKDY Night StopMap
WKDY_NTStopMap <- WKDY_RSNT %>%
  left_join(stops, by = "stop_id")

WKDY_NTStopMap_sf <- hnl_stops_sf %>%
  right_join(WKDY_NTStopMap, by = "stop_id")

WKDY_NTStopMap_sf %>% 
  ggplot() + 
  geom_sf(aes(color=Mean_Headway)) +
  scale_color_gradient(low="blue", high="red") +
  ggtitle("WKDY Night Period (6PM - Last trip)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WKDY_NTStopMap.pdf", width = 11, height = 8.5)

####END StopMap Plots####

#Master Route and Stop
WKDY_Master <- WKDY_RSEM %>%
  rename("EM Departures" = Departures) %>%
  rename("EM Median Headways (Min)" = Median_Headway) %>%
  rename("EM Mean Headways (Min)" = Mean_Headway) %>%
  full_join(WKDY_RSAM, by = c("route_id", "direction_id", "stop_id")) %>%
  rename("AM Departures" = Departures) %>%
  rename("AM Median Headways (Min)" = Median_Headway) %>%
  rename("AM Mean Headways (Min)" = Mean_Headway) %>%
  full_join(WKDY_RSBASE, by = c("route_id", "direction_id", "stop_id")) %>%
  rename("BASE Departures" = Departures) %>%
  rename("BASE Median Headways (Min)" = Median_Headway) %>%
  rename("BASE Mean Headways (Min)" = Mean_Headway) %>%
  full_join(WKDY_RSPM, by = c("route_id", "direction_id", "stop_id")) %>%
  rename("PM Departures" = Departures) %>%
  rename("PM Median Headways (Min)" = Median_Headway) %>%
  rename("PM Mean Headways (Min)" = Mean_Headway) %>%
  full_join(WKDY_RSNT, by = c("route_id", "direction_id", "stop_id")) %>%
  rename("NT Departures" = Departures) %>%
  rename("NT Median Headways (Min)" = Median_Headway) %>%
  rename("NT Mean Headways (Min)" = Mean_Headway) %>%
  rename("Route" = route_id) %>%
  rename("Direction" = direction_id) %>%
  rename("Stop ID" = stop_id)

WKDY_Master$Direction <- ifelse(WKDY_Master$Direction == "0", "West", ifelse(WKDY_Master$Direction == "1", "East", NA))
  
WKDY_Master <- WKDY_Master %>%
    select(Route, Direction, "Stop ID", "EM Median Headways (Min)", "AM Median Headways (Min)", "BASE Median Headways (Min)", "PM Median Headways (Min)", "NT Median Headways (Min)", "EM Mean Headways (Min)", "AM Mean Headways (Min)", "BASE Mean Headways (Min)", "PM Mean Headways (Min)", "NT Mean Headways (Min)", "EM Departures", "AM Departures", "BASE Departures", "PM Departures", "NT Departures")


#WKDY Master Route Frequency File
WKDY_RTE_Master <- WKDY_REM %>%
  rename("EM Departures" = Departures) %>%
  rename("EM Median Headways (Min)" = Median_Headway) %>%
  rename("EM Mean Headways (Min)" = Mean_Headway) %>%
  rename("EM Headway Std Dev (Min)" = st_dev_headways) %>%
  rename("EM Stop Count" = stop_count) %>%
  full_join(WKDY_RAM, by = "route_id") %>%
  rename("AM Departures" = Departures) %>%
  rename("AM Median Headways (Min)" = Median_Headway) %>%
  rename("AM Mean Headways (Min)" = Mean_Headway) %>%
  rename("AM Headway Std Dev (Min)" = st_dev_headways) %>%
  rename("AM Stop Count" = stop_count) %>%
  full_join(WKDY_RBASE, by = "route_id") %>%
  rename("BASE Departures" = Departures) %>%
  rename("BASE Median Headways (Min)" = Median_Headway) %>%
  rename("BASE Mean Headways (Min)" = Mean_Headway) %>%
  rename("BASE Headway Std Dev (Min)" = st_dev_headways) %>%
  rename("BASE Stop Count" = stop_count) %>%
  full_join(WKDY_RPM, by = "route_id") %>%
  rename("PM Departures" = Departures) %>%
  rename("PM Median Headways (Min)" = Median_Headway) %>%
  rename("PM Mean Headways (Min)" = Mean_Headway) %>%
  rename("PM Headway Std Dev (Min)" = st_dev_headways) %>%
  rename("PM Stop Count" = stop_count) %>%
  full_join(WKDY_RNT, by = "route_id") %>%
  rename("NT Departures" = Departures) %>%
  rename("NT Median Headways (Min)" = Median_Headway) %>%
  rename("NT Mean Headways (Min)" = Mean_Headway) %>%
  rename("NT Headway Std Dev (Min)" = st_dev_headways) %>%
  rename("NT Stop Count" = stop_count) %>%
  rename("Route" = route_id)


WKDY_RTE_Master <- WKDY_RTE_Master %>%
  select(Route, "EM Median Headways (Min)", "AM Median Headways (Min)", "BASE Median Headways (Min)", "PM Median Headways (Min)", "NT Median Headways (Min)", "EM Mean Headways (Min)", "AM Mean Headways (Min)", "BASE Mean Headways (Min)", "PM Mean Headways (Min)", "NT Mean Headways (Min)","EM Headway Std Dev (Min)", "AM Headway Std Dev (Min)", "BASE Headway Std Dev (Min)", "PM Headway Std Dev (Min)", "NT Headway Std Dev (Min)", "EM Departures", "AM Departures", "BASE Departures", "PM Departures", "NT Departures", "EM Stop Count", "AM Stop Count", "BASE Stop Count", "PM Stop Count", "NT Stop Count")

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

WKDY_RTE_Master[is.nan(WKDY_RTE_Master)] <- NA

V <- c("A", "C", "E", "1", "1L", "2", "2L", "3", "4", "6", "8", "9", "13", "19", "20", "11", "22", "23", "40", "41", "42", "43", "51", "52", "53", "54", "60", "61", "65", "66", "67", "5", "7", "9S", "10", "14", "15", "16", "17", "18", "24", "31", "32", "44", "69", "71", "72", "73", "74", "76", "234", "235", "401", "402", "403", "411", "413", "414", "415", "416", "432", "433", "434", "501", "503", "504" ,"651", "671", "672", "673", "674", "80", "80A", "80B", "81", "82", "83", "84", "84A", "85", "86", "87", "88", "88A", "89", "90", "91", "92", "93", "94", "96", "97", "98", "98A", "99", "101", "102", "103", "PH1", "PH2", "PH3", "PH4", "PH6", "PH7", "W1", "W2", "W3")

WKDY_RTE_Master <- WKDY_RTE_Master %>%
  slice(match(V, Route))


####ROUTE MAP PLOTS####
gtfs_sf <- gtfs_as_sf(gtfs)
WKDY_routes_sf <- get_route_geometry(gtfs_sf, service_ids = WKDYservice_id)

#Early Morning Routes
WKDY_EM_RouteMap_sf <- WKDY_routes_sf %>%
  inner_join(WKDY_REM, by = "route_id")

WKDY_EM_RouteMap_sf %>%
  ggplot() + 
  geom_sf(aes(colour=Mean_Headway)) +
  scale_color_gradient(low="blue", high="red") +
  labs(color = "Headways") +
  ggtitle("WKDY Early Morning (1st trip - 6AM)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WKDY_EMRouteMap.pdf", width = 11, height = 8.5)

#AM Peak Routes
WKDY_AM_RouteMap_sf <- WKDY_routes_sf %>%
  inner_join(WKDY_RAM, by = "route_id")

WKDY_AM_RouteMap_sf %>%
  ggplot() + 
  geom_sf(aes(colour=Mean_Headway)) +
  scale_color_gradient(low="blue", high="red") +
  labs(color = "Headways") +
  ggtitle("WKDY AM Peak (6AM - 9AM)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WKDY_AMRouteMap.pdf", width = 11, height = 8.5)

#BASE Routes
WKDY_BASE_RouteMap_sf <- WKDY_routes_sf %>%
  inner_join(WKDY_RBASE, by = "route_id")

WKDY_BASE_RouteMap_sf %>%
  ggplot() + 
  geom_sf(aes(colour=Mean_Headway)) +
  scale_color_gradient(low="blue", high="red") +
  labs(color = "Headways") +
  ggtitle("WKDY BASE Period (9AM - 2PM)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WKDY_BASERouteMap.pdf", width = 11, height = 8.5)

#PM Peak Routes
WKDY_PM_RouteMap_sf <- WKDY_routes_sf %>%
  inner_join(WKDY_RPM, by = "route_id")

WKDY_PM_RouteMap_sf %>%
  ggplot() + 
  geom_sf(aes(colour=Mean_Headway)) +
  scale_color_gradient(low="blue", high="red") +
  labs(color = "Headways") +
  ggtitle("WKDY PM Peak (2PM - 6PM)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WKDY_PMRouteMap.pdf", width = 11, height = 8.5)

#NT Peak Routes
WKDY_NT_RouteMap_sf <- WKDY_routes_sf %>%
  inner_join(WKDY_RNT, by = "route_id")

WKDY_NT_RouteMap_sf %>%
  ggplot() + 
  geom_sf(aes(colour=Mean_Headway)) +
  scale_color_gradient(low="blue", high="red") +
  labs(color = "Headways") +
  ggtitle("WKDY PM Peak (2PM - 6PM)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WKDY_PMRouteMap.pdf", width = 11, height = 8.5)

write.csv(WKDY_Master, "WKDY_StopHeadways.csv", row.names = F)
write.csv(WKDY_RTE_Master, "WKDY_RouteHeadways.csv", row.names = F)

