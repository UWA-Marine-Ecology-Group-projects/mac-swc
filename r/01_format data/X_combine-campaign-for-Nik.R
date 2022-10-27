bossmet <- read.csv("data/staging/2020-2021_south-west_BOSS.checked.metadata.csv")

boss <- read.csv("data/staging/2020-2021_south-west_BOSS.checked.maxn.csv") %>%
  right_join(bossmet, by = c("id", "campaignid", "sample")) %>%
  dplyr::mutate(method = "BOSS", 
                site = as.character(1:nrow(.)),
                location = "South-west Corner",
                time = "12:00") %>%
  glimpse()

bruvmet <- read.csv("data/staging/2020_south-west_stereo-BRUVs.checked.metadata.csv")

bruv <- read.csv("data/staging/2020_south-west_stereo-BRUVs.checked.maxn.csv") %>%
  right_join(bruvmet, by = c("campaignid","sample")) %>%
  dplyr::mutate(method = "BRUV",
                location = "South-west Corner") %>%
  glimpse()

maxn <- bind_rows(bruv, boss) %>%
  dplyr::select(-id) %>%
  dplyr::mutate(project = "South-west_test-synthesis")

write.csv(maxn, file = "data/staging/Archive/South-west_test-synthesis.maxn.csv",
          row.names = F)
