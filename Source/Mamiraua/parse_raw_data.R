
library(dplyr)
library(readxl)
library(tidyxl)

# sheets:
# 1 = "Cheia"
# 2 = "Seca - Adultos"
# 3 = "Seca - Filhos"


raw <- list()

# read in first-row column names of sheet 1
line1names <- read_excel("raw/Mamiraua_Amazon_Pedro.xlsx", sheet = 1, n_max = 0) |> names()
raw[[1]] <- read_excel("raw/Mamiraua_Amazon_Pedro.xlsx", sheet = 1, skip = 1) 
colnames(raw[[1]])[10:24] <- line1names

line1names <- read_excel("raw/Mamiraua_Amazon_Pedro.xlsx", sheet = 2, n_max = 0) |> names()
line1names[line1names == "...13"] <- "Altura sentada a (cm)"
line1names <- c(line1names, "Altura em pé a (cm)")
raw[[2]] <- read_excel("raw/Mamiraua_Amazon_Pedro.xlsx", sheet = 2, skip = 1) 
colnames(raw[[2]])[10:24] <- line1names

line1names <- read_excel("raw/Mamiraua_Amazon_Pedro.xlsx", sheet = 3, n_max = 0) |> names()
raw[[3]] <- read_excel("raw/Mamiraua_Amazon_Pedro.xlsx", sheet = 3, skip = 1) 
colnames(raw[[3]])[10:25] <- line1names

# method to extract comments
com <- xlsx_cells("raw/Mamiraua_Amazon_Pedro.xlsx", sheets = 1)

# goal: a single table, one-row-per-measurement, for height and for weight

# checks to ensure consistency:
any(duplicated(raw[[1]]$`Código da pessoa`))
any(duplicated(raw[[2]]$`Código da pessoa`))
any(duplicated(raw[[3]]$`Código da pessoa`))

dat <- select(raw[[1]], 
  person_id = `Código da pessoa`,
  season_at_measure = `Sazonalidade`,
  date_of_measure = `Data`,
  date_of_birth = `Data de nascimento`,
  age_at_measure = `Idade`,
  sex = `Sexo`,
  reproductive_status = `Estado reprodutivo`,
  height_cm = `A100 (altura cm)`,
  weight_kg = `A300 (Peso kg)`
)

add <- select(raw[[1]], 
  person_id = `Código da pessoa`,
  season_at_measure = `Sazonalidade`,
  date_of_measure = `Data`,
  date_of_birth = `Data de nascimento`,
  age_at_measure = `Idade`,
  sex = `Sexo`,
  reproductive_status = `Estado reprodutivo`,
  height_cm = `A100a (altura cm)`
)

dat <- bind_rows(dat, add)

# meaning of `Estado reprodutivo` A = amamentando, N-não gravida, não amamentando



# clean bad measurements

dat$date_of_measure[dat$date_of_measure == "25-07´15"] <- "42210" # using 1899-12-30 as starting date
dat$date_of_measure <- as.Date(as.integer(dat$date_of_measure), origin = "1899-12-30")

dat$date_of_birth[dat$date_of_birth == "X"] <- NA
dat$date_of_birth <- as.Date(as.integer(dat$date_of_birth), origin = "1899-12-30")

dat$weight_kg[dat$weight_kg == "X"] <- NA
dat$weight_kg <- gsub(",", ".", dat$weight_kg)
dat$weight_kg <- as.numeric(dat$weight_kg)

dat$height_cm[dat$height_cm == "X"] <- NA
dat$height_cm <- as.numeric(dat$height_cm)

dat <- filter(dat, !is.na(height_cm) | !is.na(weight_kg))

dat$site <- "Mamiraua"

stopifnot(nrow(dat) == 700)

write.csv(dat, "site_measurements.csv", row.names = FALSE)
