# update the NICHD reporter dashboard
## note: be sure the po_lookup table is up to date


## step 1, get the data
source("r/functions/get_nih_reporter.r")
mq <- create_query(IC = "NICHD", include_active = TRUE, exclude_subprojects = TRUE)
grants <- get_nih_reporter(mq, outfile = "projects/dashboard reporter shiny/reporter_data.txt")


## step 2: filter and clean the data
library(tidyverse)
grants <- grants %>% 
  filter(mechanism_code_dc != "IM")

grants <- grants %>% 
  mutate(organization_type_code_desc = case_when(
    organization_type_code == "10" ~ "10: Academic",
    organization_type_code == "20" ~ "20: Institute",
    organization_type_code == "30" ~ "30: Hospital",
    organization_type_code == "FP" ~ "FP: For-Profit",
    organization_type_code == "NP" ~ "NP: Non-Profit",
    TRUE ~ "OT: Unknown"
  ),
  award_type_desc = case_when(
    award_type == "1" ~ "1: New",
    award_type == "2" ~ "2: Renewal",
    award_type == "3" ~ "3: Revision",
    grepl("4", award_type) ~ "4: Extension",
    award_type == "5" ~ "5: Continuation",
    award_type %in% c("6", "7", "8", "9") ~ "Other",
    TRUE ~ "Not applicable"
  ),
  org_name_clean = case_when(
    grepl("ALBERT EINSTEIN COLLEGE OF MEDICINE", organization_org_name) ~ "ALBERT EINSTEIN COLLEGE OF MEDICINE",
    grepl("HARVARD", organization_org_name) ~ "HARVARD UNIVERSITY",
    grepl("RUTGERS", organization_org_name) ~ "RUTGERS UNIVERSITY",
    grepl("DARTMOUTH", organization_org_name) ~ "DARTMOUTH COLLEGE",
    grepl("^TUFTS", organization_org_name) ~ "TUFTS UNIVERSITY",
    grepl("BOSTON UNIVERSITY", organization_org_name) ~ "BOSTON UNIVERSITY",
    grepl("COLUMBIA UNIV", organization_org_name) ~ "COLUMBIA UNIVERSITY",
    grepl("WEILL MEDICAL COLL|WEILL CORNELL|CORNELL UNIV", organization_org_name) ~ "CORNELL UNIVERSITY",
    grepl("INDIANA UNIV", organization_org_name) ~ "INDIANA UNIVERSITY",
    grepl("LSU |LOUISIANA STATE UNIV", organization_org_name) ~ "LOUISIANA STATE UNIVERSITY",
    grepl("FRED HUTCHINSON", organization_org_name) ~ "FRED HUTCHINSON CANCER RESEARCH CENTER",
    grepl("NEW YORK UNIV", organization_org_name) ~ "NEW YORK UNIVERSITY",
    grepl("NORTHWESTERN UNIV", organization_org_name) ~ "NORTHWESTERN UNIVERSITY",
    grepl("PENNSYLVANIA STATE UNIV", organization_org_name) ~ "PENNSYLVANIA STATE UNIVERSITY",
    grepl("STATE UNIVERSITY OF NEW YORK|STATE UNIVERSITY NEW YORK|SUNY ", organization_org_name) ~ "STATE UNIVERSITY OF NEW YORK",
    grepl("TEXAS A&M UNIV", organization_org_name, fixed = TRUE) ~ "TEXAS A&M UNIVERSITY",
    grepl("TEXAS TECH UNIV", organization_org_name) ~ "TEXAS TECH UNIVERSITY",
    grepl("UNIV OF MARYLAND|UNIVERSITY OF MARYLAND", organization_org_name) ~ "UNIVERSITY OF MARYLAND",
    grepl("UNIV OF NORTH CAROLINA|UNIVERSITY OF NORTH CAROLINA", organization_org_name) ~ "UNIVERSITY OF NORTH CAROLINA",
    grepl("UNIV OF ARKANSAS|UNIVERSITY OF ARKANSAS", organization_org_name) ~ "UNIVERSITY OF ARKANSAS",
    grepl("UNIV OF ALABAMA|UNIVERSITY OF ALABAMA", organization_org_name) ~ "UNIVERSITY OF ALABAMA",
    grepl("UNIV OF ALASKA|UNIVERSITY OF ALASKA", organization_org_name) ~ "UNIVERSITY OF ALASKA",
    grepl("UNIV OF CONNECTICUT|UNIVERSITY OF CONNECTICUT", organization_org_name) ~ "UNIVERSITY OF CONNECTICUT",
    grepl("UNIV OF COLORADO|UNIVERSITY OF COLORADO", organization_org_name) ~ "UNIVERSITY OF COLORADO",
    grepl("UNIV OF HAWAII|UNIVERSITY OF HAWAII", organization_org_name) ~ "UNIVERSITY OF HAWAII",
    grepl("UNIV OF ILLINOIS|UNIVERSITY OF ILLINOIS", organization_org_name) ~ "UNIVERSITY OF ILLINOIS",
    grepl("UNIV OF KANSAS|UNIVERSITY OF KANSAS", organization_org_name) ~ "UNIVERSITY OF KANSAS",
    grepl("UNIV OF MASSACHUSETTS|UNIVERSITY OF MASSACHUSETTS", organization_org_name) ~ "UNIVERSITY OF MASSACHUSETTS",
    grepl("UNIV OF MIAMI|UNIVERSITY OF MIAMI", organization_org_name) ~ "UNIVERSITY OF MIAMI",
    grepl("UNIV OF MICHIGAN|UNIVERSITY OF MICHIGAN", organization_org_name) ~ "UNIVERSITY OF MICHIGAN",
    grepl("UNIVERSITY OF MISSOURI", organization_org_name) ~ "UNIVERSITY OF MISSOURI",
    grepl("UNIVERSITY OF MISSISSIPPI", organization_org_name) ~ "UNIVERSITY OF MISSISSIPPI",
    grepl("UNIVERSITY OF NEBRASKA", organization_org_name) ~ "UNIVERSITY OF NEBRASKA",
    grepl("UNIVERSITY OF NEVADA", organization_org_name) ~ "UNIVERSITY OF NEVADA",
    grepl("UNIVERSITY OF NEW MEXICO", organization_org_name) ~ "UNIVERSITY OF NEW MEXICO",
    grepl("UNIVERSITY OF NORTH TEXAS", organization_org_name) ~ "UNIVERSITY OF NORTH TEXAS",
    grepl("UNIVERSITY OF OAKLAHOMA", organization_org_name) ~ "UNIVERSITY OF OAKLAHOMA",
    grepl("UNIVERSITY OF PUERTO RICO", organization_org_name) ~ "UNIVERSITY OF PUERTO RICO",
    grepl("UNIVERSITY OF SOUTH CAROLINA", organization_org_name) ~ "UNIVERSITY OF SOUTH CAROLINA",
    grepl("UNIVERSITY OF TENNESSEE", organization_org_name) ~ "UNIVERSITY OF TENNESSEE",
    grepl("UNIVERSITY OF TEXAS|UT SOUTHWESTERN MEDICAL CENTER", organization_org_name) ~ "UNIVERSITY OF TEXAS SYSTEM",
    grepl("UNIVERSITY OF TOLEDO", organization_org_name) ~ "UNIVERSITY OF TOLEDO",
    grepl("UNIV OF WISCONSIN|UNIVERSITY OF WISCONSIN", organization_org_name) ~ "UNIVERSITY OF WISCONSIN",
    grepl("VANDERBILT UNIV", organization_org_name) ~ "VANDERBILT UNIVERSITY",
    grepl("WAKE FOREST UNIV", organization_org_name) ~ "WAKE FOREST UNIVERSITY",
    grepl("LUNDQUIST INSTITUTE.+", organization_org_name) ~ "LUNDQUIST INSTITUTE",
    grepl("UTAH STATE HIGHER.+UTAH", organization_org_name) ~ "UNIVERSITY OF UTAH",
    grepl("HENRY FORD HEALTH.+", organization_org_name) ~ "HENRY FORD HEALTH",
    grepl("REHABILITATION INSTITUTE OF CHICAGO.+", organization_org_name) ~ "REHABILITATION INSTITUTE OF CHICAGO",
    grepl("NEW YORK STATE PSYCHIATRIC INSTITUTE.+", organization_org_name) ~ "NEW YORK STATE PSYCHIATRIC INSTITUTE",
    TRUE ~ organization_org_name
  ),
  org_name_clean = str_to_title(org_name_clean),
  org_name_clean = gsub(" Of ", " of ", org_name_clean),
  org_name_clean = gsub(" At ", " at ", org_name_clean),
  org_name_clean = gsub(" And ", " and ", org_name_clean),
  organization_org_state = replace_na(organization_org_state, "ZZ"),
  funding_mechanism = gsub("Non-SBIR/STTR", "Research Projects", funding_mechanism),
  competing = ifelse(grepl("1|2|C|9", award_type), "Yes", "No"),
  nofo_type = gsub("-.+", "", opportunity_number),
  nofo_type = replace_na(nofo_type, "OTH")
  )
#grants$nofo_type[is.na(grants$nofo_type)] <- "OTH"

zips <- read.csv("projects/reference data/2021_gaz_zcta_national.txt", sep = "\t", colClasses = "character")
grants$org_zip <- as.character(grants$organization_org_zipcode)
grants$org_zip[which(nchar(grants$org_zip) == 8)] <- gsub("(^\\d)", "0\\1", grants$org_zip[which(nchar(grants$org_zip) == 8)])
grants$org_zip <- strtrim(grants$org_zip, 5)
grants <- grants %>% 
  left_join(zips, by = c("org_zip" = "GEOID"))


## step 3: add primary theme and crosscutting theme labels
tterms <- read.csv("projects/reference data/nichd_theme_terms2.csv", stringsAsFactors = FALSE, allowEscapes = TRUE)
source("r/functions/vbic_classify.r")
grants <- grants %>% 
  mutate(doc_text = paste(project_title, abstract_text, phr_text, project_title, project_title, sep = ". "))
vbic_theme <- vbic_classify(grants, tterms[grepl("theme|ncmrr", tterms$concept),], concept_threshold = 50, doc_id_column = "appl_id", doc_text_column = "doc_text")
theme_out <- vbic_theme$concept_list %>% 
  select(doc_id, primary_concept, all_concepts) %>% 
  rename(appl_id = doc_id, primary_theme = primary_concept, all_themes = all_concepts)
vbic_crosscut <- vbic_classify(grants, tterms[grepl("theme|ncmrr", tterms$concept) == FALSE,], concept_threshold = 25, unclassified = TRUE, doc_id_column = "appl_id", doc_text_column = "doc_text")
crosscut_out <- vbic_crosscut$concept_list %>% 
  select(doc_id, primary_concept, all_concepts) %>% 
  rename(appl_id = doc_id, primary_crosscut = primary_concept, all_crosscuts = all_concepts)
grants <- grants %>% 
  left_join(theme_out, by = "appl_id") %>% 
  left_join(crosscut_out, by = "appl_id")
#vbic_crosscut$concept_list %>% separate_rows(all_concepts, sep = ";") %>% count(all_concepts)

## step 4: create and save the summary tables
sum_state <- grants %>% 
  group_by(organization_org_state) %>% 
  summarise(
    Awards = length(appl_id),
    Funding = sum(award_amount, na.rm = TRUE) / 1000000,
    funded_orgs = length(unique(organization_org_name)),
    .groups = "drop"
  )
invest_state <- grants %>% 
  select(organization_org_state, pi_profile_ids) %>% 
  separate_rows(pi_profile_ids, sep = ";") %>% 
  unique() %>% 
  count(organization_org_state) %>% 
  rename(funded_invest = n)
sum_state <- sum_state %>% 
  left_join(invest_state, by = "organization_org_state") %>% 
  mutate(run_date = as.character(Sys.Date()))
write.csv(sum_state, file = "shiny dashboards/hd_reporter_states/data/state_summary.csv", row.names = FALSE)

mech_state <- grants %>% 
  group_by(funding_mechanism, organization_org_state) %>% 
  summarise(
    Awards = length(appl_id),
    Funding = sum(award_amount, na.rm = TRUE) / 1000000,
    .groups = "drop"
  ) 
mech_state <- mech_state %>% 
  pivot_longer(c(Awards, Funding), names_to = "measure", values_to = "amount")
write.csv(mech_state, file = "shiny dashboards/hd_reporter_states/data/mech_data.csv", row.names = FALSE)

awardType_state <- grants %>% 
  group_by(award_type_desc, organization_org_state) %>% 
  summarise(
    Awards = length(appl_id),
    Funding = sum(award_amount, na.rm = TRUE) / 1000000,
    .groups = "drop"
  ) 
awardType_state <- awardType_state %>% 
  pivot_longer(c(Awards, Funding), names_to = "measure", values_to = "amount")
write.csv(awardType_state, file = "shiny dashboards/hd_reporter_states/data/awardType_data.csv", row.names = FALSE)

nofo_state <- grants %>% 
  group_by(nofo_type, organization_org_state) %>% 
  summarise(
    Awards = length(appl_id),
    Funding = sum(award_amount, na.rm = TRUE) / 1000000,
    .groups = "drop"
  ) 
nofo_state <- nofo_state %>% pivot_longer(c(Awards, Funding), names_to = "measure", values_to = "amount")
write.csv(nofo_state, file = "shiny dashboards/hd_reporter_states/data/nofo_data.csv", row.names = FALSE)

theme_labs <- read.csv("projects/reference data/nichd_theme_labels.csv", stringsAsFactors = FALSE)
theme_state <- grants %>% 
  separate_rows(all_themes, sep = ";") %>% 
  group_by(all_themes, organization_org_state) %>% 
  summarise(
    Awards = length(appl_id),
    Funding = sum(award_amount, na.rm = TRUE) / 1000000,
    .groups = "drop"
  ) 
theme_dat <- theme_state %>% 
  pivot_longer(c(Awards, Funding), names_to = "measure", values_to = "amount") %>% 
  left_join(theme_labs[,c(1,3)], by = c("all_themes" = "all_concepts"))
write.csv(theme_dat, file = "shiny dashboards/hd_reporter_states/data/theme_data.csv", row.names = FALSE)

crosscut_state <- grants %>% 
  separate_rows(all_crosscuts, sep = ";") %>% 
  group_by(all_crosscuts, organization_org_state) %>% 
  summarise(
    Awards = length(appl_id),
    Funding = sum(award_amount, na.rm = TRUE) / 1000000,
    .groups = "drop"
  ) 
crosscut_dat <- crosscut_state %>% 
  pivot_longer(c(Awards, Funding), names_to = "measure", values_to = "amount") %>% 
  left_join(theme_labs[,c(1,3)], by = c("all_crosscuts" = "all_concepts")) %>% 
  filter(short_label != "Unclassified")
write.csv(crosscut_dat, file = "shiny dashboards/hd_reporter_states/data/crosscut_data.csv", row.names = FALSE)

po_lookup <- read.csv("projects/dashboard reporter shiny/po_lookup.csv", stringsAsFactors = FALSE)
branch_theme_state <- grants %>% 
  left_join(po_lookup, by = "po_full_names") %>% 
  separate_rows(all_themes, sep = ";") %>% 
  mutate(major_topic = replace_na(major_topic, "Unknown")) %>% 
  group_by(all_themes, major_topic, organization_org_state) %>% 
  summarise(
    Awards = n(),
    Funding = sum(award_amount, na.rm = TRUE) / 1000000,
    .groups = "drop"
  ) %>% 
  filter(all_themes != "unclassified", major_topic != "Unknown") %>% 
  left_join(theme_labs[,c(1,3)], by = c("all_themes" = "all_concepts")) %>% 
  pivot_longer(c(Awards, Funding), names_to = "measure", values_to = "amount")
write.csv(branch_theme_state, file = "shiny dashboards/hd_reporter_states/data/branch_theme_data.csv", row.names = FALSE)

orgType_state <- grants %>% 
  group_by(organization_type_code_desc, organization_org_state) %>% 
  summarise(
    Awards = length(appl_id),
    Funding = sum(award_amount, na.rm = TRUE) / 1000000,
    .groups = "drop"
  )  %>% 
  pivot_longer(c(Awards, Funding), names_to = "measure", values_to = "amount")
write.csv(orgType_state, file = "shiny dashboards/hd_reporter_states/data/orgType_data.csv", row.names = FALSE)

org_sum <- grants %>% 
  group_by(org_name_clean, org_zip, organization_org_state, INTPTLAT, INTPTLONG) %>% 
  summarise(
    Awards = n(),
    Funding = sum(award_amount, na.rm = TRUE) / 1000000,
    .groups = "drop"
  ) %>% 
  filter(is.na(org_name_clean) == FALSE, is.na(org_zip) == FALSE) %>% 
  mutate(across(c(INTPTLAT, INTPTLONG), as.numeric)) %>% 
  pivot_longer(c(Awards, Funding), names_to = "measure", values_to = "amount")
write.csv(org_sum, file = "shiny dashboards/hd_reporter_states/data/org_data.csv", row.names = FALSE)

#us_base <- rnaturalearth::ne_states("united states of america", returnclass = "sf")
#saveRDS(us_base, file = "projects/dashboard reporter shiny/us_base_sf_data.rds")
#sf::write_sf(us_base, "shiny dashboards/hd_reporter_states/data/us_base_data.shp")


tagged_data <- grants %>% 
  select(appl_id, core_project_num, project_num, funding_mechanism, fiscal_year, project_title, org_name_clean, organization_org_state, all_themes, all_crosscuts)
write.csv(tagged_data, file = "shiny dashboards/hd_reporter_states/data/grant_data_tagged.csv", row.names = FALSE)

## check to see if any new PO names have showed up and need to be added to the po_lookup table
po_check <- grants %>% 
  select(po_full_names) %>% 
  unique() %>% 
  arrange(po_full_names) %>% 
  mutate(is_new = po_full_names %in% po_lookup$po_full_names == FALSE)
po_check %>% filter(is_new == TRUE)

rsconnect::writeManifest(appDir = "shiny dashboards/hd_reporter_states/")

#---
po_lookup <- read.csv("projects/dashboard reporter shiny/po_lookup.csv", stringsAsFactors = FALSE)
branch_theme <- grants %>% 
  left_join(po_lookup, by = "po_full_names") %>% 
  separate_rows(all_themes, sep = ";") %>% 
  mutate(major_topic = replace_na(major_topic, "Unknown")) %>% 
  group_by(all_themes, major_topic) %>% 
  summarise(
    Awards = n(),
    Funding = sum(award_amount, na.rm = TRUE) / 1000000,
    .groups = "drop"
  ) %>% 
  filter(all_themes != "unclassified", major_topic != "Unknown") %>% 
  left_join(theme_labs[,c(1,3)], by = c("all_themes" = "all_concepts"))

p1 <- ggplot(branch_theme, aes(short_label, major_topic, fill = Funding))
p1 + geom_tile() + scale_fill_viridis_c() + scale_x_discrete(position = "top", guide = guide_axis(n.dodge = 2)) + 
  labs(x = "Strategic Plan Theme", y = "NICHD Branch", fill = "Amount") + 
  theme_gray(base_size = 16)

po_sum <- grants %>% 
  count(po_full_names)
write.csv(po_sum, file = "projects/dashboard reporter shiny/po_lookup.csv", row.names = FALSE)

theme_labs <- read.csv("projects/reference data/nichd_theme_labels.csv", stringsAsFactors = FALSE)
theme_state_mech <- grants %>% 
  separate_rows(all_themes, sep = ";") %>% 
  group_by(all_themes, organization_org_state, funding_mechanism) %>% 
  summarise(
    Awards = length(appl_id),
    Funding = sum(award_amount, na.rm = TRUE) / 1000000,
    .groups = "drop"
  ) 
theme_mech_dat <- theme_state_mech %>% 
  pivot_longer(c(Awards, Funding), names_to = "measure", values_to = "amount") %>% 
  left_join(theme_labs[,c(1,3)], by = c("all_themes" = "all_concepts"))
write.csv(theme_dat, file = "projects/dashboard reporter shiny/theme_data2.csv", row.names = FALSE)

tmp <- theme_mech_dat %>% 
  filter(measure == "Funding") %>% 
  group_by(short_label, funding_mechanism) %>% 
  summarise(
    amount = sum(amount),
    .groups = "drop"
  )
p1 <- ggplot(tmp, aes(funding_mechanism, short_label, fill = amount))
p1 + geom_tile() + scale_fill_viridis_c()
sum(is.na(grants$spending_categories_desc)) ## 2904 / 3506
View(vbic_theme$term_vocab)
vbic_theme$concept_list %>% separate_rows(all_concepts, sep = ";") %>% count(all_concepts)

the_terms <- vbic_theme$term_vocab %>% 
  slice_max(term_count, n = 100) %>% 
  mutate(disp_term = gsub("\\[.+\\]", "", term), 
         disp_term = gsub("\\b", "", disp_term, fixed = TRUE),
         disp_term = gsub("s*", "", disp_term, fixed = TRUE),
         disp_term = gsub("*", "", disp_term, fixed = TRUE)
         )
