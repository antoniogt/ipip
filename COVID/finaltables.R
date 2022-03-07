# Make the tables

## Read the data
tablafeno = read.table("Fenotipos&Patologias_Tabla.tsv", sep = "\t")


our_summary1 <-
  list("Characteristics" =
         list("Age median (Q3-Q1)"       = ~ qwraps2::median_iqr(Age)),
       "Gender" =
         list("Male"       = ~ qwraps2::n_perc(Gender == "Male"),
              "Female"    = ~ qwraps2::n_perc(Gender == "Female")),
       "Chronic diseases" =
         list("Number of Chronic diseases median (Q3-Q1)"       = ~ qwraps2::median_iqr(NPC),
              "Number of systems affected median (Q3-Q1)"       = ~ qwraps2::median_iqr(NSIST),
              "Asthma"       = ~ qwraps2::n_perc(Asthma == 1),
              "Obisity"       = ~ qwraps2::n_perc(Obesity == 1),
              "Diabetes mellitus"       = ~ qwraps2::n_perc(DM == 1),
              "Heart failure"       = ~ qwraps2::n_perc(IC == 1),
              "COPD"       = ~ qwraps2::n_perc(EPOC == 1),
              "Arterial hypertension"       = ~ qwraps2::n_perc(HTA == 1),
              "Depression"       = ~ qwraps2::n_perc(DEP == 1),
              "VIH"       = ~ qwraps2::n_perc(VIH == 1),
              "Ischemic cardiomyopathy"       = ~ qwraps2::n_perc(CI == 1),
              "Stroke"       = ~ qwraps2::n_perc(ACV == 1),
              "Renal insufficiency"       = ~ qwraps2::n_perc(IRC == 1),
              "Cirrhosis"       = ~ qwraps2::n_perc(CIR == 1),
              "Osteoporosis"       = ~ qwraps2::n_perc(OST == 1),
              "Osteoarthritis"       = ~ qwraps2::n_perc(ARTROSIS == 1),
              "Arthritis"       = ~ qwraps2::n_perc(ARTRITIS == 1),
              "Dementia"       = ~ qwraps2::n_perc(DEM == 1),
              "Chronic pain"       = ~ qwraps2::n_perc(DC == 1)
         ),
       "Outcome" =
         list("Discharge" = ~ qwraps2::n_perc(Status == "Discharge"),
              "Expired"  = ~ qwraps2::n_perc(Status == "Expired"))
  )



by_hosp <- summary_table(dplyr::group_by(tablafeno, Hospitalized), our_summary1)
by_UCI <- summary_table(dplyr::group_by(tablafeno, UCI), our_summary1)

# By hosp
write.table(by_UCI, "by_UCI.tsv", sep = "\t")
write.table(by_hosp, "by_hosp.tsv", sep = "\t")

# By status
by_status <- summary_table(dplyr::group_by(tablafeno, Status), our_summary1)
by_status

write.table(by_status, "by_status.tsv", sep = "\t")