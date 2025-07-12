library(haven)
library(dplyr)
library(writexl)

# Step 1: Load data
setwd("D:/Video/NSS77_33pt1")

# Load data files
household_data <- read_dta("Visit1  Level - 01 (Block 1) -identification of sample household.dta")
insurance_data <- read_dta("Visit 1 Level 18 (Block 16) particulars of other aspects of farming during the period July - December 2018.dta")
cattle_output <- read_dta("Visit 1 Level 11 (Block 9) disposal of produce and value of output on farming of animals during last 30 days.dta")
livestock <- read_dta("Visit 1 Level 9 (Block 8) livestock, poultry, duckery, etc. owned by the household as on the date of survey.dta")
loans <- read_dta("Visit 1 Level 14 (Block 12) purchase and sale of productive assets during January - June 2019.dta")
household_a <- read_dta("Visit1  Level - 02 (Block 3) - demographic and other particulars of household members.dta")
household_b <- read_dta("Visit1  Level - 03 (Block 4) - demographic and other particulars of household members  .dta")
crop_output <- read_dta("Visit 1 Level - 06 (Block 6) output of crops produced during the period July - December 2018.dta")
technical_access <- read_dta("Visit 1 Level 17 (Block 15) access to technical advice related to the agricultural activity undertook by the household during the period July - December 2018.dta")

# Step 2: Process cattle data 
cattle_data <- livestock %>%
  mutate(across(c(b8q1, b8q4, b8q5), ~as.numeric(as.character(.))))

cattle_summary <- cattle_data %>%
  filter(b8q1 %in% c(1, 2, 3)) %>%
  group_by(HHID, State, District) %>%
  summarise(
    total_female_cattle = sum(b8q4, na.rm = TRUE),  
    total_cattle = sum(b8q5, na.rm = TRUE),         
    milk_producing_cattle = sum(if_else(b8q1 == 1, b8q5, 0), na.rm = TRUE),  
    weight = first(MLT/100)  
  ) %>%
  ungroup()

# Step 3: Process milk production 
cattle_output <- cattle_output %>%
  mutate(across(c(b9q1, b9q14), ~as.numeric(as.character(.))))

output_cattle <- cattle_output %>%
  group_by(HHID, State, District) %>%
  summarise(
    total_milk_produce = sum(if_else(b9q1 == 1, b9q14, 0), na.rm = TRUE), 
    total_cattle_livestock_produce = sum(if_else(b9q1 == 7, b9q14, 0), na.rm = TRUE),  
    weight = first(MLT/100)  
  ) %>%
  mutate(total_cattle_produce = total_milk_produce + total_cattle_livestock_produce) %>%
  ungroup()


# Step 4: Household income and average per member
household_income <- household_a %>%
  group_by(HHID) %>%
  summarise(
    total_income = sum(b3q12, b3q13, b3q14, na.rm = TRUE),
    num_members = n(),
    avg_income_per_member = total_income / num_members,
    weight = first(MLT/100)
    
  )

# Step 5: Technical access
technical_access <- technical_access %>%
  mutate(across(c(b15q1, b15q3), ~as.numeric(as.character(.))))

vetinery_service <- technical_access %>%
  group_by(HHID, State, District) %>%
  summarise(
    accessed_veterinary = if_else(any(b15q1 == 7 & b15q3 == 1), 1, 2),
    accessed_cooperative = if_else(any(b15q1 == 8 & b15q3 == 1), 1, 2),
    weight = first(MLT/100)  
  ) %>%
  mutate(accessed_any_service = if_else(accessed_veterinary == 1 | accessed_cooperative == 1, 1, 2)) %>%
  ungroup()

# Step 6: Land holding 
crop_output <- crop_output %>%
  mutate(across(c(b6q1, b6q4, b6q6), ~as.numeric(as.character(.))))

landholding <- crop_output %>%
  filter(b6q1 == 9) %>%
  group_by(HHID, State, District) %>%
  summarise(
    total_irrigated = sum(b6q4, na.rm = TRUE),  
    total_unirrigated = sum(b6q6, na.rm = TRUE),  
    weight = first(MLT/100) 
  ) %>%
  mutate(total_land = total_irrigated + total_unirrigated) %>%
  ungroup()

# Step 7: Merge all data
merged_data <- household_b %>%
  mutate(across(c(b4q1, b4q2, b4q3, b4q8, b4q5, b4q10, b4q11, b4q12, b4q14, b4q15, b4q13, b4q17, b4q18, b4q20, b4q21), 
                ~as.numeric(as.character(.)))) %>%
  left_join(output_cattle, by = c("HHID", "State", "District")) %>%
  left_join(cattle_summary, by = c("HHID", "State", "District")) %>%
  left_join(household_income, by = "HHID") %>%
  left_join(vetinery_service, by = c("HHID", "State", "District")) %>%
  left_join(landholding, by = c("HHID", "State", "District"))

# Clean up columns and rename
merged_data <- merged_data %>%
  select(-ends_with(".x"), -ends_with(".y")) %>%
  rename(
    household_size = b4q1,
    religion = b4q2,
    social_group = b4q3,
    monthly_expenditure = b4q8,
    monthly_purchases_for_consumption = b4q5,
    value_of_yearly_agriculture_produce = b4q10,
    dwelling_status = b4q11,
    structure_type = b4q12,
    MGNREG_jobcard = b4q14,
    MGNREG_employment = b4q15,
    bank_account = b4q13,
    kcc = b4q17,
    soil_health_card = b4q18,
    animal_health_card = b4q20,
    PMFBY = b4q21
  )



# step 7- verifying data, after applying weights
livestock_stats <- merged_data %>%
  summarise(
    # Percentage of households with milk cattle
    total_households = n(),
    households_with_cattle = sum(milk_producing_cattle > 0, na.rm = TRUE),
    total_weight = sum(weight, na.rm = TRUE),
    weighted_hh_with_cattle = sum((milk_producing_cattle > 0) * weight, na.rm = TRUE),
    
    # Calculate the statistics
    pct_households_with_cattle = 100 * weighted_hh_with_cattle / total_weight,
    avg_cattle_per_100_hh = 100 * sum(milk_producing_cattle * weight, na.rm = TRUE) / total_weight
  )

# Create comparison table to campare our data with the official nsso report
comparison_table <- data.frame(
  Metric = c("% Households with milk cattle", "Avg milk cattle per 100 HH"),
  Calculated_Value = c(livestock_stats$pct_households_with_cattle, 
                       livestock_stats$avg_cattle_per_100_hh),
  NSS_Report_Value = c(16.4, 21.9)
)


print("Comparison with Statement 3.1 (All category, milk cattle only):")
print(comparison_table)

# Print additional verification info
print("\nDetailed Statistics:")
print(livestock_stats)

# Calculate livestock statistics from merged data
livestock_stats <- merged_data %>%
  summarise(
    # Total weighted households
    total_weighted_hh = sum(weight, na.rm = TRUE),
    
    # Percentage of households with milk cattle
    pct_with_milk_cattle = 100 * sum((milk_producing_cattle > 0) * weight, na.rm = TRUE) / 
      sum(weight, na.rm = TRUE),
    
    # Average number of milk cattle per 100 households
    avg_milk_cattle_per_100hh = 100 * sum(milk_producing_cattle * weight, na.rm = TRUE) / 
      sum(weight, na.rm = TRUE)
  )

# Create comparison table
comparison_table <- data.frame(
  Metric = c("% Households with milk cattle", "Avg milk cattle per 100 HH"),
  Calculated_Value = c(livestock_stats$pct_with_milk_cattle, 
                       livestock_stats$avg_milk_cattle_per_100hh),
  NSS_Report_Value = c(16.4, 21.9)
)


print("\nComparison with Statement 3.1 (All category, milk cattle only):")
print(comparison_table)


print("\nWeighted statistics details:")
print(livestock_stats) 


# Calculate state-wise income comparison
state_income_stats <- merged_data %>%
  filter(!is.na(weight)) %>%
  group_by(State) %>%
  summarise(
    n_households = n(),
    n_with_income = sum(!is.na(total_income)),
    n_with_weight = n(),  # This will be same as n_households since we filtered
    total_income = weighted.mean(total_income, weight, na.rm = TRUE),
    raw_mean_income = mean(total_income, na.rm = TRUE)
  ) %>%
  ungroup()

#data from the report for comparison
report_income_values <- data.frame(
  State = c(1:36),  # State codes
  Report_Income = c(
    16990,  # 01 JAMMU & KASHMIR
    9955,   # 02 HIMACHAL PRADESH
    21705,  # 03 PUNJAB
    16734,  # 04 CHANDIGARH (using Group of UTs value)
    9778,   # 05 UTTARAKHAND
    16530,  # 06 HARYANA
    16734,  # 07 DELHI (using Group of UTs value)
    9156,   # 08 RAJASTHAN
    6130,   # 09 UTTAR PRADESH
    6278,   # 10 BIHAR
    11848,  # 11 SIKKIM
    17524,  # 12 ARUNACHAL PRADESH
    8907,   # 13 NAGALAND
    10327,  # 14 MANIPUR
    13586,  # 15 MIZORAM
    8982,   # 16 TRIPURA
    26973,  # 17 MEGHALAYA
    9557,   # 18 ASSAM
    6084,   # 19 WEST BENGAL
    3993,   # 20 JHARKHAND
    4013,   # 21 ODISHA
    8588,   # 22 CHHATTISGARH
    6756,   # 23 MADHYA PRADESH
    10386,  # 24 GUJARAT
    16734,  # 25 DAMAN & DIU (using Group of UTs value)
    16734,  # 26 D & N HAVELI (using Group of UTs value)
    9592,   # 27 MAHARASHTRA
    8768,   # 28 ANDHRA PRADESH
    11195,  # 29 KARNATAKA
    16734,  # 30 GOA (using Group of UTs value)
    16734,  # 31 LAKSHADWEEP (using Group of UTs value)
    16022,  # 32 KERALA
    10448,  # 33 TAMIL NADU
    16734,  # 34 PUDUCHERRY (using Group of UTs value)
    16734,  # 35 A & N ISLANDS (using Group of UTs value)
    8292    # 36 TELANGANA
  )
)

# Merge calculated values with report values
income_comparison <- state_income_stats %>%
  mutate(State = as.numeric(State)) %>%
  left_join(report_income_values, by = "State") 

print("\nState-wise Income Comparison (Calculated vs Report Values):")
print(income_comparison %>%
        arrange(State) %>%
        mutate(
          total_income = round(total_income, 2),
        ))

# Calculate all-India average 
all_india_income <- merged_data %>%
  filter(!is.na(weight)) %>% 
  summarise(
    n_total = n(),
    n_with_income = sum(!is.na(total_income)),
    all_india_avg = weighted.mean(total_income, weight, na.rm = TRUE)
  )
# Filter only households with total cattle produce > 0
estimation_data <- merged_data %>%
  filter(!is.na(total_cattle_produce) & total_cattle_produce != 0)

# filter for primarily self-employed in agriculture (AFTER merging)
estimation_data <- estimation_data %>%
  filter(b4q4 == 1)



# Create a crop insurance (PMFBY) indicator:
estimation_data$pmfby_ind <- ifelse(estimation_data$PMFBY == "1", 1, 0)

#interaction model

estimation_data <- estimation_data %>%
  drop_na(
    total_income,
    total_cattle,
    pmfby_ind,
    total_land,
  )
estimation_data$State <- as.numeric(estimation_data$State)
estimation_data$State <- factor(estimation_data$State)




estimation_data$State <- relevel(estimation_data$State, ref = "21")

# Model 1: Interaction Regression Model
interaction_model <- lm(total_income ~ total_cattle + pmfby_ind + total_cattle:pmfby_ind + State, data = estimation_data)
summary(interaction_model)

# Model 2: Sample Splitting Approach .

# Subset the data for insured (pmfby_ind == 1) and uninsured (pmfby_ind == 0) farmers
data_insured <- subset(estimation_data, pmfby_ind == 1)
data_uninsured <- subset(estimation_data, pmfby_ind == 0)

# Regression for insured farmers (pmfby_ind == 1)
model_insured <- lm(total_income ~ total_cattle+State, data = data_insured)
summary(model_insured)

# Regression for uninsured farmers (pmfby_ind == 0)
model_uninsured <- lm(total_income ~ total_cattle + State, data = data_uninsured)
summary(model_uninsured)




# Regression
reg_data <- estimation_data[, c("total_income","total_cattle", "State",
                                "bank_account", "PMFBY","accessed_veterinary",    
                                "MGNREG_employment", "total_land")]

# Convert binary columns to factors: 1 = Yes, 2 = No

reg_data$bank_account <- factor(reg_data$bank_account, levels = c(1, 2), labels = c("Yes", "No"))
reg_data$MGNREG_employment <- factor(reg_data$MGNREG_employment, levels = c(1, 2), labels = c("Yes", "No"))
reg_data$PMFBY <- factor(reg_data$PMFBY, levels = c(1, 2), labels = c("Yes", "No"))
reg_data$accessed_veterinary<- factor(reg_data$accessed_veterinary, levels = c(1,2), labels = c("Yes","No"))
# Remove rows with missing values
reg_data <- na.omit(reg_data)
reg_data <- reg_data[reg_data$total_income > 0, ]

reg_model <- lm(total_income ~ total_cattle + bank_account + PMFBY + 
                  accessed_veterinary + MGNREG_employment + total_land, 
                data = reg_data)

summary(reg_model)

# Create mapping of state codes to names
state_map <- data.frame(
  Code = sprintf("%02d", 1:36),
  State_Name = c(
    "JAMMU & KASHMIR", "HIMACHAL PRADESH", "PUNJAB", "CHANDIGARH", "UTTARAKHAND", 
    "HARYANA", "DELHI", "RAJASTHAN", "UTTAR PRADESH", "BIHAR", 
    "SIKKIM", "ARUNACHAL PRADESH", "NAGALAND", "MANIPUR", "MIZORAM", 
    "TRIPURA", "MEGHALAYA", "ASSAM", "WEST BENGAL", "JHARKHAND", 
    "ODISHA", "CHHATTISGARH", "MADHYA PRADESH", "GUJARAT", "DAMAN & DIU", 
    "D & N HAVELI", "MAHARASHTRA", "ANDHRA PRADESH", "KARNATAKA", "GOA", 
    "LAKSHADWEEP", "KERALA", "TAMIL NADU", "PUDUCHERRY", "A & N ISLANDS", 
    "TELANGANA"
  ),
  stringsAsFactors = FALSE
)


# Select relevant columns including 'state'
reg_data_ <- estimation_data[, c("total_income", "total_cattle", 
                                 "bank_account", "PMFBY", "accessed_veterinary",    
                                 "MGNREG_employment", "total_land", "State")]

# Remove rows with missing values
reg_data_ <- na.omit(reg_data_)
reg_data_ <- reg_data_[reg_data_$total_income > 0, ]
reg_data_$State <- as.numeric(reg_data_$State)
reg_data_$State <- factor(reg_data_$State)


# Convert binary columns to factors, and State = 21 as baseline
reg_data_$bank_account <- factor(reg_data_$bank_account, levels = c(1, 2), labels = c("Yes", "No"))
reg_data_$MGNREG_employment <- factor(reg_data_$MGNREG_employment, levels = c(1, 2), labels = c("Yes", "No"))
reg_data_$PMFBY <- factor(reg_data_$PMFBY, levels = c(1, 2), labels = c("Yes", "No"))
reg_data_$accessed_veterinary <- factor(reg_data_$accessed_veterinary, levels = c(1, 2), labels = c("Yes", "No"))


reg_data_$State <- relevel(reg_data_$State, ref = "21")


# Run log-linear regression with state fixed effects
model_fe <- lm(total_income ~ total_cattle + bank_account + PMFBY + 
                 accessed_veterinary + MGNREG_employment + total_land + State, 
               data = reg_data_)

# View model summary
summary(model_fe)

# Recode state column to two-digit character to match mapping
reg_data_$State_Code <- sprintf("%02d", as.numeric(as.character(reg_data_$State)))

# Compute stats by state code
state_summary <- reg_data_ %>%
  group_by(State_Code) %>%
  summarise(
    avg_cattle = mean(total_cattle, na.rm = TRUE),
    vet_access_pct = mean(accessed_veterinary == "Yes") * 100,
    avg_land = mean(total_land, na.rm = TRUE),
    avg_income = mean(total_income, na.rm = TRUE),
  )

# Join with state name
state_stats_named <- left_join(state_summary, state_map, by = c("State_Code" = "Code")) %>%
  select(State_Name, everything()) %>%
  arrange(State_Code)

# results
print(state_stats_named)



