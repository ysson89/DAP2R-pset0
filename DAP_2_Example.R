library(tidyverse)
library(ggplot2)
library(sf)
library(spData)
library(usmap)

### Loading school-level data from the Department of Education's
### Common Core of Data (CCD)

#Loads school directory data
directory_data <- read_csv('Example/ccd_sch_029_2122_w_1a_071722.csv', col_types = cols()) %>%
  rename(state = ST,
         school_name = SCH_NAME,
         school_district_name = LEA_NAME,
         school = NCESSCH,
         school_district = LEAID,
         city = LCITY,
         zip = LZIP,
         school_status = SY_STATUS_TEXT,
         school_type = SCH_TYPE_TEXT,
         charter_school = CHARTER_TEXT,
         no_grades = NOGRADES,
         lowest_grade = GSLO,
         highest_grade = GSHI,
         school_level = LEVEL) %>%
  select(state, school, school_district, school, school_district,
         city, zip, school_status, school_type, charter_school, no_grades,
         lowest_grade, highest_grade, school_level) %>%
  #Only keeps schools with some grade levels
  filter(no_grades == "No") %>%
  #Only keeps schools that are currently operating
  filter(school_status %in% c('Open', 'New', 'Added', 'Reopened', 'Changed Boundary/Agency')) %>%
  #Only keeps schools offering K-12 grades
  filter(school_level != 'Adult Education' & school_level != 'Not applicable' & school_level != 'Ungraded')

#Loads school membership data
#Note that it is important to include the "na.rm = T" argument when summing observations
membership_data <- read_csv('Example/ccd_SCH_052_2122_l_1a_071722.csv', col_types = cols()) %>%
  rename(school = NCESSCH) %>%
  filter(GRADE == 'No Category Codes' & TOTAL_INDICATOR != 'Education Unit Total') %>%
  #Takes sums of male/female/White/Black/Hispanic/total students for each school
  group_by(school) %>%
  summarise(male_students = sum(STUDENT_COUNT[SEX == 'Male'], na.rm = T),
            female_students = sum(STUDENT_COUNT[SEX == 'Female'], na.rm = T),
            white_students = sum(STUDENT_COUNT[RACE_ETHNICITY == 'White'], na.rm = T),
            black_students = sum(STUDENT_COUNT[RACE_ETHNICITY == 'Black or African American'], na.rm = T),
            hispanic_students = sum(STUDENT_COUNT[RACE_ETHNICITY == 'Hispanic/Latino'], na.rm = T),
            total_students = sum(STUDENT_COUNT[TOTAL_INDICATOR == 'Derived - Education Unit Total minus Adult Education Count'], na.rm = T)) %>%
  mutate_at(vars(male_students, female_students, white_students, black_students, hispanic_students, total_students),  ~ if_else(is.na(.), 0, .))

#Loads staff data
staff_data <- read_csv('Example/ccd_sch_059_2122_l_1a_071722.csv', col_types = cols()) %>%
  rename(school = NCESSCH,
         num_teachers = TEACHERS) %>%
  select(school, num_teachers)

#Loads school characteristics data
school_characteristics_data <- read_csv('Example/ccd_sch_129_2122_w_1a_071722.csv', col_types = cols()) %>%
  rename(school = NCESSCH,
         virtual_status = VIRTUAL_TEXT,
         title_i_status = TITLEI_STATUS_TEXT,
         nslp_status = NSLP_STATUS_TEXT,
         magnet_school = MAGNET_TEXT) %>%
  select(school, virtual_status, title_i_status, nslp_status, magnet_school)

#Loads lunch program eligibility data
lunch_program_eligib_data <- read_csv('Example/ccd_sch_033_2122_l_1a_071722.csv', col_types = cols()) %>%
  rename(school = NCESSCH,
         lunch_program = LUNCH_PROGRAM,
         student_count = STUDENT_COUNT) %>%
  group_by(school) %>%
  filter(lunch_program %in% c('Reduced-price lunch qualified', 'Free lunch qualified')) %>%
  summarise(free_or_reduced_lunch_count = sum(student_count, na.rm = T))

#Merges datasets together
merged_data <- directory_data %>%
  left_join(membership_data, by = 'school') %>%
  left_join(staff_data, by = 'school') %>%
  left_join(school_characteristics_data, by = 'school') %>%
  left_join(lunch_program_eligib_data, by = 'school') %>%
  #Only keeps school if it has positive number of students
  filter(total_students > 0 & ! is.na(total_students)) %>%
  #Only keeps school if number of teachers is known and is non-zero
  filter(num_teachers > 0 & ! is.na(num_teachers)) %>%
  #Removes the bottom 0.1% and the top 0.1% of data based on number of students (clear outliers, often because of online instruction)
  filter(total_students > quantile(total_students, .001) &
           total_students < quantile(total_students, .999))


### Charts

#Chart 1: Stacked bar chart of percentages of schools/number of students of each type

#Aggregates data by type of school
school_totals <- merged_data %>%
  mutate(school_level = if_else(school_level %in% c('Secondary', 'Other'),
                                'Multiple \ngrade levels', school_level)) %>%
  group_by(school_level) %>%
  summarise(total_schools = n(),
            total_students = sum(total_students, na.rm = T)) %>%
  ungroup() %>%
  mutate(perc_schools = total_schools / sum(total_schools) * 100,
         perc_students = total_students / sum(total_students) * 100) %>%
  select(-c(total_schools, total_students)) %>%
  mutate(order = c(2, 4, 3, 5, 1)) %>%
  arrange(order) %>%
  select(-order) %>%
  pivot_longer(cols = c('perc_schools', 'perc_students'), names_to = 'category', values_to = 'vals') %>%
  mutate(category = if_else(category == 'perc_schools', 'Schools', 'Students'))

#Stacked bar chart
ggplot(data = school_totals, aes(width = .8)) +
  geom_bar(width = 1, stat = 'identity', aes(x = category, y = vals, fill = fct_inorder(school_level))) +
  theme_classic() +
  ylab('Share of total') +
  xlab('') +
  ggtitle("Percentage Breakdowns of Public Schools and Public School \nStudents, By Grade Level") +
  theme(legend.title=element_blank()) +
  theme(plot.caption = element_text(hjust = 0, vjust = 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), n.breaks = 6) +
  scale_x_discrete(expand = c(0.35, 0.35)) +
  labs(caption = 'Note: Enrollment data are missing for Nevada throughout this analysis. Analysis is done \nfor a random sample of 5,000 schools in the Common Core of Data.') +
  theme(plot.caption = element_text(hjust = 0))


#Chart 2: Histogram of public school enrollments
ggplot(data = merged_data) +
  geom_histogram(aes(x = total_students), binwidth = 100, fill = 'lightgrey', color = 'black') +
  xlab('Student enrollment') +
  ylab('Number of schools') +
  ggtitle('Distribution of Public School Enrollments') +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), n.breaks = 10) +
  geom_vline(xintercept = median(merged_data$total_students), color = 'red') +
  labs(caption = paste0('Note: The red line indicates the median student enrollment of ',
                        median(merged_data$total_students), '. Analysis is done \nfor a random sample of 5,000 schools in the Common Core of Data.')) +
  theme(plot.caption = element_text(hjust = 0))


#Chart 3: Bar chart of total number of students, by subgroup

#Calculates demographic data of students
student_totals <- merged_data %>%
  summarise(total = sum(total_students, na.rm = T),
            male = sum(male_students, na.rm = T),
            female = sum(female_students, na.rm = T),
            white = sum(white_students, na.rm = T),
            black = sum(black_students, na.rm = T),
            hispanic = sum(hispanic_students, na.rm = T),
            free_or_reduced_lunch = sum(free_or_reduced_lunch_count, na.rm = T)) %>%
  pivot_longer(cols = c('total', 'male', 'female', 'white', 'black', 'hispanic', 'free_or_reduced_lunch'), names_to = 'chart_item', values_to = 'value') %>%
  mutate(chart_item = if_else(chart_item == 'free_or_reduced_lunch', 'free or \nreduced \nPrice lunch', chart_item)) %>%
  mutate(chart_item = gsub('Or', 'or', str_to_title(chart_item))) %>%
  mutate(category = if_else(chart_item %in% c('Male', 'Female'), 'sex',
                                         if_else(chart_item %in% c('White', 'Black', 'Hispanic'), 'race/ethnicity', 'income'))) %>%
  #Flips order of rows, since a later command (fct_inorder) otherwise puts them in wrong order
  arrange(desc(row_number())) %>%
  #Calculates percentages of total enrollment
  mutate(perc = value / last(value) * 100) %>%
  filter(chart_item != 'Total')

#Bar chart, color coded by type of data (sex, race/ethnicity, or income)
ggplot(data = student_totals) +
  geom_col(aes(perc, fct_inorder(chart_item), group = category, fill = category)) +
  theme_classic() +
  xlab('Percent of total students') +
  ylab('') +
  ggtitle("Decomposition of Public School Enrollment") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  guides(fill = guide_legend(reverse=T)) +
  theme(legend.title=element_blank()) +
  labs(caption = paste0('Note: Analysis is done for a random sample of 5,000 schools in the Common \nCore of Data.')) +
  theme(plot.caption = element_text(hjust = 0))


#Chart 4: Pie chart of total number of students, by type of school

#Aggregates data by type of school
student_totals2 <- merged_data %>%
  mutate(type = if_else(charter_school == 'Yes', 'Charter School',
                        if_else(magnet_school == 'Yes', 'Magnet School',
                                if_else(school_type == 'Regular School', 'Regular School', 'Other')))) %>%
  group_by(type) %>%
  summarise(total = sum(total_students, na.rm = T)) %>%
  mutate(perc = total / sum(total) * 100) %>%
  mutate(perc_text = paste0(round(perc, 2), '%')) %>%
  mutate(order = c(2, 3, 4, 1)) %>%
  arrange(order) %>%
  select(-order)

#Pie chart (which in ggplot2 is bar chart + polar coordinates)
ggplot(data = student_totals2) +
  geom_bar(width = 1, stat = 'identity', aes(x = '', y = perc, fill = fct_inorder(type))) +
  geom_text(aes(x = '', y = perc, label = perc_text), position=position_dodge(width=0.9), vjust= c(0, -1, -2.5, -4), hjust = c(1, -0.2, 0.4, 0.5)) +
  coord_polar("y", start=0) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  theme_classic() +
  ylab('') +
  xlab('') +
  ggtitle("Public School Enrollment Decomposed by School Type") +
  theme(legend.title=element_blank()) +
  theme(line = element_blank(),
        axis.text.x=element_blank()) +
  labs(caption = 'Note: Percentages are reported in terms of the total number of public school students. Analysis \nis done for a random sample of 5,000 schools in the Common Core of Data.') +
  theme(plot.caption = element_text(hjust = 0, vjust = 15))


#Chart 5: Map of charter school enrollment as percent of total public school enrollment

#Creates data frame of state abbreviations and state names
#Excludes Washington DC because the us_states dataset in spData also excludes it
state_names <- data.frame(state = state.abb,
                          NAME = state.name)

#Calculates state level values
state_totals <- merged_data %>%
  group_by(state) %>%
  summarise(total = sum(total_students, na.rm = T),
            charter = sum(total_students[charter_school == 'Yes'], na.rm = T)) %>%
  mutate(perc_charter = charter / total * 100) %>%
  #Merges with full state names
  left_join(., state_names, by = 'state') %>%
  drop_na(NAME)

state_totals <- state_totals %>%
  mutate(perc_bin = if_else(perc_charter == 0, '0%',
                            if_else(perc_charter <= 5, '0 - 5%',
                                    if_else(perc_charter <= 10, '5 - 10%',
                                            paste0('10 - ', ceiling(max(state_totals$perc_charter)/5)*5, '%'))))) %>%
  arrange(perc_charter)

#Turns bins (0%, 0-5%, 5-10% etc.) into factors without alphabetizing them automatically
state_totals <- state_totals %>%
  mutate(perc_bin = factor(perc_bin, levels = unique(perc_bin)))
 
#Loads US maps and merges the state-level charter school data
usa <- us_states %>%
  left_join(state_totals, by = 'NAME')

#Map of state-level charter school enrollment
ggplot() +
  geom_sf(data = usa, aes(fill = perc_bin)) +
  labs(title = "Share of Public School Students Enrolled in Charter School") + 
  theme(panel.background = element_blank()) +
  scale_fill_discrete(na.value = "white") +
  labs(caption = 'Note: Enrollment data are missing for Nevada. Analysis is done for a random sample of 5,000 \nschools in the Common Core of Data.') +
  theme(plot.caption = element_text(hjust = 0, size = 9),
        plot.title = element_text(size = 14)) +
  theme(legend.title=element_blank(),
        legend.text = element_text(size = 6),
        legend.box.background = element_rect(color = "black"))
