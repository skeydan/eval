library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)

##################################################################################
#                                   data wrangling                               #
##################################################################################

df <- read_csv('data/CSV/s_transformed.csv', skip = 2)
df

headings <- colnames(df)

df <- df %>% select(lastname:additional_comments)

df <- df %>% filter(Reduce(`+`, lapply(., is.na)) < ncol(.) - 4)

df <- df %>% mutate_all(funs(replace(., is.na(.), 0)))

df <-
  df %>% mutate_at(
    funs(replace(., . != 0, 1)),
    .vars = vars(
      starts_with("reason_"),
      work_1:work_4,
      autonomy_1:autonomy_4,
      equipment_1:equipment_4,
      interdisciplinary_collaboration_1:interdisciplinary_collaboration_4,
      workflow_efficiency_1:workflow_efficiency_4,
      colleagues_1:colleagues_4,
      intra_org_collaboration_1:intra_org_collaboration_4,
      supervisor_support_1:supervisor_support_4,
      supervisor_communication_1:supervisor_communication_4,
      personal_esteem_1:personal_esteem_4,
      recognition_1:recognition_4,
      salary_1:salary_4,
      benefits_1:benefits_4,
      perspective_1:perspective_4,
      education_1:education_4,
      company_information_1:company_information_4,
      company_vision_1:company_vision_4,
      company_culture_1:company_culture_4,
      company_image_1:company_image_4,
      trivadis_return_yes:trivadis_return_no
    )
  )


get_max = function(col1, col2, col3, col4) {
  if (max(col1, col2, col3, col4) == 0)
    NA
  else
    which.max(c(col1, col2, col3, col4))
}

df <-
  df %>% rowwise() %>% mutate(work = get_max(work_1, work_2, work_3, work_4)) %>% select(-(work_1:work_4))
df <-
  df %>% rowwise() %>% mutate(autonomy = get_max(autonomy_1, autonomy_2, autonomy_3, autonomy_4)) %>% select(-(autonomy_1:autonomy_4))
df <-
  df %>% rowwise() %>% mutate(equipment = get_max(equipment_1, equipment_2, equipment_3, equipment_4)) %>% select(-(equipment_1:equipment_4))
df <- df %>% rowwise() %>% mutate(
  interdisciplinary_collaboration =
    get_max(
      interdisciplinary_collaboration_1,
      interdisciplinary_collaboration_2,
      interdisciplinary_collaboration_3,
      interdisciplinary_collaboration_4
    )
) %>% select(-(
  interdisciplinary_collaboration_1:interdisciplinary_collaboration_4
))
df <- df %>% rowwise() %>% mutate(
  workflow_efficiency =
    get_max(
      workflow_efficiency_1,
      workflow_efficiency_2,
      workflow_efficiency_3,
      workflow_efficiency_4
    )
) %>%
  select(-(workflow_efficiency_1:workflow_efficiency_4))
df <-
  df %>% rowwise() %>% mutate(colleagues = get_max(colleagues_1, colleagues_2, colleagues_3, colleagues_4)) %>% select(-(colleagues_1:colleagues_4))
df <-
  df %>% rowwise() %>% mutate(
    intra_org_collaboration = get_max(
      intra_org_collaboration_1,
      intra_org_collaboration_2,
      intra_org_collaboration_3,
      intra_org_collaboration_4
    )
  ) %>% select(-(intra_org_collaboration_1:intra_org_collaboration_4))
df <-
  df %>% rowwise() %>% mutate(
    supervisor_support = get_max(
      supervisor_support_1,
      supervisor_support_2,
      supervisor_support_3,
      supervisor_support_4
    )
  ) %>% select(-(supervisor_support_1:supervisor_support_4))
df <-
  df %>% rowwise() %>% mutate(
    supervisor_communication = get_max(
      supervisor_communication_1,
      supervisor_communication_2,
      supervisor_communication_3,
      supervisor_communication_4
    )
  ) %>% select(-(supervisor_communication_1:supervisor_communication_4))
df <-
  df %>% rowwise() %>% mutate(
    personal_esteem = get_max(
      personal_esteem_1,
      personal_esteem_2,
      personal_esteem_3,
      personal_esteem_4
    )
  ) %>% select(-(personal_esteem_1:personal_esteem_4))
df <-
  df %>% rowwise() %>% mutate(recognition = get_max(recognition_1,
                                                    recognition_2,
                                                    recognition_3,
                                                    recognition_4)) %>% select(-(recognition_1:recognition_4))
df <-
  df %>% rowwise() %>% mutate(salary = get_max(salary_1,
                                               salary_2,
                                               salary_3,
                                               salary_4)) %>% select(-(salary_1:salary_4))
df <-
  df %>% rowwise() %>% mutate(benefits = get_max(benefits_1,
                                                 benefits_2,
                                                 benefits_3,
                                                 benefits_4)) %>% select(-(benefits_1:benefits_4))
df <-
  df %>% rowwise() %>% mutate(perspective = get_max(perspective_1,
                                                    perspective_2,
                                                    perspective_3,
                                                    perspective_4)) %>% select(-(perspective_1:perspective_4))
df <-
  df %>% rowwise() %>% mutate(education = get_max(education_1,
                                                  education_2,
                                                  education_3,
                                                  education_4)) %>% select(-(education_1:education_4))
df <-
  df %>% rowwise() %>% mutate(
    company_information = get_max(
      company_information_1,
      company_information_2,
      company_information_3,
      company_information_4
    )
  ) %>% select(-(company_information_1:company_information_4))

df <-
  df %>% rowwise() %>% mutate(
    company_vision = get_max(
      company_vision_1,
      company_vision_2,
      company_vision_3,
      company_vision_4
    )
  ) %>% select(-(company_vision_1:company_vision_4))

df <-
  df %>% rowwise() %>% mutate(
    company_culture = get_max(
      company_culture_1,
      company_culture_2,
      company_culture_3,
      company_culture_4
    )
  ) %>% select(-(company_culture_1:company_culture_4))

df <-
  df %>% rowwise() %>% mutate(
    company_image = get_max(
      company_image_1,
      company_image_2,
      company_image_3,
      company_image_4
    )
  ) %>% select(-(company_image_1:company_image_4))

df <-
  df %>% rowwise() %>% mutate(trivadis_return = if (trivadis_return_no == 1)
    -1
    else if (trivadis_return_yes == 1)
      1
    else
      NA) %>% select(-(trivadis_return_yes:trivadis_return_no))

df_raw <- df


##################################################################################
#                                   rescale                                      #
##################################################################################



df_numeric <- df %>% select_if(is.numeric)

df_numeric

rescale <- function(val) {
    switch(as.character(val),
           "1" = 1,
           "2" = 0.5,
           "3" = -0.5,
           "4" = -1,
           "NA" = NA)
}

rescale_column <- function(vec) sapply(vec, rescale)

df_rescaled <- df_numeric %>% mutate_at(vars(work:company_image), rescale_column)
df_rescaled


##################################################################################
#                                   data exploration                             #
##################################################################################



df <- df_rescaled

corrs <- cor(df, use = 'pairwise.complete.obs')
corrs


ggplot(data=subset(df, !is.na(equipment)), aes(x=factor(na.omit(equipment),  levels = c(-1,-0.5,0.5,1), 
                        labels = c("sehr schlecht", "eher schlecht", "gut", "sehr gut")))) +
  geom_bar()+ ggtitle("") + xlab("") + scale_x_discrete(drop=FALSE)





####################


hists <- apply(df, 2, function(column) {ggplot(subset(df, !is.na(column)), 
                                                  aes(x=factor(
                                                    na.omit(column),
                                                    levels = c(-1,-0.5,0.5,1), 
                                                    labels = c("sehr schlecht", "eher schlecht", "gut", "sehr gut")
                                                    ))) + geom_bar() +  scale_x_discrete(drop=FALSE) +
    xlab("")})

columns <- colnames(df)
plots <- list()

for ( i in seq_along(columns)) {
  plot <- hists[[i]] + ggtitle(columns[[i]])
  plots[[i]] <- plot
}

plots
do.call('grid.arrange', list('grobs' = plots, 'ncol' = 5))

#ggpairs(df)


