### Packages ----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(statsr)

# Load Data
load("./data/gss.Rdata")

### Part 1: Data ------------------------------------------------------------

# i) Description
# The data used in this project comes from the General Social Survey, which is a
# sociological survey collected by the University of Chicago.
# It is one of the most distinguished studies in the social sciences and has
# been monitoring the change in the society of the United States since 1972.
# Some of the topics in the GSS are: marijuana use, crime, intergroup relations, social and economic life,
# lifestyle, civil liberties, sexual behavior, etc.
# Consequently, the General Social Survey provides social scientists &
# statisticians with high-quality data, which can help them identify
# trends & changes in american society.

# ii) Methodology

# * The target population are adults (18+) living in households in the
# United States.
# * The sample is drawn using a proportional sampling technique,
# using an area probability design that randomly selects respondents.
# * Respondents are from a mix of urban, suburban, and rural geographic
# areas.
# * Participation in the study is voluntary.

# Note that the General Social Survey is indeed, an observational study.

# Therefore, statistical analyses can only identify association
# between independent variables and the outcome of interest.

# Part 2: Research Question -----------------------------------------------

# Research Question
# Is there a considerable income inequality among families of different
# races in the United States?
# 
# Using statistical inference methods, we'll assess whether
# the data in the GSS supports the idea of a significant
# inequality in the income of families from different races.
# 
# Remember that, since the GSS is an observational study, we can
# only reveal an association between the variables.
# In this context, race might as well be a confounding variable,
# and the difference in incomes may be caused by other social
# factors such as achievement gaps, size of the family,
# gender, etc.



# Part 3.0: EDA: Opinion of Family Income ------------------------------------------

df <- gss %>% 
        select(race, coninc, finrela) %>% 
        na.omit() %>% 
        rename(income = coninc)

data <- 
        df %>% 
        mutate(
                finrela = case_when(
                        finrela == "Far Below Average" ~ "Below Average",
                        finrela == "Far Above Average" ~ "Above Average",
                        TRUE ~ as.character(finrela)
                ),
                finrela = factor(finrela, ordered = T, 
                                 levels = c("Below Average", "Average",
                                            "Above Average"))
        ) %>% 
        group_by(race, finrela) %>% 
        summarise(n=n()) %>%
        transmute(finrela, percent = n/sum(n))


ggplot(data = data,
       mapping = aes(x = finrela,
                     y = percent,
                     fill = race)) +
        geom_bar(stat = "identity") +
        labs(title = "Opinion of Family Income",
             x = "",
             y = "Count") +
        facet_wrap(. ~ race) +
        theme_light() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.text.x = element_text(
                        face = "bold",
                        color = "dimgrey",
                        angle = 90
                ),
                strip.background = element_rect(fill = "red2"),
                strip.text = element_text(face = "bold", color = "white")
        ) + guides(fill = "none")



### Part 3.1: EDA: Respondents by Race ------------------------------------

# Number of Respondents
ggplot(data = df, mapping = aes(x = race, fill = race)) +
        geom_bar(aes(y = (..count..)/sum(..count..))) +
        labs(
                title = "Survey Respondents by Race",
                subtitle = "Bar Chart",
                caption = "Data: General Social Survey",
                x = "Race",
                y = "Percentage"
        ) +
        theme_light() +
        scale_fill_brewer(type = "qual", palette = 2) +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                plot.subtitle = element_text(face = "bold", hjust = .5,
                                             color = "dimgrey"),
                plot.caption = element_text(color = "gray"),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey"),
        ) + guides(col = "none", fill = "none")

# Respondents Count & Percentage grouped by Race
df %>% 
        group_by(race) %>% 
        summarize(n_respondents = n()) %>% 
        mutate(p_respondents = n_respondents/sum(n_respondents))


### Part 3.2: EDA: Income Variable ------------------------------------------


# Histogram of the Income variable
ggplot(data = df, mapping = aes(x = income)) +
        geom_histogram(fill = "skyblue", col = "black") +
        theme_minimal() +
        labs(
                title = "Total Family Income",
                subtitle = "Histogram",
                caption = "Data: General Social Survey",
                x = "Income (Constant U$D)",
                y = "Count"
        ) +
        theme_classic() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                plot.subtitle = element_text(face = "bold", hjust = .5,
                                             color = "dimgrey"),
                plot.caption = element_text(color = "gray"),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey")
        )


### Part 3.3: EDA: Income by Race -------------------------------------------

# Violin Plot: Income by Race
ggplot(data = df, mapping = aes(x = income, y = race, fill = race)) +
        geom_violin(alpha = .9, color = "gray") +
        geom_boxplot(width=0.1, color = "black") +
        stat_summary(fun = mean, col = "red", aes(shape = "Mean")) +
        labs(
                title = "Income by Race",
                subtitle = "Violin Plot with Boxplot",
                caption = "Data: General Social Survey",
                x = "Income",
                y = "Race"
        ) +
        scale_fill_brewer(palette = "Set3") +
        scale_color_viridis_d(option = "viridis", begin = .7) +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                plot.subtitle = element_text(face = "bold", hjust = .5,
                                             color = "dimgrey"),
                plot.caption = element_text(color = "gray"),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey"),
                legend.title = element_text(face = "bold",
                                            color = "dimgrey")
        ) + guides(col = "none", fill = "none", 
                   shape = guide_legend(title = "Statistic"))

# --- Quantiles, shown in the Boxplot ---
df %>% 
        na.omit() %>% 
        group_by(race) %>% 
        do(data.frame(t(quantile(.$income, probs = c(.25, .5, .75)))))

# --- Income Mean by Race (White Dot) ---
# Forcing Linear Regression through the Origin
lm(data = df, formula = income ~ 0 +  race) %>% coef()

