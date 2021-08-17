# Packages ----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(statsr)


# Loading & Subsetting Data -----------------------------------------------

load("./data/gss.Rdata")

df <- gss %>% 
        select(race, coninc, finrela) %>%  
        na.omit() %>% 
        rename(income = coninc)

# Number of Survey Respondents ------------------------------------------------

# Barchart
ggplot(data = df, mapping = aes(x = race, fill = race)) +
        geom_bar(aes(y = (..count..) / sum(..count..))) +
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
                plot.subtitle = element_text(
                        face = "bold",
                        hjust = .5,
                        color = "dimgrey"
                ),
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


# Opinion of Family Income ------------------------------------------------

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
             y = "Percentage") +
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

data


# Distribution of Total Family Income -------------------------------------

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

# Summary of the Total Family Income
with(df, summary(income))


# Income by Race ----------------------------------------------------------

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
        scale_x_continuous(breaks = seq(0, 15E4, by = 25E3)) +
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

# --- Income Mean by Race (Red Dot) ---
# Forcing Linear Regression through the Origin and
# getting it's coefficients
lm(data = df, formula = income ~ race - 1) %>% coef()


# ANOVA Test --------------------------------------------------------------

test_data <- gss %>%
        select(coninc, race)

# Fitting the Linear Regression Model
# Income as Response
# Race as Dummy Predictor
fit <-
        lm(
                data = test_data,
                formula = coninc ~ race
        )

# Applying the ANOVA f-test to the Linear Model
test <-
        anova(fit)

test


# Pairwise T-tests with Bonferroni Correction -----------------------------

pairwise_tests <-
        pairwise.t.test(x=test_data$coninc, 
                        g = test_data$race,
                        p.adjust.method = "bonferroni", 
                        alternative = "two.sided")

pairwise_tests


