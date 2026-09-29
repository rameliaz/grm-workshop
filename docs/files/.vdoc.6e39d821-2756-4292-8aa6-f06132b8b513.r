#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
install.packages(
    c("tidyverse", # data wrangling
      "psych", # descriptive stats & unidimensionality checks
      "pak", # to install a GitHub-only package (ggmirt)
      "mirt", # the main GRM engine
      "skimr", # quick look at data distributions
      "haven" # to import the NaDiRA STATA .dta file
    ),
    dependencies = TRUE
)

pak::pak("masurp/ggmirt") # ggmirt available only on github
#
#
#
#
#
library(tidyverse)
library(psych)
library(mirt)
library(ggmirt)
library(skimr)
library(haven)
#
#
#
#
#
#
#
#
#
USE_SIMULATED_DATA <- FALSE # set TRUE if you don't have an access to the dataset

DATA_PATH <- "data/NaDiRa_Teaching_w0_clean.dta" # path to the dataset don't forget to create a new folder `data/` in your project root, then keep the dataset file there

ITEM_COLS <- c(
    "ker_diskerf_service", "ker_diskerf_respekt", "ker_diskerf_nernst",
    "ker_diskerf_angst", "ker_diskerf_bedbel", "ker_diskerf_beleid",
    "ker_diskerf_angriff"
)

N_CATEGORIES <- 6 # response categories per item (1-6)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
if (USE_SIMULATED_DATA) {
    # ---- calibrated simulation: parameters estimated once on the real data ----
    # (a = discrimination, d = intercepts; this array reproduces the item
    # difficulty/discrimination pattern of the real dataset without using any
    # real participants' answers)
    set.seed(2026)

    a_par <- c(2.51, 3.48, 2.87, 1.21, 1.68, 1.97, 1.13) # discrimination params
    d_par <- matrix(                                     # intercepts (d = -a * b)
         c(
            # d1        d2        d3        d4        d5
            1.9284,  -1.5920,  -3.0283,  -5.0120,  -6.7521,  # service
            2.5426,  -1.7032,  -3.6920,  -5.5658,  -7.7253,  # respekt
            3.0612,  -0.7804,  -2.6710,  -4.2892,  -6.9545,  # nernst
            -0.3967,  -2.3890,  -3.2141,  -4.2315,  -5.4047,  # angst
            -0.5662,  -2.9232,  -4.3999,  -5.5944,  -6.6881,  # bedbel
            0.3987,  -2.2918,  -3.8572,  -5.1107,  -7.8703,  # beleid
            -2.0059,  -4.3481,  -5.0828,  -5.4529,  -6.7298   # angriff
        ),
        nrow = length(ITEM_COLS), ncol = N_CATEGORIES - 1, byrow = TRUE
    )

    data <- simdata(a = a_par, d = d_par, N = 970, itemtype = "graded") + 1
    data <- as.data.frame(data)
    colnames(raw) <- ITEM_COLS
} else {
    data <- read_dta(DATA_PATH) %>%
        select(all_of(ITEM_COLS)) %>%
        mutate(across(everything(), as.numeric)) # drop haven_labelled class, keep plain numeric codes
}
#
#
#
#
#
#
#
describe(data)
skim(data)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
efa <- irt.fa(data, nfactors = 1, fm = "minres", plot = FALSE)
print(efa$fa)
#
#
#
#
#
#
#
set.seed(2026) # fixes the simulated reference line so the plot is identical on every render
a <- fa.parallel(polychoric(data)$rho, n.obs = nrow(data), fm = "minres", main = "Scree Plot Everyday Discrimination Scale - NaDiRA Test Dataset")
a$fa.values
#
#
#
#
#
#
#
#
#
#
#
model <- paste0("theta = 1-", length(ITEM_COLS)) # specifying the model
model # we can see here the model contains one LV (theta) with 7 items
fit <- mirt(data = data, model = model, itemtype = "graded", SE = TRUE, verbose = FALSE) # now fit the model
coefs <- coef(fit, IRTpars = TRUE, simplify = TRUE) # keep item parameters in a list
print(coefs) # print item parameters
#
#
#
#
#
#
#
#
#
#
#
tracePlot(fit, title = "Item Probability Curves Everyday Discrimination Scale - NaDIRa Test Dataset") + labs(color = "Response Category")
#
#
#
#
#
#
#
#
#
#
#
itemInfoPlot(fit, facet = TRUE, title = "Item Information Functions")
#
#
#
#
#
#
#
testInfoPlot(fit, title = "Test Information Function")
#
#
#
#
#
#
#
#
#
#
#
marginal_rxx(fit)
#
#
#
#
#
theta_se <- fscores(fit, full.scores.SE = TRUE)
empirical_rxx(theta_se)
#
#
#
# using mirt's own plot() rather than ggmirt::conRelPlot() here -- for some reason,
# mirt/ggmirt version combination conRelPlot() crashes R outright
# mirt's native rxx plot shows the same thing
plot(fit, type = "rxx", main = "Reliability Across the θ Continuum")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
M2(fit, type = "C2") # we use C2 because we're dealing with polytomous data
#
#
#
#
#
#
#
#
#
itemfit(fit)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
q3 <- residuals(fit, type = "Q3", verbose = FALSE) # asking mirt to compute q3
round(q3, 2) # round the value to only two decimals
#
#
#
#
#
#
#
#
#
baseline <- mean(q3[upper.tri(q3)]) # calculating mean off-diagonal, to see the "no-LD" baseline
as.data.frame(q3) %>%
  rownames_to_column("item1") %>%
  pivot_longer(-item1, names_to = "item2", values_to = "Q3") %>%
  filter(item1 < item2) %>%
  mutate(Q3d = Q3 - baseline) %>%
  filter(Q3 > 0, Q3d > 0.2) # flag problematic items that have more paired correlations more than 0.2 from the baseline (-0.113)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
sessionInfo()
#
#
#
#
