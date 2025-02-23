The Median Voter Theorem
================
João Pedro Nogueira

The median voter theorem states that if voters and candidates are
distributed along a one-dimensional spectrum and voters have
single-peaked preferences, any voting method that is compatible with
majority-rule will elect the candidate preferred by the median voter.

We’ll test it’s validity using data from [Chin’s
(2023)](https://www.aeaweb.org/articles?id=10.1257/app.20210529)
[replication
package](https://www.openicpsr.org/openicpsr/project/168901/version/V1/view).
Chin’s paper provides broader evidence for the validity of the theorem,
by leveraging the fact that electoral rules in Brazil establish that
municipalities with over 200.000 electors must have run-off elections if
a candidate fails to achieve 50% + 1 valid votes in the first round of
voting. This allows the author to implement a Regression Discontinuity
Design and verify that municipalities in which the run-off took place,
there wan an increase in the provision of public goods.

``` r
source("brazil_tworound_functions.R")

elections <- readRDS("elections.rds")
spend <- readRDS("spending.rds")
schools <- readRDS("schools.rds")
lights <- readRDS("lights.rds")
simulation <- readRDS("simulation.rds")

# regression specifications
bw = 50000
spec_baseline =
  "tworound + cut_dist + pop_den_pre + tworound:cut_dist + tworound:pop_den_pre + factor(elect_year)|0|0|tse_code"
spec_no_controls = 
  "tworound + cut_dist + tworound:cut_dist|0|0|tse_code"
spec_no_controls_yr = 
  "tworound + cut_dist + tworound:cut_dist + factor(elect_year)|0|0|tse_code"
spec_controls = 
  "tworound + cut_dist + pop_den_pre + tworound:cut_dist + tworound:pop_den_pre + area_chg_T0 + pop_growth_T0 + pop_0_15_T0 + illit_T0 + h_inc_T0 + h_dem_T0 + inc_T0 + inc_0_50_T0 + unempl_T0 + gini_T0 + factor(elect_year)|0|0|tse_code"
spec_quadratic = 
  "tworound + cut_dist + cut_dist^2 + pop_den_pre + tworound:cut_dist + tworound:cut_dist^2 + tworound:pop_den_pre + factor(elect_year)|0|0|tse_code"
spec_num_cand =
  "tworound + cut_dist + pop_den_pre + tworound:cut_dist + tworound:pop_den_pre + num_cand + factor(elect_year)|0|0|tse_code"

# color and size formatting for plots
lightmaroon = "#BF7C7C"
maroon = "#800000"
blue = "#354B99"
lightblue = "#98CAE0"

fw = 5
fh = 2
```

In this exercise, we’ll focus only on right-wing parties. Specifically,
we’re interested in Figure B.17, which seems to suggest that in
municipalities in which the run-off elections took place, there was a
higher likelihood that the eventual winner would come from a right-wing
party. Thinking about the median voter theorem, this makes sense. In the
first round, with a greater variety of candidates, the candidates also
have a greater variety of ideological positions. In the second round,
when there are only two candidates left, the competition for votes means
that ideological radicalism decreases and the candidates have closer
positions.

With this in mind, and as the paper points to the provision of public
goods as the main outcome, in this exercise we will test whether there
is a discontinuity in the provision of public goods in municipalities
whose winning candidates were elected by right-wing parties. Given that
right-wing parties are generally associated with greater fiscal rigor,
one would expect their members, when elected, to be less likely to spend
on public goods. If there is a discontinuity, therefore, we can take it
as a sign that, in the run-off, candidates from these parties modified
their platforms to appeal to the median voter.

``` r
output <- estRD(
  list(elections),
  lhs = c(
    "party_10_cwin", "party_11_cwin", "party_12_cwin", "party_13_cwin", "party_14_cwin",
    "party_15_cwin", "party_19_cwin", "party_20_cwin", "party_22_cwin", "party_23_cwin", 
    "party_25_cwin", "party_35_cwin", "party_40_cwin", "party_43_cwin", "party_45_cwin", 
    "party_55_cwin", "party_41_cwin", "party_extreme_cwin"),
  spec = spec_baseline,
  bandwidths = bw
)

plotCoef(
  output[str_subset(names(output), "_bw_1")][-length(str_subset(names(output), "_bw_1"))], 
  "tworound",
  reg_lab_numeric = FALSE, reg_lab = c(
    "PRB", "PP", "PDT", "PT", "PTB", "PMDB", "PODE", "PSC", "PL", "PPS", "DEM", "PMB", "PSB", "PV", 
    "PSDB", "PSD (55)", "PSD (41)", "Extreme Party"
  ), 
  xlab = "Party of winner",
  annot_N = TRUE, flip = TRUE, 
  file = "figureb17.pdf", w = 10, h = 8
)
```

![](median_voter_theorem_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

We begin by creating a dummy that indicates whether the winner of the
election belonged to one of the following parties: PSD, PMB, DEM, PL,
PSC, PODEMOS, PTB, PP and PRB. We classify these parties following the
same classification used by the author.

``` r
elections2 <- elections %>%
  dplyr::mutate("rw_winner"=data.table::fifelse(party_num_cwin==10 | party_num_cwin==11 | party_num_cwin==14 | party_num_cwin==19 | party_num_cwin==20 | party_num_cwin==22 | party_num_cwin==25 | party_num_cwin==35 | party_num_cwin==55 | party_num_cwin==41,1,0))
```

Then, we test whether there is a discontinuity in the number of
candidates elected from right-wing parties.

``` r
figura_extensao <- plotRD(elections2,
  "rw_winner", "RW Party", "tse_code", spec = spec_baseline,
  file = "figura_extensao.pdf", w = fw, h = fh-.5)
figura_extensao
```

![](median_voter_theorem_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
output <- estRD(
  list(elections2),
  lhs = c("rw_winner"),
  spec = spec_baseline,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "tabela1_extensao.tex")

t <- readLines("tabela1_extensao.tex")
t2 <- gsub(".textbf.(.*?)\\}", "\\1", t)
DF <- read.table(text = t2, sep = "&", header = FALSE, 
  strip.white = TRUE, check.names = FALSE, comment.char = "\\")
DF
```

    ##               V1      V2
    ## 1       TwoRound   0.103
    ## 2                (0.110)
    ## 3   Observations     296
    ## 4 Municipalities      92

As we can see, there really is a discontinuity in the number of
candidates elected by right-wing parties.

Finally, we test whether there was an increase in public goods provision
in the cities which elected candidates from right-wing parties in the
run-off elections compared to those that elected candidates from
right-wing parties without run-off elections.

``` r
elections2 <- elections2[,c("elect_year","tse_code","rw_winner")] %>%
  dplyr::mutate("year"=elect_year) %>%
  dplyr::select(c(4,2,3))

schools2 <- schools %>%
  dplyr::left_join(elections2,by=c("year","tse_code"))

output <- estRD(
  list(schools2), 
  lhs = c("eq_pca_ptile_mean", "dep_pca_ptile_mean", "eq_pca_ptile_sd", "dep_pca_ptile_sd"),
  spec = paste0("tworound:rw_winner + ", spec_baseline),
  keep = c("tworound", "tworound:rw_winner"),
  covariate.labels = c("TwoRound", "TwoRound * RW Party"),
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "tabela2_extensao.tex")

t <- readLines("tabela2_extensao.tex")
t2 <- gsub(".textbf.(.*?)\\}", "\\1", t)
DF <- read.table(text = t2, sep = "&", header = FALSE, 
  strip.white = TRUE, check.names = FALSE, comment.char = "\\")
DF
```

    ##                    V1      V2      V3       V4       V5
    ## 1            TwoRound   0.063   0.055 $-$0.016 $-$0.017
    ## 2                     (0.033) (0.034)  (0.010)  (0.016)
    ## 3 TwoRound * RW Party   0.046   0.026    0.009 $-$0.015
    ## 4                     (0.028) (0.025)  (0.012)  (0.017)
    ## 5        Observations     229     229      229      229
    ## 6      Municipalities      79      79       79       79

***Note:*** In the table, the first two columns refer to the average
level of resources. The first column refers to equipment and the second
to infrastructure. The last two columns refer to the standard deviation
of resources. The third column refers to equipment and the fourth to
infrastructure.

The results show that there is a discontinuity in the provision of
public goods in municipalities close to the cut-off point whose mayors
were elected by right-wing parties. We can interpret this result as
meaning that in elections with two rounds, the elected mayors implement
a level of public good closer to that desired by the median voter. It
should be noted, however, that the treatment effect does not depend on
whether the elected mayor is right-wing; on the contrary, elected mayors
from center and left-wing parties provide even more public good. We can
interpret this fact in a few ways. One is that although they come close
to the median voter in the campaign, they still choose to implement a
level of public good below what they promise.
