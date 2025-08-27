# Testing script to see if the US Okun slopes differ significantly across states.
# Estimated models for each state are saved.

library(dplyr)
library(car)
library(broom)
library(stringr)
library(sandwich)
library(ggplot2)

# --- Estimate model ---
data.states <- read.csv("data/processed.csv") %>%
  filter(Area != "United States")

mod.states <- lm(gdp_growth ~ unrate_diff*Area, data = data.states)

# --- Results ---
linearHypothesis(mod.states,
                 matchCoefs(mod.states, "unrate_diff:Area"),
                 vcov. = vcovHC, type = "HC1")
  #   Res.Df Df      F  Pr(>F)  
  # 1   4028                    
  # 2   3978 50 1.3146 0.06864 .
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coeftest(mod.states, vcov. = vcovHC, type = "HC1")
  #                                       Estimate Std. Error t value  Pr(>|t|)    
  # (Intercept)                           1.476499   0.424072  3.4817 0.0005036 ***
  # unrate_diff                          -4.682994   1.116736 -4.1935 2.807e-05 ***
  # AreaAlaska                           -0.088396   1.022433 -0.0865 0.9311079    
  # AreaArizona                           0.953717   0.595230  1.6023 0.1091763    
  # AreaArkansas                          0.170736   0.769151  0.2220 0.8243409    
  # AreaCalifornia                        1.365543   0.665569  2.0517 0.0402649 *  
  # AreaColorado                          1.409093   0.588228  2.3955 0.0166442 *  
  # AreaConnecticut                      -0.551981   0.850426 -0.6491 0.5163345    
  # AreaDelaware                         -0.017847   1.146389 -0.0156 0.9875795    
  # AreaDistrict of Columbia              0.130699   0.597866  0.2186 0.8269662    
  # AreaFlorida                           0.950179   0.657691  1.4447 0.1486156    
  # AreaGeorgia                           0.579628   0.568326  1.0199 0.3078448    
  # AreaHawaii                           -0.020688   0.556292 -0.0372 0.9703361    
  # AreaIdaho                             1.480485   0.743503  1.9912 0.0465239 *  
  # AreaIllinois                         -0.374000   0.580626 -0.6441 0.5195266    
  # AreaIndiana                           0.442350   0.708899  0.6240 0.5326665    
  # AreaIowa                             -0.047386   0.710814 -0.0667 0.9468524    
  # AreaKansas                            0.511153   0.792666  0.6449 0.5190601    
  # AreaKentucky                          0.072550   0.613527  0.1183 0.9058753    
  # AreaLouisiana                        -1.124890   0.781475 -1.4394 0.1501031    
  # AreaMaine                            -0.013017   0.606403 -0.0215 0.9828744    
  # AreaMaryland                          0.138568   0.624691  0.2218 0.8244673    
  # AreaMassachusetts                     0.866075   0.594501  1.4568 0.1452479    
  # AreaMichigan                         -0.396643   0.783908 -0.5060 0.6128976    
  # AreaMinnesota                         0.037424   0.614430  0.0609 0.9514353    
  # AreaMississippi                      -0.473916   0.680680 -0.6962 0.4863197    
  # AreaMissouri                         -0.241972   0.611852 -0.3955 0.6925138    
  # AreaMontana                           0.670366   0.678649  0.9878 0.3233134    
  # AreaNebraska                          0.894963   0.772971  1.1578 0.2470058    
  # AreaNevada                            0.661942   0.737613  0.8974 0.3695542    
  # AreaNew Hampshire                     0.571037   0.780565  0.7316 0.4644746    
  # AreaNew Jersey                       -0.027837   0.700833 -0.0397 0.9683186    
  # AreaNew Mexico                       -0.026136   0.736111 -0.0355 0.9716786    
  # AreaNew York                          0.603274   0.721031  0.8367 0.4028214    
  # AreaNorth Carolina                    0.773902   0.640052  1.2091 0.2266869    
  # AreaNorth Dakota                      2.449410   1.045644  2.3425 0.0192047 *  
  # AreaOhio                             -0.161157   0.606045 -0.2659 0.7903178    
  # AreaOklahoma                          0.968085   0.958720  1.0098 0.3126682    
  # AreaOregon                            0.968930   0.617335  1.5695 0.1166027    
  # AreaPennsylvania                      0.133323   0.611465  0.2180 0.8274098    
  # AreaRhode Island                     -0.303880   0.742912 -0.4090 0.6825325    
  # AreaSouth Carolina                    0.676530   0.629484  1.0747 0.2825578    
  # AreaSouth Dakota                      0.611712   0.970835  0.6301 0.5286726    
  # AreaTennessee                         0.823511   0.650204  1.2665 0.2053933    
  # AreaTexas                             1.923911   0.613564  3.1356 0.0017273 ** 
  # AreaUtah                              2.292738   0.601677  3.8106 0.0001407 ***
  # AreaVermont                          -0.173641   0.611420 -0.2840 0.7764284    
  # AreaVirginia                          0.410515   0.535399  0.7667 0.4432787    
  # AreaWashington                        2.175348   0.675391  3.2209 0.0012883 ** 
  # AreaWest Virginia                    -0.219386   0.670154 -0.3274 0.7434077    
  # AreaWisconsin                        -0.174872   0.577046 -0.3030 0.7618702    
  # AreaWyoming                           0.132340   1.080586  0.1225 0.9025327    
  # unrate_diff:AreaAlaska                0.534728   1.268906  0.4214 0.6734797    
  # unrate_diff:AreaArizona              -0.175847   1.744226 -0.1008 0.9197013    
  # unrate_diff:AreaArkansas             -0.430145   1.945099 -0.2211 0.8249926    
  # unrate_diff:AreaCalifornia            1.186497   1.340126  0.8854 0.3760149    
  # unrate_diff:AreaColorado              0.527378   1.203171  0.4383 0.6611757    
  # unrate_diff:AreaConnecticut           0.358675   1.250236  0.2869 0.7742149    
  # unrate_diff:AreaDelaware              1.884182   1.265987  1.4883 0.1367483    
  # unrate_diff:AreaDistrict of Columbia  0.697326   1.507128  0.4627 0.6436150    
  # unrate_diff:AreaFlorida               0.739299   1.470126  0.5029 0.6150754    
  # unrate_diff:AreaGeorgia              -0.772025   1.532420 -0.5038 0.6144336    
  # unrate_diff:AreaHawaii                2.075999   1.120384  1.8529 0.0639658 .  
  # unrate_diff:AreaIdaho                -0.550917   1.884074 -0.2924 0.7699905    
  # unrate_diff:AreaIllinois              1.414713   1.271594  1.1126 0.2659689    
  # unrate_diff:AreaIndiana              -0.159018   1.652357 -0.0962 0.9233373    
  # unrate_diff:AreaIowa                  0.217094   1.597305  0.1359 0.8918972    
  # unrate_diff:AreaKansas               -0.736553   1.446792 -0.5091 0.6107145    
  # unrate_diff:AreaKentucky             -0.876626   1.361355 -0.6439 0.5196538    
  # unrate_diff:AreaLouisiana             1.651658   1.605125  1.0290 0.3035469    
  # unrate_diff:AreaMaine                -0.853005   1.594488 -0.5350 0.5926998    
  # unrate_diff:AreaMaryland             -0.113386   1.339575 -0.0846 0.9325492    
  # unrate_diff:AreaMassachusetts         1.710834   1.241087  1.3785 0.1681276    
  # unrate_diff:AreaMichigan              0.895168   1.581257  0.5661 0.5713498    
  # unrate_diff:AreaMinnesota            -0.899677   1.445340 -0.6225 0.5336701    
  # unrate_diff:AreaMississippi          -0.507510   1.627636 -0.3118 0.7552028    
  # unrate_diff:AreaMissouri             -0.647634   1.614525 -0.4011 0.6883462    
  # unrate_diff:AreaMontana              -0.713900   1.972745 -0.3619 0.7174599    
  # unrate_diff:AreaNebraska             -4.206354   1.922726 -2.1877 0.0287492 *  
  # unrate_diff:AreaNevada                1.486693   1.284259  1.1576 0.2470859    
  # unrate_diff:AreaNew Hampshire         0.273711   1.503101  0.1821 0.8555154    
  # unrate_diff:AreaNew Jersey            1.069313   1.312111  0.8150 0.4151461    
  # unrate_diff:AreaNew Mexico           -1.142264   2.101660 -0.5435 0.5868124    
  # unrate_diff:AreaNew York              1.500310   1.223658  1.2261 0.2202390    
  # unrate_diff:AreaNorth Carolina        0.131377   1.472043  0.0892 0.9288890    
  # unrate_diff:AreaNorth Dakota         -1.311679   1.240765 -1.0572 0.2905058    
  # unrate_diff:AreaOhio                  0.182081   1.521713  0.1197 0.9047622    
  # unrate_diff:AreaOklahoma              0.062197   1.267052  0.0491 0.9608518    
  # unrate_diff:AreaOregon                0.529377   1.306252  0.4053 0.6853053    
  # unrate_diff:AreaPennsylvania         -0.175969   1.484188 -0.1186 0.9056282    
  # unrate_diff:AreaRhode Island          1.419843   1.305922  1.0872 0.2769993    
  # unrate_diff:AreaSouth Carolina       -0.287820   1.698118 -0.1695 0.8654171    
  # unrate_diff:AreaSouth Dakota         -2.209455   2.001037 -1.1042 0.2695927    
  # unrate_diff:AreaTennessee            -0.652217   1.695524 -0.3847 0.7005026    
  # unrate_diff:AreaTexas                 0.332513   1.296858  0.2564 0.7976561    
  # unrate_diff:AreaUtah                  0.793048   1.375506  0.5765 0.5642762    
  # unrate_diff:AreaVermont              -0.841127   1.486606 -0.5658 0.5715591    
  # unrate_diff:AreaVirginia              0.838391   1.321418  0.6345 0.5258151    
  # unrate_diff:AreaWashington            1.473018   1.411003  1.0440 0.2965714    
  # unrate_diff:AreaWest Virginia         0.323126   1.335458  0.2420 0.8088246    
  # unrate_diff:AreaWisconsin             0.605532   1.390960  0.4353 0.6633436    
  # unrate_diff:AreaWyoming              -5.420342   3.020903 -1.7943 0.0728446 .  
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(mod.states)
  # Residuals:
  #     Min      1Q  Median      3Q     Max 
  # -22.057  -2.660  -0.146   2.346  55.713
  # 
  # Residual standard error: 5.232 on 3978 degrees of freedom
  # Multiple R-squared:  0.4717,	Adjusted R-squared:  0.4583 
  # F-statistic: 35.17 on 101 and 3978 DF,  p-value: < 2.2e-16

# --- Sort and save model's coefficients ---
slopes.states.base <- tidy(mod.states) %>%
  filter(term == "unrate_diff") %>%
  pull(estimate)

slopes.states <- tidy(mod.states) %>%
  filter(str_detect(term, "unrate_diff")) %>%
  mutate(
    Area = ifelse(term == "unrate_diff", "Alabama", str_remove(term, "unrate_diff:Area")),
    slope = ifelse(Area == "Alabama", slopes.states.base, slopes.states.base + estimate)
  ) %>%
  select(Area, slope)

intercepts.states.base <- tidy(mod.states) %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)

intercepts.states <- tidy(mod.states) %>%
  filter(!str_detect(term, "unrate_diff")) %>%
  mutate(
    Area = ifelse(term == "(Intercept)", "Alabama", str_remove(term, "Area")),
    intercept = ifelse(Area == "Alabama", intercepts.states.base, intercepts.states.base + estimate)
  ) %>%
  select(Area, intercept)
  
coefs.states <- full_join(intercepts.states, slopes.states, by = "Area") %>%
  arrange(slope)

write.csv(coefs.states, "results/coefs.states.csv", row.names = F)

# --- Clear all variables ---
rm(list = ls())