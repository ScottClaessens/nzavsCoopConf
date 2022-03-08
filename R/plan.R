# drake plan
plan <- drake_plan(
  
  #############
  # Load data #
  #############
  
  # load data
  d = loadData(),
  
  ###########
  # Study 1 #
  ###########
  
  # filter data
  d1 = filterData1(d),
  # table of items
  itemTable = makeItemTable(),
  # table of comprehension
  compTable = makeCompTable(d1, d2),
  # sample characteristics
  samplePlot = makeSamplePlot(d1),
  # correlations
  cor = makeCor(d1),
  # pca
  pca1.1 = runPCA(d1, extraGames = F, nfactors = 7, rotate = "none"),
  pca1.2 = runPCA(d1, extraGames = F, nfactors = 2, rotate = "varimax"),
  pca2.1 = runPCA(d1, extraGames = T, nfactors = 9, rotate = "none"),
  pca2.2 = runPCA(d1, extraGames = T, nfactors = 2, rotate = "varimax"),
  corPcaPlot = makeCorPcaPlot(cor, pca1.2, pca2.2),
  screePlot = makeScreePlot(pca1.1, pca2.1),
  # cfa
  cfa1 = runCFA(d1, extraGames = F),
  cfa2 = runCFA(d1, extraGames = T),
  # Cronbach's alpha
  alphaSDO = alpha(d1 %>% select(SDO01.T10:SDO06r.T10))$total$std.alpha,
  alphaRWA = alpha(d1 %>% select(RWA01.T10:RWA06r.T10))$total$std.alpha,
  # sem
  sem1 = runSEM(d1),
  sem2 = runSEM(d1, controls = " + Age.T10.c + Gender.T10"),
  sem3 = runSEM(d1, controls = " + EthnicCats.T10_Pakeha"),
  sem4 = runSEM(d1, controls = " + NZREG.T10.c"),
  sem5 = runSEM(d1, controls = " + NZSEI13.T10.c + NZDepRAW.2013.T10.c"),
  sem6 = runSEM(d1, controls = " + Religious.T10"),
  sem7 = runSEM(d1, controls = " + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10"),
  coopPunPlot = makeCoopPunPlot(d1, sem1),
  semTable = makeSEMtable(sem7),
  # individual games with controls
  indGamesSDO = fitIndGames(d1, "SDO.T10 ~ RWA.T10.c"),
  indGamesRWA = fitIndGames(d1, "RWA.T10 ~ SDO.T10.c"),
  indGamesPlotSDO = makeIndPlotGrid(d1, indGamesSDO, "SDO.T10"),
  indGamesPlotRWA = makeIndPlotGrid(d1, indGamesRWA, "RWA.T10"),
  # policies
  policy01 = fitPolicy(d1, "Issue.IncomeRedistribution.T10", "coop + ", "pun + "),
  policy02 = fitPolicy(d1, "IncomeAttribution.T10",          "coop + ", "pun + "),
  policy03 = fitPolicy(d1, "Env.SacWilling.T09",             "coop + ", "pun + "),
  policy04 = fitPolicy(d1, "Issue.SameSexMarriage.T09",      "coop + ", "pun + "),
  policy05 = fitPolicy(d1, "Issue.Euthanasia.T09",           "coop + ", "pun + "),
  policy06 = fitPolicy(d1, "Issue.Abortion.AnyReason.T10",   "coop + ", "pun + "),
  policy07 = fitPolicy(d1, "Issue.Payments.Jobseeker.T06",   "coop + ", "pun + "),
  policy08 = fitPolicy(d1, "Issue.Payments.SoleParent.T06",  "coop + ", "pun + "),
  policy09 = fitPolicy(d1, "Issue.TaxPolicy.T05",            "coop + ", "pun + "),
  policyPlotCoop = makePolicyGrid(d1, sem1),
  policyR2Plot = makePolicyR2Grid(d1),
  policy01table = makePolicyTable(policy01),
  policy02table = makePolicyTable(policy02),
  policy03table = makePolicyTable(policy03),
  policy04table = makePolicyTable(policy04),
  policy05table = makePolicyTable(policy05),
  policy06table = makePolicyTable(policy06),
  policy07table = makePolicyTable(policy07),
  policy08table = makePolicyTable(policy08),
  policy09table = makePolicyTable(policy09),
  # effect sizes
  r_SDO_coop       = semiPartialR1(d1, sem1, "", "coop", "coop \\~ SDO\\.T10\\.c \\+ RWA\\.T10\\.c", "coop ~ RWA.T10.c"),
  r_RWA_coop       = semiPartialR1(d1, sem1, "", "coop", "coop \\~ SDO\\.T10\\.c \\+ RWA\\.T10\\.c", "coop ~ SDO.T10.c"),
  r_SDO_pun        = semiPartialR1(d1, sem1, "", "pun",  "pun  \\~ SDO\\.T10\\.c \\+ RWA\\.T10\\.c", "pun ~ RWA.T10.c" ),
  r_RWA_pun        = semiPartialR1(d1, sem1, "", "pun",  "pun  \\~ SDO\\.T10\\.c \\+ RWA\\.T10\\.c", "pun ~ SDO.T10.c" ),
  r_indGamesSDO    = semiPartialR2(indGamesSDO),
  r_indGamesRWA    = semiPartialR2(indGamesRWA),
  r_policy01_coop  = semiPartialR3(d1, policy01, "", "pun + "),
  r_policy02_coop  = semiPartialR3(d1, policy02, "", "pun + "),
  r_policy03_coop  = semiPartialR3(d1, policy03, "", "pun + "),
  r_policy04_coop  = semiPartialR3(d1, policy04, "", "pun + "),
  r_policy05_coop  = semiPartialR3(d1, policy05, "", "pun + "),
  r_policy06_coop  = semiPartialR3(d1, policy06, "", "pun + "),
  r_policy07_coop  = semiPartialR3(d1, policy07, "", "pun + "),
  r_policy08_coop  = semiPartialR3(d1, policy08, "", "pun + "),
  r_policy09_coop  = semiPartialR3(d1, policy09, "", "pun + "),
  r_policy01_pun   = semiPartialR3(d1, policy01, "coop + ", ""),
  r_policy02_pun   = semiPartialR3(d1, policy02, "coop + ", ""),
  r_policy03_pun   = semiPartialR3(d1, policy03, "coop + ", ""),
  r_policy04_pun   = semiPartialR3(d1, policy04, "coop + ", ""),
  r_policy05_pun   = semiPartialR3(d1, policy05, "coop + ", ""),
  r_policy06_pun   = semiPartialR3(d1, policy06, "coop + ", ""),
  r_policy07_pun   = semiPartialR3(d1, policy07, "coop + ", ""),
  r_policy08_pun   = semiPartialR3(d1, policy08, "coop + ", ""),
  r_policy09_pun   = semiPartialR3(d1, policy09, "coop + ", ""),
  # extra punishment analysis
  compWorld01 = fitPolicy(d1, "Comp.World01.T06", "coop + ", "pun + "),
  compWorld02 = fitPolicy(d1, "Comp.World02r.T06", "coop + ", "pun + "),
  compWorld03 = fitPolicy(d1, "Comp.World01.T06", "SDO.T10.c + ", ""),
  
  ###########
  # Study 2 #
  ###########
  
  # filter data
  d2 = filterData2(d),
  # table of test-retest reliability
  testRetestTable = makeTestRetestTable(d2),
  # RULE FOLLOWING TASK
  # prepare data for modelling
  d2RF = rfData(d2),
  # spearman's correlation
  corRF = rfCor(d2),
  # distribution
  histRF = rfHist(d2),
  # fit rf models
  rf0 = fitRFModel(d2RF, bf("ruleFollow ~ 0 + Intercept + round + (1 + round | Questionnaire.Num)")),
  rf1 = fitRFModel(d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + (1 + round | Questionnaire.Num)")),
  rf2 = fitRFModel(d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + SDO.T10.c + (1 + round | Questionnaire.Num)")),
  rf3 = fitRFModel(d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + Gender.T10 + (1 + round | Questionnaire.Num)")),
  rf4 = fitRFModel(d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + Age.T10.c + (1 + round | Questionnaire.Num)")),
  rf5 = fitRFModel(d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + NZDepRAW.2013.T10.c + (1 + round | Questionnaire.Num)")),
  rf6 = fitRFModel(d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + EthnicCats.T10 + (1 + round | Questionnaire.Num)")),
  rf7 = fitRFModel(d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + NZREG.T10.c + (1 + round | Questionnaire.Num)")),
  rf8 = fitRFModel(d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + NZSEI13.T10.c + (1 + round | Questionnaire.Num)")),
  rf9 = fitRFModel(d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + SDO.T10.c + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c + (1 + round | Questionnaire.Num)")),
  # kfold rf
  kfoldRF0 = kfold(rf0),
  kfoldRF1 = kfold(rf1),
  kfoldRFcompare = loo_compare(kfoldRF0, kfoldRF1),
  # fit policy rf models
  policyRF01 = fitPolicyRF(d2, bf("Issue.IncomeRedistribution.T10    ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyRF02 = fitPolicyRF(d2, bf("IncomeAttribution.T10             ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyRF03 = fitPolicyRF(d2, bf("Env.SacWilling.T09                ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyRF04 = fitPolicyRF(d2, bf("Issue.SameSexMarriage.T09         ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyRF05 = fitPolicyRF(d2, bf("Issue.Euthanasia.T09              ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyRF06 = fitPolicyRF(d2, bf("Issue.Abortion.AnyReason.T10      ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyRF07 = fitPolicyRF(d2, bf("Issue.Payments.Jobseeker.T06      ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyRF08 = fitPolicyRF(d2, bf("Issue.Payments.SoleParent.T06     ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyRF09 = fitPolicyRF(d2, bf("Issue.TaxPolicy.T05               ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyRF10 = fitPolicyRF(d2, bf("Issue.Abortion.SpecificReason.T10 ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyRF11 = fitPolicyRF(d2, bf("Issue.ReligiousEd.T10             ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyRF12 = fitPolicyRF(d2, bf("RaceEssent.T10                    ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyRF13 = fitPolicyRF(d2, bf("SexualPrejudice01r.T10            ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  # BEAST
  # get beast images
  beastImages = getBeastImages(),
  # prepare data for modelling
  d2BEAST = beastData(d2),
  # distribution
  histBEAST = beastHist(d2),
  # fit beast models
  beast0  = fitBEASTModel(d2BEAST, bf("bScore ~ 0 + Intercept + (1 | round) + (1 | Questionnaire.Num)")),
  beast1  = fitBEASTModel(d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + (1 | round) + (1 | Questionnaire.Num)")),
  beast2  = fitBEASTModel(d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + SDO.T10.c + (1 | round) + (1 | Questionnaire.Num)")),
  beast3  = fitBEASTModel(d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + Gender.T10 + (1 | round) + (1 | Questionnaire.Num)")),
  beast4  = fitBEASTModel(d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + Age.T10.c + (1 | round) + (1 | Questionnaire.Num)")),
  beast5  = fitBEASTModel(d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + NZDepRAW.2013.T10.c + (1 | round) + (1 | Questionnaire.Num)")),
  beast6  = fitBEASTModel(d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + EthnicCats.T10 + (1 | round) + (1 | Questionnaire.Num)")),
  beast7  = fitBEASTModel(d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + NZREG.T10.c + (1 | round) + (1 | Questionnaire.Num)")),
  beast8  = fitBEASTModel(d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + NZSEI13.T10.c + (1 | round) + (1 | Questionnaire.Num)")),
  beast9  = fitBEASTModel(d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + SDO.T10.c + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c + (1 | round) + (1 | Questionnaire.Num)")),
  # kfold beast
  kfoldBEAST0 = kfold(beast0),
  kfoldBEAST1 = kfold(beast1),
  kfoldBEASTcompare = loo_compare(kfoldBEAST0, kfoldBEAST1),
  # plot beast and rf data and predictions
  rfBeastPlot = plotRFBEAST(d2, beast1, rf1),
  # fit policy beast models
  policyBEAST01 = fitPolicyBEAST(d2, bf("Issue.IncomeRedistribution.T10    ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyBEAST02 = fitPolicyBEAST(d2, bf("IncomeAttribution.T10             ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyBEAST03 = fitPolicyBEAST(d2, bf("Env.SacWilling.T09                ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyBEAST04 = fitPolicyBEAST(d2, bf("Issue.SameSexMarriage.T09         ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyBEAST05 = fitPolicyBEAST(d2, bf("Issue.Euthanasia.T09              ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyBEAST06 = fitPolicyBEAST(d2, bf("Issue.Abortion.AnyReason.T10      ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyBEAST07 = fitPolicyBEAST(d2, bf("Issue.Payments.Jobseeker.T06      ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyBEAST08 = fitPolicyBEAST(d2, bf("Issue.Payments.SoleParent.T06     ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyBEAST09 = fitPolicyBEAST(d2, bf("Issue.TaxPolicy.T05               ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyBEAST10 = fitPolicyBEAST(d2, bf("Issue.Abortion.SpecificReason.T10 ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyBEAST11 = fitPolicyBEAST(d2, bf("Issue.ReligiousEd.T10             ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyBEAST12 = fitPolicyBEAST(d2, bf("RaceEssent.T10                    ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  policyBEAST13 = fitPolicyBEAST(d2, bf("SexualPrejudice01r.T10            ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  # plot policy beast models
  policyPlotBEAST = makePolicyGridBEAST(d2, policyBEAST06, policyBEAST10, policyBEAST11, policyBEAST12, policyBEAST13),
  # tables
  rfTable = makeRFtable(rf9),
  beastTable = makeBEASTtable(beast9),
  beastPolicyTable04 = makeBEASTpolicyTable(policyBEAST04),
  beastPolicyTable05 = makeBEASTpolicyTable(policyBEAST05),
  beastPolicyTable06 = makeBEASTpolicyTable(policyBEAST06),
  beastPolicyTable10 = makeBEASTpolicyTable(policyBEAST10),
  beastPolicyTable11 = makeBEASTpolicyTable(policyBEAST11),
  beastPolicyTable12 = makeBEASTpolicyTable(policyBEAST12),
  beastPolicyTable13 = makeBEASTpolicyTable(policyBEAST13),
  
  ##############
  # Manuscript #
  ##############
  
  # render manuscript
  manuscript = rmarkdown::render(knitr_in("manuscript.Rmd"), quiet = TRUE)
)
