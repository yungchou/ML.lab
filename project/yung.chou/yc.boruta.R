#-------------------
# Feature Reduction
#-------------------
library(Boruta)

# Feature Selection
set.seed(1-0)
train.boruta <- Boruta(SalePrice~., data=train.imp, doTrace=2, maxRuns=500)

print(train.boruta);
plot(train.boruta , las=2, cex.axis=0.7, xlab='');
#plotImpHistory(boruta)

train.boruta.fix <- TentativeRoughFix(train.boruta)
train.boruta.selected.features <-  getSelectedAttributes(train.boruta.fix, withTentative = F)

saveRDS(train.boruta.selected.features,
        'boruta/train.boruta.selected.features.rds')

train.boruta.selected.features.stats <- attStats(train.boruta.fix)
saveRDS(train.boruta.selected.features.stats,
        'boruta/train.boruta.selected.features.stats.rds')

