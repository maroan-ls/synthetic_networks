# Other small measures calculated that are mentioned

all.equal(sum(weighted_degree$Out_Strength), sum(out_strength))
max(weighted_degree$Out_Strength)/sum(weighted_degree$Out_Strength)
sum(head(sort(weighted_degree$Out_Strength,decreasing=T), n=10))/sum(weighted_degree$Out_Strength)


lenght(out_deg == 0)/vcount()




## For creating Result Table

#install.packages("stargazer")

# Load the package
library(stargazer)

# Export ERGM model results as LaTeX table
stargazer(f14_summary, type = "latex", title = "ERGM Results", out = "ergm_results.tex")



f14_summary <- summary(fit_weight14)
f14_summary


library(xtable)
print(xtable(f14_summary$coefficients), type = "latex", file = "ergm_table.tex")


install.packages("texreg")
library(texreg)
# Save ERGM model output as a LaTeX table
texreg(fit_weight14, file = "ergm_results.tex", 
       caption = "ERGM Model Results",
       label = "tab:ergm",
       digits = 3,
       include.loglik = TRUE)
