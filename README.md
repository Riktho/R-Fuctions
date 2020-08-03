##### Function for getting Percentage Missing Records for Variables #####

missingRec <- function(x) {

df <- as.data.frame(summary(is.na(x)))
df_1 <- df[seq(2,nrow(df),3),]
df_1 <- df_1 %>% 
                select(Var2, Freq) %>%
                mutate(nonmissing = as.numeric(str_replace(df_1$Freq, "FALSE:", "")),
                       missingPerc = 1 - (nonmissing/nrow(x))) %>%
                arrange(desc(missingPerc))
}

missingdf <- missingRec(flights)

