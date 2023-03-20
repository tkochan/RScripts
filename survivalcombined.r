library(survival)
library(ggplot2)
library(survminer)
library(dplyr)

# set the working directory to the folder with the .csv files
setwd("~/Desktop/Rscripts/CSV_files")

# get a list of all .csv files in the folder
file_list <- list.files(pattern = "*.csv")

# create an empty list to store the data frames
csv_list <- list()

# loop over the .csv files and load them into R
for (i in 1:length(file_list)) {
    csv_list[[i]] <- read.csv(file_list[i], header=FALSE)
}

# save the list of data frames as an .RData file
save(csv_list, file = "csv_list.RData")

df_list <- list()
df_list_final <- list()
num_csv <- length(csv_list)
for (j in 1:num_csv) {
    data <- csv_list[[j]]
    num_cols <- ncol(data) # Get the total number of columns
    num_rows <- nrow(data)
    # Loop over columns with data
    for (i in 2:num_cols) {

        data2 <- data.frame(Strain = character(),
                            Dose = numeric(),
                            LT = numeric(),
                            Dead = integer(),
                            stringsAsFactors = FALSE)
        data2 <- data2[rep(NA, num_rows), ]

        data2$LT <- data[,1]
        data2$Dead <- data[,i]
        insert_value <- data[1, i]
        data2$Strain[data2$Dead == 1] <- insert_value
        data2$Strain[data2$Dead == 0] <- insert_value

        insert_value <- data[2, i]
        data2$Dose[data2$Dead == 1] <- insert_value
        data2$Dose[data2$Dead == 0] <- insert_value
        data2 <- data2[!is.na(data2$Strain), ]

        df_list[[i]] <- data2
    }
    df_list_final <- append(df_list_final, df_list)
    }

df_combined <- bind_rows(df_list_final)

# create a list of dataframes, one for each unique value in column Strain
df_list_by_strain <- df_combined %>%
    group_split(Strain)

num_dataframes <- length(df_list_by_strain)

plot_list <- list()

for (i in 1:num_dataframes) {
    # Create a survival object
    surv_obj <- Surv(time = df_list_by_strain[[i]]$LT, event = df_list_by_strain[[i]]$Dead == 1)

    # Fit a survival curve
    fit <- survfit(surv_obj ~ Dose, data = df_list_by_strain[[i]])

    # Plot the survival curve
    plot <- ggsurvplot(
        fit,
        data = df_list_by_strain[[i]],
        title = paste0("Survival plot for Strain ", unique(df_list_by_strain[[i]]$Strain)),
        xlab = "Time (hours)",
        xlim = c(0, 350),
        legend.title = "Dose",
        legend = c(0.8, 0.5)
    )
    print(plot)
    plot_list[[i]] <- plot
}
# Save the plots as png files
for (i in 1:num_dataframes) {
    if (i <= length(plot_list)) {
        png(file = paste0("~/Desktop/Rscripts/Plots/AllCombined/", unique(df_list_by_strain[[i]]$Strain), ".png"))
        print(plot_list[[i]])
        dev.off()
    }
}
