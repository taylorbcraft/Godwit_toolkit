library(cronR)

# ## remove schedule
# cron_rm(id = "update_gps")

# Define the cron command to run the script every day
cmd <- cron_rscript("location_data.R")

# Schedule the task to run every hour
cron_add(cmd, frequency = "daily", id = "update_gps", description = "Update GPS tracking data")
