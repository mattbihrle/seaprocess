# Print Session Info------------------------------------------------------------
# Run this line to log package versions used to analyze data in this script.
sink("eoc/seaprocess_session_packages.txt")
# Log System Time
print("System Date and Time")
print(Sys.time())

# Log Package Information
print("SeaProcess Session Info")
print(sessionInfo())
# Return the output to the console
sink()
