
# Stop the script if zero stops were detected.....
if (FAIL) {
   stop("\n>>>>No stops, so no visualization.", 
     call. = FALSE)
} 


###################################################################################################
# ✅ If user defined subsetAnimals TRUE, then run the first app and return selected animals

if(subsetAnimals) {

cat("\n\nClick STOP to end the visualization and return to the console.\n\n")
flush.console()

app1 <- shinyApp(
  ui = uiF,
  server = serverF
)

# Run the app for choosing a subset of animals....
listAnimals <- runApp(app1, launch.browser = TRUE)

# Show choices in console....
if (length(listAnimals) > 0) {
  cat("You selected:\n")
  print(listAnimals)
} else {
  cat("No selection made or user stopped the script.\n\n>>>>> THE ERRORS BELOW ARE EXPECTED. <<<<<\n\n")
}

} else {
cat("\nAll animals are included because subsetAnimals was defined as: ",subsetAnimals,"\n")
cat("\n\nClick STOP to end the visualization and return to the console.\n\n")
flush.console()

}

###################################################################################################
# It's Show Time....

# First update the data preparation....
prepOut     <- prepare_data()
dataLeaflet <- prepOut$dataLeaflet
lc_colors   <- prepOut$lc_colors
pal         <- prepOut$palette

# Now update the UI object....
uiOut <- build_ui()
ui    <- uiOut$ui

# Define app2 and then run it....
app2 <- shinyApp(
  ui = ui,
  server = server
)

runApp(app2, launch.browser = TRUE)
