# To begin, select "Site Info" from a course area on Sakai, then "Printable
# version" next to the participants list.  Clean up the list so that it has only
# one entry per line (and remove header lines).
#
# Make sure that the given group name exists before attempting to import the new
# users.

# Main function call
makeTable <- function( sourcePath, savePath, groupName,
                       timelimit    = "-1",
                       jobtimelimit = "-1"
                       ) {
  
  # Read data from the file
  lines     <- readLines(sourcePath)
  
  # capture elements with a regular expression
  matches   <- regexec("(.*?)\\,\\s(.*?)\\(\\s(.*?)\\s\\)\\s(.*)", lines)
  matchList <- regmatches(lines, matches)
  
  # Extract based on match group in the RE
  lastName  <- vapply(matchList, function(n) { n[2] }, character(1))
  firstName <- vapply(matchList, function(n) { n[3] }, character(1))
  userName  <- vapply(matchList, function(n) { n[4] }, character(1))
  status    <- vapply(matchList, function(n) { n[5] }, character(1))
  
  # Figure out which entries are students
  studentFilter <- grep("Student", status)
  
  # Create a new data.frame
  table <- data.frame(
      Username      = userName[studentFilter],
      Group         = rep(groupName, length(studentFilter)),
      Password      = lastName[studentFilter],
      FullName      = paste( firstName[studentFilter], lastName[studentFilter]),
      Email         = paste(userName[studentFilter], "@wlu.edu", sep = ""),
      TimeLimit     = rep(timelimit, length(studentFilter)),
      JobTimeLimit  = rep(jobtimelimit, length(studentFilter))
    )
  
  write.table(table,
              file      = savePath,
              quote     = FALSE,
              sep       = "\t",
              row.names = FALSE)
  
  return(table)
  
}
