# Student Marks Analyzer ----

# Step 1: Input students and subjects ----
n <- as.integer(readline("Enter number of students: "))
students <- character(n)
for(i in 1:n){
  students[i] <- readline(paste("Enter name of student", i, ": "))
}

s <- as.integer(readline("Enter number of subjects: "))
subjects <- character(s)
for(j in 1:s){
  subjects[j] <- readline(paste("Enter name of subject", j, ": "))
}

# Step 2: Create marks data frame ----
marks_df <- data.frame(Student = students)

for (sub in subjects) {
  marks <- numeric(n)
  for (i in 1:n) {
    marks[i] <- as.numeric(readline(paste("Enter", sub, "marks for", students[i], ": ")))
  }
  marks_df[[sub]] <- marks # for adding new column
}

# Step 3: Add total & average columns ----
marks_df$Total <- rowSums(marks_df[ , -1])
marks_df$Average <- rowMeans(marks_df[ , -1])

# Step 4: Menu-driven analysis ----
repeat {
  cat("\n--- Student Marks Analyzer Menu ---\n")
  cat("1. Show Marks Table\n")
  cat("2. Subject-wise Totals\n")
  cat("3. Student-wise Totals\n")
  cat("4. Pass Students\n")
  cat("5. Fail Students\n")
  cat("6. Show Topper\n")
  cat("7. Plot Student-wise Totals (Bar Graph)\n")
  cat("8. Plot Subject-wise Averages (Bar Graph)\n")
  cat("9. Plot Pass vs Fail (Pie Chart)\n")
  cat("10. Save Data to CSV\n")
  cat("11. Exit\n")
  
  choice <- as.integer(readline("Enter your choice: "))
  
  if(choice == 1){
    print(marks_df)
  }
  else if(choice == 2){
    cat("\nSubject-wise Totals:\n")
    print(colSums(marks_df[ , subjects]))
  }
  else if(choice == 3){
    cat("\nStudent-wise Totals:\n")
    print(marks_df[, c("Student","Total")])
  }
  else if(choice == 4){
    cat("\nPass Students (Average >= 40):\n")
    print(marks_df[marks_df$Average >= 40, c("Student","Average")])
  }
  else if(choice == 5){
    cat("\nFail Students (Average < 40):\n")
    print(marks_df[marks_df$Average < 40, c("Student","Average")])
  }
  else if(choice == 6){
    topper <- marks_df[which.max(marks_df$Total), ]
    cat("\nTopper:\n")
    print(topper)
  }
  else if(choice == 7){
    barplot(marks_df$Total,
            names.arg = marks_df$Student,
            col = "skyblue",
            main = "Total Marks per Student",
            xlab = "Students",
            ylab = "Total Marks")
  }
  else if(choice == 8){
    subject_avg <- colMeans(marks_df[ , subjects])
    barplot(subject_avg,
            col = "lightgreen",
            main = "Average Marks per Subject",
            xlab = "Subjects",
            ylab = "Average Marks")
  }
  else if(choice == 9){
    result <- ifelse(marks_df$Average >= 40, "Pass", "Fail")
    pie(table(result),
        col = c("green","red"),
        main = "Pass vs Fail Students")
  }
  else if(choice == 10){
    filename <- readline("Enter filename to save (e.g. marks.csv): ")
    write.csv(marks_df, filename, row.names = FALSE)
    cat("Data saved successfully to", filename, "\n")
  }
  else if(choice == 11){
    cat("Exiting program...\n")
    break
  }
  else {
    cat("Invalid choice! Please try again.\n")
  }
}