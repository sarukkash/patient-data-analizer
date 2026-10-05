# ============================================================
# SIMPLE RESUME ANALYZER - SHINY APP
# ============================================================

# Load Shiny
library(shiny)

# ------------------------------------------------------------
# User Interface
# ------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Resume Analyzer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3("Login"),
      
      textInput(
        inputId = "username",
        label = "Username:",
        placeholder = "Enter username"
      ),
      
      passwordInput(
        inputId = "password",
        label = "Password:",
        placeholder = "Enter password"
      ),
      
      actionButton(
        inputId = "login",
        label = "Login"
      ),
      
      br(),
      br(),
      
      textOutput("login_message"),
      
      hr(),
      
      h3("Resume Analysis"),
      
      fileInput(
        inputId = "resume",
        label = "Upload Resume",
        accept = c(".txt", ".pdf")
      ),
      
      actionButton(
        inputId = "analyze",
        label = "Analyze Resume"
      )
      
    ),
    
    mainPanel(
      
      h3("Analysis Result"),
      
      textOutput("result"),
      
      br(),
      
      h4("Skills Found"),
      
      tableOutput("skills")
      
    )
  )
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # Login status
  logged_in <- reactiveVal(FALSE)
  
  # ----------------------------------------------------------
  # Login
  # ----------------------------------------------------------
  
  observeEvent(input$login, {
    
    if (input$username == "admin" &&
        input$password == "1234") {
      
      logged_in(TRUE)
      
      output$login_message <- renderText({
        "Login successful!"
      })
      
    } else {
      
      logged_in(FALSE)
      
      output$login_message <- renderText({
        "Invalid username or password."
      })
      
    }
    
  })
  
  # ----------------------------------------------------------
  # Resume Analysis
  # ----------------------------------------------------------
  
  observeEvent(input$analyze, {
    
    # Check login
    if (!logged_in()) {
      
      output$result <- renderText({
        "Please login first."
      })
      
      return()
    }
    
    # Check file
    if (is.null(input$resume)) {
      
      output$result <- renderText({
        "Please upload a resume."
      })
      
      return()
    }
    
    # Read uploaded file
    file_name <- input$resume$name
    
    # --------------------------------------------------------
    # TXT file
    # --------------------------------------------------------
    
    if (grepl("\\.txt$", file_name, ignore.case = TRUE)) {
      
      resume_text <- paste(
        readLines(
          input$resume$datapath,
          warn = FALSE
        ),
        collapse = " "
      )
      
    }
    
    # --------------------------------------------------------
    # PDF file
    # --------------------------------------------------------
    
    else if (grepl("\\.pdf$", file_name, ignore.case = TRUE)) {
      
      if (!requireNamespace("pdftools", quietly = TRUE)) {
        
        output$result <- renderText({
          "PDF support requires the pdftools package."
        })
        
        return()
      }
      
      resume_text <- paste(
        pdftools::pdf_text(input$resume$datapath),
        collapse = " "
      )
      
    }
    
    else {
      
      output$result <- renderText({
        "Please upload a TXT or PDF file."
      })
      
      return()
    }
    
    # Convert to lowercase
    resume_text <- tolower(resume_text)
    
    # --------------------------------------------------------
    # Skill database
    # --------------------------------------------------------
    
    skills_db <- c(
      "python",
      "r",
      "java",
      "c",
      "c++",
      "sql",
      "mysql",
      "html",
      "css",
      "javascript",
      "react",
      "node",
      "machine learning",
      "deep learning",
      "artificial intelligence",
      "tensorflow",
      "pytorch",
      "excel",
      "power bi",
      "tableau",
      "git",
      "github",
      "linux",
      "cloud",
      "aws",
      "azure"
    )
    
    # --------------------------------------------------------
    # Find skills
    # --------------------------------------------------------
    
    found_skills <- c()
    
    for (skill in skills_db) {
      
      if (grepl(
        fixed(skill),
        resume_text,
        ignore.case = TRUE
      )) {
        
        found_skills <- c(
          found_skills,
          skill
        )
        
      }
      
    }
    
    # --------------------------------------------------------
    # Calculate score
    # --------------------------------------------------------
    
    score <- length(found_skills) /
      length(skills_db) * 100
    
    # --------------------------------------------------------
    # Display score
    # --------------------------------------------------------
    
    output$result <- renderText({
      
      paste(
        "Resume analyzed successfully!",
        "\nMatch Score:",
        round(score, 2),
        "%"
      )
      
    })
    
    # --------------------------------------------------------
    # Display skills
    # --------------------------------------------------------
    
    output$skills <- renderTable({
      
      if (length(found_skills) == 0) {
        
        data.frame(
          Skills = "No skills found"
        )
        
      } else {
        
        data.frame(
          Skills = found_skills
        )
        
      }
      
    })
    
  })
  
}

# ------------------------------------------------------------
# Run Shiny App
# ------------------------------------------------------------

shiny::shinyApp(
  ui = ui,
  server = server
)