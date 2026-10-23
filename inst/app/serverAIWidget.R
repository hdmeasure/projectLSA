# serverAIWidget.R

# Answer text -> HTML for a chat bubble (escaped, line breaks kept), with a note on
# which model answered and whether a backup was needed.
ai_answer_html <- function(r) {
  if (!isTRUE(r$ok)) return(list(ok = FALSE, html = htmltools::htmlEscape(r$message)))
  via <- paste0("via ", htmltools::htmlEscape(r$via$provider), " \u00B7 ", htmltools::htmlEscape(r$via$model),
                if (isTRUE(r$fallback)) " (backup model)" else "")
  list(ok = TRUE, html = paste0(gsub("\n", "<br>", htmltools::htmlEscape(r$text)), "<div class='ai-via'>", via, "</div>"))
}

server_ai_widget <- function(input, output, session, ai_context, ai_state) {
  
  # Chat History State
  ai_chat_history <- reactiveVal(character(0))
  
  # Summary Result State
  ai_summary_res <- reactiveVal("")
  
  # Helper to format chat history as HTML
  output$ai_global_chat_history <- renderUI({
    hist <- ai_chat_history()
    if (length(hist) == 0) {
      return(HTML("<div class='ai-placeholder'>No conversation yet. Ask me anything about your current results.</div>"))
    }
    HTML(paste(hist, collapse = ""))
  })
  
  # Render Summary
  output$ai_global_summary_result <- renderUI({
    res <- ai_summary_res()
    if (res == "") {
      return(HTML("<div class='ai-placeholder'>Your summary will appear here.</div>"))
    }
    HTML(res)
  })
  
  # Chat Send Logic
  observeEvent(input$ai_global_chat_send, {
    prompt <- trimws(input$ai_global_chat_input)
    if (prompt == "") return()
    
    # Update input to empty
    updateTextInput(session, "ai_global_chat_input", value = "")
    
    # Add User message
    current_chat <- ai_chat_history()
    user_msg <- paste0("<div class='ai-bubble ai-me'>", gsub("\n", "<br>", htmltools::htmlEscape(prompt)), "</div>")
    current_chat <- c(current_chat, user_msg)
    ai_chat_history(current_chat)
    
    session$sendCustomMessage("scroll_ai_chat", "scroll")
    
    # We could show a thinking indicator or just append it
    current_chat <- c(current_chat, "<div id='ai_thinking' class='ai-bubble ai-bot ai-typing'><span></span><span></span><span></span></div>")
    ai_chat_history(current_chat)
    session$sendCustomMessage("scroll_ai_chat", "scroll")
    
    # Construct full prompt with context
    ctx_text <- ai_context$results_text
    sys_prompt <- "You are an AI assistant for projectLSA. You are in a conversational Q&A mode. The user's latest analysis results are provided as Context. ONLY answer the user's specific Question. DO NOT generate a full summary of the Context unless explicitly asked by the user in the Question."
    full_prompt <- paste0(sys_prompt, "\n\nContext:\n", ctx_text, "\n\nQuestion:\n", prompt)
    
    # Call AI
    tryCatch({
      ans <- ai_answer_html(ai_ask_state(ai_state, full_prompt))

      # Remove thinking and add response
      current_chat <- current_chat[-length(current_chat)]
      ai_msg <- paste0("<div class='ai-bubble ai-bot", if (!ans$ok) " ai-error", "'>", ans$html, "</div>")
      current_chat <- c(current_chat, ai_msg)
      ai_chat_history(current_chat)
      
    }, error = function(e) {
      current_chat <- current_chat[-length(current_chat)]
      err_msg <- paste0("<div class='ai-bubble ai-bot ai-error'>", htmltools::htmlEscape(e$message), "</div>")
      current_chat <- c(current_chat, err_msg)
      ai_chat_history(current_chat)
    })
    
    session$sendCustomMessage("scroll_ai_chat", "scroll")
  })
  
  # Clear Chat
  observeEvent(input$ai_global_chat_clear, {
    ai_chat_history(character(0))
  })
  
  # Generate Summary Logic
  observeEvent(input$ai_global_summary_generate, {
    ctx_text <- ai_context$results_text
    
    if (is.null(ctx_text) || trimws(ctx_text) == "") {
      ai_summary_res("<span style='color:red;'>No active analysis results. Please run an analysis first.</span>")
      return()
    }
    
    ai_summary_res("<i>Generating summary...</i>")
    
    format <- input$ai_summary_format
    lang <- input$ai_summary_lang
    
    # Parse context
    context_str <- ""
    if (!is.null(input$ai_context_text) && trimws(input$ai_context_text) != "") {
      context_str <- trimws(input$ai_context_text)
    }
    
    if (!is.null(input$ai_context_file)) {
      file_path <- input$ai_context_file$datapath
      ext <- tools::file_ext(input$ai_context_file$name)
      file_content <- tryCatch({
        if (tolower(ext) == "txt") {
          paste(readLines(file_path, warn = FALSE), collapse = "\n")
        } else if (tolower(ext) == "pdf" && requireNamespace("pdftools", quietly = TRUE)) {
          paste(pdftools::pdf_text(file_path), collapse = "\n")
        } else if (tolower(ext) == "docx" && requireNamespace("officer", quietly = TRUE)) {
          doc <- officer::read_docx(file_path)
          content <- officer::docx_summary(doc)
          paste(content$text[!is.na(content$text)], collapse = "\n")
        } else {
          ""
        }
      }, error = function(e) { paste("Error reading file:", e$message) })
      
      if (file_content != "") {
        context_str <- paste0(context_str, "\n\n[Context from uploaded document:]\n", file_content)
      }
    }
    
    context_prompt_part <- ""
    if (context_str != "") {
      context_prompt_part <- paste0("TAKE THE FOLLOWING RESEARCH CONTEXT INTO ACCOUNT (use it to tailor your interpretation):\n", context_str, "\n\n")
    }

    # Manuscript format specific instruction
    manuscript_instruction <- ""
    if (format == "Manuscript") {
      manuscript_instruction <- paste0(
        "Because the requested format is 'Manuscript (Narrative)', you MUST write a journal-article style report. ",
        "Compose complete, flowing, professional paragraphs. ",
        "You MUST refer to tables and figures within the text (e.g. 'As presented in Table 1...' or 'Based on Figure 1...'). ",
        "You MUST insert a placeholder immediately after the paragraph that refers to a table/figure, using the format `[Insert Table/Figure X here]`. ",
        "Do not build the table itself; only write the placeholder so the user can paste the actual table from the application later.\n"
      )
    }

    prompt <- paste0(
      "Your ONLY task is to produce a comprehensive summary of the analysis results below. ",
      "Do NOT add chat greetings, small talk, or any conversational text.\n",
      context_prompt_part,
      "IMPORTANT: If the results include a history of several models (such as Model_1, Model_2, etc.), ",
      "describe the condition of the initial model, the reason for and details of the modifications applied to the subsequent models, ",
      "and how those modifications improved the final result (model comparison).\n",
      manuscript_instruction,
      "REQUIRED: Always include in-text citations for any standard or cut-off criterion you invoke (e.g. Hu & Bentler, 1999; Hair et al., 2010; Schreiber et al., 2006; Nylund et al., 2007). ",
      "In addition, you MUST add a dedicated section titled 'References' at the end of your summary containing the full reference list for every citation you used, so that the references are exported together with the summary.\n\n",
      "Requested format: ", format, "\n",
      "Write the summary in this language: ", lang, "\n\n",
      "Analysis Results:\n", ctx_text
    )
    
    tryCatch({
      ans <- ai_answer_html(ai_ask_state(ai_state, prompt))
      ai_summary_res(if (ans$ok) ans$html else paste0("<span style='color:#c4515a;'>", ans$html, "</span>"))
    }, error = function(e) {
      ai_summary_res(paste0("<span style='color:red;'>Error: ", e$message, "</span>"))
    })
  })

  # Add to report
  observeEvent(input$ai_add_to_report, {
    res <- ai_summary_res()
    if (res != "" && !grepl("<i>Generating summary...</i>", res)) {
      ai_context$ai_report_text <- res
      showNotification("AI Summary added! It will be included when you export HTML report.", type = "message", duration = 5)
    } else {
      showNotification("Please generate an AI summary first.", type = "warning")
    }
  })

}
