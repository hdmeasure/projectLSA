ai_widget_ui <- function() {
  absolutePanel(
    id = "ai_floating_panel",
    fixed = TRUE,
    draggable = FALSE,
    bottom = 20,
    right = 20,
    width = "auto",
    height = "auto",
    class = "floating-widget",
    style = "z-index: 9999; border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.2); background: white; border: 1px solid #ccc; display: flex; flex-direction: column; overflow: hidden;",

    # Header
    div(
      id = "ai_widget_header",
      style = "background: #2563eb; color: white; padding: 8px 12px; cursor: pointer; display: flex; justify-content: space-between; align-items: center; font-weight: bold; border-top-left-radius: 8px; border-top-right-radius: 8px;",
      tags$span(tags$img(src = "logoProjectLSA.png", style = "width: 18px; height: 18px; margin-right: 5px; vertical-align: middle;"), " AI Assistant"),
      tags$div(style = "display: flex; align-items: center; gap: 12px;",
        actionButton("dock_ai_widget",
          label = "", icon = icon("external-link-alt"), title = "View in Tab",
          style = "background: transparent; border: none; color: white; box-shadow: none; padding: 0; font-size: 13px; line-height: 1;"
        ),
        actionButton("toggle_ai_widget",
          label = "", icon = icon("chevron-up"),
          style = "background: transparent; border: none; color: white; box-shadow: none; padding: 0; font-size: 13px; line-height: 1;"
        )
      )
    ),

    # Body
    div(
      id = "ai_widget_body_wrapper",
      style = "display: none;", # Start minimized
      div(
        id = "ai_widget_body",
        style = "width: 380px; height: 450px; display: flex; flex-direction: column; background: #fff; resize: both; overflow: auto; min-width: 300px; min-height: 300px;",
        tabsetPanel(
          id = "ai_widget_tabs",
          type = "tabs",

          # Chat Tab
          tabPanel("Ask AI",
                   conditionalPanel(
                     condition = "input.ai_api_key == '' || input.ai_api_key == null",
                     div(style = "padding: 20px; text-align: center; height: 100%; display: flex; flex-direction: column; justify-content: center; color: #888;",
                         tags$h4(icon("lock", class="fa-2x")),
                         tags$p("Please configure your API Key in the Settings tab first.")
                     )
                   ),
                   conditionalPanel(
                     condition = "input.ai_api_key != '' && input.ai_api_key != null",
                     div(style = "display: flex; flex-direction: column; height: 100%;",
                         # Action row
                         div(style = "display: flex; justify-content: flex-end; align-items: center; padding: 5px 10px; background: #f8f9fa; border-bottom: 1px solid #eee; flex-shrink: 0;",
                             actionLink("ai_global_chat_clear", "Clear chat", style = "font-size: 12px; color: #dc3545; margin-right: auto;"),
                             tags$button(
                               class = "btn btn-default btn-xs", icon("download"), " Export HTML",
                               onclick = "downloadAiChat()",
                               style = "font-size: 12px;"
                             )
                         ),
                         # History
                         div(
                           id = "ai_chat_history_container",
                           style = "flex: 1; overflow-y: auto; padding: 15px; background: #ffffff; font-size: 13px;",
                           uiOutput("ai_global_chat_history")
                         ),
                         # Input area
                         div(
                           style = "padding: 10px; border-top: 1px solid #ddd; background: #f8f9fa; display: flex; gap: 8px; align-items: center; flex-shrink: 0;",
                           textInput("ai_global_chat_input", label = NULL, placeholder = "Type a question... (press Enter)", width = "100%"),
                           actionButton("ai_global_chat_send", label = "", icon = icon("paper-plane"), class = "btn-primary")
                         )
                     )
                   )
          ),
          
          # Summary Tab
          tabPanel("Create Summary",
                   conditionalPanel(
                     condition = "input.ai_api_key == '' || input.ai_api_key == null",
                     div(style = "padding: 20px; text-align: center; height: 100%; display: flex; flex-direction: column; justify-content: center; color: #888;",
                         tags$h4(icon("lock", class="fa-2x")),
                         tags$p("Please configure your API Key in the Settings tab first.")
                     )
                   ),
                   conditionalPanel(
                     condition = "input.ai_api_key != '' && input.ai_api_key != null",
                     div(style = "padding: 15px; height: 100%; display: flex; flex-direction: column; flex-grow: 1; background: #ffffff;",
                         # Context Inputs
                         bsCollapse(id = "ai_context_collapse",
                           bsCollapsePanel("Research Context (Optional)", style = "info",
                             textAreaInput("ai_context_text", "Theoretical background, hypotheses, etc.:", rows = 3, width = "100%", placeholder = "E.g., The goal of this research is to measure..."),
                             fileInput("ai_context_file", "Or upload document (txt/pdf/docx)", accept = c(".txt", ".pdf", ".docx"), width = "100%")
                           )
                         ),
                         
                         # Inputs
                         div(style = "display: flex; gap: 10px; margin-bottom: 10px;",
                             div(style="flex:1;",
                                 selectInput("ai_summary_format", "Format:", choices = c("Manuscript (Narrative)" = "Manuscript", "APA 7" = "APA 7", "Paragraph" = "Paragraf", "Bullet" = "Poin-poin", "Table" = "Tabel"), width="100%")
                             ),
                             div(style="flex:1;",
                                 selectInput("ai_summary_lang", "Language:", choices = c("English", "Indonesian"), width="100%")
                             )
                         ),
                         # Generate button
                         actionButton("ai_global_summary_generate", "Generate Summary from Analysis Results", class = "btn-success", icon = icon("wand-magic-sparkles"), style = "width: 100%; margin-bottom: 10px; font-weight: bold;"),
                         
                         # Result tools
                         div(style = "display: flex; justify-content: flex-end; gap: 8px; margin-bottom: 5px;",
                             tags$button(
                               id = "copy_summary_btn", class = "btn btn-default btn-xs", icon("copy"), " Copy Text",
                               onclick = "
                                 let temp = $('<textarea>'); $('body').append(temp);
                                 temp.val($('#ai_global_summary_result')[0].innerText).select();
                                 document.execCommand('copy'); temp.remove();
                                 let btn = $(this); let originalText = btn.html();
                                 btn.html('<i class=\"fa fa-check\"></i> Copied!');
                                 setTimeout(function() { btn.html(originalText); }, 2000);
                               ",
                               style = "font-size: 12px;"
                             ),
                             actionButton("ai_add_to_report", tagList(icon("file-medical"), " Add to Report"), class = "btn btn-default btn-xs", style = "font-size: 12px; padding: 1px 5px;")
                         ),
                         # Summary Output
                         div(
                           id = "ai_summary_result_container",
                           style = "flex: 1; overflow-y: auto; border: 1px solid #ddd; padding: 12px; border-radius: 4px; background: #f8f9fa; font-size: 13px;",
                           uiOutput("ai_global_summary_result")
                         )
                     )
                   )
          ),
          
          # Settings Tab
          tabPanel("Settings",
            icon = icon("cog"),
            div(
              style = "padding: 15px; text-align: left; height: 400px; display: flex; flex-direction: column; overflow-y: auto;",
              tags$b(icon("cog"), " AI Assistant Setup"),
              tags$hr(style = "margin-top: 10px; margin-bottom: 15px;"),
              selectInput("ai_provider", "Provider:",
                choices = c(
                  "Google Gemini" = "gemini",
                  "OpenAI" = "openai",
                  "Groq" = "groq",
                  "OpenRouter" = "openrouter"
                ),
                width = "100%"
              ),
              textInput("ai_model", "Model:", value = "gemini-2.5-flash", placeholder = "e.g., gemini-2.5-flash", width = "100%"),
              passwordInput("ai_api_key", "API Key:", placeholder = "Enter your API key here", width = "100%"),
              div(
                style = "display: flex; gap: 10px; align-items: center; margin-top: 10px;",
                actionButton("test_ai_connection", "Test Connection", icon = icon("plug"), class = "btn-primary"),
                uiOutput("ai_connection_status_ui")
              ),
              tags$hr(style = "margin-top: 15px; margin-bottom: 15px;"),
              div(
                style = "font-size: 11px; margin-bottom: 15px; background: #f8f9fa; padding: 10px; border-radius: 5px;",
                tags$b("How to get an API Key:"),
                tags$ul(
                  style = "padding-left: 15px; margin-bottom: 0;",
                  tags$li("Gemini: ", tags$a(href = "https://aistudio.google.com/app/apikey", target = "_blank", "Google AI Studio")),
                  tags$li("OpenAI: ", tags$a(href = "https://platform.openai.com/api-keys", target = "_blank", "OpenAI Platform")),
                  tags$li("Groq: ", tags$a(href = "https://console.groq.com/keys", target = "_blank", "Groq Console")),
                  tags$li("OpenRouter: ", tags$a(href = "https://openrouter.ai/keys", target = "_blank", "OpenRouter"))
                )
              ),
              tags$small(style = "color: #666;", "Required to use the AI interpretation feature across all modules.")
            )
          )
        )
      )
    ),

    # Simple JS to change icon on toggle
    tags$script(HTML("
      // Export the current chat transcript as a standalone HTML file
      function downloadAiChat() {
        var el = document.getElementById('ai_chat_history_container');
        var body = el ? el.innerHTML : '';
        if (!body || body.trim() === '') {
          alert('There is no conversation to export yet.');
          return;
        }
        var stamp = new Date().toISOString().slice(0, 10);
        var html = '<!DOCTYPE html><html><head><meta charset=\"utf-8\">' +
                   '<title>projectLSA AI Assistant Chat</title>' +
                   '<style>body{font-family:Segoe UI,Arial,sans-serif;font-size:13px;' +
                   'line-height:1.6;max-width:860px;margin:30px auto;padding:0 20px;color:#222;}' +
                   'h1{font-size:18px;border-bottom:2px solid #2563eb;padding-bottom:6px;}' +
                   'b{color:#2563eb;}</style></head><body>' +
                   '<h1>projectLSA - AI Assistant Chat (' + stamp + ')</h1>' +
                   body + '</body></html>';
        var blob = new Blob([html], {type: 'text/html;charset=utf-8'});
        var a = document.createElement('a');
        a.href = URL.createObjectURL(blob);
        a.download = 'projectLSA_AI_chat_' + stamp + '.html';
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(a.href);
      }

      // Send chat on Enter
      $(document).on('keydown', '#ai_global_chat_input', function(e) {
        if (e.which === 13 && !e.shiftKey) {
          e.preventDefault();
          $('#ai_global_chat_send').click();
        }
      });

      $(document).on('click', '#toggle_ai_widget', function(e) {
        e.preventDefault();
        var icon = $(this).find('i');
        var wrapper = $('#ai_widget_body_wrapper');
        if (wrapper.is(':visible')) {
          wrapper.hide();
          icon.removeClass('fa-chevron-down').addClass('fa-chevron-up');
        } else {
          wrapper.show();
          icon.removeClass('fa-chevron-up').addClass('fa-chevron-down');
        }
      });
      // Click header to toggle
      $(document).on('click', '#ai_widget_header', function(e) {
        if ($(e.target).closest('button').length === 0) {
          var wrapper = $('#ai_widget_body_wrapper');
          var icon = $('#toggle_ai_widget i');
          if (wrapper.is(':visible')) {
            wrapper.hide();
            icon.removeClass('fa-chevron-down').addClass('fa-chevron-up');
          } else {
            wrapper.show();
            icon.removeClass('fa-chevron-up').addClass('fa-chevron-down');
          }
        }
      });
      // Scroll to bottom on new message
      Shiny.addCustomMessageHandler('scroll_ai_chat', function(msg) {
        setTimeout(function() {
          var objDiv = document.getElementById('ai_chat_history_container');
          if (objDiv) objDiv.scrollTop = objDiv.scrollHeight;
        }, 100);
      });
    ")),

    # CSS to make sure the tabsetPanel and tab-content take up full height
    tags$style(HTML("
      #ai_widget_body .tab-content {
        flex: 1;
        display: flex;
        flex-direction: column;
        overflow: hidden;
      }
      #ai_widget_body .tab-pane.active {
        flex: 1;
        display: flex;
        flex-direction: column;
        height: 100%;
      }
    ")),
    tags$script(HTML("
      // Locate the module's main tabset.
      // The CFA/SEM module nests its tabset inside a col-sm-9; LPA, LCA, LTA and
      // EFA put their tabset at the top level of the module, so fall back to the
      // first visible tabset that is not part of the AI widget itself.
      function findHostTabset() {
        let candidates = $('.col-sm-9:visible').filter(function() {
          return $(this).find('.nav-tabs').length > 0;
        });
        let navTabs = null, tabContent = null;
        if (candidates.length) {
          navTabs = candidates.first().find('.nav-tabs').first();
          tabContent = candidates.first().find('.tab-content').first();
        } else {
          navTabs = $('.nav-tabs:visible').not('#ai_widget_body .nav-tabs').first();
          tabContent = navTabs.length ? navTabs.nextAll('.tab-content').first() : $();
        }
        return {navTabs: navTabs, tabContent: tabContent};
      }

      $(document).on('click', '#dock_ai_widget', function(e) {
        e.preventDefault();
        let aiBody = $('#ai_widget_body');

        let host = findHostTabset();
        let navTabs = host.navTabs;
        let tabContent = host.tabContent;

        if (navTabs.length && tabContent.length) {
          let tabId = 'ai_assistant_docked_tab_' + Date.now();
          aiBody.attr('data-docked-tab-id', tabId);

          navTabs.append('<li><a href=\"#' + tabId + '\" data-toggle=\"tab\">AI Assistant <span class=\"undock-ai-btn\" style=\"margin-left:8px;cursor:pointer;color:#888;\" title=\"Float Assistant\"><i class=\"fa fa-window-restore\"></i></span></a></li>');
          tabContent.append('<div class=\"tab-pane\" id=\"' + tabId + '\" style=\"padding-top: 15px;\"></div>');

          aiBody.css({
             'width': '100%',
             'height': '75vh',
             'resize': 'none',
             'border': '1px solid #ddd',
             'box-shadow': 'none'
          });

          $('#' + tabId).append(aiBody);
          $('#ai_floating_panel').hide();

          navTabs.find('a[href=\"#' + tabId + '\"]').tab('show');
        } else {
          alert('Cannot dock AI in this module layout. Try using it here.');
        }
      });

      // Undock logic
      $(document).on('click', '.undock-ai-btn', function(e) {
        e.preventDefault(); e.stopPropagation();
        let aiBody = $('#ai_widget_body');

        aiBody.css({
           'width': '380px',
           'height': '450px',
           'resize': 'both',
           'border': 'none'
        });

        $('#ai_widget_header').next().append(aiBody);
        $('#ai_floating_panel').show();

        let tabId = aiBody.attr('data-docked-tab-id');
        if (tabId) {
           $('a[href=\"#' + tabId + '\"]').parent('li').remove();
           $('#' + tabId).remove();
        }

        // Go back to the first tab of the host module
        let host = findHostTabset();
        if (host.navTabs.length) {
            host.navTabs.find('li:first-child a').tab('show');
        }
      });
    "))
  )
}
