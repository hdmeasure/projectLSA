ai_widget_ui <- function() {
  # Shown on a tab while no API key has been entered
  need_key <- function() {
    conditionalPanel(
      condition = "!output.ai_ready",
      div(
        class = "pl-ai-empty",
        tags$img(src = "avatar_stiker.png", alt = ""),
        tags$b("I need an API key first"),
        tags$p("Add a model in the AI settings (top right). A free-tier provider is enough to start, and keys stay in this session only."),
        tags$button(
          type = "button", class = "btn btn-primary btn-sm", onclick = "plAiDrawer(true)",
          icon("gear"), " Open AI settings"
        )
      )
    )
  }
  has_key <- "output.ai_ready"

  absolutePanel(
    id = "ai_floating_panel", fixed = TRUE, draggable = FALSE,
    bottom = 20, right = 20, width = "auto", height = "auto",
    class = "floating-widget pl-ai",

    # Header: doubles as the launcher while the panel is collapsed
    div(
      id = "ai_widget_header", class = "pl-ai-head",
      tags$span(
        class = "pl-ai-title",
        tags$img(src = "avatar_kepala.png", class = "pl-ai-avatar", alt = ""),
        tags$span(tags$b("AI Assistant"), tags$small("Ask about your results"))
      ),
      tags$div(
        class = "pl-ai-actions",
        actionButton("toggle_ai_widget", label = "", icon = icon("chevron-up"),
                     title = "Show or hide", class = "pl-ai-iconbtn")
      )
    ),

    # Body
    div(
      id = "ai_widget_body_wrapper", style = "display: none;",
      div(
        id = "ai_widget_body", class = "pl-ai-body",
        uiOutput("ai_switcher_ui"),
        tabsetPanel(
          id = "ai_widget_tabs", type = "pills", selected = "Ask AI",

          # ---- guide for the open tab (filled by the page; see projectlsa.js) ----
          tabPanel(
            "Guide", icon = icon("compass"),
            div(
              class = "pl-ai-pane pl-ai-scroll pl-guide",
              tags$img(src = "avatar_stiker.png", alt = "", class = "pl-guide-avatar"),
              div(id = "pl-coach-msg", class = "pl-coach-msg", `aria-live` = "polite"),
              div(id = "pl-coach-actions", class = "pl-coach-actions")
            )
          ),

          # ---- chat ----
          tabPanel(
            "Ask AI", icon = icon("comments"),
            need_key(),
            conditionalPanel(
              condition = has_key,
              div(
                class = "pl-ai-pane",
                div(
                  class = "pl-ai-bar",
                  actionLink("ai_global_chat_clear", icon("trash-can"), title = "Clear the conversation", class = "pl-ai-tool"),
                  tags$button(type = "button", class = "pl-ai-tool", title = "Export the conversation as HTML", icon("download"), onclick = "downloadAiChat()")
                ),
                div(id = "ai_chat_history_container", class = "pl-ai-history", uiOutput("ai_global_chat_history")),
                div(
                  class = "pl-ai-compose",
                  textInput("ai_global_chat_input", label = NULL, placeholder = "Ask about your results... (Enter to send)", width = "100%"),
                  actionButton("ai_global_chat_send", label = "", icon = icon("paper-plane"), class = "btn-primary")
                )
              )
            )
          ),

          # ---- summary ----
          tabPanel(
            "Create Summary", icon = icon("wand-magic-sparkles"),
            need_key(),
            conditionalPanel(
              condition = has_key,
              div(
                class = "pl-ai-pane pl-ai-scroll",
                bsCollapse(
                  id = "ai_context_collapse",
                  bsCollapsePanel(
                    "Add context (optional)", style = "default",
                    textAreaInput("ai_context_text", "Theoretical background, hypotheses, ...", rows = 3, width = "100%",
                                  placeholder = "E.g., The goal of this research is to measure..."),
                    fileInput("ai_context_file", "Or upload a document (txt, pdf, docx)", accept = c(".txt", ".pdf", ".docx"), width = "100%")
                  )
                ),
                div(
                  class = "pl-ai-row",
                  selectInput("ai_summary_format", "Format",
                              choices = c("Manuscript (narrative)" = "Manuscript", "APA 7" = "APA 7", "Paragraph" = "Paragraf",
                                          "Bullet points" = "Poin-poin", "Table" = "Tabel"), width = "100%"),
                  selectInput("ai_summary_lang", "Language", choices = c("English", "Indonesian"), width = "100%")
                ),
                actionButton("ai_global_summary_generate", "Write the summary", icon = icon("wand-magic-sparkles"),
                             class = "btn-primary pl-ai-wide"),
                div(
                  class = "pl-ai-bar",
                  tags$button(
                    id = "copy_summary_btn", type = "button", class = "pl-ai-tool", title = "Copy the text", icon("copy"),
                    onclick = "
                      let temp = $('<textarea>'); $('body').append(temp);
                      temp.val($('#ai_global_summary_result')[0].innerText).select();
                      document.execCommand('copy'); temp.remove();
                      let btn = $(this); let originalText = btn.html();
                      btn.html('<i class=\"fa fa-check\"></i>');
                      setTimeout(function() { btn.html(originalText); }, 2000);"
                  ),
                  actionButton("ai_add_to_report", tagList(icon("file-medical"), " Add to report"), class = "pl-ai-tool pl-ai-tool-text")
                ),
                div(id = "ai_summary_result_container", class = "pl-ai-result", uiOutput("ai_global_summary_result"))
              )
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
        // New HTML shell: the module's tabset lives in .pl-stage (its own tab strip is hidden)
        let shellNav = $('.pl-stage .tabbable > ul.nav').first();
        if (shellNav.length) {
          return {navTabs: shellNav, tabContent: shellNav.parent().children('.tab-content').first(), shell: true};
        }
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

          if (host.shell) {
            navTabs.append('<li><a href=\"#' + tabId + '\" data-toggle=\"tab\" data-value=\"' + tabId + '\">AI Assistant</a></li>');
            $('#pl-subnav').append('<button type=\"button\" class=\"pl-ai-entry\" data-tab=\"' + tabId + '\" data-title=\"AI Assistant\" data-sub=\"Ask about your results or draft a summary.\"><i class=\"fa-solid fa-wand-magic-sparkles\"></i><span>AI Assistant</span></button>');
            tabContent.append('<div class=\"tab-pane\" id=\"' + tabId + '\" data-value=\"' + tabId + '\"><div style=\"margin-bottom:10px;\"><button type=\"button\" class=\"btn btn-default btn-sm undock-ai-btn\"><i class=\"fa fa-window-restore\"></i> Float assistant</button></div></div>');
          } else {
            navTabs.append('<li><a href=\"#' + tabId + '\" data-toggle=\"tab\">AI Assistant <span class=\"undock-ai-btn\" style=\"margin-left:8px;cursor:pointer;color:#888;\" title=\"Float Assistant\"><i class=\"fa fa-window-restore\"></i></span></a></li>');
            tabContent.append('<div class=\"tab-pane\" id=\"' + tabId + '\" style=\"padding-top: 15px;\"></div>');
          }

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
           'width': '410px',
           'height': '560px',
           'resize': 'both',
           'border': 'none'
        });

        $('#ai_widget_header').next().append(aiBody);
        $('#ai_floating_panel').show();

        let tabId = aiBody.attr('data-docked-tab-id');
        if (tabId) {
           $('a[href=\"#' + tabId + '\"]').parent('li').remove();
           $('#' + tabId).remove();
           $('.pl-ai-entry[data-tab=\"' + tabId + '\"]').remove();
        }

        // Go back to the first tab of the host module
        let host = findHostTabset();
        if (host.shell) {
            $('.pl-subnav button[data-tab]').first().click();
        } else if (host.navTabs.length) {
            host.navTabs.find('li:first-child a').tab('show');
        }
      });
    "))
  )
}
