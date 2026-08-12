console_widget_ui <- function() {
  absolutePanel(
    id = "console_floating_panel",
    fixed = TRUE,
    draggable = FALSE,
    bottom = 20,
    left = 20,
    width = "auto",
    height = "auto",
    class = "floating-widget",
    style = "z-index: 9999; border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.2); background: white; border: 1px solid #ccc; display: flex; flex-direction: column; overflow: hidden;",
    
    # Header
    div(
      id = "console_widget_header",
      style = "background: #343a40; color: white; padding: 12px 15px; cursor: pointer; display: flex; justify-content: space-between; align-items: center; font-weight: bold; border-top-left-radius: 8px; border-top-right-radius: 8px;",
      tags$span(icon("r-project", style = "font-size:1.2em;")),
      actionButton("toggle_console_widget", label = "", icon = icon("chevron-up"), 
                   style = "background: transparent; border: none; color: white; box-shadow: none; padding: 0;")
    ),
    
    # Body
    div(
      id = "console_widget_body_wrapper",
      style = "display: none;", # Start minimized
      div(
        id = "console_widget_body",
        style = "resize: both; height: 350px; width: 450px; display: flex; flex-direction: column; background: #2b2b2b; color: #a9b7c6; font-family: Consolas, monospace; overflow: hidden;",
        
        # Toolbar for resize and copy
        div(
          style = "display: flex; justify-content: flex-end; background: #1e1e1e; padding: 4px 8px; gap: 6px; border-bottom: 1px solid #444;",
          tags$button(id = "decrease_font", type = "button", class = "btn btn-xs", style = "background: #444; color: white; border: none; padding: 2px 8px; font-size: 11px; border-radius: 4px;", "- A"),
          tags$button(id = "increase_font", type = "button", class = "btn btn-xs", style = "background: #444; color: white; border: none; padding: 2px 8px; font-size: 11px; border-radius: 4px;", "+ A"),
          tags$button(id = "copy_console", type = "button", class = "btn btn-xs", style = "background: #444; color: white; border: none; padding: 2px 8px; font-size: 11px; border-radius: 4px;", tagList(icon("copy"), "Copy"))
        ),

        div(
          style = "flex-grow: 1; overflow: auto; padding: 10px;",
          # We need to style the verbatimTextOutput to have a dark theme to match the body, and use Consolas font
          tags$style(HTML("
            #global_console_output {
              background-color: transparent;
              color: #a9b7c6;
              border: none;
              font-family: Consolas, monospace;
              font-size: 11px;
              padding: 0;
              white-space: pre-wrap;
            }
          ")),
          verbatimTextOutput("global_console_output")
        )
      )
    ),
    
    # Simple JS to change icon on toggle
    tags$script(HTML("
      $(document).on('click', '#toggle_console_widget', function(e) {
        e.preventDefault();
        var icon = $(this).find('i');
        var wrapper = $('#console_widget_body_wrapper');
        if (wrapper.is(':visible')) {
          wrapper.hide();
          icon.removeClass('fa-chevron-down').addClass('fa-chevron-up');
        } else {
          wrapper.show();
          icon.removeClass('fa-chevron-up').addClass('fa-chevron-down');
        }
      });
      // Click header to toggle
      $(document).on('click', '#console_widget_header', function(e) {
        if ($(e.target).closest('button').length === 0) {
          var wrapper = $('#console_widget_body_wrapper');
          var icon = $('#toggle_console_widget i');
          if (wrapper.is(':visible')) {
            wrapper.hide();
            icon.removeClass('fa-chevron-down').addClass('fa-chevron-up');
          } else {
            wrapper.show();
            icon.removeClass('fa-chevron-up').addClass('fa-chevron-down');
          }
        }
      });
      
      // Font resizing logic
      $(document).on('click', '#increase_font', function(e) {
        e.stopPropagation();
        var el = $('#global_console_output');
        var size = parseInt(el.css('font-size'));
        el.css('font-size', (size + 1) + 'px');
      });

      $(document).on('click', '#decrease_font', function(e) {
        e.stopPropagation();
        var el = $('#global_console_output');
        var size = parseInt(el.css('font-size'));
        el.css('font-size', (size - 1) + 'px');
      });

      // Copy logic
      $(document).on('click', '#copy_console', function(e) {
        e.stopPropagation();
        var text = $('#global_console_output').text();
        
        var $btn = $(this);
        var originalHTML = $btn.html();
        
        var textArea = document.createElement('textarea');
        textArea.value = text;
        textArea.style.position = 'fixed';
        textArea.style.left = '-999999px';
        document.body.appendChild(textArea);
        textArea.focus();
        textArea.select();
        try {
            document.execCommand('copy');
            $btn.html('<i class=\"fa fa-check\"></i> Copied!');
            setTimeout(function() {
              $btn.html(originalHTML);
            }, 1500);
        } catch (err) {
            console.error('Copy failed', err);
        }
        document.body.removeChild(textArea);
      });
    "))
  )
}
