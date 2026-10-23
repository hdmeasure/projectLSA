# console_widget_ui.R
# The R console output, in a drawer opened from the button at the top right (next to the AI
# settings). It shows what R printed for the open method.
console_widget_ui <- function() {
  tags$div(
    id = "pl-console-drawer", class = "pl-drawer", `aria-hidden` = "true", role = "dialog", `aria-label` = "R console output",
    tags$div(class = "pl-drawer-backdrop", onclick = "plDrawer('pl-console-drawer', false)"),
    tags$aside(
      class = "pl-drawer-panel pl-console-panel",
      tags$header(
        class = "pl-drawer-head",
        tags$span(class = "pl-console-ico", icon("r-project")),
        tags$b(class = "pl-console-title", "R console output"),
        tags$div(
          class = "pl-console-tools",
          tags$button(id = "decrease_font", type = "button", class = "pl-iconbtn", title = "Smaller text", `aria-label` = "Smaller text", tags$span("A−")),
          tags$button(id = "increase_font", type = "button", class = "pl-iconbtn", title = "Larger text", `aria-label` = "Larger text", tags$span("A+")),
          tags$button(id = "copy_console", type = "button", class = "pl-iconbtn", title = "Copy all", `aria-label` = "Copy all", icon("copy")),
          tags$button(type = "button", class = "pl-iconbtn", `aria-label` = "Close", onclick = "plDrawer('pl-console-drawer', false)", icon("xmark"))
        )
      ),
      tags$div(class = "pl-drawer-body pl-console-body", verbatimTextOutput("global_console_output"))
    ),
    tags$script(HTML("
      $(document).on('click', '#increase_font, #decrease_font', function(e) {
        e.stopPropagation();
        var el = $('#global_console_output');
        var size = parseInt(el.css('font-size'));
        el.css('font-size', Math.max(8, size + (this.id === 'increase_font' ? 1 : -1)) + 'px');
      });
      $(document).on('click', '#copy_console', function(e) {
        e.stopPropagation();
        var text = $('#global_console_output').text();
        var $btn = $(this), original = $btn.html();
        // HTML with a monospace font on every line, so Word keeps the console look (and the alignment of columns)
        var font = \"font-family:Consolas,'Courier New',monospace;font-size:9pt;margin:0;white-space:pre;\";
        var html = '<div>' + text.split('\\n').map(function(l) {
          var esc = $('<div>').text(l).html().replace(/^ +/, function(m) { return new Array(m.length + 1).join('&nbsp;'); }).replace(/ {2}/g, ' &nbsp;');
          return '<p style=\"' + font + '\">' + (esc || '&nbsp;') + '</p>';
        }).join('') + '</div>';
        window.plCopyHtml(html, text, function(ok) {
          $btn.html(ok ? '<i class=\"fa fa-check\"></i>' : '<i class=\"fa fa-xmark\"></i>');
          setTimeout(function() { $btn.html(original); }, 1800);
        });
      });
    "))
  )
}
