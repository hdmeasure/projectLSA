# ai_settings_ui.R
# AI settings drawer, opened from the button at the top right of the app. Several
# models can be added, ordered, tested together and switched; see ai_helper.R.

ai_settings_ui <- function() {
  provs <- ai_providers()
  by <- function(free) {
    ids <- names(provs)[vapply(provs, function(p) identical(p$free, free), logical(1))]
    stats::setNames(ids, vapply(provs[ids], function(p) p$label, character(1)))
  }
  provider_choices <- list(
    "Free tier first" = by(TRUE),
    "Paid" = by(FALSE),
    "Your own server" = by(NA)
  )

  tags$div(
    id = "pl-ai-drawer", class = "pl-drawer", `aria-hidden` = "true", role = "dialog", `aria-label` = "AI settings",
    tags$div(class = "pl-drawer-backdrop", onclick = "plAiDrawer(false)"),
    tags$aside(
      class = "pl-drawer-panel",
      tags$header(
        class = "pl-drawer-head",
        tags$img(src = "avatar_stiker.png", alt = ""),
        tags$div(tags$b("AI settings"), tags$small("Choose the models that help you interpret and write up results")),
        tags$button(type = "button", class = "pl-iconbtn", `aria-label` = "Close", onclick = "plAiDrawer(false)",
                    tags$i(class = "fa-solid fa-xmark"))
      ),
      tags$div(
        class = "pl-drawer-body",

        tags$section(
          class = "pl-ai-sec",
          tags$h4(tags$i(class = "fa-solid fa-shuffle"), " How the models are used"),
          radioButtons(
            "ai_mode", NULL, selected = "auto",
            choiceNames = list(
              tags$span(tags$b("Automatic"), tags$small("Uses your first model. If its quota runs out, the key is rejected or the service is down, the next one takes over, and the first one is used again once it recovers.")),
              tags$span(tags$b("Single model"), tags$small("Uses only the model you marked as current. Nothing else is tried."))
            ),
            choiceValues = c("auto", "single")
          )
        ),

        tags$section(
          class = "pl-ai-sec",
          tags$h4(tags$i(class = "fa-solid fa-layer-group"), " Your models"),
          uiOutput("ai_slots_ui"),
          tags$div(
            class = "pl-ai-sec-actions",
            actionButton("ai_test_all", tagList(icon("bolt"), " Test all models together"), class = "btn btn-info btn-sm")
          )
        ),

        tags$section(
          class = "pl-ai-sec",
          tags$h4(tags$i(class = "fa-solid fa-circle-plus"), " Add a model"),
          selectInput("ai_new_provider", "Provider", choices = provider_choices, selected = "gemini", width = "100%"),
          uiOutput("ai_provider_hint"),
          conditionalPanel(
            condition = "input.ai_new_provider == 'custom'",
            textInput("ai_new_base", "Base URL", placeholder = "http://localhost:11434/v1", width = "100%")
          ),
          passwordInput("ai_new_key", "API key", placeholder = "Paste your key here", width = "100%"),
          selectizeInput(
            "ai_new_model", "Model", choices = NULL, width = "100%",
            options = list(create = TRUE, placeholder = "Choose, or type a model name")
          ),
          textInput("ai_new_label", "Label (optional)", placeholder = "e.g. Gemini fast", width = "100%"),
          tags$div(
            class = "pl-ai-sec-actions",
            actionButton("ai_fetch_models", tagList(icon("rotate"), " Load models from provider"), class = "btn btn-default btn-sm"),
            actionButton("ai_add_slot", tagList(icon("plus"), " Add model"), class = "btn btn-primary btn-sm")
          )
        ),

        tags$p(
          class = "pl-ai-foot",
          tags$i(class = "fa-solid fa-lock"),
          " Keys are kept only in this browser session on the machine running the app. They are never written to disk, and nothing is sent until you ask the assistant something or press a test button. ",
          "Free tiers are offered by the providers themselves; their limits can change."
        )
      )
    )
  )
}
