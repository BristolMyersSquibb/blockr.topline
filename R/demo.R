#' @export
new_ard_summary_block <- function(header = "**{level}**",
                                  continous = "{median} ({p25}, {p75})",
                                  categorical = "{n} ({p}%)",
                                  ...) {

  new_flextable_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          list(
            expr = reactive(
              bquote(
                {
                  data |>
                    dplyr::filter(group1_level != "Total") |>
                    gtsummary::tbl_ard_summary(
                      by = "TRT",
                      statistic = list(
                        gtsummary::all_continuous() ~ .(cont),
                        gtsummary::all_categorical() ~ .(cat))
                    ) |>
                    gtsummary::add_stat_label() |>
                    gtsummary::modify_header(
                      gtsummary::all_stat_cols() ~ .(head)
                    ) |>
                    gtsummary::as_flex_table()
                  },
                list(
                  head = sub("\\n", "\n", input$header, fixed = TRUE),
                  cont = sub("\\n", "\n", input$continous, fixed = TRUE),
                  cat = sub("\\n", "\n", input$categorical, fixed = TRUE)
                )
              )
            ),
            state = list(
              header = reactive(input$header),
              continous = reactive(input$continous),
              categorical = reactive(input$categorical)
            )
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        textInput(
          NS(id, "header"),
          label = "Header",
          value = header
        ),
        textInput(
          NS(id, "continous"),
          label = "Continous",
          value = continous
        ),
        textInput(
          NS(id, "categorical"),
          label = "Categorical",
          value = categorical
        )
      )
    },
    class = "ard_summary_block",
    ...
  )
}

#' @export
new_ard_barplot_block <- function(group = character(), ...) {

  blockr.ggplot:::new_ggplot_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          grps <- reactive(unique(data()[["group1"]]))
          grp <- reactiveVal(group)

          observeEvent(input$group, grp(input$group))

          observeEvent(
            grps(),
            {
              updateSelectInput(
                session,
                inputId = "group",
                choices = grps(),
                selected = grp()
              )
            }
          )

          list(
            expr = reactive(
              bquote(
                {
                  format_stat <- function(dat, var, stat, fun = identity, ...) {
                    hit <- which(dat$stat_name == stat & dat$variable == var)
                    format(fun(dat$stat[[hit]]), ...)
                  }
                  data <- data[data$group1 == .(grp), ]

                  data$stat <- as.numeric(data$stat)
                  data$group1_level <- factor(
                    data$group1_level,
                    levels = c("PBO", "DEUC 6 mg")
                  )

                  est_all <- data[grepl("RESPONSE RATE", data$variable), ]

                  est_dat <- est_all[est_all$stat_name == "p", ]

                  est_err <- merge(
                    est_all[est_all$stat_name == "ci_low", ],
                    est_all[est_all$stat_name == "ci_high", ],
                    by = c("group1", "group1_level", "variable")
                  )

                  max_val <- max(est_err$stat.y)

                  ggplot2::ggplot(est_dat,
                                  ggplot2::aes(group1_level, stat)) +
                    ggplot2::geom_col(ggplot2::aes(fill = group1_level)) +
                    ggplot2::geom_errorbar(data = est_err,
                                           ggplot2::aes(x = group1_level,
                                                        ymin = stat.x,
                                                        y = stat.x,
                                                        ymax = stat.y),
                                           width = 0.2) +
                    ggplot2::labs(y = unique(est_dat$variable), x = NULL) +
                    ggplot2::scale_fill_manual(
                      values = c(
                        PBO = "grey",
                        `DEUC 6 mg` = "lightblue3"
                      ),
                      guide = "none"
                    ) +
                    ggsignif::geom_signif(
                      y_position = max_val * 1.1,
                      xmin = 1,
                      xmax = 2,
                      annotation = paste0(
                        "atop(Delta ~ ",
                        format_stat(data, "DIFFERENCE VS PLACEBO (%)", "n",
                                    digits = 3),
                        "~and~italic(P) == ",
                        format_stat(data, "P-VALUE", "p_value", digits = 3),
                        ", (95~'%'~CI~",
                        format_stat(data, "DIFFERENCE VS PLACEBO (%)",
                                    "ci_low", digits = 3),
                        "~to~",
                        format_stat(data, "DIFFERENCE VS PLACEBO (%)",
                                    "ci_high", digits = 3),
                        "))"
                      ),
                      parse = TRUE,
                      textsize = 5
                    ) +
                    ggplot2::ylim(0, max_val * 1.4) +
                    ggplot2::theme_minimal(base_size = 20)
                },
                list(grp = grp())
              )
            ),
            state = list(group = grp)
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        selectInput(
          NS(id, "group"),
          label = "Group",
          choices = group,
          selected = group
        )
      )
    },
    class = "ard_barplot_block",
    ...
  )
}

#' @export
new_attrition_plot_block <- function(criteria = character(), ...) {

  input_row <- function(key, val, ind, id) {
    fluidRow(
      id = NS(id, paste0("attrition_criterion_", ind)),
      column(3, textInput(NS(id, paste0("key_", ind)), NULL, key)),
      column(3, textInput(NS(id, paste0("val_", ind)), NULL, val)),
      column(2, actionButton(NS(id, paste0("del_", ind)), "Remove",
                             icon = icon("trash"), class = "btn-danger",
                             style = "padding-top: 6px; padding-bottom: 6px"))
    )
  }

  get_input <- function(id, which, input) {
    input[[paste0(which, "_", id)]]
  }

  blockr.ggplot:::new_ggplot_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          len <- reactiveVal(length(criteria))
          ids <- reactiveVal(seq_along(criteria))

          del <- reactive(
            {
              inp <- lapply(paste0("del_", ids()), function(i) input[[i]])
              req(all(lengths(inp)))
              int_xtr(inp, 1L)
            }
          )

          kv <- reactive(
            {
              val <- lapply(ids(), get_input, "val", input)
              key <- lapply(ids(), get_input, "key", input)
              req(all(lengths(val)), all(lengths(key)))
              set_names(chr_xtr(val, 1L), chr_xtr(key, 1L))
            }
          )

          observeEvent(
            del(),
            {
              hit <- which(del() > 0)
              if (length(hit)) {
                removeUI(
                  paste0("#", session$ns(paste0("attrition_criterion_", hit)))
                )
                ids(ids()[-hit])
              }
            }
          )

          observeEvent(
            input$add_crit,
            {
              len(len() + 1L)
              insertUI(
                paste0("#", session$ns("attrition_criteria")),
                "beforeEnd",
                input_row("", "", len(), session$ns(NULL))
              )
              ids(c(ids(), len()))
            }
          )

          list(
            expr = reactive(
              bquote(
                data |>
                  visR::get_attrition(
                    criteria_descriptions = .(keys),
                    criteria_conditions   = .(vals),
                    subject_column_name   = "USUBJID"
                  ) |>
                  visR::visr("Criteria", "Remaining N"),
                list(keys = names(kv()), vals = kv())
              )
            ),
            state = list(criteria = kv)
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        do.call(
          div,
          c(
            id = NS(id, "attrition_criteria"),
            Map(
              input_row,
              names(criteria),
              criteria,
              seq_along(criteria),
              MoreArgs = list(id = id),
              USE.NAMES = FALSE
            )
          )
        ),
        actionButton(
          NS(id, "add_crit"),
          "Add",
          icon = icon("plus"),
          class = "btn-success"
        )
      )
    },
    class = "attrition_plot_block",
    ...
  )
}

