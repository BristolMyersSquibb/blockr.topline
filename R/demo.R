#' @export
new_ard_summary_block <- function(header = "**{level}**", ...) {

  blockr.gt:::new_gt_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          list(
            expr = reactive(
              bquote(
                {
                  gtsummary::tbl_ard_summary(data, by = ARM) |>
                    gtsummary::add_stat_label() |>
                    gtsummary::modify_header(
                      gtsummary::all_stat_cols() ~ "**{level}**"
                    ) |>
                    gtsummary::as_gt() |>
                    gt::tab_style(
                      gt::cell_fill("grey"),
                      gt::cells_column_labels(stat_1)
                    ) |>
                    gt::tab_style(
                      gt::cell_fill("lightblue"),
                      gt::cells_column_labels(stat_2)
                    ) |>
                    gt::tab_style(
                      gt::cell_fill("lightblue3"),
                      gt::cells_column_labels(stat_3)
                    )
                  },
                list(
                  header = sub("\\n", "\n", input$header, fixed = TRUE)
                )
              )
            ),
            state = list(
              header = reactive(input$header)
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
                  format_stat <- function(dat, stat, fun = identity, ...) {
                    format(fun(dat$stat[[which(dat$stat_name == stat)]]), ...)
                  }
                  data <- data[data$group1 == .(grp), ]
                  est_dat <- data[grepl("estimate[0-9]+", data$stat_name), ]
                  est_dat$stat <- unlist(est_dat$stat)

                  max_val <- max(est_dat$stat)

                  ggplot2::ggplot(est_dat,
                                  ggplot2::aes(stat_label, unlist(stat))) +
                    ggplot2::geom_col(ggplot2::aes(fill = stat_label)) +
                    ggplot2::labs(y = unique(data$variable), x = NULL) +
                    ggplot2::scale_fill_manual(
                      values = c(
                        "grey",
                        if (grepl("High", .(grp))) "lightblue" else "lightblue3"
                      ),
                      guide = "none"
                    ) +
                    ggsignif::geom_signif(
                      y_position = max_val * 1.1,
                      xmin = 1,
                      xmax = 2,
                      annotation = paste0(
                        "atop(Delta ~ ",
                        format_stat(data, "estimate", digits = 3),
                        "~and~italic(P) == ",
                        format_stat(data, "p.value", digits = 3),
                        ", (",
                        format_stat(data, "conf.level", function(x) x * 100,
                                    digits = 2),
                        "~'%'~CI~",
                        format_stat(data, "conf.low", digits = 3),
                        "~to~",
                        format_stat(data, "conf.level", digits = 3),
                        "))"
                      ),
                      tip_length = (max_val * 1.1 - est_dat$stat) / 2,
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

