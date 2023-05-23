#' @export
theme_perso0 <- function(p, cex = 1, show_axis = TRUE) {
    axis <- element_text(
        face = "bold.italic",
        size = 18 * cex
    )
    p <- p +
        theme(
            axis.title = axis,
            axis.text = element_text(size = 10 * cex),
            axis.ticks = element_line(size = 1.2),
            plot.title = element_text(face = "bold", size = 22 * cex, hjust = 0.5),
            legend.title = element_text(face = "italic", size = 12 * cex),
            legend.text = element_text(colour = "black", size = 10 * cex)
        )
    if (show_axis) {
        p <- p + theme(axis.line = element_line(size = 1.2))
    }
    p
}

#' @export
theme_histo <- function(p, angle = 90) {
    (p +
        theme_classic() +
        theme(axis.text.x = element_text(angle = angle, vjust = 1, hjust = 1)) +
        xlab("")) %>%
        theme_perso0()
}

#' @export
theme_perso_2D <- function(p) {
    theme_perso0(
        p +
            theme_classic() +
            geom_vline(
                xintercept = 0,
                col = "grey",
                linetype = "dashed",
                size = 0.5
            ) +
            geom_hline(
                yintercept = 0,
                col = "grey",
                linetype = "dashed",
                size = 0.5
            )
    )
}


#' Histogram settings
#'
#' Default font for a vertical barplot.
#'
#' @param p A ggplot object.
#' @param df A dataframe with a column named "order"
#' @param title A character string giving a graphic title
#' @param color A vector of character giving the colors for the rows
#' @examples
#' df <- data.frame(x = runif(30), order = 30:1)
#' library("ggplot2")
#' p <- ggplot(df, aes(order, x))
#' plotHistogram(p, df, "This is my title", "red")
#' # Add colors per levels of a variable
#' df$color <- rep(c(1, 2, 3), each = 10)
#' p <- ggplot(df, aes(order, x, fill = color))
#' plotHistogram(p, df, "Histogram", as.character(df$color))
#' @export plotHistogram
plotHistogram <- function(p = NULL, df = NULL, hjust = 0, vjust = 0.5, n = 100, title = "", color_title = "black", color_gradient = c("gray", "#99000D"), cex = 1, ratio = 15, dec = 0, rows = NULL, colors = NULL) {
  if (is.null(p)) {
    df0 <- as.data.frame(df)
    colnames(df0)[1] <- "val"
    if (n > nrow(df0)) {
      n <- nrow(df0)
    }
    df <- (
      df0 %>%
        mutate(name = rownames(.)) %>%
        arrange(desc(val)) %>%
        mutate(order = rev(seq(nrow(df0))))
    ) %>% head(n) %>%
      set_rownames(.$name)
    p <- ggplot(df, aes(order, val, fill = order)) +
      theme_minimal()
  }
  if (is.null(colors))
    colors <- rev(colorRampPalette(color_gradient)(length(p$data$val)))
  y_lab <- p$data$val / 2
  x_lab <- ""
  if (!is.null(rows))
    x_lab <- (round(p$data$val, dec) / rows * 100) %>%
    round(dec) %>%
    paste("%")
  x_lab[p$data$val < 2] <- ""
  p +
    # TODO: if NB_ROW > X, uncomment this
    # geom_hline(yintercept = c(-.5,.5), col="grey", linetype="dotted", size=1) +
    geom_hline(yintercept = 0, col = "grey", size = 1) +
    geom_bar(stat = "identity") +
    expand_limits(y = max(p$data$val) + max(p$data$val) / ratio) +
    coord_flip() +
    scale_x_continuous(breaks = df$order, labels = rownames(df)) +
    labs(
      title = title,
      x = "",
      y = ""
    ) +
    geom_text(
      aes(color = I("white"), y = y_lab, label = x_lab),
      size = cex * 3.5
    ) +
    # theme_classic() +
    # theme_perso() +
    theme(
      axis.text.y = element_text(size = cex * 10, face = "italic", color = colors),
      axis.text.x = element_text(size = cex * 10, face = "italic", color = "darkgrey"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = cex * 16, face = "bold", color = color_title),
      plot.subtitle = element_text(hjust = 0.5, size = cex * 16, face = "italic"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    # label = round(..y.., dec) %>% paste0("%")
    geom_text(aes(label = round(..y.., dec) %>% paste0("%")), hjust = hjust, vjust = vjust, size = cex * 4, color = colors) +
    theme(legend.position = "none") +
    scale_fill_gradient(low = color_gradient[1], high = color_gradient[2])
}

#' Default font for plots
#' @export
theme_perso <- function() {
    theme(
        legend.text = element_text(size = 13),
        legend.title = element_text(face = "bold.italic", size = 16),
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5, margin = margin(0, 0, 20, 0))
    )
}

#' @export
set_clusters <- function(Y) {
    path <- file.path(golem::get_golem_wd(), "inst", "results", "tsv")
    cls <- read.csv2(file.path(path, "clusters_som2.temp.tsv"))
    k <- length(unique(cls[, 2]))
    if (!is.null(names(Y))) {
        cl <- left_join(data.frame(Y, X = as.double(names(Y))), cls)[, 3]
    } else {
        cl <- left_join(data.frame(X = as.double(rownames(blocks))), cls)[, 2]
    }
    colors_k <- get_colors()[seq(k) + 2]
    if (k > 2) {
        colors_k <- c(colors_k[seq(2)], "red", colors_k[3])
    }

    return(list(cl = cl, colors_k = colors_k, k = k))
}

#' @export
get_ctr <- function(x, i = "var") {
    x <- x[[i]]$contrib
    sapply(
        seq(ncol(x)),
        function(i) {
            which((x[, i] %>% sort(TRUE)) > (1 / nrow(x) * 100))
        }
    )
}

#' @export
get_ctr0 <- function(x) {
    sapply(
        seq(ncol(x)),
        function(i) {
            which((x[, i] %>% abs() %>% sort(TRUE)) > (1 / nrow(x)))
        }
    )
}

#' @export
plot_alluvial <- function(df, col_stratum = NULL, col_alluvium = NULL, label_stratum = NULL, label = NULL) {
    if (is.null(col_stratum)) {
        col_stratum <- c(rep(get_colors()[seq(2)], 2))
    }
    if (is.null(col_alluvium)) {
        col_alluvium <- c(rep(c(get_colors()[c(2, 4, 8, 1)], "gray40"), 2))
    }
    if (is.null(label)) {
        label <- str_to_title(colnames(df)[seq(2)])
    }
    df <- count(df, pull(df, 1), pull(df, 2))
    fct_count(pull(df, 1))
    ggplot(df, aes(y = n, axis1 = pull(df, 1), axis2 = pull(df, 2))) +
        geom_alluvium(fill = col_alluvium, width = 0.5, knot.pos = 0) +
        geom_stratum(width = 0.5, fill = col_stratum, color = "grey") +
        geom_text(
            stat = "stratum",
            color = "white",
            aes(label = after_stat(stratum)),
            size = 8
        ) +
        scale_x_discrete(
            limits = label,
            expand = c(.05, .05)
        ) +
        scale_fill_manual(values = get_colors()[seq(2)]) +
        scale_y_continuous(
            breaks = seq(0, 40, 5),
            minor_breaks = seq(36),
            sec.axis = sec_axis(trans = ~., name = "n", breaks = seq(0, 40, 5))
        ) +
        labs(y = "n") +
        theme_minimal() +
        theme(legend.position = "none", axis.ticks.x = element_blank(), panel.grid.major.x = element_blank()) %>%
        theme_perso0(1.5, show_axis = FALSE)
}

get_gene_term <- function(x) {
    sink(file = "/dev/null")
    tmp <- enrichr(x, dbs)[-3]
    sink()
    tmp %>%
        list.map(
            f(i) ~ {
                pull(i, Term) %>%
                    str_remove_all("\\(.*") %>%
                    str_remove_all("((ORPHA)|(WP)|(HSA)|(R-)).*") %>%
                    str_trunc(50) %>%
                    str_trim() -> temp
                paste0(toupper(substr(temp, 1, 1)), substr(temp, 2, nchar(temp)))
            }
        ) %>%
        compact()
}

#' @export
plot_enrich <- function(x, n = 20, title = NULL, gsea = FALSE, cex = 1, ratio = 5) {
    df <- arrange(x, Adjusted.P.value) %>%
        head(n) %>%
        mutate(
            label = {
                str_remove_all(Term, "\\(.*") %>%
                    str_remove_all("((ORPHA)|(WP)|(HSA)|(R-)).*") %>%
                    str_trunc(50) -> temp
                paste0(toupper(substr(temp, 1, 1)), substr(temp, 2, nchar(temp)))
            },
            rank = n + 1 - row_number(Adjusted.P.value)
        )
    if (gsea) {
        df <- mutate(
            df,
            generatio = str_split(Overlap, "/") %>% sapply(length)
        )
    } else {
        df <- mutate(
            df,
            Count = str_remove_all(Overlap, "\\/.*") %>% as.numeric(),
            generatio = {
                str_split(Overlap, "/") %>%
                    sapply(function(i) as.numeric(i[1]) / as.numeric(i[2]))
            }
        )
    }
    ggplot(df, aes(generatio, rank)) +
        geom_point(aes(color = Adjusted.P.value, size = Count)) +
        scale_size(range = c(0.5, 12), name = "Gene count") +
        theme_minimal() %>%
        theme_perso0(cex * 1.2) +
        labs(title = title, x = "Gene ratio", y = "") +
        theme(
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(
                size = 16 * cex,
                colour = ifelse(df$Adjusted.P.value <= 0.05, get_colors()[1], "gray")
            )
        ) +
        scale_color_gradientn(
            # labels = label_pvalue(),
            # breaks = breaks_width(width = 5, offset = 0),
            name = "FDR",
            colours = c(get_colors()[1], "gray", get_colors()[2])
        ) +
        scale_y_continuous(breaks = df$rank, labels = df$label) +
        expand_limits(y = max(df$generatio) + max(df$generatio) / ratio)
}

#' @export
kable0 <- function(x, align = "c") {
    kbl(x, escape = FALSE, align = c("l", rep(align, ncol(x) -1))) %>%
        kable_minimal(full_width = FALSE) %>%
        column_spec(1, bold = TRUE, color = "#a9a9a9")
}

#' @export
add_significance0 <- function(x, p.col = NULL) {
    add_significance(
        x,
        p.col = NULL,
        cutpoints = c(0, 1e-03, 1e-02, 5e-02, 1),
        symbols = c("***", "**", "*", "ns")
    )
}

#' @export
volcano_plot <- function(res, top_genes = NULL, title = "", legend = "right", cex = 1.5) {
    # p_lab <- list.mapv(seq(3), f(.) ~ stri_dup("*", .))
    # ps <- filter(res, padj <= 0.05) %>% pull(log10p) %>% min()
    ps <- -log(0.05, 10)
    # ds <- c(min(top_genes$log2fc), max(top_genes$log2fc))
    # ds <- c(-1.2, -0.8, -0.5, 0.5, 0.8, 1.2)
    res0 <- data.frame(t(c(rep(NA, 6), -100, NA, -1, ""))) %>%
        mutate_all(as.numeric) %>%
        data.frame(c("Up-regulated", "Down-regulated")) %>%
        set_colnames(colnames(res)) %>%
        rbind(res)
    p <- ggplot(res0, aes(log2fc, log10p)) +
        geom_point(aes(color = Expression), size = 3 * cex, alpha = 0.5) +
        geom_vline(xintercept = c(-0.5, 0.5), colour = "gray", lty = 2, lwd = 1.2) +
        geom_hline(yintercept = ps, colour = "gray", lty = 2, lwd = 1.2) +
        xlab(expression("log"[2] * "FC")) +
        ylab(expression("-log"[10] * "P")) +
        scale_color_manual(values = c("dodgerblue3", "gray50", "firebrick3")) +
        guides(colour = guide_legend(override.aes = list(size = 2))) +
        scale_x_continuous(limits = max(abs(res$log2fc)) * c(-1, 1)) +
        scale_y_continuous(limits = c(0, max(abs(res$log10p)))) +
        # scale_y_continuous(breaks = ps, labels = p_lab) +
        ggtitle(label = title) +
        theme_classic() %>%
        theme_perso0(cex) +
        theme(legend.position = legend)
    if (!is.null(top_genes)) {
        p + geom_text_repel(
            data = top_genes,
            mapping = aes(log2fc, log10p, label = name, color = Expression),
            size = cex * 6
        )
    } else {
        p
    }
}

# filter(res, Expression != "ns")
#' @export
get_top <- function(res, fc_threshold = 1, p_threshold = 0.05, n = 1000) {
    bind_rows(
        res %>%
            filter(log2fc >= fc_threshold & padj <= p_threshold),
        res %>%
            filter(log2fc <= -fc_threshold & padj <= p_threshold)
    ) %>%
        arrange(desc(abs(log2fc), padj)) %>%
        head(n)
}

get_top0 <- function(res, fc_threshold = 0.5, p_threshold = 0.05, n = 1000, rank = FALSE) {
    temp <- res %>%
        mutate(
            rank_p = rank(desc(log10p)),
            rank_fc = rank(desc(abs(log2FoldChange))),
            pfc = log10p * log2fc,
        ) %>%
        mutate(rank = rank(rank_p + rank_fc)) %>%
        arrange(rank) %>%
        filter(abs(log2FoldChange) >= fc_threshold & padj <= p_threshold) %>%
        head(n)
    if (!rank) {
          temp <- dplyr::select(temp, -c(rank, rank_p, rank_fc))
      }
    return(temp)
}

#' @export
differential_analysis <- function(x, fc_threshold = 0.8, p_threshold = 0.01) {
    select(x, -c(contains(c("No", "Yes", "fc2", "sd", ".y.", "n1", "n2", "NA", "statistic")), starts_with("group"))) %>%
        tibble() %>%
        adjust_pvalue(method = "BH") %>%
        add_significance0(p.col = p.adj) %>%
        mutate(
            log2fc = D,
            log10p = -log(p, 10),
            Expression = case_when(
                log2fc >= fc_threshold & p <= p_threshold ~ "Up-regulated",
                log2fc <= -fc_threshold & p <= p_threshold ~ "Down-regulated",
                TRUE ~ "ns"
            )
        )
}


cor_test0 <- function(x, y, method = "spearman") {
  list.map(
    x,
    {
      tmp <- cor.test(., y, method = method)
      # dl = tmp$parameter,
      data.frame(t = tmp$statistic, p = tmp$p.value, R = tmp$estimate ^ 2)
    }
  ) %>% list.rbind()
}

print_cor <- function(x, dec = 3) {
  arrange(x, desc(R)) %>%
    adjust_pvalue(method = "BH") %>%
    add_significance0(p.col = "p.adj") %>%
    mutate(
    t = round(t, 1),
    p = ifelse(p < 0.001, "< 0.001", round(p, dec)),
    p.adj = round(p.adj , dec),
    R = round(R, 2)
  )
}


chi_plot <- function(
  x,
  colour1 = NULL,
  colour2 = NULL,
  cex = 1,
  cex_main = 15 * cex,
  cex_sub = 13 * cex,
  cex_axis = 17 * cex,
  method = "chisq",
  method_adjust = "BH",
  wrap = 10,
  ratio = 7
) {
  df0 <- set_colnames(df, c("var1", "var2"))
  if (is.null(colour1)) {
    colour1 <- get_colors()[seq(unique(pull(df, 1)))]
  }
  if (is.null(colour2)) {
    colour2 <- get_colors()[seq(unique(pull(df, 1))) + 9]
  }
  counts <- data.frame(table(df0)) %>%
    mutate(label = Freq)

  max_val <- group_by(df0, var2) %>%
    summarise(label = length(var2)) %>%
    pull(label) %>%
    max(na.rm = TRUE)
  stats <- post_hoc_chi2(df, method = method, method_adjust = method_adjust) %>%
    filter(p <= 0.05) %>%
    mutate(
      var1 = df0$var1[1],
      y.position = max_val + (as.numeric(rownames(.)) * max_val / ratio)
    )

  p <- ggplot(data = counts, aes(x = var2, y = Freq, fill = var1)) +
    geom_bar(stat = "identity", position = "stack") +
    xlab(colnames(df)[2]) +
    ylab("Count") +
    scale_fill_manual(values = colour1, name = colnames(df)[1]) +
    geom_text(
      aes(label = label, y = Freq),
      position = position_stack(vjust = 0.5),
      colour = I("white"),
      size = cex * 7
    ) +
    scale_x_discrete(
      labels = group_by(df0, var2) %>%
        summarise(label = length(var2)) %>%
        mutate(label = paste0(var2, " (N = ", label, ")")) %>%
        pull(label) %>%
        str_wrap(wrap)
    ) +
    labs(
      subtitle = table(df0) %>% get(paste0(method, "_test"))() %>% print_chi2_test()
    ) +
    ggpubr::stat_pvalue_manual(
      stats,
      label = " ",
      color = "gray50",
      bracket.size = 0.7,
      size = cex * 6,
      hide.ns = TRUE,
      tip.length = 0
    )
  theme_violin1(
    p,
    cex = cex,
    cex_main = cex_main,
    cex_sub = cex_sub,
    cex_axis = cex_axis,
    guide = TRUE,
    color_subtitle = colour2
  ) +
    theme(
      axis.text.y = element_text(
        size = cex * 13,
        color = "gray50"
      )
    )
}
