#' @export
theme_perso0 <- function(p, cex = 1, show_axis = TRUE) {
    axis <- element_text(
        face = "bold.italic",
        size = 18 * cex
    )
    p <- p +
        theme(
            axis.title = axis,
            axis.text = element_text(size = 15 * cex, color = "gray50"),
            axis.ticks = element_line(color = "gray50"), #size = 1.2),
            plot.title = element_text(face = "bold", size = 22 * cex, hjust = 0.5),
            legend.title = element_text(face = "italic", size = 12 * cex),
            legend.text = element_text(colour = "black", size = 10 * cex)
        )
    if (show_axis) {
        p <- p + theme(axis.line = element_line(color = "gray50")) #size = 1))
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
plot_alluvial <- function(x, col_stratum = NULL, col_alluvium = NULL, label_stratum = NULL, label = NULL) {
    n <- ncol(x)
    if (is.null(col_stratum)) {
        col_stratum <- c(rep(get_colors()[seq(n)], n))
        label_stratum <- lapply(df, function(i) unique(i) %>% sort(decreasing = TRUE) %>% str_replace_all("zz", "NA")) %>% unlist()
    }
    if (is.null(label)) {
      col_stratum <- str_to_title(colnames(x)[seq(n)])
    }
    df <- count(x, !!!syms(colnames(x)))
    # if (is.null(col_alluvium)) {
    #   col_alluvium <- select(df, -n) %>% lapply(function(i) i %>% factor(labels = levels(as.factor(.)) %>% seq() %>% get_colors()[.]) %>% as.character() %>% replace_na("gray40")) %>% unlist()
    # }

    df <- sapply(x, function(x) as.character(x)) %>% as.data.frame()
    df[is.na(df)] <- "zz"
    df0 <- df %>% mutate(id = rownames(.)) %>% pivot_longer(-id)
      ggplot(df0, aes(x = name, stratum = value, alluvium = id, fill = value, label = value)) +
      geom_flow() +
      geom_stratum(alpha = .5) +

    # ggplot(df, aes(y = n, !!!syms(colnames(x)))) +
    #     geom_alluvium(aes(fill = !!sym(colnames(x)[1])), width = 0.5, knot.pos = 0) +
    #     geom_stratum(width = 0.5, fill = col_stratum, color = "grey") +
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
        scale_fill_manual(values = rev(get_colors()[seq(n)])) +
        # scale_y_continuous(
        #     breaks = seq(0, 40, 5),
        #     minor_breaks = seq(40),
        #     sec.axis = sec_axis(trans = ~., name = "n", breaks = seq(0, 40, 5))
        # ) +
        labs(y = "n") +
        theme_minimal() +
        theme(legend.position = "none", axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(), axis.title.x = element_blank()) %>%
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
plot_enrich <- function(x, n = 20, title = NULL, type = "go", cex = 1, ratio = 5, wrap = 50) {
    func <- function(x) {
      str_remove_all(x, "Genes ((down)|(up))-regulated ((in ?)|(with))") %>%
      str_remove_all("comparison of ") %>%
      str_remove_all("Genes ((posi)|(nega))tively correlated with ") %>%
      str_remove_all("\\[GeneID=\\d*\\]") %>%
      str_remove_all("([uU]ntreated )?peripheral blood mono((nuclear)|(cytes))( cells?)?( \\(PBMC\\))?( from)? ") %>%
      str_remove_all("the ")
      }
    if (type == "gsea") {
      df <- mutate(
            x,
            Adjusted.P.value = p.adjust,
            Term = Description %>%
              func() %>%
              to_title(),
            Count = setSize,
            Overlap = core_enrichment,
            generatio = str_split(Overlap, "/") %>% sapply(length) / setSize
        ) %>% arrange(desc(generatio))
    } else if (type == "kegg") {
      df <- mutate(
        x,
        Adjusted.P.value = p.adjust,
        Term = Description %>%
          func() %>%
          to_title(),
        Count = str_split(GeneRatio, "/") %>%
          sapply(function(i) as.numeric(i[1])),
        generatio = str_split(GeneRatio, "/") %>%
          sapply(function(i) as.numeric(i[1]) / as.numeric(i[2]))
      )
    } else {
        df <- mutate(
            x,
            Count = str_remove_all(Overlap, "\\/.*") %>% as.numeric(),
            generatio = {
                str_split(Overlap, "/") %>%
                    sapply(function(i) as.numeric(i[1]) / as.numeric(i[2]))
            }
        )
    }
    df0 <- filter(df, Adjusted.P.value <= 0.05) %>%
      filter(!is.na(Term))
    if (nrow(df0) < n)
      df0 <- df
    if (type %in% c("gsea", "kegg")) {
    df0 <- arrange(df0, Adjusted.P.value)
    y <- "Adjusted.P.value"
     } else {
      df0  <- arrange(df0, desc(Combined.Score))
      y <- "Combined.Score"
    }
    df <- head(df0, n) %>%
      mutate(
        label = {
          str_remove_all(Term, "\\(.*\\)") %>%
            str_remove_all("((ORPHA)|(WP)|(HSA)|(R-)).*") %>%
            str_trunc2(wrap) %>%
            str_trim() %>%
            to_title()
        },
        rank = rev(row_number(!!sym(y)))
      )
    if (type %in% c("gsea", "kegg")) {
      df <- mutate(df, rank = rev(row_number(!!sym(y))))
    } else {
      df <- mutate(df, rank = row_number(!!sym(y)))
    }
    colour_x <- ifelse(df$Adjusted.P.value <= 0.05, get_colors()[1], "gray50")
    p <- ggplot(df, aes(generatio, rank)) +
        geom_point(aes(color = Adjusted.P.value, size = Count)) +
        scale_size(range = c(0.5, 12), name = "Gene count") +
        labs(title = title, x = "Gene ratio", y = "") +
      scale_y_continuous(breaks = df$rank, labels = df$label)+
      scale_x_continuous(breaks = pretty_breaks(n = 3), labels = label_percent(1)) +
      scale_size_continuous(breaks = pretty_breaks(n = 3), labels = label_number_auto())
      theme_enrich(p, cex, colour = colour_x)  +
        theme(
          axis.text.x = element_text(
            size = 16 * cex,
            angle = 45,
            hjust = 1,
            vjust = 1,
            colour = "gray50"
          )
        ) +
        guides(
          size = guide_legend(order = 1)
        )
        # expand_limits(y = max(df$generatio) + max(df$generatio) / ratio)
}

plot_enrich_gsea <- function(x, n = 15, cex = 0.7, wrap = 50, colour = "gray50") {
  if (is(x, "compareClusterResult")) {
    x@compareClusterResult$Cluster <- x@compareClusterResult$Cluster %>%
      str_trunc(50) %>% str_wrap(20)
  }
  dotplot(
    x,
    showCategory = n,
    label_format = function(x) str_trunc(x, wrap) %>% to_title(),

    ) %>%
    theme_enrich(cex = cex, colour) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = colour)) +
    xlab("")
}

theme_enrich <- function(p, cex = 1, colour = "gray50") {
  p + theme_minimal() %>%
    theme_perso0(cex * 1.5) +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_text(
        size = 16 * cex,
        colour = colour
      )
    ) +
    scale_color_gradientn(
      # labels = label_pvalue(),
      labels = label_number_auto(),
      # breaks = breaks_pretty(3),
      # breaks = breaks_width(width = 5, offset = 0),
      name = "FDR",
      colours = c(get_colors()[1], "gray50", get_colors()[2])
    )
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

expx_trans <- function(x, base = 2) {
  2 ^(abs(x)) * sign(x)
}

logx_trans <- function(x, base = 2) {
  log(abs(x), base) * sign(x)
}

log2x_trans <- trans_new("log2x", function(x) logx_trans(x, 2), function(x) expx_trans(x, 2), breaks = breaks_extended(4), format = label_number_auto())
log10nx_trans <- trans_new("log10nx", function(x) -log10(x), function(x) 10^(-x), breaks = breaks_extended(6), format = label_number_auto())

#' @export
volcano_plot <- function(res, top_genes = NULL, title = "", legend = "right", cex = 1.5, fc_threshold = 0.5, p_threshold = 0.05, force = 10, ...) {
  # seq(3, 10) %>% paste0("2^", .) %>% sapply(function(x) parse(text = x) %>% eval())
    # ds <- c(1.5, 3, 6, 20, 60)
    ds <- c(min(res$log2fc, na.rm = TRUE), max(res$log2fc, na.rm = TRUE)) %>% pretty(5) %>% expx_trans() %>% c(1)
    ps <- min(res$padj, na.rm = TRUE) %>% log10() %>% `-`(1) %>% seq(-1, .) %>% pretty(3) %>% paste0("1e", .) %>% as.numeric() %>% c(1)
    # ds <- c(-1.2, -0.8, -0.5, 0.5, 0.8, 1.2)
    tmp <- data.frame(t(c(rep(NA, 4), -1, NA, Inf, NA, -1, NA))) %>%
      mutate_all(as.numeric) %>%
      data.frame(c("Up-regulated", "Down-regulated")) %>%
      set_colnames(colnames(res))
    res0 <- rbind(tmp, res)
    top_genes <- rbind(tmp, top_genes)
    p <- ggplot(res0, aes(expx_trans(log2fc, 2), padj)) +
      geom_point(aes(fill = Expression), size = 3 * cex, alpha = 0.5, pch = 21, stroke = NA) +
      geom_vline(xintercept = expx_trans(fc_threshold) * c(-1, 1), colour = "gray", lty = 2, lwd = 1.2) +
      geom_hline(yintercept = p_threshold, colour = "gray", lty = 2, lwd = 1.2) +
      xlab("FC") +
      ylab("FDR") +
      scale_fill_manual(values = c("#A6CEE3", "gray50", "#FB9A99")) +
      scale_color_manual(values = c("dodgerblue3", "firebrick3")) +
      guides(colour = guide_legend(override.aes = list(size = 2))) +
      scale_x_continuous(trans = log2x_trans, breaks = c(rev(ds) * -1, ds), labels = label_number_auto()) +
      scale_y_continuous(breaks = ps, trans = log10nx_trans, labels = label_number_auto()) +
      ggtitle(label = title) +
      theme_classic() %>%
      theme_perso0(cex) +
      theme(legend.position = legend)
    if (!is.null(top_genes)) {
        p + geom_text_repel(
            data = top_genes,
            mapping = aes(expx_trans(log2fc, 2), padj, label = name, color = Expression),
            size = cex * 6,
            force = force,
            ...
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

get_top0 <- function(res, fc_threshold = 0.5, p_threshold = 0.05, n = 1000, rank = FALSE, var = "pfc", f = desc) {
    temp <- res %>%
      mutate(
            rank_p = rank(desc(log10p)),
            rank_fc = rank(desc(abs(log2FoldChange))),
            pfc = log10p * log2fc,
            rank = rank(rank_p + rank_fc)
        ) %>%
        dplyr::arrange(f(abs(!!sym(var)))) %>%
        filter(abs(log2FoldChange) >= fc_threshold & padj <= p_threshold) %>%
        head(n)
    if (!rank) {
          temp <- dplyr::select(temp, -c(pfc, contains("rank")))
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

get_intersection0 <- function(x) {
  res <- gplots::venn(x)
  attributes(res)$intersections
}

get_intersection <- function(x, n = 1) {
  intersections <- get_intersection0(x)
  regex <- paste0("^([^:]*:){", n, ",}[^:]*$")
  intersections[grepl(regex, names(intersections))]
}
}

cnetplot0 <- function(
    x,
    highlighted = NULL,
    col_highlight = get_colors()[1],
    showCategory = 3,
    foldChange  = NULL,
    cex = 0.7,
    wrap = 20,
    n = 5,
    node_label = "all"
    ) {
  # foldChange = rep(1, length(highlighted))
  # names(foldChange) <- highlighted

  if (is(x, "compareClusterResult")) {
    x@compareClusterResult$Description <- to_title(x@compareClusterResult$Description) %>%
      str_trunc(50) %>% str_wrap(wrap)
  }
  p <- cnetplot(
    x,
    node_label = node_label,
    cex.params = list(category_label = 1.2 * cex, gene_label = 1 * cex),
    color.params = list(foldChange = foldChange, category = "gray50"),
    shadowtext = "none",
    showCategory = n
    )
    if (is(x, "compareClusterResult")) {
      p +
        scale_fill_manual(
          values = get_colors()[seq_along(x@geneClusters)],
          name = ""
        ) +
        guides(color = "none")
    } else {
      p +
        scale_color_gradientn(colours = colors_ind, name = "FDR") +
        theme(legend.position = "right")
    }
  # p$data$name <- to_title(p$data$name)
  # if (!is.null(highlighted)) {
  #   highlighted0 <- p$data$name %in% highlighted
  #   p$data$alpha[!highlighted0] <- 0
  #   p$data$color[highlighted0] <- get_colors()[1]
  # }
}

get_enrich_genes <- function(x, type = "kegg") {
  if (is(x[[1]], "gseaResult")){
    var_gene <- "core_enrichment"
  } else {
    var_gene <- "geneID"
  }
  if (type == "go") {
    key <- "ENSEMBL"
  } else {
    key <- "ENTREZID"
  }
  list.map(
    x,
    f(j) ~ {
      as.tibble(j) %>%
        pull(var_gene) %>%
        str_split("/") %>%
        pluck(1) %>%
        bitr(
          fromType = key,
          toType = "SYMBOL",
          OrgDb = get(organism)
        ) %>%
        pull("SYMBOL") %>%
        sort()
      })
}

heatplot0 <- function(x, foldChange, n = 15, wrap = 50) {
  heatplot(
    x,
    showCategory = 15,
    foldChange = foldChange,
    label_format = function(x) str_trunc(x, wrap) %>% to_title()
  )
}

get_kegg_path <- function(x, query) {
  if (is(x[[1]], "gseaResult")) {
    gen_col <- "core_enrichment"
    x <- list(x)
  } else {
    gen_col <- "geneID"
  }

  list.map(x, list.map(., f(j) ~ as.tibble(j) %>% filter(str_detect(Description,  query)) %>% pull(ID))) %>% unlist() %>% unique() -> name
  list.map(x, list.map(., f(j) ~ as.tibble(j) %>% filter(str_detect(Description,  query)) %>% pull(Description))) %>% unlist() %>% unique() %>% c(name) %>% print()
  res <- list.map(x, list.map(., f(j) ~ as.tibble(j) %>% filter(str_detect(Description, query)) %>% pull(gen_col) %>% str_split("/") %>% pluck(1)) %>% compact())

  if (is(x[[1]][[1]], "gseaResult")) {
    res <- get_intersection0(res[[1]])
  } else {
    res <- get_intersection0(c(res[[1]], res[[2]]))
  }
  res0 <- list.map(res, f(i, j) ~ data.frame(id = i, col = get_colors()[-c(12, 15:16)][j + 9])) %>% list.rbind()

  save_tsv(res0, paste0(name, ".tsv"), cwd = cwd)
  browseKEGG(x[[1]][[1]], name)

  df <- names(res) %>% str_replace_all(":", " / ") %>% #str_wrap(30) %>%
    data.frame(id = ., col = get_colors()[-c(3, 6:7)][seq_along(.)])
  ggplot(df, aes(x = 1, y = id, label = str_wrap(id, 1000), color = col)) +
    geom_text(size = 8, hjust = 0.5) +
    scale_color_manual(values = df$col) +
    theme_void() +
    theme(legend.position = "none")
}

get_genes_path <- function(x, query, name = "Genes", detect = "Term", cutoff = 0.05) {
  list.map(
    query,
    f(i) ~ {
      list.map(
        x,
        f(j) ~ {
          as.data.frame(j) %>%
            filter(Adjusted.P.value <= cutoff) -> temp
          if(nrow(temp) == 0)
            return(NULL)
          if (detect == "Genes") {
            temp <- lapply(
              seq(nrow(temp)),
              function(x) {
                slice(temp, x) %>%
                  pull(detect) %>%
                  str_split(";") %>%
                  unlist() %>%
                  str_detect(paste0("^", i, "$") %>% str_remove_all("\\.\\.\\.$")) %>%
                  any()
              }
              ) %>%
              unlist() %>%
              filter(temp, .)
          } else {
            temp <- filter(temp, str_detect(!!sym(detect), str_remove_all(i, "\\.\\.\\.$")))
          }
            pull(temp, name)  %>%
            str_split(";") %>% unlist()
        }) %>% unlist() %>% unique() %>% sort() %>% unname()
    }
  ) %>% compact()
}

get_genes_path0 <- function(x, query, name = "Genes", detect = "Term", cutoff = 0.05, p_adjust = "Adjusted.P.value", split_pattern = ";", path = "Term") {
  list.map(
    query,
    f(i) ~ {
      list.map(
        x,
        f(j) ~ {
          as.data.frame(j) %>%
            filter(!!sym(p_adjust) <= cutoff) -> temp
          if(nrow(temp) == 0)
            return(NULL)
          if (detect == "Genes") {
            temp <- lapply(
              seq(nrow(temp)),
              function(k) {
                slice(temp, k) %>%
                  pull(detect) %>%
                  str_split(";") %>%
                  unlist() %>%
                  str_detect(grepl(str_remove_all(i, "\\.\\.\\.$"), !!sym(detect), ignore.case = TRUE)) %>%
                  any()
              }
            ) %>%
              unlist() %>%
              filter(temp, .)
          } else {
            temp <- filter(temp, grepl(str_remove_all(i, "\\.\\.\\.$"), !!sym(detect), ignore.case = TRUE))
          }
          pull(temp, name)  %>%
            str_split(split_pattern) %>% lapply(sort) %>% set_names(pull(temp, path)) #%>% unlist()
        }) %>% compact() #%>% unlist() %>% unique() %>% sort() %>% unname()
    }
  ) %>% compact()
}

format_path0 <- function(x) {
  pluck(x, 1) %>%
    Reduce(function(i, j) c(i, j), .) %>%
    set_names(names(.) %>% str_remove_all("(\\(.*)|((ORPHA)|(WP)|(HSA)|(R-)).*") %>% str_trim() %>% to_title())
}

tree_plot <- function(x, ratio = ratio, wrap = 20, n = 50) {
  # if (is(x, "compareClusterResult")) {
  #   x@compareClusterResult$Description <- to_title(x@compareClusterResult$Description) %>%
  #     str_trunc(wrap)
  # }
  enrichplot::pairwise_termsim(x) %>%
    treeplot(showCategory = 50, offset.params = list(extend = ratio)) # +
  # scale_fill_gradientn(
  #   name = "FDR",
  #   colours = c(get_colors()[1], "gray50", get_colors()[2])
  # )
}
get_ncbi_gene <- function(x) {
  bitr(x, fromType = "ALIAS", toType = "ENTREZID", OrgDb = get(organism)) %>%
    pull(ENTREZID) %>%
    .[1] %>%
    paste0("https://www.ncbi.nlm.nih.gov/gene/", .) %>%
    read_html() %>%
    html_element("#summaryDl") %>%
    html_text2() %>%
    str_extract("Summary\n(.*)\n", group = 1) %>%
    str_trim() %>%
    str_remove_all("^(T(his)|(he)) ") %>%
    str_remove_all("^((gene)|(locus)|(protein)) ") %>%
    str_remove_all("^encode[sd] (a (matrix )?protein )?((which binds)|(that is))?") %>%
    str_remove_all("^(is) ") %>%
    str_remove_all("^(a member of )*(the)?") %>%
    str_remove_all("^((by )?this gene,?) ") %>%
    str_remove_all("^(that plays a role in) ") %>%
    str_remove_all("^predicted (to )?") %>%
    str_remove_all("^(enables?) (to )?") %>%
    str_remove_all("^(was) ") %>%
    str_remove_all("^belongs? (to )?") %>%
    str_remove_all(". \\[.*\\]$") %>%
    str_trim() %>%
    GimmeMyPlot:::to_title() %>%
    paste0(x, ": ", .)
}
