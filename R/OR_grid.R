add_meta_cols <- function(dat,
                          OR = "OR",
                          p.value = "p.value",
                          alpha = 0.05,
                          breaks = c(0, .3, .7, 1, 2, 5, Inf),
                          big = FALSE) {

    dat[, "OR.cat"] <- cut(dat[, OR], breaks = breaks)
    if (!big) {
        dat[, "Significant"] <- ifelse(dat[,p.value] < alpha, "Yes", "No")
    } else {
        dat[, "Significant"] <- ifelse(dat[,p.value] < alpha, "*", "")
    }
    dat[, OR] <- round(dat[, OR], 1)

    big.idx <- which(dat[, OR] > 99)
    small.idx <- which(dat[, OR] < 0.1)

    dat[big.idx, OR] <- ">99"
    dat[small.idx, OR] <- "<0.1"

    return(dat)
}

get_breaks <- function(x, n.categories = 3, midpoint = 1) {

    bottom_breaks <- quantile(x[which(x < midpoint)],
                              probs = seq(0, 1, 1/n.categories))
    bottom_breaks <- bottom_breaks[-c(1, length(bottom_breaks))]
    top_breaks <- quantile(x[which(x > midpoint)],
                           probs = seq(0, 1, 1/n.categories))
    top_breaks <- top_breaks[-c(1, length(top_breaks))]

    return(c(0, bottom_breaks, 1, top_breaks, Inf))

}

#' Make a color-coded grid of odds ratios
#'
#' \code{OR_grid} Makes a grid of ORs, color-coded OR and significance
#' @param dat A data frame
#' @param x The value to map to the x axis
#' @param y The value to map to the y axis
#' @param OR The odds ratio
#' @param p.value The p-value
#' @param alpha The cutoff for significance
#' @param breaks The discrete values by which to color code ORs
#' @param label_size The size of the labels
#' @param coord_flip Whether or not to flip coordinates
#' @value A plot containing ORs and indications of significance
#' @seealso \code{\link[sggplots]{OR_grid_large}}
#' @export
OR_grid <- function(dat,
                    x,
                    y,
                    OR = "OR",
                    p.value = "p.value",
                    alpha = 0.05,
                    breaks = c(0, .3, .7, 1, 2, 5, Inf),
                    label_size = NULL,
                    coord_flip = FALSE) {

    if (is.null(breaks)) {
        breaks <- get_breaks(dat[, OR])
    }
    dat <- add_meta_cols(dat,
                         OR = OR,
                         p.value = p.value,
                         alpha = alpha,
                         breaks = breaks)

    if (is.factor(dat[, x])) {
        if (coord_flip) {
            dat[, x] <- factor(dat[, x], rev(levels(dat[, x])))
        } else {
            dat[, y] <- factor(dat[, y], rev(levels(dat[, y])))
        }
    }

    plot <- dat %>%
        ggplot(aes_string(x = x, y = y)) +
        theme_linedraw() +
        theme(panel.grid.major = element_line("lightgrey"),
              panel.grid.minor = element_blank()) +
        geom_tile(aes(fill=OR.cat), color = "black") +
        scale_fill_brewer(palette = "PRGn", name = "Odds Ratio")

    if (!is.null(label_size)) {
        plot <- plot +
            geom_text(aes_string(label = OR,
                                 col = "Significant"),
                      size = as.numeric(label_size))
    } else {
        plot <- plot + geom_text(aes_string(label = OR, col = "Significant"))
    }

    plot <- plot +
        scale_color_manual(values = c("black", "red")) +
        theme(axis.text.x = element_text(angle = 90,
                                         hjust=1,
                                         vjust = 0.5),
              axis.title = element_blank())

    if (coord_flip) {
        plot <- plot + coord_flip()
    }
    return(plot)
}

#' Make a color-coded grid of odds ratios
#'
#' @inheritParams OR_grid
#' @export
OR_grid_large <- function(dat,
                          x = "Exposure.Variable",
                          y = "Strata",
                          OR = "OR",
                          p.value = "p.value",
                          alpha = 0.05,
                          breaks = c(0, .3, .7, 1, 2, 5, Inf),
                          label_size = NULL,
                          coord_flip = FALSE) {


    if (is.null(breaks)) {
        breaks <- get_breaks(dat[, OR])
    }

    if (is.factor(dat[, x])) {
        if (coord_flip) {
            dat[, x] <- factor(dat[, x], rev(levels(dat[, x])))
        } else {
            dat[, y] <- factor(dat[, y], rev(levels(dat[, y])))
        }
    }

    dat <- add_meta_cols(dat,
                         OR = OR,
                         p.value = p.value,
                         alpha = alpha,
                         breaks = breaks,
                         big = TRUE)

    p <- dat %>%
        ggplot(aes_string(x = x, y = y, label = "Significant")) +
        theme_linedraw() +
        theme(panel.grid.major = element_line("lightgrey"),
              panel.grid.minor = element_blank())+
        geom_tile(aes(fill=OR.cat), color = "black") +
        scale_fill_brewer(palette = "PRGn", name = "Odds Ratio") +
        guides(col = FALSE) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5),
              axis.title = element_blank())

    if (!coord_flip) {
        p <- p +
            geom_text(col = "red") +
            theme(legend.position = "top") +
            guides(fill = guide_legend(nrow = 1,
                                       label.position = "bottom",
                                       title.position = "top"))

    } else  {
        p <- p +
            geom_text(col = "red", angle = 90) +
            coord_flip()
    }

    return(p)
}