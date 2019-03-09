# -------------------------------------------------
# $Id: asdog.plmodel.report.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# Ploidy model latex reporting
#

# =================================================
# API
# =================================================

# -------------------------------------------------
#' produce plody model tex report 
#
asdog.tex.report.plmodel <- function(tex, model) {
  
  params <- model$params
  
  # -----------------------------------------------
  # @fixme: there is problem with pdf driver and par()
  #         the main and labels family is not inherited
  #         so we do have to set it each time
  # -----------------------------------------------
  
  # -----------------------------------------------
  # parameters section
  #
  
  preseg <- params$plmodel.preseg.mode
  pexcl <- if (preseg == "shmm") "bcp:" else "shmm:"
  p <- asdog.filter.params(params, in.filter="cnpredict:", out.filter=pexcl)
  p <- as.matrix(p, ncol=2)
  p <- as.data.frame(apply(p, 1, paste))
  names(p) <- 'value'
  tex.section(tex, "Parameters")
  tex.print(tex, p, size='small')
  
  # -----------------------------------------------
  # summary section
  # @to be completed
  #
  
  p <- t(data.frame(contamination=model$best$alpha,
                    ref.ploidy=model$best$q0,
                    mean.ploidy=model$best$qmean,
                    quality=model$best$qual))
  colnames(p) <- "value"
  tex.section(tex, "Summary")
  tex.print(tex, p, size='small')

  # -----------------------------------------------
  # Profiles section
  #  
  
  tex.tag(tex, "newpage")
  tex.section(tex, "SNPs Profiles")
  
  # fig on
  #
  tex <- tex.fig.on(tex, width=7, height=7, family="Times")
  plot(model, what="profile.obs")
  tex <- tex.fig.off(tex)
  #
  # fig off
  
  # -----------------------------------------------
  # RC-AF diagram section
  #  
  
  tex.tag(tex, "newpage")
  tex.section(tex, "RC-AF diagram")
  
  # fig on
  #
  tex <- tex.fig.on(tex, width=7, height=7, family="Times")
  b <- model$best
  plot(model, what="rcaf", 
       main=paste0("a=", b$alpha, " Q=", b$q0, " qual=", round(b$qual,2)))
  tex <- tex.fig.off(tex)
  #
  # fig off

  # -----------------------------------------------
  # Fit section
  #  
  
  tex.tag(tex, "newpage")
  tex.section(tex, "Model Fit")
  
  # fig on
  #
  tex <- tex.fig.on(tex, width=7, height=7, family="Times")
  plot(model, what="fit")
  tex <- tex.fig.off(tex)
  #
  # fig off

  invisible(tex)
}
