# R/nwis_utils.R
# NWIS helper utilities.

# Return drainage area in square miles for a NWIS site.
# Uses drain_area_va when available; falls back to other fields if needed.
#
# NOTE: named da_sqmi() because some code paths referenced this symbol.
# If called with no args, returns NA (so the app doesn't crash).

da_sqmi <- function(site_num = NULL) {
  if (is.null(site_num) || !nzchar(as.character(site_num))) return(NA_real_)

  info <- tryCatch(dataRetrieval::readNWISsite(site_num), error = function(e) NULL)
  if (is.null(info) || nrow(info) == 0) return(NA_real_)

  # Common fields seen from NWIS site records
  candidates <- c("drain_area_va", "drain_area", "drain_sqmi", "drain_area_sqmi")
  for (nm in candidates) {
    if (nm %in% names(info)) {
      val <- suppressWarnings(as.numeric(info[[nm]][1]))
      if (!is.na(val) && val > 0) return(val)
    }
  }

  NA_real_
}
