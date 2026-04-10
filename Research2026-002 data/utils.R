############################################################
# 1. ONE Function to Code/Standardize data for RSM + GP + DL Models
############################################################

ensure_openxlsx_tempdir <- function(preferred_dir = NULL) {
  candidates <- unique(c(
    preferred_dir,
    Sys.getenv("R_OPENXLSX_TMPDIR", unset = ""),
    Sys.getenv("TMPDIR", unset = ""),
    Sys.getenv("TEMP", unset = ""),
    Sys.getenv("TMP", unset = ""),
    file.path(getwd(), ".openxlsx_tmp")
  ))

  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]

  for (candidate in candidates) {
    dir.create(candidate, recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(candidate) && file.access(candidate, 2) == 0) {
      Sys.setenv(
        R_OPENXLSX_TMPDIR = candidate,
        TMPDIR = candidate,
        TEMP = candidate,
        TMP = candidate
      )
      tempdir(check = TRUE)
      return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
    }
  }

  stop("Unable to create or access a writable temporary directory for workbook export.")
}

save_workbook_safe <- function(workbook, file_path, overwrite = TRUE) {
  output_dir <- dirname(file_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  preferred_tmp <- file.path(output_dir, ".openxlsx_tmp")
  ensure_openxlsx_tempdir(preferred_tmp)
  openxlsx::saveWorkbook(workbook, file = file_path, overwrite = overwrite)
}

scale_design <- function(train_data,
                         test_data = NULL,
                         method = c("standardize", "code"),
                         factor_ranges = NULL,
                         vars_to_standardize = NULL) {
  method <- match.arg(method)
  params <- list()
  
  #-----------------------------
  # Start with original data
  #-----------------------------
  train_scaled <- train_data
  test_scaled  <- if (!is.null(test_data)) test_data else NULL
  
  # -----------------------------
  # STANDARDIZATION
  # -----------------------------
  if (method == "standardize") {
    
    if (is.null(vars_to_standardize)) {
      stop("vars_to_standardize must be provided for standardization.")
    }
    missing_train <- setdiff(vars_to_standardize, names(train_data))
    if (length(missing_train) > 0) {
      stop("scale_design(standardize): these columns are missing from train_data: ",
           paste(missing_train, collapse = ", "))
    }
    if (!is.null(test_data)) {
      missing_test <- setdiff(vars_to_standardize, names(test_data))
      if (length(missing_test) > 0) {
        stop("scale_design(standardize): these columns are missing from test_data: ",
             paste(missing_test, collapse = ", "))
      }
    }
    
    means <- sapply(train_data[vars_to_standardize], mean, na.rm = TRUE)
    sds   <- sapply(train_data[vars_to_standardize], sd, na.rm = TRUE)
    
    #---------------------------------
    # Replace original variables with standardized versions
    #---------------------------------
    train_scaled[vars_to_standardize] <-
      as.data.frame(scale(train_data[vars_to_standardize],
                          center = means,
                          scale  = sds))
    
    params$means <- means
    params$sds   <- sds
    
    #----------------------
    # Standardize test data
    #----------------------
    if (!is.null(test_data)) {
      test_scaled[vars_to_standardize] <-
        as.data.frame(scale(test_data[vars_to_standardize],
                            center = means,
                            scale  = sds))
    }
  }
  
  # -----------------------------
  # CODING (-1, 0, 1)
  # -----------------------------
  if (method == "code") {
    
    if (is.null(factor_ranges)) {
      stop("factor_ranges must be provided for coding.")
    }
    missing_train <- setdiff(names(factor_ranges), names(train_data))
    if (length(missing_train) > 0) {
      stop("scale_design(code): these factor_ranges names are missing from train_data: ",
           paste(missing_train, collapse = ", "))
    }
    if (!is.null(test_data)) {
      missing_test <- setdiff(names(factor_ranges), names(test_data))
      if (length(missing_test) > 0) {
        stop("scale_design(code): these factor_ranges names are missing from test_data: ",
             paste(missing_test, collapse = ", "))
      }
    }
    
    centers <- c()
    deltas  <- c()
    
    #-------------------------------
    # Replace original variables with coded versions
    #-------------------------------
    for (factor in names(factor_ranges)) {
      
      low  <- factor_ranges[[factor]][1]
      high <- factor_ranges[[factor]][2]
      
      center <- (low + high) / 2
      delta  <- (high - low) / 2
      
      train_scaled[[factor]] <-
        (train_data[[factor]] - center) / delta
      
      centers[factor] <- center
      deltas[factor]  <- delta
    }
    
    params$centers <- centers
    params$deltas  <- deltas
    
    #----------------------
    # Code test data
    #----------------------
    if (!is.null(test_data)) {
      for (factor in names(factor_ranges)) {
        test_scaled[[factor]] <-
          (test_data[[factor]] - centers[factor]) / deltas[factor]
      }
    }
  }
  
  return(list(
    train = train_scaled,
    test  = test_scaled,
    parameters = params,
    method = method
  ))
}


#----------------------------------
# Inverse transform results
#----------------------------------
inverse_scale <- function(scaled_data, params, method) {
  
  original <- scaled_data
  
  if (method == "standardize") {
    original <- sweep(scaled_data, 2, params$sds, "*")
    original <- sweep(original, 2, params$means, "+")
  }
  
  if (method == "code") {
    for (factor in names(params$centers)) {
      original[[factor]] <-
        scaled_data[[factor]] * params$deltas[factor] +
        params$centers[factor]
    }
  }
  
  return(original)
}

############################################################
# 2. Function to Compute MASE (Mean Absolute Scaled Error)
############################################################

mase <- function(model, train, test, response) {
  if (!require(rsm)) {
    install.packages("rsm")
    library(rsm)
  }
  pred <- predict(model, newdata = test)
  mae_model <- mean(abs(test[[response]] - pred))
  mae_naive <- mean(abs(diff(train[[response]])))
  mae_model / mae_naive
}

############################################################
# 3. Function to Compute MAPE (Mean Absolute Percentage Error)
############################################################

mape <- function(actual, predicted) {
  if (!require(rsm)) {
    install.packages("rsm", dependencies = TRUE)
    library(rsm)
  }
  #-----------------------------------
  # Remove cases where actual is zero to avoid division by zero
  #-----------------------------------
  non_zero_idx <- actual != 0
  if (any(!non_zero_idx)) {
    warning("Some actual values are zero; these will be excluded from MAPE calculation.")
  }
  actual <- actual[non_zero_idx]
  predicted <- predicted[non_zero_idx]
  
  mean(abs((actual - predicted) / actual)) * 100
}

######################################################################
# 4. Function to fit A Generalized Gaussian Process (GP) to DOE data
######################################################################
gp_master_smallDOE <- function(data,
                               response,
                               factors,
                               test_data = NULL,
                               normalize = TRUE,
                               covtype = c("Gaussian", "Matern5_2", "Matern3_2"),
                               lower = 0.1,
                               upper = 5,
                               selection_metric = c("LOOCV_MASE", "LOOCV_MAPE")) {
  
  if (!require(hetGP)) {
    install.packages("hetGP")
    library(hetGP)
  }

  selection_metric <- match.arg(selection_metric)
  covtype <- unique(as.character(covtype))
  covtype <- covtype[!is.na(covtype) & nzchar(covtype)]
  if (length(covtype) == 0) {
    stop("gp_master_smallDOE(): covtype must contain at least one valid kernel name.")
  }
  
  #-----------------
  # Extract X and y
  #-----------------
  X <- as.matrix(data[, factors])
  y <- data[[response]]

  # Remove rows with NA in predictors or response
  complete <- complete.cases(X, y)
  if (sum(!complete) > 0) {
    warning(sprintf("gp_master_smallDOE(): dropping %d rows with NA in predictors/response.",
                    sum(!complete)))
    X <- X[complete, , drop = FALSE]
    y <- y[complete]
  }
  
  n <- nrow(X); d <- ncol(X)
  
  #------------------------
  # Normalize once
  #------------------------
  if (normalize) {
    mins <- apply(X, 2, min)
    maxs <- apply(X, 2, max)
    ranges <- maxs - mins
    ranges[ranges == 0] <- 1
    X <- sweep(X, 2, mins, "-")
    X <- sweep(X, 2, ranges, "/")
  }

  safe_mase <- function(actual, predicted, reference) {
    valid <- is.finite(actual) & is.finite(predicted)
    if (!any(valid)) {
      return(NA_real_)
    }
    scale_term <- mean(abs(diff(reference)), na.rm = TRUE)
    if (!is.finite(scale_term) || scale_term == 0) {
      return(NA_real_)
    }
    mean(abs(actual[valid] - predicted[valid]), na.rm = TRUE) / scale_term
  }

  safe_mape <- function(actual, predicted) {
    valid <- is.finite(actual) & is.finite(predicted) & actual != 0
    if (!any(valid)) {
      return(NA_real_)
    }
    mean(abs((actual[valid] - predicted[valid]) / actual[valid]), na.rm = TRUE) * 100
  }

  fit_one_kernel <- function(kernel_name) {
    # First attempt: no jitter — let hetGP group exact duplicates natively
    # (hetGP is designed for replicated observations).
    # Retries: progressively larger jitter only if Cholesky fails.
    jitter_attempts <- c(0, 1e-6, 1e-4, 1e-3, 1e-2)
    model <- NULL
    last_err <- NULL

    for (jit in jitter_attempts) {
      X_try <- if (jit > 0) X + matrix(rnorm(n * d, 0, jit), n, d) else X
      model <- tryCatch({
        suppressMessages(suppressWarnings(
          mleHetGP(
            X = X_try,
            Z = y,
            covtype = kernel_name,
            lower = rep(lower, ncol(X)),
            upper = rep(upper, ncol(X))
          )
        ))
      }, error = function(e) e)

      if (!inherits(model, "error")) break
      last_err <- model
      model <- NULL
    }

    if (is.null(model) || inherits(model, "error")) {
      return(list(
        model = NULL,
        kernel_function = kernel_name,
        LOOCV_MASE = NA_real_,
        LOOCV_MAPE = NA_real_,
        test_metrics = NULL,
        fit_success = FALSE,
        message = if (!is.null(last_err)) conditionMessage(last_err)
                  else "All jitter attempts failed"
      ))
    }

    # Try multiple access paths for leaveOneOut (export status varies by hetGP version)
    loo <- NULL
    for (.loo_accessor in list(
      function(m) hetGP::leaveOneOut(m),                                     # exported generic
      function(m) getFromNamespace("leaveOneOut", "hetGP")(m),               # unexported generic
      function(m) getFromNamespace("leaveOneOut.mleHetGP", "hetGP")(m),      # direct S3 method
      function(m) {                                                          # manual LOO
        n <- length(m$Z0)
        Ki <- chol2inv(chol(m$Ki))
        alpha <- Ki %*% (m$Z0 - m$beta0)
        loo_mean <- m$Z0 - alpha / diag(Ki)
        list(mean = as.numeric(loo_mean))
      }
    )) {
      loo <- tryCatch(.loo_accessor(model), error = function(e) NULL)
      if (!is.null(loo) && !is.null(loo$mean)) break
      loo <- NULL
    }
    if (is.null(loo))
      warning(sprintf("LOOCV failed for kernel '%s': no working accessor found", kernel_name))

    loo_preds <- if (!is.null(loo)) loo$mean else rep(NA_real_, length(y))
    # hetGP LOO returns predictions for unique design points (Z0),
    # not all observations — match actual values to the same length.
    loo_actual <- if (!is.null(loo) && !is.null(model$Z0) &&
                      length(model$Z0) == length(loo_preds)) model$Z0 else y
    loo_mase <- safe_mase(loo_actual, loo_preds, y)
    loo_mape <- safe_mape(loo_actual, loo_preds)

    test_metrics <- NULL
    if (!is.null(test_data)) {
      X_test <- as.matrix(test_data[, factors])
      y_test <- test_data[[response]]

      if (normalize) {
        X_test <- sweep(X_test, 2, mins, "-")
        X_test <- sweep(X_test, 2, ranges, "/")
      }

      preds_test <- tryCatch({
        predict(model, X_test)$mean
      }, error = function(e) rep(NA_real_, length(y_test)))

      test_metrics <- list(
        MASE = safe_mase(y_test, preds_test, y),
        MAPE = safe_mape(y_test, preds_test)
      )
    }

    list(
      model = model,
      kernel_function = kernel_name,
      LOOCV_MASE = loo_mase,
      LOOCV_MAPE = loo_mape,
      test_metrics = test_metrics,
      fit_success = TRUE,
      message = NULL
    )
  }

  grid_results <- lapply(covtype, fit_one_kernel)
  valid_results <- Filter(function(result) isTRUE(result$fit_success) && !is.null(result$model), grid_results)

  if (length(valid_results) == 0) {
    return(list(
      model = NULL,
      LOOCV = NULL,
      test_metrics = NULL,
      kernel_function = NA_character_,
      covtype_candidates = covtype,
      grid_search = do.call(
        rbind,
        lapply(grid_results, function(result) {
          data.frame(
            kernel_function = result$kernel_function,
            fit_success = result$fit_success,
            LOOCV_MASE = result$LOOCV_MASE,
            LOOCV_MAPE = result$LOOCV_MAPE,
            TEST_MASE = if (!is.null(result$test_metrics)) result$test_metrics$MASE else NA_real_,
            TEST_MAPE = if (!is.null(result$test_metrics)) result$test_metrics$MAPE else NA_real_,
            message = if (!is.null(result$message)) result$message else NA_character_,
            stringsAsFactors = FALSE
          )
        })
      ),
      selection_metric = selection_metric,
      message = "GP model failed to fit for all requested kernels. Try adjusting bounds, kernels, or checking data."
    ))
  }

  rank_metric <- vapply(valid_results, function(result) {
    as.numeric(result[[selection_metric]])
  }, numeric(1))
  tie_metric <- vapply(valid_results, function(result) {
    if (selection_metric == "LOOCV_MASE") result$LOOCV_MAPE else result$LOOCV_MASE
  }, numeric(1))

  rank_metric[!is.finite(rank_metric)] <- Inf
  tie_metric[!is.finite(tie_metric)] <- Inf

  # If ALL LOOCV metrics are Inf (LOO failed for every kernel), fall back to

  # the log-likelihood of the fitted model so that selection is not arbitrary.
  if (all(is.infinite(rank_metric))) {
    warning("gp_master_smallDOE(): LOOCV failed for all kernels — ",
            "falling back to negative log-likelihood for model selection.")
    rank_metric <- vapply(valid_results, function(result) {
      ll <- tryCatch(result$model$ll, error = function(e) NA_real_)
      if (is.finite(ll)) -ll else Inf          # minimise negative log-lik
    }, numeric(1))
    tie_metric <- rep(0, length(rank_metric))   # no secondary metric
  }

  best_index <- order(rank_metric, tie_metric)[1]
  best_result <- valid_results[[best_index]]
  best_model <- best_result$model

  grid_search <- do.call(
    rbind,
    lapply(grid_results, function(result) {
      data.frame(
        kernel_function = result$kernel_function,
        fit_success = result$fit_success,
        LOOCV_MASE = result$LOOCV_MASE,
        LOOCV_MAPE = result$LOOCV_MAPE,
        TEST_MASE = if (!is.null(result$test_metrics)) result$test_metrics$MASE else NA_real_,
        TEST_MAPE = if (!is.null(result$test_metrics)) result$test_metrics$MAPE else NA_real_,
        selected = identical(result$kernel_function, best_result$kernel_function),
        message = if (!is.null(result$message)) result$message else NA_character_,
        stringsAsFactors = FALSE
      )
    })
  )

  #------------------------
  # Return minimal, stable output
  #------------------------
  return(list(
    model = best_model,
    LOOCV_MASE = best_result$LOOCV_MASE,
    LOOCV_MAPE = best_result$LOOCV_MAPE,
    test_metrics = best_result$test_metrics,
    kernel_function = best_result$kernel_function,
    covtype_candidates = covtype,
    grid_search = grid_search,
    selection_metric = selection_metric,
    predict = function(newdata) {
      newX <- as.matrix(newdata)
      if (normalize) {
        newX <- sweep(newX, 2, mins, "-")
        newX <- sweep(newX, 2, ranges, "/")
      }
      predict(best_model, newX)
    }
  ))
}

#############################################################################
# 5. ANN model
#############################################################################
# Plot the topology of an H2O Deep Learning model using DiagrammeR
plot_h2o_dl_topology <- function(model, train = NULL, x = NULL, y = NULL,
                                 edge_limit_per_pair = 2000,
                                 show_input_labels = FALSE,
                                 input_label_max = 50,
                                 graph_title = NULL) {
  if (!require(h2o)) {
    install.packages("h2o")
    library(h2o)
  }
  if (!require(DiagrammeR)) {
    install.packages("DiagrammeR")
    library(DiagrammeR)
  }
  
  # ---- Basic checks ----
  if (!inherits(model, "H2OModel")) stop("`model` must be an H2OModel.")
  algo <- model@algorithm
  if (!grepl("deeplearning", algo, ignore.case = TRUE)) {
    stop("This function is designed for h2o.deeplearning() models.")
  }
  if (is.null(train) || is.null(x) || is.null(y)) {
    warning("For accurate input/output sizing, supply `train`, `x`, and `y`.",
            " Trying to infer from model may not always be accurate.")
  }
  
  # ---- Determine input size ----
  # Prefer user-provided `train`, `x`, `y`
  n_in <- NA_integer_
  input_labels <- NULL
  if (!is.null(train) && !is.null(x)) {
    # Ensure we're working with an H2OFrame
    if (!inherits(train, "H2OFrame")) stop("`train` must be an H2OFrame.")
    n_in <- length(x)
    if (show_input_labels) {
      # Limit to avoid overly wide node labels
      input_labels <- as.character(x)
      if (length(input_labels) > input_label_max) {
        input_labels <- c(input_labels[1:input_label_max], "…")
      }
    }
  } else {
    # Fallback: try to infer from model
    # Note: DL model stores hidden sizes but not always the input width directly.
    # We'll read from model@model$scoring_history or training metrics when available
    # If unavailable, we require `train` and `x` from the user.
    if (!is.null(model@model$training_metrics@metrics$scoring_history)) {
      # Not guaranteed to have num predictors; keep NA if uncertain
      n_in <- NA_integer_
    }
  }
  
  # ---- Hidden layers ----
  hidden <- model@allparameters$hidden
  if (is.null(hidden)) hidden <- model@parameters$hidden  # older access path
  if (is.null(hidden)) stop("Could not read hidden layer sizes from the model.")
  
  # ---- Determine output size ----
  # For binomial: 1 output node (probability of positive class),
  # For multinomial: number of classes,
  # For regression: 1
  distribution <- model@allparameters$distribution
  family <- tryCatch(model@model$distribution, error = function(e) NULL)
  
  # Prefer model's training frame response domain
  # If `train` and `y` present, we can read the domain directly
  n_out <- 1L
  out_labels <- NULL
  if (!is.null(train) && !is.null(y)) {
    response_col <- train[[y]]
    is_enum <- h2o.isfactor(response_col)
    if (is_enum) {
      # Multiclass or binomial
      dom <- h2o.levels(response_col)
      n_out <- length(dom)
      out_labels <- dom
      if (n_out == 2L) {
        # H2O’s DL returns a single probability output node (p1) in scoring;
        # For topology diagram, either show 1 node (prob) or 2 class logits.
        # We'll default to 1 probability node to stay consistent with scoring.
        n_out <- 1L
        out_labels <- c("p(positive)")
      }
    } else {
      n_out <- 1L
      out_labels <- "ŷ"
    }
  } else {
    # Fallback: use model family
    if (!is.null(family) && grepl("bernoulli|binomial", family, ignore.case = TRUE)) {
      n_out <- 1L
      out_labels <- "p(positive)"
    } else if (!is.null(family) && grepl("multinomial", family, ignore.case = TRUE)) {
      # Without train/y we don't know number of classes; default to 3 as placeholder
      n_out <- 3L
      out_labels <- paste0("class_", seq_len(n_out))
    } else {
      n_out <- 1L
      out_labels <- "ŷ"
    }
  }
  
  # Require input size
  if (is.na(n_in)) {
    stop("Unable to infer input size. Please provide `train`, `x`, and `y`.")
  }
  
  # ---- Build DOT graph ----
  # Create node ids for each layer
  # Layers: L0 (inputs), L1..Lk (hidden), Lk+1 (output)
  layer_sizes <- c(n_in, hidden, n_out)
  L <- length(layer_sizes)
  
  # Utility to create node id
  node_id <- function(layer_idx, node_idx) sprintf("L%02dN%04d", layer_idx, node_idx)
  
  # Node definitions
  node_lines <- c()
  
  # Title
  if (is.null(graph_title)) {
    act <- model@allparameters$activation
    if (is.null(act)) act <- model@parameters$activation
    graph_title <- sprintf("H2O Deep Learning Topology — %s | hidden = [%s]",
                           ifelse(is.null(act), "activation: unknown", paste0("activation: ", act)),
                           paste(hidden, collapse = ", "))
  }
  
  # Input layer subgraph
  input_labels_full <- if (!is.null(input_labels) && length(input_labels) == n_in) {
    input_labels
  } else {
    paste0("x", seq_len(n_in))
  }
  # If we truncated labels, still set n_in nodes but label last as "…"
  if (!is.null(input_labels) && length(input_labels) != n_in && show_input_labels) {
    # place first (input_label_max -1) and one "…" then rest unnamed
    input_labels_full <- rep("", n_in)
    keep <- min(input_label_max - 1, n_in)
    if (keep > 0) input_labels_full[1:keep] <- x[1:keep]
    if (keep < n_in) input_labels_full[keep + 1] <- "…"
  }
  
  node_style <- function(layer_idx) {
    # Different shapes/colors per layer type
    if (layer_idx == 1) {
      # Inputs
      'shape=box, style="rounded,filled", fillcolor="#E8F1FA", color="#3265A6"'
    } else if (layer_idx == L) {
      # Output
      'shape=box, style="rounded,filled", fillcolor="#EAF7EA", color="#2E7D32"'
    } else {
      # Hidden
      'shape=circle, style="filled", fillcolor="#F6F0FF", color="#673AB7"'
    }
  }
  
  # Build subgraphs per layer with same rank
  subgraphs <- c()
  for (li in seq_len(L)) {
    sz <- layer_sizes[li]
    sg <- c(sprintf('subgraph cluster_%02d {', li),
            '  rank=same;')
    for (ni in seq_len(sz)) {
      nid <- node_id(li, ni)
      label <- if (li == 1) {
        # Inputs
        if (show_input_labels) input_labels_full[ni] else ""
      } else if (li == L) {
        # Output
        if (!is.null(out_labels) && length(out_labels) == layer_sizes[L]) {
          out_labels[ni]
        } else if (layer_sizes[L] == 1L) {
          "output"
        } else {
          paste0("out_", ni)
        }
      } else {
        paste0("h", li - 1, "_", ni)
      }
      sg <- c(sg,
              sprintf('  %s [label="%s", %s];', nid, label, node_style(li)))
    }
    # Layer label
    layer_name <- if (li == 1) {
      sprintf('Inputs (%d)', sz)
    } else if (li == L) {
      sprintf('Output (%d)', sz)
    } else {
      sprintf('Hidden %d (%d)', li - 1, sz)
    }
    sg <- c(sg, sprintf('  label="%s";', layer_name), "}")
    subgraphs <- c(subgraphs, sg)
  }
  
  # Edges (fully connected between consecutive layers)
  edge_lines <- c()
  for (li in seq_len(L - 1)) {
    left_sz  <- layer_sizes[li]
    right_sz <- layer_sizes[li + 1]
    total_edges <- left_sz * right_sz
    if (total_edges <= edge_limit_per_pair) {
      for (i in seq_len(left_sz)) {
        for (j in seq_len(right_sz)) {
          edge_lines <- c(edge_lines,
                          sprintf("%s -> %s [color=\"#B0B0B0\"];", node_id(li, i), node_id(li + 1, j)))
        }
      }
    } else {
      # Summarize connections with a single meta-edge per layer pair
      # (still informative while keeping the graph readable)
      meta_left  <- node_id(li, 1)
      meta_right <- node_id(li + 1, 1)
      edge_lines <- c(edge_lines,
                      sprintf('%s -> %s [color="#B0B0B0", label="%s fully-connected (%s edges)"];',
                              meta_left, meta_right,
                              paste0("L", li, "→L", li + 1),
                              format(total_edges, big.mark = ",")))
    }
  }
  
  # Global graph attributes
  dot <- c(
    "digraph DL {",
    '  graph [rankdir=LR, fontsize=12, fontname="Helvetica", labelloc=t, labeljust=c];',
    '  node  [fontname="Helvetica"];',
    '  edge  [fontname="Helvetica"];',
    sprintf('  labelloc="t"; label="%s";', gsub('"', '\\"', graph_title)),
    subgraphs,
    edge_lines,
    "}"
  )
  dot_str <- paste(dot, collapse = "\n")
  
  # Render with DiagrammeR
  DiagrammeR::grViz(dot_str)
}

summarize_h2o_dl <- function(model) {
  stopifnot(inherits(model, "H2OModel"))
  p <- model@allparameters
  cat("=== H2O Deep Learning Model Summary ===\n")
  cat("Activation: ", p$activation, "\n", sep = "")
  cat("Hidden:     [", paste(p$hidden, collapse = ", "), "]\n", sep = "")
  cat("L1/L2:      ", p$l1, "/", p$l2, "\n", sep = "")
  cat("Dropout:    input=", p$input_dropout_ratio,
      ", hidden=", paste(p$hidden_dropout_ratios %||% NA, collapse = ", "),
      "\n", sep = "")
  cat("Epochs:     ", p$epochs, "\n", sep = "")
  cat("Loss:       ", model@model$training_metrics@metrics$MSE %||%
        model@model$training_metrics@metrics$logloss %||% NA, "\n", sep = "")
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

#---------------------------------------------------------------------------
# ANN with full hidden-layer grid search
# Returns: list(model, MASE, MAPE, topology, hyperparams, hyperparams_flat, all_model_perfs)
#----------------------------------------------------------------------------
run_DOE_ANN_full <- function(data,
                             x,
                             y,
                             seed                = 123,
                             show_input_labels   = TRUE,
                             input_label_max     = 20,
                             edge_limit_per_pair = 2000,
                             graph_title         = NULL,
                             max_runtime_secs    = 120,
                             nfolds              = 5,
                             max_models_per_arch = 10,
                             standardize_response = TRUE) {

  if (!requireNamespace("h2o", quietly = TRUE)) stop("Install 'h2o' first.")
  can_plot <- requireNamespace("DiagrammeR", quietly = TRUE)
  library(h2o)

  if (!is.data.frame(data) && !inherits(data, "tbl"))
    stop("`data` must be a data.frame or tibble.")
  if (!all(c(x, y) %in% names(data)))
    stop("Predictor names `x` and response `y` must exist in `data`.")

  h2o_running <- FALSE
  try({ h2o::h2o.getConnection(); h2o_running <- TRUE }, silent = TRUE)
  if (!h2o_running) h2o::h2o.init()

  # ---- Standardize the response variable ----
  # Neural networks learn far better when target values are centered and scaled.
  # We train on z-scored y, then inverse-transform predictions before returning.
  resp_mean <- 0
  resp_sd   <- 1
  train_df  <- data
  if (standardize_response) {
    resp_mean <- mean(data[[y]], na.rm = TRUE)
    resp_sd   <- sd(data[[y]], na.rm = TRUE)
    if (!is.finite(resp_sd) || resp_sd == 0) resp_sd <- 1
    train_df <- data
    train_df[[y]] <- (data[[y]] - resp_mean) / resp_sd
  }

  hf <- h2o::as.h2o(train_df)

  # Guard: H2O's FoldAssignment.fromInternalFold throws java.lang.AssertionError
  # when there are fewer than 2 rows per fold. Require nrow >= 2 * nfolds.
  # Disable CV (nfolds=0) for very small DOE datasets.
  safe_nfolds <- if (nrow(data) >= 2L * nfolds) nfolds else 0L
  if (safe_nfolds == 0L)
    message("[run_DOE_ANN_full] nfolds set to 0 (CV disabled): ",
            nrow(data), " rows < 2 * ", nfolds, " folds.")

  # ---- Candidate hidden architectures ----
  # Sized for small DOE datasets (typically 13–36 rows, 2–10 factors).
  # Rule of thumb: free parameters should not exceed nrow/2 to avoid
  # overfitting.  Architectures are ordered from simplest to most complex;
  # deeper / wider nets are included only for the larger DOEs.
  n_inputs <- length(x)
  n_rows   <- nrow(data)

  # Tier 1 — always safe for small DOEs (≤ ~50 params)
  hidden_candidates <- list(
    c(3), c(5), c(8),
    c(5,3), c(8,5)
  )
  # Tier 2 — add if dataset can support ~50-150 params
  if (n_rows >= 20) {
    hidden_candidates <- c(hidden_candidates, list(
      c(10), c(10,5), c(15,10)
    ))
  }
  # Tier 3 — only for larger DOEs that can support 150+ params
  if (n_rows >= 30 && n_inputs <= 6) {
    hidden_candidates <- c(hidden_candidates, list(
      c(20,10), c(20,10,5)
    ))
  }

  dropout_templates <- list(
    c(0), c(0.1), c(0.2), c(0.3),
    c(0.1, 0.1), c(0.2, 0.2), c(0.3, 0.3),
    c(0.2, 0.1), c(0.1, 0.2)
  )

  expand_dropout <- function(template, n) {
    vec <- rep(as.numeric(template), length.out = n)[1:n]
    vec[is.na(vec)] <- 0
    vec
  }

  base_search_criteria <- list(
    strategy           = "RandomDiscrete",
    max_runtime_secs   = max_runtime_secs,
    max_models         = max_models_per_arch,
    seed               = seed,
    stopping_metric    = "RMSE",
    stopping_rounds    = 5,
    stopping_tolerance = 1e-4
  )

  successful_models <- list()
  model_perfs <- data.frame(model_id = character(0), rmse = numeric(0),
                             stringsAsFactors = FALSE)

  for (hid in hidden_candidates) {
    n_layers <- length(hid)
    dropout_candidates <- unique(lapply(dropout_templates, expand_dropout, n = n_layers),
                                  recursive = FALSE)
    dropout_candidates <- c(list(rep(0, n_layers)), dropout_candidates)
    dropout_candidates <- lapply(dropout_candidates, function(v) {
      v <- as.numeric(v)
      if (length(v) != n_layers) v <- rep(v, length.out = n_layers)[1:n_layers]
      v[is.na(v)] <- 0
      v
    })
    if (!all(vapply(dropout_candidates,
                    function(v) is.numeric(v) && length(v) == n_layers && !any(is.na(v)),
                    logical(1)))) {
      warning("Skipping hidden architecture ", paste(hid, collapse = "-"),
              ": dropout candidates are invalid.")
      next
    }

    search_spaces <- list(
      list(
        suffix = "plain",
        hyper_params = list(
          hidden              = list(hid),
          activation          = c("Rectifier", "Tanh"),
          input_dropout_ratio = c(0),
          l1 = seq(0, 1e-3, length.out = 5),
          l2 = seq(0, 1e-3, length.out = 5),
          epochs = c(50, 100, 200, 500)
        )
      ),
      list(
        suffix = "dropout",
        hyper_params = list(
          hidden                = list(hid),
          activation            = c("RectifierWithDropout", "TanhWithDropout"),
          input_dropout_ratio   = seq(0, 0.3, length.out = 4),
          hidden_dropout_ratios = dropout_candidates,
          l1 = seq(0, 1e-3, length.out = 5),
          l2 = seq(0, 1e-3, length.out = 5),
          epochs = c(50, 100, 200, 500)
        )
      )
    )

    for (search_space in search_spaces) {
      grid_id_local <- paste0(
        "DOE_grid_", search_space$suffix, "_", paste(hid, collapse = "-"), "_", sample(1e6, 1)
      )

      grid_local <- tryCatch({
        h2o::h2o.grid(
          algorithm       = "deeplearning",
          grid_id         = grid_id_local,
          x               = x,
          y               = y,
          training_frame  = hf,
          nfolds          = safe_nfolds,
          fold_assignment = if (safe_nfolds > 0L) "Modulo" else "AUTO",
          hyper_params    = search_space$hyper_params,
          search_criteria = base_search_criteria,
          # Model-level early stopping: stop individual models when
          # they plateau, preventing overfitting on small DOE data.
          stopping_rounds    = 5,
          stopping_metric    = "RMSE",
          stopping_tolerance = 1e-4,
          # Score frequently for reliable early stopping signal
          score_each_iteration = FALSE,
          score_interval       = 0,
          overwrite_with_best_model = TRUE,
          reproducible       = TRUE,
          seed               = seed
        )
      }, error = function(e) {
        warning(sprintf("Grid (%s) for hidden=%s failed: %s",
                        search_space$suffix,
                        paste(hid, collapse = "-"),
                        conditionMessage(e)))
        NULL
      })
      if (is.null(grid_local)) next

      grid_perf <- tryCatch(
        suppressWarnings(h2o::h2o.getGrid(grid_id_local, sort_by = "rmse", decreasing = FALSE)),
        error = function(e) NULL
      )
      if (is.null(grid_perf) || length(grid_perf@model_ids) == 0) next

      for (mid in grid_perf@model_ids) {
        mdl <- tryCatch(h2o::h2o.getModel(mid), error = function(e) NULL)
        if (is.null(mdl)) next
        rmse_val <- tryCatch({
          # Prefer CV metrics (generalisation error) over training metrics
          # to avoid always selecting the largest topology that overfits.
          cv_metrics <- mdl@model$cross_validation_metrics
          tm         <- mdl@model$training_metrics
          rmse_field <- NA_real_
          if (!is.null(cv_metrics)) {
            rmse_field <- cv_metrics@metrics$RMSE %||% NA
          }
          if (is.na(rmse_field) && !is.null(tm)) {
            rmse_field <- tm@metrics$RMSE %||% tm$rmse %||% NA
          }
          if (!is.na(rmse_field)) as.numeric(rmse_field) else {
            preds <- as.data.frame(h2o::h2o.predict(mdl, hf))
            pred_col <- if ("predict" %in% names(preds)) preds$predict
                        else preds[[which(vapply(preds, is.numeric, logical(1)))[1]]]
            sqrt(mean((as.numeric(as.data.frame(hf[[y]])[[1]]) - pred_col)^2, na.rm = TRUE))
          }
        }, error = function(e) NA_real_)
        successful_models[[mid]] <- mdl
        model_perfs <- rbind(model_perfs,
                             data.frame(model_id = mid, rmse = rmse_val,
                                        stringsAsFactors = FALSE))
      }
    }
  }

  if (length(successful_models) == 0)
    stop("No successful models produced. Check H2O logs and hyper-parameter validity.")

  model_perfs <- model_perfs[order(model_perfs$rmse, na.last = TRUE), ]
  best_model  <- successful_models[[model_perfs$model_id[[1]]]]

  # ---- Predictions on training frame ----
  pred_df <- as.data.frame(h2o::h2o.predict(best_model, hf))
  preds   <- if ("predict" %in% names(pred_df)) pred_df$predict
             else pred_df[[which(vapply(pred_df, is.numeric, logical(1)))[1]]]

  # Inverse-transform predictions back to original scale
  if (standardize_response) {
    preds <- preds * resp_sd + resp_mean
  }

  actual <- data[[y]]  # always compare to original-scale actuals
  if (!is.numeric(actual))
    actual <- suppressWarnings(as.numeric(as.character(actual)))

  eps        <- .Machine$double.eps * 100
  denom      <- ifelse(abs(actual) < eps, NA_real_, actual)
  mape_val   <- if (all(is.na(denom))) NA_real_
                else mean(abs((actual - preds) / denom), na.rm = TRUE) * 100
  mase_val   <- if (length(actual) < 2) NA_real_ else {
    nd <- mean(abs(diff(actual)), na.rm = TRUE)
    if (is.na(nd) || nd == 0) NA_real_
    else mean(abs(actual - preds), na.rm = TRUE) / nd
  }

  # ---- Topology & hyperparams ----
  hidden_vals <- best_model@allparameters$hidden %||% best_model@parameters$hidden
  # Grid models store hidden as a list (e.g. list(c(10,5))); unlist to get a plain vector
  if (is.list(hidden_vals)) hidden_vals <- unlist(hidden_vals)
  int_vals <- suppressWarnings(as.integer(hidden_vals))
  topology <- if (is.null(hidden_vals) || length(hidden_vals) == 0) "<none>"
              else if (anyNA(int_vals)) paste(as.character(hidden_vals), collapse = "-")
              else paste(int_vals, collapse = "-")
  hp <- best_model@allparameters

  normalize_hdr <- function(hdr, hl) {
    if (is.null(hdr)) return(NA_character_)
    if (is.list(hdr) && length(hdr) == 1 && is.numeric(hdr[[1]])) hdr <- hdr[[1]]
    if (is.list(hdr) && all(vapply(hdr, is.numeric, logical(1)))) {
      m <- Filter(function(v) length(v) == hl, hdr)
      hdr <- if (length(m) >= 1) m[[1]] else hdr[[1]]
    }
    if (is.numeric(hdr)) {
      vec <- if (length(hdr) == hl) hdr
             else if (length(hdr) == 1) rep(hdr, hl)
             else rep(hdr, length.out = hl)[1:hl]
      return(paste(formatC(vec, digits = 6, format = "f"), collapse = ";"))
    }
    as.character(hdr)
  }

  flatten_hp <- function(x) {
    if (is.null(x)) return(NA_character_)
    if (is.list(x)) paste(sapply(x, function(el) paste(as.character(el), collapse = ";")),
                           collapse = " | ")
    else paste(as.character(x), collapse = ";")
  }

  hl      <- if (is.null(hidden_vals)) 0L else length(hidden_vals)
  hp_flat <- list(
    activation            = flatten_hp(hp$activation),
    hidden                = topology,
    input_dropout_ratio   = flatten_hp(hp$input_dropout_ratio),
    hidden_dropout_ratios = normalize_hdr(hp$hidden_dropout_ratios, hl),
    l1 = flatten_hp(hp$l1),
    l2 = flatten_hp(hp$l2),
    epochs = flatten_hp(hp$epochs),
    rate   = flatten_hp(hp$rate),
    rate_annealing  = flatten_hp(hp$rate_annealing),
    momentum_start  = flatten_hp(hp$momentum_start),
    momentum_ramp   = flatten_hp(hp$momentum_ramp),
    momentum_stable = flatten_hp(hp$momentum_stable)
  )

  # ---- Plot topology (non-fatal) ----
  if (can_plot) {
    tryCatch(
      plot_h2o_dl_topology(model = best_model, train = hf, x = x, y = y,
                           show_input_labels   = show_input_labels,
                           input_label_max     = input_label_max,
                           edge_limit_per_pair = edge_limit_per_pair,
                           graph_title         = graph_title),
      error = function(e) warning("Plotting topology failed: ", conditionMessage(e))
    )
  }
  tryCatch(summarize_h2o_dl(best_model),
           error = function(e) warning("summarize_h2o_dl failed: ", conditionMessage(e)))

  invisible(list(
    model           = best_model,
    MASE            = mase_val,
    MAPE            = mape_val,
    topology        = topology,
    hyperparams     = hp,
    hyperparams_flat = hp_flat,
    all_model_perfs  = model_perfs,
    resp_mean        = resp_mean,
    resp_sd          = resp_sd,
    standardize_response = standardize_response
  ))
}

################################################################################   
# 6. Save the Model Metrics to Excel
################################################################################
save_model_metrics <- function(file_path,
                               sheet_name,
                               design_type,
                               X_train,
                               X_test,
                               model_name,
                               mase,
                               mape,
                               topology,
                               hyperparams_flat) {
  
  library(openxlsx)
  
  # Convert hyperparameters list to a one-row data frame
  hp_df <- as.data.frame(t(unlist(hyperparams_flat)), stringsAsFactors = FALSE)
  
  # Core metrics
  core_df <- data.frame(
    Design     = design_type,
    Model      = model_name,
    MASE       = mase,
    MAPE       = mape,
    Topology   = topology,
    Train_N    = nrow(X_train),
    Test_N     = nrow(X_test),
    stringsAsFactors = FALSE
  )
  
  # Combine core metrics + hyperparameters
  df <- cbind(core_df, hp_df)
  
  # Write or append to Excel
  # NOTE: openxlsx::loadWorkbook() can fail with 'object sheetrId not found'
  # on workbooks it created itself.  Avoid loadWorkbook entirely: build a fresh
  # workbook each time, re-reading existing sheets via read.xlsx().
  existing_sheets <- if (file.exists(file_path)) {
    tryCatch(openxlsx::getSheetNames(file_path), error = function(e) character(0))
  } else character(0)

  wb <- createWorkbook()

  # Copy every existing sheet into the new workbook
  for (s in existing_sheets) {
    old_data <- tryCatch(openxlsx::read.xlsx(file_path, sheet = s), error = function(e) NULL)
    addWorksheet(wb, s)
    if (!is.null(old_data) && nrow(old_data) > 0)
      writeData(wb, sheet = s, x = old_data)
  }

  if (sheet_name %in% existing_sheets) {
    existing <- tryCatch(openxlsx::read.xlsx(file_path, sheet = sheet_name),
                         error = function(e) NULL)
    start_row <- if (!is.null(existing)) nrow(existing) + 2 else 1
    writeData(wb, sheet = sheet_name, x = df, startRow = start_row,
              colNames = (start_row == 1))
  } else {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, x = df)
  }

  save_workbook_safe(wb, file_path, overwrite = TRUE)
}


############################################################
# doe_meta_model()
#
# Orchestrates RSM + GP + ANN + Ensemble for every response
# variable by calling the functions already defined above:
#   • rsm::rsm()              — user-supplied formula
#   • gp_master_smallDOE()   — heteroskedastic GP fit
#   • run_DOE_ANN_full()      — ANN with full grid search
#
# Arguments:
#   train_data           – data.frame of training observations
#   test_data            – data.frame of test / validation observations
#   responses            – character vector of response column names
#   predictors           – character vector of predictor column names
#   rsm_formulas         – named list of RSM formulas (name = response)
#   design_type          – label written to the Excel file
#   excel_file           – path to the output .xlsx workbook
#   ann_seed             – RNG seed passed to run_DOE_ANN_full()
#   ann_max_runtime_secs – time budget per ANN architecture grid (seconds, default 120)
#   ann_nfolds           – CV folds inside run_DOE_ANN_full() (default 5)
#   ann_max_models       – max models per hidden architecture (default 10)
#
# Excel sheet "Model Metrics" columns:
#   DESIGN_TYPE | SCALING_METHOD | RESPONSE | NUM_FACTORS |
#   TRAIN_SIZE | TEST_SIZE | MODEL | MASE | MAPE | TOPOLOGY |
#   EPOCHS | KERNEL_FUNCTION | ACTIVATION_FUNCTION
#
# Scaling (automatic, per model — no user selection required):
#   RSM — coded to [-1, 0, 1] via scale_design(method="code")
#          factor_ranges auto-computed from train_data min/max if not supplied
#   GP  — standardized (mean=0, sd=1) via scale_design(method="standardize")
#   ANN — standardized (mean=0, sd=1) via scale_design(method="standardize")
#
# Returns: invisibly, a named list of per-response results
############################################################
doe_meta_model <- function(train_data,
                           test_data,
                           responses,
                           predictors,
                           rsm_formulas,
                           design_type          = "DOE",
                           factor_ranges        = NULL,
                           excel_file           = "DOE_Metrics.xlsx",
                           ann_seed             = 123,
                           ann_max_runtime_secs = 120,
                           ann_nfolds           = 5,
                           ann_max_models       = 10) {

  # ---- Package checks ----
  for (pkg in c("rsm", "h2o", "hetGP", "openxlsx", "Metrics")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(sprintf("Package '%s' is required but not installed.", pkg))
  }
  library(rsm); library(h2o); library(hetGP); library(openxlsx); library(Metrics)

  if (!is.data.frame(train_data)) {
    stop("`train_data` must be a data.frame. Received class: ",
         paste(class(train_data), collapse = ", "))
  }
  if (!is.data.frame(test_data)) {
    stop("`test_data` must be a data.frame. Received class: ",
         paste(class(test_data), collapse = ", "))
  }

  missing_train_predictors <- setdiff(predictors, names(train_data))
  missing_test_predictors  <- setdiff(predictors, names(test_data))
  missing_train_responses  <- setdiff(responses, names(train_data))
  missing_test_responses   <- setdiff(responses, names(test_data))

  if (length(missing_train_predictors) > 0 || length(missing_train_responses) > 0 ||
      length(missing_test_predictors) > 0 || length(missing_test_responses) > 0) {
    problems <- c(
      if (length(missing_train_predictors) > 0)
        paste0("train_data missing predictors: ", paste(missing_train_predictors, collapse = ", ")),
      if (length(missing_train_responses) > 0)
        paste0("train_data missing responses: ", paste(missing_train_responses, collapse = ", ")),
      if (length(missing_test_predictors) > 0)
        paste0("test_data missing predictors: ", paste(missing_test_predictors, collapse = ", ")),
      if (length(missing_test_responses) > 0)
        paste0("test_data missing responses: ", paste(missing_test_responses, collapse = ", "))
    )
    stop("doe_meta_model(): column validation failed. ", paste(problems, collapse = " | "))
  }

  # ---- Metric helpers ----
  calc_mase <- function(actual, pred, naive_ref) {
    ok <- is.finite(actual) & is.finite(pred)
    if (sum(ok) < 2) return(NA_real_)
    ref_ok <- is.finite(naive_ref)
    if (sum(ref_ok) < 2) return(NA_real_)
    nd <- mean(abs(diff(naive_ref[ref_ok])))
    if (!is.finite(nd) || nd == 0) return(NA_real_)
    mean(abs(actual[ok] - pred[ok])) / nd
  }
  calc_mape <- function(actual, pred) {
    ok <- is.finite(actual) & is.finite(pred) &
          abs(actual) > .Machine$double.eps * 100
    if (sum(ok) < 1) return(NA_real_)
    mean(abs((actual[ok] - pred[ok]) / actual[ok])) * 100
  }
  safe_rmse <- function(a, p) {
    ok  <- is.finite(a) & is.finite(p)
    val <- tryCatch(Metrics::rmse(a[ok], p[ok]), error = function(e) Inf)
    if (!is.finite(val) || val <= 0) Inf else val
  }
  coerce_vec <- function(x, n, fb) {
    if (is.null(x)) return(rep(fb, n))
    if (is.list(x) && !is.null(x$mean)) x <- x$mean
    if (is.data.frame(x) || is.matrix(x)) x <- as.numeric(x[, 1])
    x <- suppressWarnings(as.numeric(as.vector(x)))
    x[!is.finite(x)] <- fb
    if (length(x) < n) x <- c(x, rep(fb, n - length(x)))
    if (length(x) > n) x <- x[seq_len(n)]
    x
  }
  is_already_coded <- function(df, cols) {
    if (length(cols) == 0) return(TRUE)
    all(vapply(cols, function(col) {
      vals <- suppressWarnings(as.numeric(df[[col]]))
      vals <- vals[is.finite(vals)]
      if (length(vals) == 0) return(FALSE)
      uniq <- sort(unique(round(vals, 8)))
      all(uniq %in% c(-1, 0, 1))
    }, logical(1)))
  }
  is_already_standardized <- function(df, cols, mean_tol = 1e-6, sd_tol = 1e-6) {
    if (length(cols) == 0) return(TRUE)
    all(vapply(cols, function(col) {
      vals <- suppressWarnings(as.numeric(df[[col]]))
      vals <- vals[is.finite(vals)]
      if (length(vals) < 2) return(FALSE)
      abs(mean(vals)) <= mean_tol && abs(stats::sd(vals) - 1) <= sd_tol
    }, logical(1)))
  }
  fmt_scalar <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    x <- as.character(x[[1]])
    if (!nzchar(x)) NA_character_ else x
  }
  fmt_integer_scalar <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_integer_)
    val <- suppressWarnings(as.numeric(x[[1]]))
    if (!is.finite(val)) return(NA_integer_)
    as.integer(round(val))
  }

  # ---- Coerce columns to numeric ----
  for (col in c(predictors, responses)) {
    if (col %in% names(train_data) && !is.numeric(train_data[[col]]))
      train_data[[col]] <- suppressWarnings(as.numeric(as.character(train_data[[col]])))
    if (col %in% names(test_data) && !is.numeric(test_data[[col]]))
      test_data[[col]]  <- suppressWarnings(as.numeric(as.character(test_data[[col]])))
  }

  # ---- RSM: code predictors to [-1, 0, 1] only if not already coded ----
  rsm_scaling_label <- "NONE"
  if (is_already_coded(train_data, predictors)) {
    train_rsm <- train_data
    test_rsm  <- test_data
  } else {
    if (is.null(factor_ranges)) {
      factor_ranges <- setNames(
        lapply(predictors, function(p) range(train_data[[p]], na.rm = TRUE)),
        predictors
      )
      cat("[doe_meta_model] factor_ranges auto-computed from training data (min/max).\n")
    }
    scaled_rsm <- scale_design(train_data, test_data,
                               method        = "code",
                               factor_ranges = factor_ranges)
    train_rsm <- scaled_rsm$train
    test_rsm  <- scaled_rsm$test
    rsm_scaling_label <- "CODED"
  }

  # ---- GP & ANN: standardize predictors only if not already standardized ----
  std_scaling_label <- "NONE"
  if (is_already_standardized(train_data, predictors)) {
    train_std <- train_data
    test_std  <- test_data
  } else {
    scaled_std <- scale_design(train_data, test_data,
                               method              = "standardize",
                               vars_to_standardize = predictors)
    train_std <- scaled_std$train
    test_std  <- scaled_std$test
    std_scaling_label <- "STANDARDIZED"
  }

  n_factors <- length(predictors)
  n_train   <- nrow(train_data)
  n_test    <- nrow(test_data)
  all_rows  <- list()
  gp_search_rows <- list()
  all_fits  <- list()

  for (resp in responses) {

    cat(sprintf("\n>>> Fitting models for response: %s\n", resp))

    y_train  <- as.numeric(train_data[[resp]])
    y_test   <- as.numeric(test_data[[resp]])
    fallback <- mean(y_train, na.rm = TRUE)

    # ------------------------------------------------------------------
    # 1. RSM — user-supplied formula
    # ------------------------------------------------------------------
    rsm_form <- if (!is.null(names(rsm_formulas)) && resp %in% names(rsm_formulas)) {
      rsm_formulas[[resp]]
    } else {
      rsm_formulas[[match(resp, responses)]]
    }
    rsm_fit  <- tryCatch(
      rsm::rsm(rsm_form, data = train_rsm),
      error = function(e) { warning("RSM failed: ", e$message); NULL }
    )
    rsm_pred <- coerce_vec(
      tryCatch(predict(rsm_fit, newdata = test_rsm), error = function(e) NULL),
      n_test, fallback
    )
    rsm_mase <- calc_mase(y_test, rsm_pred, y_train)
    rsm_mape <- calc_mape(y_test, rsm_pred)

    # ------------------------------------------------------------------
    # 2. GP — gp_master_smallDOE()  (defined above in this file)
    # ------------------------------------------------------------------
    gp_fit  <- tryCatch(
      gp_master_smallDOE(data      = train_std,
                         response  = resp,
                         factors   = predictors,
                         test_data = test_std,
                         normalize = FALSE),   # already standardized
      error = function(e) { warning("GP failed: ", e$message); list(model = NULL) }
    )
    gp_pred <- coerce_vec(
      if (!is.null(gp_fit$model))
        tryCatch(gp_fit$predict(test_std[, predictors, drop = FALSE]),
                 error = function(e) NULL)
      else NULL,
      n_test, fallback
    )
    # Prefer MASE/MAPE already computed by gp_master_smallDOE on the test set
    if (!is.null(gp_fit$test_metrics) && !is.null(gp_fit$test_metrics$MASE)) {
      gp_mase <- as.numeric(gp_fit$test_metrics$MASE)
      gp_mape <- as.numeric(gp_fit$test_metrics$MAPE)
    } else {
      gp_mase <- calc_mase(y_test, gp_pred, y_train)
      gp_mape <- calc_mape(y_test, gp_pred)
    }

    # ------------------------------------------------------------------
    # 3. ANN — run_DOE_ANN_full() — full hidden-layer grid search
    #    Returns list($model, $MASE, $MAPE, $topology, $hyperparams_flat)
    # ------------------------------------------------------------------
    ann_result <- tryCatch(
      run_DOE_ANN_full(data                = train_std,
                       x                   = predictors,
                       y                   = resp,
                       seed                = ann_seed,
                       max_runtime_secs    = ann_max_runtime_secs,
                       nfolds              = ann_nfolds,
                       max_models_per_arch = ann_max_models),
      error = function(e) { warning("ANN failed: ", e$message); NULL }
    )
    ann_model <- if (!is.null(ann_result)) ann_result$model else NULL
    ann_pred  <- coerce_vec(
      if (!is.null(ann_model))
        tryCatch(
          as.numeric(as.data.frame(
            h2o::h2o.predict(ann_model, h2o::as.h2o(test_std)))[, 1]),
          error = function(e) NULL)
      else NULL,
      n_test, fallback
    )
    # Inverse-transform ANN predictions if response was standardized during training
    if (!is.null(ann_result) && isTRUE(ann_result$standardize_response)) {
      ann_pred <- ann_pred * ann_result$resp_sd + ann_result$resp_mean
    }
    ann_mase       <- calc_mase(y_test, ann_pred, y_train)
    ann_mape       <- calc_mape(y_test, ann_pred)
    ann_topology   <- if (!is.null(ann_result)) ann_result$topology else "NA"

    # Free all H2O objects (models, grids, frames) now that ANN predictions
    # have been extracted into plain R vectors. Prevents JVM heap exhaustion
    # across successive responses / doe_meta_model() calls.
    tryCatch(h2o::h2o.removeAll(), error = function(e) NULL)
    ann_epochs     <- if (!is.null(ann_result)) fmt_integer_scalar(ann_result$hyperparams_flat$epochs) else NA_integer_
    ann_activation <- if (!is.null(ann_result)) fmt_scalar(ann_result$hyperparams_flat$activation) else NA_character_
    gp_kernel      <- if (!is.null(gp_fit$kernel_function)) fmt_scalar(gp_fit$kernel_function) else NA_character_

    if (!is.null(gp_fit$grid_search) && nrow(gp_fit$grid_search) > 0) {
      gp_search_df <- as.data.frame(gp_fit$grid_search, stringsAsFactors = FALSE)
      if (!"selected" %in% names(gp_search_df)) gp_search_df$selected <- NA
      if (!"fit_success" %in% names(gp_search_df)) gp_search_df$fit_success <- NA
      if (!"message" %in% names(gp_search_df)) gp_search_df$message <- NA_character_

      gp_search_rows[[length(gp_search_rows) + 1]] <- data.frame(
        DESIGN_TYPE      = design_type,
        SCALING_METHOD   = std_scaling_label,
        RESPONSE         = resp,
        NUM_FACTORS      = n_factors,
        TRAIN_SIZE       = n_train,
        TEST_SIZE        = n_test,
        SELECTION_METRIC = gp_fit$selection_metric %||% NA_character_,
        KERNEL_FUNCTION  = as.character(gp_search_df$kernel_function),
        FIT_SUCCESS      = as.logical(gp_search_df$fit_success),
        LOOCV_MASE       = as.numeric(gp_search_df$LOOCV_MASE),
        LOOCV_MAPE       = as.numeric(gp_search_df$LOOCV_MAPE),
        TEST_MASE        = as.numeric(gp_search_df$TEST_MASE),
        TEST_MAPE        = as.numeric(gp_search_df$TEST_MAPE),
        SELECTED         = as.logical(gp_search_df$selected),
        MESSAGE          = as.character(gp_search_df$message),
        stringsAsFactors = FALSE
      )
    }

    # ------------------------------------------------------------------
    # 4. Ensemble — inverse-RMSE weighted combination
    # ------------------------------------------------------------------
    rmse_v <- c(safe_rmse(y_test, rsm_pred),
                safe_rmse(y_test, gp_pred),
                safe_rmse(y_test, ann_pred))
    inv_w  <- 1 / rmse_v
    inv_w[!is.finite(inv_w)] <- 0
    w <- if (sum(inv_w) > 0) inv_w / sum(inv_w) else rep(1 / 3, 3)

    ens_pred <- w[1] * rsm_pred + w[2] * gp_pred + w[3] * ann_pred
    ens_mase <- calc_mase(y_test, ens_pred, y_train)
    ens_mape <- calc_mape(y_test, ens_pred)

    # ------------------------------------------------------------------
    # Print summary for this response
    # ------------------------------------------------------------------
    cat(sprintf("  RSM      — MASE: %.4f  MAPE: %.4f%%\n",
                rsm_mase %||% NaN, rsm_mape %||% NaN))
    cat(sprintf("  GP       — MASE: %.4f  MAPE: %.4f%%\n",
                gp_mase  %||% NaN, gp_mape  %||% NaN))
    cat(sprintf("  ANN      — MASE: %.4f  MAPE: %.4f%%  topology: %s\n",
                ann_mase %||% NaN, ann_mape %||% NaN, ann_topology))
    cat(sprintf("  Ensemble — MASE: %.4f  MAPE: %.4f%%\n",
                ens_mase %||% NaN, ens_mape %||% NaN))

    # ------------------------------------------------------------------
    # Build one Excel row per model
    # ------------------------------------------------------------------
    make_row <- function(model_name, scaling_label, m_mase, m_mape,
                         topo = "NA", epochs = NA_integer_,
                         kernel_function = NA_character_, activation_function = NA_character_) {
      data.frame(
        DESIGN_TYPE      = design_type,
        SCALING_METHOD   = scaling_label,
        RESPONSE         = resp,
        NUM_FACTORS      = n_factors,
        TRAIN_SIZE       = n_train,
        TEST_SIZE        = n_test,
        MODEL            = model_name,
        MASE             = as.numeric(m_mase),
        MAPE             = as.numeric(m_mape),
        TOPOLOGY         = topo,
        EPOCHS           = epochs,
        KERNEL_FUNCTION  = kernel_function,
        ACTIVATION_FUNCTION = activation_function,
        stringsAsFactors = FALSE
      )
    }

    all_rows <- c(all_rows, list(
      make_row("RSM",      rsm_scaling_label, rsm_mase, rsm_mape, "NA"),
      make_row("GP",       std_scaling_label, gp_mase,  gp_mape,  "NA",
               kernel_function = gp_kernel),
      make_row("ANN",      std_scaling_label, ann_mase, ann_mape, ann_topology,
               epochs = ann_epochs, activation_function = ann_activation),
      make_row("Ensemble", if (rsm_scaling_label == "NONE" && std_scaling_label == "NONE") "NONE"
                            else paste(c(
                              if (rsm_scaling_label != "NONE") "RSM:CODED" else NULL,
                              if (std_scaling_label != "NONE") "GP/ANN:STANDARDIZED" else NULL
                            ), collapse = "; "),
               ens_mase, ens_mape, ann_topology,
               epochs = ann_epochs, kernel_function = gp_kernel,
               activation_function = ann_activation)
    ))

    all_fits[[resp]] <- list(
      rsm_fit    = rsm_fit,
      gp_fit     = gp_fit,
      ann_model  = ann_model,
      rsm_pred   = rsm_pred,
      gp_pred    = gp_pred,
      ann_pred   = ann_pred,
      ens_pred   = ens_pred,
      gp_grid_search = gp_fit$grid_search,
      weights    = w,
      metrics    = data.frame(
        Model    = c("RSM", "GP", "ANN", "Ensemble"),
        MASE     = c(rsm_mase, gp_mase, ann_mase, ens_mase),
        MAPE     = c(rsm_mape, gp_mape, ann_mape, ens_mape),
        topology = c("NA", "NA", ann_topology, ann_topology),
        stringsAsFactors = FALSE
      )
    )
  }

  # ---- Write / append to Excel (sheet "Model Metrics") ----
  out_df <- do.call(rbind, all_rows)
  gp_out_df <- if (length(gp_search_rows) > 0) do.call(rbind, gp_search_rows) else NULL
  sheet  <- "Model Metrics"
  gp_sheet <- "GP Kernel Search"

  # Safely combine old and new data.frames even when columns differ
  safe_rbind <- function(old_df, new_df) {
    if (is.null(old_df) || nrow(old_df) == 0) return(new_df)
    if (is.null(new_df) || nrow(new_df) == 0) return(old_df)
    all_cols <- union(names(old_df), names(new_df))
    for (col in setdiff(all_cols, names(old_df))) old_df[[col]] <- NA
    for (col in setdiff(all_cols, names(new_df))) new_df[[col]] <- NA
    rbind(old_df[, all_cols, drop = FALSE], new_df[, all_cols, drop = FALSE])
  }

  sort_gp_kernel_search <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)

    df$SELECTED[is.na(df$SELECTED)] <- FALSE
    df$FIT_SUCCESS[is.na(df$FIT_SUCCESS)] <- FALSE

    loocv_mase <- df$LOOCV_MASE
    loocv_mase[!is.finite(loocv_mase)] <- Inf
    loocv_mape <- df$LOOCV_MAPE
    loocv_mape[!is.finite(loocv_mape)] <- Inf

    df[order(
      df$DESIGN_TYPE,
      df$RESPONSE,
      -as.integer(df$SELECTED),
      -as.integer(df$FIT_SUCCESS),
      loocv_mase,
      loocv_mape,
      df$KERNEL_FUNCTION
    ), , drop = FALSE]
  }

  # Build a fresh workbook each time to avoid the openxlsx loadWorkbook()
  # 'sheetrId not found' bug.  Preserve all existing sheets via read.xlsx().
  existing_sheets <- if (file.exists(excel_file)) {
    tryCatch(openxlsx::getSheetNames(excel_file), error = function(e) character(0))
  } else character(0)

  wb <- openxlsx::createWorkbook()

  for (s in existing_sheets) {
    old_data <- tryCatch(openxlsx::read.xlsx(excel_file, sheet = s), error = function(e) NULL)
    openxlsx::addWorksheet(wb, s)
    if (!is.null(old_data) && nrow(old_data) > 0)
      openxlsx::writeData(wb, sheet = s, x = old_data)
  }

  if (sheet %in% existing_sheets) {
    existing  <- tryCatch(openxlsx::read.xlsx(excel_file, sheet = sheet),
                          error = function(e) NULL)
    combined  <- safe_rbind(existing, out_df)
    # Rewrite the entire sheet with combined data (handles column changes safely)
    openxlsx::removeWorksheet(wb, sheet)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet = sheet, x = combined)
  } else {
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet = sheet, x = out_df)
  }

  if (!is.null(gp_out_df)) {
    gp_out_df <- sort_gp_kernel_search(gp_out_df)

    if (gp_sheet %in% names(wb)) {
      existing_gp <- tryCatch(
        openxlsx::read.xlsx(excel_file, sheet = gp_sheet),
        error = function(e) NULL
      )
      gp_out_df <- tryCatch(
        sort_gp_kernel_search(safe_rbind(existing_gp, gp_out_df)),
        error = function(e) {
          warning("GP Kernel Search rbind failed, writing new data only: ", e$message)
          gp_out_df
        }
      )
      openxlsx::removeWorksheet(wb, gp_sheet)
      openxlsx::addWorksheet(wb, gp_sheet)
      openxlsx::writeData(wb, sheet = gp_sheet, x = gp_out_df)
    } else {
      openxlsx::addWorksheet(wb, gp_sheet)
      openxlsx::writeData(wb, sheet = gp_sheet, x = gp_out_df)
    }
  }

  tryCatch(
    save_workbook_safe(wb, excel_file, overwrite = TRUE),
    error = function(e) {
      # Fallback: if file is locked (e.g. open in Excel on Windows), save to a timestamped copy
      fallback_file <- sub("\\.xlsx$",
                           paste0("_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
                           excel_file)
      warning(sprintf("Could not save to '%s' (%s). Saving to fallback: %s",
                      excel_file, e$message, fallback_file))
      save_workbook_safe(wb, fallback_file, overwrite = TRUE)
    }
  )

  cat(sprintf("\nResults written to: %s  (sheet: '%s')\n", excel_file, sheet))
  invisible(all_fits)
}


# ---- Usage example ----
# source("new_function.R")
#
# Single-response example (CCD):
#
# train_data <- read.table("CCD-1 data.txt", header = TRUE)
# test_data  <- read.table("CCd-1 validate data.txt", header = TRUE)
#
# results <- doe_meta_model(
#   train_data   = train_data,
#   test_data    = test_data,
#   responses    = "Observed",
#   predictors   = c("A", "B", "C"),
#   rsm_formulas = list(Observed = Observed ~ SO(A, B, C)),
#   design_type          = "CCD",
#   excel_file           = "DOE_Metrics.xlsx",
#   ann_max_runtime_secs = 120,   # optional: increase for deeper search
#   ann_nfolds           = 5,
#   ann_max_models       = 10
# )
#
# Output sheet "Model Metrics" now includes:
#   DESIGN_TYPE, SCALING_METHOD, RESPONSE, NUM_FACTORS,
#   TRAIN_SIZE, TEST_SIZE, MODEL, MASE, MAPE, TOPOLOGY,
#   EPOCHS, KERNEL_FUNCTION
#
# Notes:
#   RSM rows: TOPOLOGY = "NA", EPOCHS = NA, KERNEL_FUNCTION = NA
#   GP rows:  TOPOLOGY = "NA", EPOCHS = NA, KERNEL_FUNCTION = covtype used in gp_master_smallDOE()
#   ANN rows: TOPOLOGY = best hidden-layer topology, EPOCHS = best model epochs, KERNEL_FUNCTION = NA
#   Ensemble rows: TOPOLOGY = ANN topology, EPOCHS = ANN epochs, KERNEL_FUNCTION = GP kernel function
#
# Multi-response example (BBD):
#
# train_data <- read.table("BBD-1 data.txt", header = TRUE)
# test_data  <- read.table("BBD-1 test.txt",  header = TRUE)
#
# results <- doe_meta_model(
#   train_data   = train_data,
#   test_data    = test_data,
#   responses    = c("t", "KGM", "WI"),
#   predictors   = c("DT", "DV", "MT"),
#   rsm_formulas = list(
#     t   = t   ~ SO(DT, DV, MT),
#     KGM = KGM ~ SO(DT, DV, MT),
#     WI  = WI  ~ SO(DT, DV, MT)
#   ),
#   design_type  = "BBD",
#   excel_file   = "DOE_Metrics.xlsx"
# )

############################################################
# Cleanup: Remove h2o log/error files older than N days
############################################################
cleanup_h2o_logs <- function(max_age_days = 7,
                             search_dirs = NULL,
                             dry_run = FALSE) {
  if (is.null(search_dirs)) {
    search_dirs <- unique(c(
      Sys.getenv("R_OPENXLSX_TMPDIR", unset = ""),
      Sys.getenv("TMPDIR", unset = ""),
      Sys.getenv("TEMP", unset = ""),
      Sys.getenv("TMP", unset = ""),
      file.path(getwd(), ".openxlsx_tmp"),
      tempdir()
    ))
    search_dirs <- search_dirs[nzchar(search_dirs)]
  }

  cutoff <- Sys.time() - as.difftime(max_age_days, units = "days")
  removed <- character(0)

  for (d in search_dirs) {
    if (!dir.exists(d)) next

    h2o_files <- list.files(
      d,
      pattern     = "h2o.*\\.(log|err|out|pid)(\\.[0-9]+)?$",
      recursive   = TRUE,
      full.names  = TRUE
    )

    if (length(h2o_files) == 0) next

    info <- file.info(h2o_files)
    old  <- h2o_files[!is.na(info$mtime) & info$mtime < cutoff]

    if (length(old) == 0) next

    if (dry_run) {
      removed <- c(removed, old)
    } else {
      ok <- file.remove(old)
      removed <- c(removed, old[ok])
    }
  }

  # Remove empty h2ologs directories
  if (!dry_run) {
    for (d in search_dirs) {
      if (!dir.exists(d)) next
      log_dirs <- list.dirs(d, recursive = TRUE, full.names = TRUE)
      log_dirs <- grep("h2ologs", log_dirs, value = TRUE)
      for (ld in log_dirs) {
        if (dir.exists(ld) && length(list.files(ld, all.files = TRUE, no.. = TRUE)) == 0) {
          unlink(ld, recursive = TRUE)
        }
      }
    }
  }

  if (length(removed) > 0) {
    cat(sprintf("[cleanup_h2o_logs] %s %d file(s) older than %d day(s):\n",
                ifelse(dry_run, "Would remove", "Removed"),
                length(removed), max_age_days))
    cat(paste("  ", removed, collapse = "\n"), "\n")
  } else {
    cat(sprintf("[cleanup_h2o_logs] No h2o log files older than %d day(s) found.\n",
                max_age_days))
  }

  invisible(removed)
}

# Example usage:
# cleanup_h2o_logs(max_age_days = 7, dry_run = TRUE)
