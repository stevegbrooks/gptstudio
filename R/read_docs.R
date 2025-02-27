read_docs <- function(user_prompt) {
  calls <- locate_double_colon_calls(user_prompt)

  if (length(calls) == 0) {
    return()
  }

  documentation <- calls |>
    purrr::map(function(x) read_html_docs(x$pkg_ref, x$topic))

  inner_text <- documentation |>
    purrr::map(docs_get_inner_text)

  purrr::map2(calls, inner_text, ~ c(.x, list(inner_text = .y)))
}


read_html_docs <- function(pkg_ref, topic_name) {
  # This should output a scalar character
  file_location <- utils::help(topic = (topic_name), package = (pkg_ref), help_type = "html") |>
    as.character()

  if (rlang::is_empty(file_location)) {
    return()
  }

  env <- rlang::new_environment()

  file_location |>
    get_help_file_path() |>
    lazyLoad(envir = env)

  # Convert Rd object to plain text
  rd_obj <- env[[topic_name]]
  if (inherits(rd_obj, "Rd")) {
    # Extract directly from Rd object
    get_section <- function(rd, what) {
      pos <- which(sapply(rd, attr, "Rd_tag") == paste0("\\", what))
      if (length(pos) == 0) return(NULL)
      
      # Extract and collapse text from section
      section_content <- unlist(rd[pos])
      if (length(section_content) == 0) return(NULL)
      
      # Clean up the content
      cleaned <- paste(section_content, collapse = " ")
      cleaned <- gsub("\\s+", " ", cleaned)
      return(trimws(cleaned))
    }
    
    # Build structured document
    return(list(
      title = get_section(rd_obj, "title"),
      description = get_section(rd_obj, "description"),
      usage = get_section(rd_obj, "usage"),
      arguments = get_section(rd_obj, "arguments"),
      value = get_section(rd_obj, "value"),
      examples = get_section(rd_obj, "examples"),
      format = get_section(rd_obj, "format")
    ))
  } else {
    return(as.character(rd_obj))
  }
}

get_help_file_path <- function(file) {
  path <- dirname(file)
  dirpath <- dirname(path)
  if (!file.exists(dirpath)) {
    stop(gettextf("invalid %s argument", sQuote("file")),
      domain = NA
    )
  }
  pkgname <- basename(dirpath)
  file.path(path, pkgname)
}


docs_get_inner_text <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  
  # If we already have a structured list, return it
  if (is.list(x)) {
    return(x)
  }
  
  # Otherwise, use the previous regex approach for string input
  x <- stringr::str_replace_all(x, "\r\n", "\n")
  
  # Extract title - the first line that's not Description/Usage etc.
  title_pattern <- "^(.*?)\\n+[A-Z][a-z]+:"
  title <- stringr::str_match(x, title_pattern)[1, 2]
  title <- stringr::str_trim(title)
  
  # Extract sections using regex
  get_section <- function(section_name) {
    pattern <- paste0(section_name, ":\\s*\\n+(.*?)(?=\\n+[A-Z][a-z]+:|$)")
    match <- stringr::str_match(x, pattern)
    if (!is.na(match[1, 2])) {
      content <- match[1, 2]
      content <- stringr::str_trim(content)
      return(content)
    }
    return(NULL)
  }
  
  list(
    title = title,
    description = get_section("Description"),
    usage = get_section("Usage"),
    arguments = get_section("Arguments"),
    format = get_section("Format"),
    value = get_section("Value"),
    examples = get_section("Examples")
  )
}

locate_double_colon_calls <- function(x) {
  all_matches <- x |>
    stringr::str_extract_all("\\b[a-zA-Z][a-zA-Z0-9\\.]*[a-zA-Z0-9]::(\\w|\\.)+\\b")

  all_matches[[1]] |>
    stringr::str_remove_all("`") |>
    stringr::str_split("::") |>
    purrr::map(~ list(pkg_ref = .x[1], topic = .x[2]))
}

docs_to_message <- function(x) {
  inner_content <- x$inner_text |>
    purrr::compact() |>
    purrr::imap_chr(function(.x, i) {
      if (i == "title") {
        return(glue::glue("# {.x}"))
      }

      section_title <- stringr::str_to_title(i)
      section_body <- if (i == "examples") glue::glue("<pre>{.x}</pre>") else .x
      glue::glue("## {section_title}\n\n{section_body}")
    }) |>
    paste0(collapse = "\n\n")

  glue::glue("gptstudio-metadata-docs-start-{x$pkg_ref}-{x$topic}-gptstudio-metadata-docs-end{inner_content}") # nolint
}

add_docs_messages_to_history <- function(skeleton_history) {
  last_user_message <- skeleton_history[[length(skeleton_history)]]$content
  docs <- read_docs(last_user_message)

  if (is.null(docs)) {
    return(skeleton_history)
  }

  purrr::walk(docs, ~ {
    if (is.null(.x$inner_text)) {
      return(NULL)
    }
    skeleton_history <<- chat_history_append(
      history = skeleton_history,
      role = "user",
      content = docs_to_message(.x),
      name = "docs"
    )
  })
  skeleton_history
}
