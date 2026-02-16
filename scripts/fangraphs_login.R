library(httr2)
library(rvest)

# persistent cookie jar location
cookie_path <- path.expand("~/.fangraphs_cookiejar")

fg_login <- function() {
  
  user <- Sys.getenv("FANGRAPHS_USER")
  pass <- Sys.getenv("FANGRAPHS_PASS")
  
  if (user == "" || pass == "") {
    stop("Add FANGRAPHS_USER and FANGRAPHS_PASS to ~/.Renviron", call. = FALSE)
  }
  
  message("Logging into Fangraphs...")
  
  # Step 1 — Load login page (WordPress requires initial cookies)
  login_page <- request("https://blogs.fangraphs.com/wp-login.php") |>
    req_cookie_preserve(path = cookie_path) |>
    req_user_agent("Mozilla/5.0") |>
    req_perform()
  
  html <- resp_body_html(login_page)
  
  redirect <- html |>
    html_element("input[name='redirect_to']") |>
    html_attr("value")
  
  # Step 2 — Submit login form
  request("https://blogs.fangraphs.com/wp-login.php") |>
    req_cookie_preserve(path = cookie_path) |>
    req_user_agent("Mozilla/5.0") |>
    req_body_form(
      log = user,
      pwd = pass,
      redirect_to = redirect,
      "wp-submit" = "Log In",
      testcookie = "1"
    ) |>
    req_perform()
  
  # Step 3 — VERY IMPORTANT
  # Activate Fangraphs main-site session (creates fg_session cookie)
  message("Activating Fangraphs session...")
  
  request("https://www.fangraphs.com/") |>
    req_cookie_preserve(path = cookie_path) |>
    req_user_agent("Mozilla/5.0") |>
    req_perform()
  
  invisible(TRUE)
}
