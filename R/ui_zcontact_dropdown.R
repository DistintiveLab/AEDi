#' Contact Item
#'
#' Creates an item to be placed in a contact dropdownmenu.
#'
#' @name contact_item
#' @param name Name
#' @param role Role
#' @param phone Phone
#' @param email Email
#' @return contact menu item
#' @importFrom shiny tagList tags a icon
#' @examples
#' contact_item("Rodrigo Borges","Data Scientist","5595959595","appsatdistintive.com")
contact_item <- function(name = "First Name, Last Name",
                         role = "Role",
                         phone = "###-###-####",
                         email = "first.last@oliverwyman.com"){

  shiny::tagList(
    shiny::tags$li(shiny::a(href = "#", shiny::h4(tags$b(name)), shiny::h5(tags$i(role)))),
    shiny::tags$li(shiny::a(shiny::icon("envelope"), href = paste0("mailto:", email), email)),
    shiny::tags$li(shiny::a(shiny::icon("phone"), href = "#", phone)),
    shiny::tags$hr()
  )

}

#' Contact Menu
#'
#' Creates a dropdown menu specific for contacts
#'
#' @name contact_menu
#' @param ... contact items to put into dropdown
#' @return menu
#'
#' @importFrom shiny tags div
contact_menu <- function(...){

  items <- c(list(...))

  shiny::tags$li(
    class = "dropdown",
    shiny::tags$a(
      href = "#",
      class = "dropdown-toggle",
      `data-toggle` = "dropdown",
      shiny::div(
        shiny::tags$i(
          class = "fa fa-phone"
        ),
        "Contato",
        style = "display: inline"
      ),
      shiny::tags$ul(
        class = "dropdown-menu",
        items)
    )
  )
}
