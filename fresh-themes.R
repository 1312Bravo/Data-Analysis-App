orangeColor <- "#fcba03"
blackColor <- "#111111"
whiteColor <- "#ffffff"
fillColor <- "#787878"
borderRadius <- "5px"
buttonWidth <- "130px"
selectBarWidth <- "200px"
radioButtonWidth3 <- "250px"

durNotify <- 5
errorNotify <- "Error! Something went wrong. Try again and take more care."
succNotify <- "If everything made sense it was done. Else, error somewhere."

errNotification <- function(ID){showNotification(ui = errorNotify, duration = durNotify, type = "error", id = ID)} 
succNotification <- function(ID){showNotification(ui = succNotify, duration = durNotify, type = "message", id = ID)}

succNotifyTag <- function(name){paste0("#shiny-notification-", name, "{position:fixed; top: calc(50%); left: calc(50% - 31px); 
                                       background-color: #fff;, color: blue; border: black 1px solid; border-radius: 5px; width: 250px}")}
errNotifyTag <- function(name){paste0("#shiny-notification-", name, "{position:fixed; top: calc(50%); left: calc(50% - 30px); 
                                      background-color: #fff; color: red; border: black 1px solid; border-radius: 5px; width: 250px}")}
  
mytheme <- create_theme(
  theme = "default",
  bs_vars_button(
    font_weight = 500,
    border_radius_base = borderRadius,
    default_color = blackColor,
    default_border = blackColor,
    primary_color = whiteColor,
    # primary_bg = orangeColor,
    # primary_border = orangeColor
  )
)

mytheme_2 <- create_theme(
  adminlte_color(
    light_blue = blackColor
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = blackColor,
    dark_hover_bg = blackColor,
    dark_color = whiteColor,
    dark_hover_color = orangeColor
  ),
  adminlte_global(
    content_bg = whiteColor,
    box_bg = whiteColor,
    info_box_bg = whiteColor
  )
)

num_var_1_num_var_2 <- "(input.num_var_1 != 'Not Selected' && input.num_var_2 != 'Not Selected' && input.fact_var == 'Not Selected')"
num_var_one <- "(input.num_var_1 != 'Not Selected' && input.num_var_2 == 'Not Selected' && input.fact_var == 'Not Selected') || (input.num_var_1 == 'Not Selected' && input.num_var_2 != 'Not Selected' && input.fact_var == 'Not Selected')"
num_var_1 <- "(input.num_var_1 != 'Not Selected' && input.num_var_2 == 'Not Selected' && input.fact_var == 'Not Selected')"
num_var_2 <- "(input.num_var_1 == 'Not Selected' && input.num_var_2 != 'Not Selected' && input.fact_var == 'Not Selected')"
fact_var <- "(input.num_var_1 == 'Not Selected' && input.num_var_2 == 'Not Selected' && input.fact_var != 'Not Selected')"
num_var_1_fact_var <- "(input.num_var_1 != 'Not Selected' && input.num_var_2 == 'Not Selected' && input.fact_var != 'Not Selected')"
num_var_2_fact_var <- "(input.num_var_1 == 'Not Selected' && input.num_var_2 != 'Not Selected' && input.fact_var != 'Not Selected')"
num_var_fact_var <- "(input.num_var_1 != 'Not Selected' && input.num_var_2 == 'Not Selected' && input.fact_var != 'Not Selected') || (input.num_var_1 == 'Not Selected' && input.num_var_2 != 'Not Selected' && input.fact_var != 'Not Selected')"
num_var_1_num_var_2_fact_var <- "(input.num_var_1 != 'Not Selected' && input.num_var_2 != 'Not Selected' && input.fact_var != 'Not Selected')"
no_var <- "(input.num_var_1 == 'Not Selected' && input.num_var_2 == 'Not Selected' && input.fact_var == 'Not Selected')"  
  

  
  
  
  
  
  
  
  


