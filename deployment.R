library(rsconnect)

readRenviron("./.env")

rs_acc_name <- Sys.getenv("name")
rs_token <- Sys.getenv("token")
rs_secret <- Sys.getenv("secret")

rsconnect::setAccountInfo(name=rs_acc_name,
                          token=rs_token,
                          secret=rs_secret)

deployApp(appDir = ".", appName = "Sales-Dashboard")
