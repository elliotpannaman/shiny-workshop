library(rsconnect)

rsconnect::setAccountInfo(name='devday',
                          token='2FA179ED87E8091FC9A3E39C6B22DC4B',
                          secret='wFXRdhepkahFIYakpc0JvAMlFvplw4DiKIzqBwFx')

rsconnect::deployApp(appDir = '/Users/elliotpannaman/Desktop/workshop_split/',
                     appName = 'workshop',
                     account = 'devday')

# runApp('/Users/elliotpannaman/Desktop/rstuff/workshop_split/')
# setwd('/Users/elliotpannaman/Desktop/rstuff/workshop_split/')
