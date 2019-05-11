#
# Add new data from detail-pages
# Only tests for now...
#

url <- 'https://www.jaap.nl/koophuizen/zuid+holland/groot-rijnmond/rotterdam/50+-woonopp/p9'
webpage <- read_html(url)

webpage

links_html <- html_nodes(webpage,".property-inner") %>%
  html_attr("href")

html_nodes(webpage,".property") %>%
  html_attr("id")


click_through <- read_html(links_html[1])

html_text(html_nodes(click_through,".short-description")[1])
html_text(html_nodes(click_through,"#long-description")[1])
html_text(html_nodes(click_through,".broker-name"))


kenmerk_html <- html_nodes(click_through,".no-dots")
waarde_html <- html_nodes(click_through,".value")

print(kenmerk_html)
waarde_html

click_through <- read_html(links_html[7])


kenmerk7_html <- html_nodes(click_through,".no-dots")
waarde7_html <- html_nodes(click_through,".value")

kenmerk7_html[26]
waarde7_html[26]
