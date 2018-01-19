# Mayors:

# INDIANA
load("./rfiles/d_IN.rdata")

a <- subset(d, select = c("City", "POPESTIMATE", "Party"))
a <- a[!duplicated(a$City),]

a$mayor <- NA
a$website <- NA

a$mayor[a$City=="Attica"] <- "Duane Roderick"
a$mayor[a$City=="Auburn"] <- "Norman E. Yoder"
a$mayor[a$City=="Batesville"] <- "Mike Bettice"
a$website[a$City=="Batesville"] <- "https://betticeforbatesville.com"
a$mayor[a$City=="Bedford"] <- "Shawna Girgis"
a$Party[a$City=="Bedford"] <- "Independent"
a$mayor[a$City=="Bloomington"] <- "John Hamilton"
a$website[a$City=="Bloomington"] <- "http://www.johnhamiltonformayor.com/"
a$mayor[a$City=="Boonville"] <- "Charlie Wyatt"
a$mayor[a$City=="Brazil"] <- "Brian Wyndham"
a$mayor[a$City=="Carmel"] <- "James Brainard"
a$website[a$City=="Carmel"] <- "http://www.jimbrainard.com/"
a$mayor[a$City=="Connersville"] <- "Harold Gordon"
a$mayor[a$City=="Elkhart"] <- "Tim Neese"
a$mayor[a$City=="Elwood"] <- "Todd Jones"
a$mayor[a$City=="Evansville"] <- "Lloyd Winnecke"
a$mayor[a$City=="Fort Wayne"] <- "Tom Henry"
a$website[a$City=="Fort Wayne"] <- "https://web.archive.org/web/20150702151223/http://www.tomhenryformayor.org/"
a$mayor[a$City=="Frankfort"] <- "Chris McBarnes"
a$website[a$City=="Gary"] <- "http://karenaboutgary.com/"
a$mayor[a$City=="Gary"] <- "Karen Freeman-Wilson"
a$mayor[a$City=="Hobart"] <- "Brian K. Snedecor"
a$mayor[a$City=="Huntingburg"] <- "Dennis Spinner"
a$mayor[a$City=="Indianapolis"] <- "Joe Hogsett"
a$website[a$City=="Indianapolis"] <- "http://joehogsett.com/"
a$mayor[a$City=="Jasper"] <- "Terry R. Seitz"
a$mayor[a$City=="Lake Station"] <- "Christopher Anderson"
a$mayor[a$City=="Madison"] <- "Damon Welch"
a$mayor[a$City=="Martinsville"] <- "Shannon Kohl"
a$mayor[a$City=="Mitchell"] <- "John England"
a$Party[a$City=="Mitchell"] <- "Independent"
a$mayor[a$City=="Monticello"] <- "Kenneth Houston"
a$mayor[a$City=="New Haven"] <- "Terry McDonald"
a$mayor[a$City=="North Vernon"] <- "Mike Ochs"
a$mayor[a$City=="Richmond"] <- "Dave Snow"
a$mayor[a$City=="Rising Sun"] <- "Brent Bascom"
a$mayor[a$City=="Rockport"] <- "Gay Ann"
a$mayor[a$City=="South Bend"] <- "Pete Buttigieg"
a$website[a$City=="South Bend"] <- "http://www.peteforsouthbend.com/"
a$mayor[a$City=="Tipton"] <- "Don Havens"
a$mayor[a$City=="Warsaw"] <- "Joseph Thallemer"
a$mayor[a$City=="Winchester"] <- "Shon Byrum"

mayors <- a
mayors$state <- "IN"

#remove everything but the mayors data frame
rm(list = ls()[ls()!="mayors"])

# LOUISIANA

load("./rfiles/d_LA.rdata")

d <- subset(d, select = c("City","POPESTIMATE","Party"))
d <- d[!duplicated(d$City),]

d$mayor <- NA
d$website <- NA

d$mayor[d$City=="Alexandria"] <- "Jacques Roy"
d$website[d$City=="Alexandria"] <- "http://www.jacquesroyformayor.com/"
d$mayor[d$City=="Blanchard"] <- "Jim Galambos"
d$mayor[d$City=="Church Point"] <- "Russell Stelly"
d$mayor[d$City=="Folsom"] <- "Lance Willie"
d$mayor[d$City=="Harahan"] <- "Tina Miceli"
d$website[d$City=="Harahan"] <- "https://web.archive.org/web/20150107032513/http://www.miceliformayor.com/"
d$mayor[d$City=="Jonesboro"] <- "James E. Bradford"
d$mayor[d$City=="Kenner"] <- "Ben Zahn"
d$website[d$City=="Kenner"] <- "http://www.benzahnmayor.com/"
d$mayor[d$City=="Melville"] <- "Erana Mayes"
d$mayor[d$City=="Minden"] <- "Tommy Davis"
d$mayor[d$City=="New Orleans"] <- "LaToya Cantrell"
d$website[d$City=="New Orleans"] <- "https://latoyacantrell.com/"
d$mayor[d$City=="Opelousas"] <- "Reggie Tatum"
d$mayor[d$City=="Patterson"] <- "Rodney Grogan"
d$mayor[d$City=="Ruston"] <- "Ronny Walker"
d$mayor[d$City=="Shreveport"] <- "Ollie Tyler"
d$mayor[d$City=="Sulphur"] <- "Christopher L. Duncan"
d$mayor[d$City=="Sunset"] <- "Charles A. James"
d$mayor[d$City=="Westlake"] <- "Robert Hardey"
d$mayor[d$City=="Winnfield"] <- "Kiah Beville"

d$state <- "LA"

mayors <- rbind(mayors, d)
mayors <- mayors[is.na(mayors$website)==F,]
mayors <- mayors[substring(mayors$website, 1, 20)!="https://web.archive.",]

#remove everything but the mayors data frame
rm(list = ls()[ls()!="mayors"])

save(mayors, file = "rfiles/mayors.rdata")
