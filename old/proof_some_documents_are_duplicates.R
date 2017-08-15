b <- numeric(length = length(d$doc[[k]]))
#loop over all other documents
for(i in 3:605){
  #check against one other document
  a <- match(d$doc[[k]], d$doc[[i]])
  #if there is a match, advance duplicate counter by one
  b[is.na(a)==F] <- b[is.na(a)==F]+1
}

b

d$doc[[2]]
d$doc[[605]]

identical(d$doc[[2]],d$doc[[605]])

d$path[605]
d$path[2]
