# guf
R package: Generally Useful Functions

This package can be thought of as an extension of 'base R', in the sense that it provides simple, "base R"-like functionality. In short (and see details in documentation/help):

%notin%
which is the inverse of and complements `%in%`

right()
which gives a specifiable number of characters from the 'right' side of a character string

left()
which gives a specifiable number of characters from the 'left' side of a character string

se()
which gives the standard error of a numerical vector

round_something()
which provides a numerical rounding of a vector, matrix or dataframe to a specifiable decimal place; the dataframe can be of mixed variable types, as only numerical columns are rounded

count()
which counts the number of elements per unique element in a vector, matrix or combination of vectors (dataframe columns); this function can 'pivot', i.e., make vertical rows horizontal

aggro()
which can make aggregates, somewhat like the already existing aggregate(), but then also with the option of 'pivoting'
