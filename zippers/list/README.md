# Simple list zipper in Elm


## Non-empty list
A zipper for a list that must containt at least one element.

    type Zipper a =
        Zipper (List a) a (List a)

## Empty 
A zipper for a list that can be empty.

    type ZipperWithHole a =
        ZipperWithHole (List a) (Maybe a) (List a)


## Todo
- Finish the non-empty implementation