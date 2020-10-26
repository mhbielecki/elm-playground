# Simple tree zipper in Elm
Uses elm-ui for layout and styling.

## Tree-zipper
A zipper for navigating a tree defined as

    type Tree a
        = Empty
        | Node a (Tree a) (Tree a)

## Todo
- Get "Next" to work
- Implement "Left sibling"
