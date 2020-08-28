#light "off"


// a tree is either a leaf node or a subtree as a tuple (node, left, right)
type Tree<'T when 'T: comparison> =
    | Nil
    | Tree of Color * 'T * Tree<'T> * Tree<'T>

// a node is either red or black
and Color =
    | Red
    | Black;;


// respecting usual BST ordering
let rec search key = function
    | Nil -> None
    | Tree(_, node, left, right) ->
        if key = node then Some node
        elif key < node then search key left
        else search key right;;

// no red node has a red child
let balance = function
    | Tree(Black, z, Tree(Red, y, Tree(Red, x, a, b), c), d)
    | Tree(Black, z, Tree(Red, x, a, Tree(Red, y, b, c)), d)
    | Tree(Black, x, a, Tree(Red, y, b, Tree(Red, z, c, d)))
    | Tree(Black, x, a, Tree(Red, z, Tree(Red, y, b, c), d)) ->
        Tree(Red, y, Tree(Black, x, a, b), Tree(Black, z, c, d))
    | other -> other;;

(*
    every simple path from a given node to any of its descendant leaves
    contains the same number of black nodes
*)
let insert item tree =
    let rec ins = function
        // (PS: inserting red does not break the black rule, only red rules)
        | Nil -> Tree(Red, item, Nil, Nil)
        | Tree(color, node, left, right) ->
            if item = node then Tree(color, node, left, right)
            elif item < node then balance (Tree(color, node, ins left, right))
            else balance (Tree(color, node, left, ins right)) in

    // the root node is always black
    match ins tree with
    | Tree(Red, root, left, right) -> Tree(Black, root, left, right)
    | other -> other;;


let rec inOrder f acc = function
    | Nil -> acc
    | Tree(_, node, left, right) ->
        let pre = inOrder f acc left in
        let mid = f pre node in
        inOrder f mid right;;
