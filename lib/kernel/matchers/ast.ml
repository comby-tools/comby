include Types.Ast

let (=) left right = Equal (left, right)

let (<>) left right = Not_equal (left, right)
