module D2LA.LinearAlgebraAST

[<Struct>]
type Mtx =
    val name: string
    new (n) = {name = n}

type ExpandModifier =
    | NewDim
    | DuplicateDim of int

type Expr =
    | M of Mtx
    | Mult of Expr * Expr * int * int
    | Add of Expr * Expr
    | Expand of Expr * ExpandModifier
    | Fix of Expr
    | Project of Expr * List<int>
