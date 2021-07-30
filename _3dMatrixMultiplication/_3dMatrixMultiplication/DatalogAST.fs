module D2LA.DatalogAST

type RName = string

type Arg =
    | FreeVar of string
    | Atom of string

type Expr = RName * List<Arg>

type Clause =
    | Fact of Expr
    | Formula of Expr * List<Expr>

type Program = List<Clause> * Expr
