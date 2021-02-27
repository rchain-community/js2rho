
type Expression = (
    ["use", string]
  | ["call", Expression, Expression[]]
  | ["arrow", Pattern[], Block]
  | [BinOp, Expression, Expression]
);
type BinOp = "-";

interface Statement {}; // TODO

type Block = (Declaration | Statement)[];
type Declaration = (
    ["let", Binding[]]
)
type Pattern = ["def", string];
type Binding = ["bind", [Pattern, Expression][]]
type ModuleDeclaration = (
    ["const", Binding[]]
)

type Module = ["module", ModuleDeclaration[]];
