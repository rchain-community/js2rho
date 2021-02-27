
type Expression = (
    [tag: "data", value: null | boolean | number | string]
  | [tag: "use", name: string]
  | [tag: "call", callee: Expression, args: Expression[]]
  | [tag: "arrow", args: Pattern[], body: Block]
  | [tag: BinOp, lhs: Expression, rhs: Expression]
);
type BinOp = "-";

type Statement = Expression; // TODO

type Block = (Declaration | Statement)[];
type Declaration = (
    [tag: "let", bindings: Binding[]]
  | ModuleDeclaration
)
type Pattern = [tag: "def", name: string];
type Binding = [tag: "bind", pat: Pattern, expr: Expression]
type ModuleDeclaration = (
    [tag: "const", bindings: Binding[]]
  | [tag: "import", clause: ["importBind", ['as', string, string][]], specifier: string]
)
type Module = [tag: "module", decls: ModuleDeclaration[]];
