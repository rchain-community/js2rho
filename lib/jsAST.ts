
type Expression = (
    [tag: "data", value: null | boolean | number | string]
  | [tag: "record", ps: PropDef[]]
  | [tag: "get", pe: Expression, id: string]
  | [tag: "use", name: string]
  | [tag: "call", callee: Expression, args: Expression[]]
  | [tag: "arrow", args: Pattern[], body: Block]
  | [tag: BinOp, lhs: Expression, rhs: Expression]
);
type BinOp = "-";
type PropDef = (
  [tag: 'prop', key: Expression, value: Expression]
  | [tag: 'method', name: string, params: Pattern[], body: Block]
  // TODO: get, set
);

type Statement = (
  Block
  | [tag: 'if', cond: Expression, t: Block, e: Block]
  // breakable... for, while, switch
  // terminator
  | [tag: 'return', e?: Expression]
  | Expression
);

type Block = [tag: 'block', body: (Declaration | Statement)[]];
type Declaration = (
    [tag: "let", bindings: Binding[]]
  | [tag: "const", bindings: Binding[]]
  | ModuleDeclaration
)
type Pattern = [tag: "def", name: string];
type Binding = [tag: "bind", pat: Pattern, expr: Expression]
type ModuleDeclaration = (
    [tag: "export", op: "const", bindings: Binding[]]
  | [tag: "exportDefault", e: Expression]
  | [tag: "import", clause: ["importBind", ['as', string, string][]], specifier: string]
)
type Module = [tag: "module", decls: ModuleDeclaration[]];
