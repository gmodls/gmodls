use crate::span::Span;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Node<T> {
    pub inner: T,
    pub span: Span,
    pub state: RefCell<NodeState>,
}

impl<T> Node<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Self {
            inner,
            span,
            state: RefCell::new(NodeState {
                leading_comments: vec![],
                trailing_comments: vec![],
                inner_comments: vec![],
            }),
        }
    }

    pub fn set_leading_comments(&self, comments: &Vec<Comment>) {
        let mut state = self.state.borrow_mut();
        state.leading_comments = (*comments).clone();
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NodeState {
    leading_comments: Vec<Comment>,
    trailing_comments: Vec<Comment>,
    inner_comments: Vec<Comment>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NodeRef<T>(Rc<Node<T>>);

impl<T> NodeRef<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Self(Rc::new(Node::new(inner, span)))
    }
}

impl<T> Deref for NodeRef<T> {
    type Target = Node<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Comment {
    Standard(String),
    Multiline(String, u16),
    StandardGmod(String),
    MultilineGmod(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Block {
    pub statements: Vec<NodeRef<Statement>>,
    pub last_statement: Option<NodeRef<Statement>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    Assignment(AssignmentStatement),
    FunctionCall(FunctionCall),
    Do(DoStatement),
    While(WhileStatement),
    Repeat(RepeatStatement),
    If(IfStatement),
    For(ForStatement),
    ForIn(ForInStatement),
    Function(FunctionStatement),
    LocalFunction(LocalFunctionStatement),
    LocalAssignment(LocalAssignmentStatement),
    Label(LabelStatement),
    Return(ReturnStatement),
    Goto(GotoStatement),
    Break,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AssignmentStatement {
    pub variables: NodeRef<VariableList>,
    pub expressions: NodeRef<ExpressionList>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DoStatement {
    pub block: NodeRef<Block>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WhileStatement {
    pub condition: NodeRef<Expression>,
    pub block: NodeRef<Block>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RepeatStatement {
    pub block: NodeRef<Block>,
    pub condition: NodeRef<Expression>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IfStatement {
    pub primary: NodeRef<ConditionalBlock>,
    pub else_ifs: Vec<NodeRef<ConditionalBlock>>,
    pub last: Option<NodeRef<Block>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ConditionalBlock {
    pub condition: NodeRef<Expression>,
    pub block: NodeRef<Block>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ForStatement {
    pub variable: NodeRef<NameLiteral>,

    pub start: NodeRef<Expression>,
    pub stop: NodeRef<Expression>,
    pub step: Option<NodeRef<Expression>>,

    pub block: NodeRef<Block>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ForInStatement {
    pub names: NodeRef<NameList>,
    pub expressions: NodeRef<ExpressionList>,
    pub block: NodeRef<Block>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionStatement {
    pub name: NodeRef<FunctionName>,
    pub body: NodeRef<FunctionBody>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LocalFunctionStatement {
    pub name: NodeRef<NameLiteral>,
    pub body: NodeRef<FunctionBody>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LocalAssignmentStatement {
    pub names: NodeRef<NameList>,
    pub expressions: Option<NodeRef<ExpressionList>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LabelStatement {
    pub name: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReturnStatement {
    pub expressions: Option<NodeRef<ExpressionList>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GotoStatement {
    pub name: NodeRef<NameLiteral>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression {
    Nil,                                  // nil
    False,                                // false
    True,                                 // true
    Number(NumberLiteral),                // 0.01
    String(StringLiteral),                // ""
    Variadic,                             // ...
    AnonymousFunction(AnonymousFunction), // function() end
    Prefix(NodeRef<PrefixExpression>),    // vars, (expr)
    Table(NodeRef<TableLiteral>),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExpressionList(pub Vec<NodeRef<Expression>>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PrefixExpression {
    Variable(Variable),
    FunctionCall(FunctionCall),
    Paren(NodeRef<Expression>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BinaryExpression {
    pub lhs: NodeRef<Expression>,
    pub op: BinaryOperation,
    pub rhs: NodeRef<Expression>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BinaryOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    Modulo,
    Concat,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UnaryExpression {
    pub op: UnaryOperation,
    pub rhs: NodeRef<Expression>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum UnaryOperation {
    Not,
    Negate,
    Length,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FunctionCall {
    Standard(StandardFunctionCall),
    This(ThisFunctionCall),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StandardFunctionCall {
    pub prefix: NodeRef<PrefixExpression>,
    pub args: NodeRef<Args>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ThisFunctionCall {
    pub prefix: NodeRef<PrefixExpression>,
    pub name: NodeRef<NameLiteral>,
    pub args: NodeRef<Args>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Args {
    Expressions(ExpressionsArgs),
    Table(TableArgs),
    String(StringArgs),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExpressionsArgs {
    pub expressions: NodeRef<ExpressionList>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TableArgs {
    pub table: NodeRef<TableLiteral>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StringArgs {
    pub string: NodeRef<StringLiteral>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Variable {
    Name(NameVariable),
    Dot(DotVariable),
    Bracket(BracketVariable),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NameVariable {
    pub name: NodeRef<NameLiteral>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DotVariable {
    pub prefix: NodeRef<PrefixExpression>,
    pub index: NodeRef<NameLiteral>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BracketVariable {
    pub prefix: NodeRef<PrefixExpression>,
    pub index: NodeRef<Expression>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VariableList(pub Vec<NodeRef<Variable>>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AnonymousFunction {
    pub body: NodeRef<FunctionBody>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionName {
    pub path: Vec<NodeRef<NameLiteral>>,
    pub last: Option<NodeRef<NameLiteral>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionBody {
    pub parameters: Option<NodeRef<ParameterList>>,
    pub block: NodeRef<Block>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParameterList {
    pub names: NodeRef<NameList>,
    pub variadic: Option<NodeRef<Variadic>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Variadic;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TableLiteral {
    pub fields: NodeRef<FieldList>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FieldList {
    pub fields: Vec<NodeRef<Field>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Field {
    ExpressionIndex(ExpressionIndexField),
    NameIndex(NameField),
    Expression(ExpressionField),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExpressionIndexField {
    pub index: NodeRef<Expression>,
    pub value: NodeRef<Expression>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NameField {
    pub index: NodeRef<NameLiteral>,
    pub value: NodeRef<Expression>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExpressionField {
    pub value: NodeRef<Expression>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NameLiteral(pub String);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NameList(pub Vec<NodeRef<NameLiteral>>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NumberLiteral(pub String);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StringLiteral(pub String);
