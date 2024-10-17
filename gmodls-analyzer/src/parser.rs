use crate::ast::*;
use crate::lex::{Lex, LexError, Token};
use crate::span;

const UNARY_PRIORITY: u8 = 8;

fn priority(binary_operation: BinaryOperation) -> (u8, u8) {
    match binary_operation {
        BinaryOperation::Add | BinaryOperation::Subtract => (6, 6),
        BinaryOperation::Multiply | BinaryOperation::Divide | BinaryOperation::Modulo => (7, 7),
        BinaryOperation::Power => (10, 9),
        BinaryOperation::Concat => (5, 4),
        BinaryOperation::Equal
        | BinaryOperation::NotEqual
        | BinaryOperation::LessThan
        | BinaryOperation::LessThanEqual
        | BinaryOperation::GreaterThan
        | BinaryOperation::GreaterThanEqual => (3, 3),
        BinaryOperation::And => (2, 2),
        BinaryOperation::Or => (1, 1),
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error("Lexical Analysis Error: {from}")]
    Lex {
        #[from]
        from: LexError,
    },

    #[error("Unexpected token \"{token:?}\".")]
    UnexpectedToken { token: Token },
}

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug)]
pub struct Parser<'source> {
    lex: Lex<'source>,
}

impl<'source> Parser<'source> {
    pub fn from_lex(mut lex: Lex<'source>) -> ParserResult<Self> {
        lex.next()?; // Initialize the lexer.

        Ok(Self { lex })
    }

    pub fn parse(&mut self) -> ParserResult<NodeRef<Block>> {
        self.parse_block()
    }
}

impl<'source> Parser<'source> {
    fn parse_block(&mut self) -> ParserResult<NodeRef<Block>> {
        let mut block = Block {
            statements: vec![],
            last_statement: None,
        };

        let start = self.lex.current_span().start();
        let mut end = start;

        loop {
            match self.lex.current_token() {
                Token::End | Token::Else | Token::ElseIf | Token::Until | Token::Eof => break,
                _ => {}
            };

            let statement = self.parse_statement()?;
            end = statement.span.end();

            match &statement.inner {
                Statement::Break | Statement::Return(_) => {
                    block.last_statement = Some(statement);
                    break;
                }
                _ => {
                    block.statements.push(statement);
                }
            };
        }

        Ok(NodeRef::new(block, span!(start, end)))
    }

    fn parse_statement(&mut self) -> ParserResult<NodeRef<Statement>> {
        Ok(match self.lex.current_token() {
            Token::If => self.parse_if()?,
            Token::While => self.parse_while()?,
            Token::Do => self.parse_do()?,
            Token::For => self.parse_for()?,
            Token::Repeat => self.parse_repeat()?,
            Token::Function => self.parse_function()?,
            Token::Local => self.parse_local()?,
            Token::Return => self.parse_return()?,
            Token::Break => self.parse_break()?,
            Token::Label(_) => self.parse_label()?,
            Token::Goto => self.parse_goto()?,
            _ => self.parse_assignment()?,
        })
    }

    fn parse_if(&mut self) -> ParserResult<NodeRef<Statement>> {
        let start = self.lex.current_span().start();

        let primary = self.parse_then()?;
        let mut else_ifs = vec![];
        let mut last = None;

        while self.lex.current_token() == Token::ElseIf {
            else_ifs.push(self.parse_then()?);
        }

        if self.lex.current_token() == Token::Else {
            self.lex.next()?;

            last = Some(self.parse_block()?);
        }

        let end = self.lex.current_span().end();

        self.check(|t| *t == Token::End)?;

        Ok(NodeRef::new(
            Statement::If(IfStatement {
                primary,
                else_ifs,
                last,
            }),
            span!(start, end),
        ))
    }

    fn parse_then(&mut self) -> ParserResult<NodeRef<ConditionalBlock>> {
        let start = self.lex.current_span().start();

        self.lex.next()?; // "if" or "elseif"

        let condition = self.parse_expression()?;

        self.check(|t| *t == Token::Then)?;

        let block = self.parse_block()?;

        let end = block.span.end();

        Ok(NodeRef::new(
            ConditionalBlock { condition, block },
            span!(start, end),
        ))
    }

    fn parse_while(&mut self) -> ParserResult<NodeRef<Statement>> {
        let start = self.lex.current_span().start();

        self.lex.next()?; // "while"

        let condition = self.parse_expression()?; // <expr>

        self.check(|t| *t == Token::Do)?; // "do"

        let block = self.parse_block()?;

        let end = self.lex.current_span().end();

        self.check(|t| *t == Token::End)?; // "end"

        Ok(NodeRef::new(
            Statement::While(WhileStatement { block, condition }),
            span!(start, end),
        ))
    }

    fn parse_do(&mut self) -> ParserResult<NodeRef<Statement>> {
        let start = self.lex.current_span().start();

        self.lex.next()?; // "do"

        let block = self.parse_block()?;

        let end = self.lex.current_span().start();

        self.check(|t| *t == Token::End)?; // "end"

        Ok(NodeRef::new(
            Statement::Do(DoStatement { block }),
            span!(start, end),
        ))
    }

    fn parse_for(&mut self) -> ParserResult<NodeRef<Statement>> {
        let start = self.lex.current_span().start();

        self.lex.next()?; // "for"

        match self.lex.current_token() {
            Token::Name(_) => {}
            _ => {
                return Err(ParserError::UnexpectedToken {
                    token: self.lex.current_token(),
                })
            }
        };

        let statement = match self.lex.lookahead_token()? {
            Token::Assign => {
                let name = self.parse_name_literal()?;
                Statement::For(self.parse_for_number(name)?)
            }
            Token::Comma | Token::In => Statement::ForIn(self.parse_for_iter()?),
            token => return Err(ParserError::UnexpectedToken { token }),
        };

        let end = self.lex.current_span().end();

        self.check(|t| *t == Token::End)?; // "end"

        Ok(NodeRef::new(statement, span!(start, end)))
    }

    fn parse_for_number(&mut self, variable: NodeRef<NameLiteral>) -> ParserResult<ForStatement> {
        self.lex.next()?; // "="

        let start = self.parse_expression()?;

        self.check(|t| *t == Token::Comma)?; // ","

        let stop = self.parse_expression()?;

        let step = match self.lex.current_token() {
            Token::Comma => {
                self.lex.next()?; // ","
                Some(self.parse_expression()?)
            }
            _ => None,
        };

        self.check(|t| *t == Token::Do)?; // "do"

        let block = self.parse_block()?;

        Ok(ForStatement {
            variable,

            start,
            stop,
            step,

            block,
        })
    }

    fn parse_for_iter(&mut self) -> ParserResult<ForInStatement> {
        let names = self.parse_name_list(false)?;

        self.check(|t| *t == Token::In)?; // "in"

        let expressions = self.parse_expression_list()?;

        self.check(|t| *t == Token::Do)?; // "do"

        let block = self.parse_block()?;

        Ok(ForInStatement {
            names,
            expressions,
            block,
        })
    }

    fn parse_repeat(&mut self) -> ParserResult<NodeRef<Statement>> {
        let start = self.lex.current_span().start();

        self.check(|t| *t == Token::Repeat)?; // "repeat"

        let block = self.parse_block()?;

        self.check(|t| *t == Token::Until)?; // "until"

        let condition = self.parse_expression()?;

        let end = condition.span.end();

        Ok(NodeRef::new(
            Statement::Repeat(RepeatStatement { block, condition }),
            span!(start, end),
        ))
    }

    fn parse_function(&mut self) -> ParserResult<NodeRef<Statement>> {
        let start = self.lex.current_span().start();

        self.check(|t| *t == Token::Function)?; // "function"

        let name = self.parse_function_name()?;
        let body = self.parse_function_body()?;

        let end = body.span.end();

        Ok(NodeRef::new(
            Statement::Function(FunctionStatement { name, body }),
            span!(start, end),
        ))
    }

    fn parse_local(&mut self) -> ParserResult<NodeRef<Statement>> {
        let start = self.lex.current_span().start();

        self.lex.next()?; // "local"

        match self.lex.current_token() {
            Token::Function => {
                self.lex.next()?; // "function"

                let name = self.parse_name_literal()?;
                let body = self.parse_function_body()?;

                let end = body.span.end();

                Ok(NodeRef::new(
                    Statement::LocalFunction(LocalFunctionStatement { name, body }),
                    span!(start, end),
                ))
            }
            Token::Name(_) => {
                let names = self.parse_name_list(false)?;

                let (expressions, end) = match self.lex.current_token() {
                    Token::Assign => {
                        self.lex.next()?; // "="

                        let expressions = self.parse_expression_list()?;
                        let end = expressions.span.end();

                        (Some(expressions), end)
                    }
                    _ => (None, names.span.end()),
                };

                Ok(NodeRef::new(
                    Statement::LocalAssignment(LocalAssignmentStatement { names, expressions }),
                    span!(start, end),
                ))
            }
            token => Err(ParserError::UnexpectedToken { token }),
        }
    }

    fn parse_return(&mut self) -> ParserResult<NodeRef<Statement>> {
        let start = self.lex.current_span().start();

        self.check(|t| *t == Token::Return)?; // "return"

        let (expressions, end) = match self.lex.current_token() {
            Token::End | Token::Else | Token::ElseIf | Token::Until => (None, start),
            _ => {
                let expressions = self.parse_expression_list()?;
                let end = expressions.span.end();
                (Some(expressions), end)
            }
        };

        Ok(NodeRef::new(
            Statement::Return(ReturnStatement { expressions }),
            span!(start, end),
        ))
    }

    fn parse_break(&mut self) -> ParserResult<NodeRef<Statement>> {
        let span = self.lex.current_span();
        Ok(NodeRef::new(Statement::Break, span))
    }

    fn parse_label(&mut self) -> ParserResult<NodeRef<Statement>> {
        match self.lex.current_token() {
            Token::Label(name) => {
                let span = self.lex.current_span();
                self.lex.next()?; // <label>
                return Ok(NodeRef::new(
                    Statement::Label(LabelStatement { name }),
                    span,
                ));
            }
            _ => unreachable!(),
        }
    }

    fn parse_goto(&mut self) -> ParserResult<NodeRef<Statement>> {
        let start = self.lex.current_span().start();

        self.lex.next()?; // "goto"

        let name = self.parse_name_literal()?;

        let end = name.span.end();

        Ok(NodeRef::new(
            Statement::Goto(GotoStatement { name }),
            span!(start, end),
        ))
    }

    fn parse_assignment(&mut self) -> ParserResult<NodeRef<Statement>> {
        let variables = self.parse_variable_list()?;

        let start = variables.span.start();

        self.check(|t| *t == Token::Assign)?; // "="

        let expressions = self.parse_expression_list()?;

        let end = expressions.span.end();

        Ok(NodeRef::new(
            Statement::Assignment(AssignmentStatement {
                variables,
                expressions,
            }),
            span!(start, end),
        ))
    }

    fn parse_variable_list(&mut self) -> ParserResult<NodeRef<VariableList>> {
        let mut variables = vec![];

        let start = self.lex.current_span().start();
        let mut end;

        loop {
            let expr = self.parse_expression_primary()?;
            let variable = match &expr.inner {
                PrefixExpression::Variable(variable) => variable.clone(),
                _ => panic!(),
            };

            end = expr.span.end();

            let variable = NodeRef::new(variable, expr.span);

            variables.push(variable);

            match self.lex.current_token() {
                Token::Comma => {
                    self.lex.next()?; // ","
                }
                _ => break,
            }
        }

        Ok(NodeRef::new(VariableList(variables), span!(start, end)))
    }

    fn parse_function_name(&mut self) -> ParserResult<NodeRef<FunctionName>> {
        let start = self.lex.current_span().start();

        let mut path = vec![];

        let mut end = loop {
            let name = self.parse_name_literal()?;
            let end = name.span.end();

            path.push(name);

            match self.lex.current_token() {
                Token::Dot => self.lex.next()?,
                _ => break end,
            };
        };

        let last = match self.lex.current_token() {
            Token::Colon => {
                let last = self.parse_name_literal()?;
                end = last.span.end();
                Some(last)
            }
            _ => None,
        };

        Ok(NodeRef::new(FunctionName { path, last }, span!(start, end)))
    }

    fn parse_args(&mut self) -> ParserResult<NodeRef<Args>> {
        let start = self.lex.current_span().start();

        let result = match self.lex.current_token() {
            Token::LeftParen => {
                self.lex.next()?;

                let expressions = self.parse_expression_list()?;

                let end = self.lex.current_span().end();
                self.check(|t| *t == Token::RightParen)?;

                NodeRef::new(
                    Args::Expressions(ExpressionsArgs { expressions }),
                    span!(start, end),
                )
            }
            Token::String(_, _) => {
                let end = self.lex.current_span().end();
                let string = self.parse_string_literal()?;

                NodeRef::new(Args::String(StringArgs { string }), span!(start, end))
            }
            Token::LeftCurly => {
                let table = self.parse_table_literal()?;
                let end = table.span.end();
                NodeRef::new(Args::Table(TableArgs { table }), span!(start, end))
            }
            _ => unreachable!(),
        };

        Ok(result)
    }

    fn parse_table_literal(&mut self) -> ParserResult<NodeRef<TableLiteral>> {
        let start = self.lex.current_span().start();

        let mut fields = vec![];

        self.lex.next()?; // "{"

        let field_list_start = self.lex.current_span().start();
        let mut field_list_end = field_list_start;

        while self.lex.current_token() != Token::RightCurly {
            let field_start = self.lex.current_span().start();

            match self.lex.current_token() {
                Token::LeftSquare => {
                    let index = self.parse_expression()?;
                    self.check(|t| *t == Token::RightSquare)?;
                    self.check(|t| *t == Token::Assign)?;
                    let value = self.parse_expression()?;

                    let field_end = value.span.end();
                    field_list_end = field_end;

                    fields.push(NodeRef::new(
                        Field::ExpressionIndex(ExpressionIndexField { index, value }),
                        span!(field_start, field_end),
                    ));
                }
                Token::Name(_) if self.lex.lookahead_token()? == Token::Assign => {
                    let index = self.parse_name_literal()?;
                    self.check(|t| *t == Token::Assign)?;
                    let value = self.parse_expression()?;

                    let field_end = value.span.end();
                    field_list_end = field_end;

                    fields.push(NodeRef::new(
                        Field::NameIndex(NameField { index, value }),
                        span!(field_start, field_end),
                    ));
                }
                _ => {
                    let value = self.parse_expression()?;

                    let field_end = value.span.end();
                    field_list_end = field_end;

                    fields.push(NodeRef::new(
                        Field::Expression(ExpressionField { value }),
                        span!(field_start, field_end),
                    ))
                }
            }

            match self.lex.current_token() {
                Token::Comma | Token::SemiColon => {
                    field_list_end = self.lex.current_span().end();
                    self.lex.next()?; // "," | ";"
                }
                _ => break,
            }
        }

        let end = self.lex.current_span().end();

        self.check(|t| *t == Token::RightCurly)?; // "}"

        Ok(NodeRef::new(
            TableLiteral {
                fields: NodeRef::new(
                    FieldList { fields },
                    span!(field_list_start, field_list_end),
                ),
            },
            span!(start, end),
        ))
    }

    fn parse_function_body(&mut self) -> ParserResult<NodeRef<FunctionBody>> {
        let start = self.lex.current_span().start();

        self.check(|t| *t == Token::LeftParen)?; // (

        // check ) preemptively to account for empty namelists
        let parameters = match self.lex.current_token() {
            Token::RightParen => None,

            _ => {
                Some(self.parse_parameter_list()?)
            }
        };

        self.check(|t| *t == Token::RightParen)?; // )
        let block = self.parse_block()?; // <contents>
        let end = self.lex.current_span().start();
        self.check(|t| *t == Token::End)?; // end

        Ok(NodeRef::new(
            FunctionBody { parameters, block },
            span!(start, end),
        ))
    }

    fn parse_expression_primary(&mut self) -> ParserResult<NodeRef<PrefixExpression>> {
        let mut expression = match self.lex.current_token() {
            Token::LeftParen => {
                let start = self.lex.current_span().start();

                self.lex.next()?; // "("
                let inner = self.parse_expression()?;

                let end = self.lex.current_span().end();

                self.check(|t| *t == Token::RightParen)?;

                NodeRef::new(PrefixExpression::Paren(inner), span!(start, end))
            }
            Token::Name(_) => {
                let span = self.lex.current_span();

                NodeRef::new(
                    PrefixExpression::Variable(Variable::Name(NameVariable {
                        name: self.parse_name_literal()?,
                    })),
                    span,
                )
            }
            token => return Err(ParserError::UnexpectedToken { token }),
        };

        let start = expression.span.start();

        loop {
            expression = match self.lex.current_token() {
                Token::Dot => {
                    self.lex.next()?; // "."

                    let index = self.parse_name_literal()?; // <name>
                    let end = index.span.end();

                    NodeRef::new(
                        PrefixExpression::Variable(Variable::Dot(DotVariable {
                            prefix: expression,
                            index,
                        })),
                        span!(start, end),
                    )
                }
                Token::LeftSquare => {
                    self.lex.next()?; // "["

                    let index = self.parse_expression()?;

                    let end = index.span.end();

                    self.check(|t| *t == Token::RightSquare)?;

                    NodeRef::new(
                        PrefixExpression::Variable(Variable::Bracket(BracketVariable {
                            prefix: expression,
                            index,
                        })),
                        span!(start, end),
                    )
                }
                Token::Colon => {
                    self.lex.next()?; // ":"

                    let name = self.parse_name_literal()?;

                    let args = self.parse_args()?;
                    let end = args.span.end();

                    NodeRef::new(
                        PrefixExpression::FunctionCall(FunctionCall::This(ThisFunctionCall {
                            prefix: expression,
                            name,
                            args,
                        })),
                        span!(start, end),
                    )
                }
                Token::LeftParen | Token::String(..) | Token::LeftCurly => {
                    let args = self.parse_args()?;
                    let end = args.span.end();

                    NodeRef::new(
                        PrefixExpression::FunctionCall(FunctionCall::Standard(
                            StandardFunctionCall {
                                prefix: expression,
                                args,
                            },
                        )),
                        span!(start, end),
                    )
                }
                _ => break,
            }
        }

        Ok(expression)
    }

    fn parse_expression_simple(&mut self) -> ParserResult<NodeRef<Expression>> {
        let span = self.lex.current_span();

        let expression = match self.lex.current_token() {
            Token::Number(inner) => Expression::Number(NumberLiteral(inner)),
            Token::String(contents, _) => Expression::String(StringLiteral(contents)),
            Token::Nil => Expression::Nil,
            Token::True => Expression::True,
            Token::False => Expression::False,
            Token::Dots => Expression::Variadic,
            Token::LeftCurly => {
                let table = self.parse_table_literal()?;
                let span = table.span;
                return Ok(NodeRef::new(Expression::Table(table), span));
            } // parse table
            Token::Function => {
                let start = self.lex.current_span().start();
                self.lex.next()?; // "function"
                let body = self.parse_function_body()?;
                let end = body.span.end();

                return Ok(NodeRef::new(
                    Expression::AnonymousFunction(AnonymousFunction { body }),
                    span!(start, end),
                ));
            } // parse function
            _ => {
                let inner = self.parse_expression_primary()?;
                let span = inner.span;

                return Ok(NodeRef::new(Expression::Prefix(inner), span));
            }
        };

        self.lex.next()?;

        Ok(NodeRef::new(expression, span))
    }

    fn parse_expression_unary(&mut self) -> ParserResult<NodeRef<Expression>> {
        let op = match self.lex.current_token() {
            Token::Not | Token::NotGmod => UnaryOperation::Not,
            Token::Subtract => UnaryOperation::Negate,
            Token::Length => UnaryOperation::Length,
            _ => return self.parse_expression_simple(),
        };

        let start = self.lex.current_span().start();

        self.lex.next()?; // <unary op>

        let (rhs, _) = self.parse_expression_binary(UNARY_PRIORITY)?;

        let end = rhs.span.end();

        Ok(NodeRef::new(
            Expression::Unary(UnaryExpression { op, rhs }),
            span!(start, end),
        ))
    }

    fn parse_expression_binary(
        &mut self,
        limit: u8,
    ) -> ParserResult<(NodeRef<Expression>, Option<BinaryOperation>)> {
        let mut lhs = self.parse_expression_unary()?;
        println!("lhs: {:?}", lhs);

        let mut op = match self.lex.current_token() {
            Token::Add => Some(BinaryOperation::Add),
            Token::Subtract => Some(BinaryOperation::Subtract),
            Token::Multiply => Some(BinaryOperation::Multiply),
            Token::Divide => Some(BinaryOperation::Divide),
            Token::Power => Some(BinaryOperation::Power),
            Token::Modulo => Some(BinaryOperation::Modulo),
            Token::Concat => Some(BinaryOperation::Concat),
            Token::LessThan => Some(BinaryOperation::LessThan),
            Token::LessThanEqual => Some(BinaryOperation::LessThanEqual),
            Token::GreaterThan => Some(BinaryOperation::GreaterThan),
            Token::GreaterThanEqual => Some(BinaryOperation::GreaterThanEqual),
            Token::Equal => Some(BinaryOperation::Equal),
            Token::NotEqual => Some(BinaryOperation::NotEqual),
            Token::And => Some(BinaryOperation::And),
            Token::Or => Some(BinaryOperation::Or),
            _ => None,
        };

        loop {
            let (priority, real_op) = match op {
                Some(op) => (priority(op), op),
                None => break,
            };

            if priority.0 <= limit {
                break;
            }

            self.lex.next()?; // <op>

            let (rhs, next_op) = self.parse_expression_binary(priority.1)?;

            let start = lhs.span.start();
            let end = rhs.span.end();

            lhs = NodeRef::new(
                Expression::Binary(BinaryExpression {
                    op: real_op,
                    lhs,
                    rhs,
                }),
                span!(start, end),
            );

            op = next_op;
        }

        Ok((lhs, op))
    }

    fn parse_expression(&mut self) -> ParserResult<NodeRef<Expression>> {
        self.parse_expression_binary(0)
            .map(|(expression, _)| expression)
    }

    fn parse_expression_list(&mut self) -> ParserResult<NodeRef<ExpressionList>> {
        let start = self.lex.current_span().start();
        let mut expressions = vec![];

        let end = loop {
            let expression = self.parse_expression()?;
            let end = expression.span.end();
            expressions.push(expression);

            match self.lex.current_token() {
                Token::Comma => {
                    self.lex.next()?;
                }
                _ => break end,
            }
        };

        Ok(NodeRef::new(ExpressionList(expressions), span!(start, end)))
    }

    fn parse_string_literal(&mut self) -> ParserResult<NodeRef<StringLiteral>> {
        let span = self.lex.current_span();

        self.check(|t| match *t {
            Token::String(..) | Token::MultilineString(..) => true,
            _ => false,
        })?;

        let literal = match self.lex.current_token() {
            Token::String(contents, _) => StringLiteral(contents),
            Token::MultilineString(contents, _) => StringLiteral(contents),
            _ => unreachable!(),
        };

        Ok(NodeRef::new(literal, span))
    }

    fn parse_name_literal(&mut self) -> ParserResult<NodeRef<NameLiteral>> {
        let (token, span) = self.lex.current();

        self.check(|t| match *t {
            Token::Name(_) => true,
            _ => false,
        })?;

        let inner = match token {
            Token::Name(inner) => inner,
            _ => unreachable!(),
        };

        Ok(NodeRef::new(NameLiteral(inner), span))
    }

    fn parse_name_list(&mut self, variadic: bool) -> ParserResult<NodeRef<NameList>> {
        self.parse_name_list_iv(None, variadic)
    }

    fn parse_name_list_iv(
        &mut self,
        name: Option<NodeRef<NameLiteral>>,
        variadic: bool,
    ) -> ParserResult<NodeRef<NameList>> {
        let start = self.lex.current_span().start();
        let mut names = match name {
            Some(name) => vec![name],
            None => vec![],
        };

        let mut end = start;
        loop {
            if variadic && self.lex.current_token() == Token::Dots {
                break;
            }

            let name = self.parse_name_literal()?;
            end = name.span.end();
            names.push(name);

            match self.lex.current_token() {
                Token::Comma => {
                    self.lex.next()?;
                }
                _ => break,
            }
        }

        Ok(NodeRef::new(NameList(names), span!(start, end)))
    }

    fn parse_parameter_list(&mut self) -> ParserResult<NodeRef<ParameterList>> {
        let names = self.parse_name_list(true)?;
        let start = names.span.start();
        let mut end = start;

        let span = self.lex.current_span();
        let variadic = match self.lex.current_token() {
            Token::Dots => {
                self.lex.next()?;
                end = span.end();
                Some(NodeRef::new(Variadic, span))
            }
            _ => None,
        };

        Ok(NodeRef::new(
            ParameterList { names, variadic },
            span!(start, end),
        ))
    }
}

impl<'source> Parser<'source> {
    fn check<F>(&mut self, f: F) -> ParserResult<Token>
    where
        F: Fn(&Token) -> bool,
    {
        let token = self.lex.current_token();
        if !f(&token) {
            Err(ParserError::UnexpectedToken { token })
        } else {
            self.lex.next()?;

            Ok(self.lex.current_token())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::src;

    #[test]
    fn parse_expressions() -> ParserResult<()> {
        let src = src!(
            br#"
print()
print(123)
        "#
        );
        let lex = Lex::new(&src);
        let mut parser = Parser::from_lex(lex)?;

        let result = parser.parse()?;
        println!("{:#?}", result);

        Ok(())
    }
}
