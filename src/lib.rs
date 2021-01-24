use anyhow::Result;
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag},
    character::complete::{digit1, multispace0, multispace1, none_of, one_of},
    combinator::{complete, eof, map, opt, value},
    error::{context, ParseError},
    multi::{many1, separated_list0},
    sequence::{preceded, terminated},
    IResult,
};
use std::{
    collections::HashMap,
    fmt::Display,
    io::{BufRead, Read, Write},
    iter::FromIterator,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    sections: Vec<Section>,
}

#[derive(Clone, Debug, PartialEq)]
struct Section {
    matcher: Option<Matcher>,
    exprs: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
enum Matcher {
    Begin,
    End,
}

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Assignment { lhs: Identifier, rhs: Box<Expr> },
    String(String),
    FieldRef(usize),
    FnCall(FnCall),
    Unit,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Assignment { lhs, rhs } => {
                write!(f, "{} = {}", lhs, rhs)
            }
            Expr::String(s) => {
                write!(f, "{}", s)
            }
            Expr::FieldRef(n) => {
                write!(f, "${}", n)
            }
            Expr::FnCall(fncall) => {
                write!(f, "{}({:#?})", fncall.identifier, fncall.args)
            }
            Expr::Unit => {
                write!(f, "nil")
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Identifier(String);

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq)]
struct FnCall {
    identifier: Identifier,
    args: Vec<Expr>,
}

pub struct ExecutionResult;

type VE<'i> = nom::error::VerboseError<&'i str>;

struct Runtime<'r> {
    field_separator: u8,
    record_separator: u8,
    output_separator: u8,
    number_of_records: u128,
    program: &'r Program,
    fns: HashMap<Identifier, Box<dyn SFn>>,
}

trait SFn {
    fn exec(&self, args: &[Expr], record: &[u8], runtime: &Runtime) -> Expr;
}

struct Print;
impl SFn for Print {
    fn exec(&self, args: &[Expr], record: &[u8], runtime: &Runtime) -> Expr {
        let record = std::str::from_utf8(record).unwrap();
        let splits = record
            .split(std::str::from_utf8(&[runtime.field_separator]).unwrap())
            .collect::<Vec<&str>>();

        let x = &[runtime.output_separator];
        let output_separator = std::str::from_utf8(x).unwrap();

        let out = args
            .iter()
            .map(|arg| match arg {
                Expr::FieldRef(n) => {
                    if n > &0usize {
                        splits[*n - 1].to_string()
                    } else {
                        record.to_string()
                    }
                }
                _ => arg.to_string(),
            })
            .collect::<Vec<String>>()
            .join(output_separator);

        println!("{}", out);

        Expr::Unit
    }
}

//////////////////////////////////////////////////
//////////////////////////////////////////////////
//////////////////////////////////////////////////

impl<'r> Runtime<'r> {
    fn new(program: &'r Program) -> Self {
        let mut fns = HashMap::new();
        fns.insert(
            Identifier("print".to_string()),
            Box::new(Print) as Box<dyn SFn>,
        );

        Self {
            field_separator: b',',
            record_separator: b'\n',
            output_separator: b',',
            number_of_records: 0,
            program,
            fns,
        }
    }

    fn execute_pre(&mut self) {}

    fn execute_record<W: Write>(&mut self, record: &[u8], sections: &Vec<&Section>, _output: W) {
        self.number_of_records += 1;

        for section in sections {
            for expr in &section.exprs {
                match expr {
                    Expr::Assignment { lhs, rhs } => {}
                    Expr::String(_) => (),
                    Expr::FieldRef(_) => (),
                    Expr::FnCall(FnCall { identifier, args }) => {
                        let f = { self.fns.get(&identifier).unwrap() };
                        let _ = f.exec(args, record, self);
                    }
                    Expr::Unit => (),
                }
            }
        }
    }

    fn execute_post(&mut self) {}
}

pub fn execute_program<R: Read + BufRead, W: Write>(
    program: &Program,
    input: R,
    mut output: W,
) -> Result<ExecutionResult> {
    // todo: set these from program PRE if they exist
    let mut runtime = Runtime::new(program);

    runtime.execute_pre();

    let mains = program
        .sections
        .iter()
        .filter(|section| {
            section.matcher != Some(Matcher::Begin) && section.matcher != Some(Matcher::End)
        })
        .collect::<Vec<&Section>>();

    for record in input.split(runtime.record_separator) {
        let record = record?;
        runtime.execute_record(&record, &mains, &mut output)
    }

    runtime.execute_post();

    Ok(ExecutionResult)
}

fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    terminated(inner, multispace0)
}

pub fn parse_program(s: &str) -> Result<Program> {
    let (_s, sections) = complete(terminated(parse_sections, eof))(s).map_err(|e| match e {
        nom::Err::Error(e) | nom::Err::Failure(e) => {
            anyhow::anyhow!(nom::error::convert_error(s, e))
        }
        nom::Err::Incomplete(needed) => anyhow::anyhow!("More input needed: {:?}", needed),
    })?;

    Ok(Program { sections })
}

fn parse_sections(s: &str) -> IResult<&str, Vec<Section>, VE> {
    let (s, sections) = context("sections separated_list1", many1(parse_section))(s)?;
    Ok((s, sections))
}

/// a section is an optional matcher followed by a {}
fn parse_section(s: &str) -> IResult<&str, Section, VE> {
    let (s, matcher) = opt(ws(matcher))(s)?;
    let (s, _) = ws(tag("{"))(s)?;
    let (s, exprs) = context(
        "semicolon-separated exprs",
        alt((
            map(terminated(ws(expr), opt(ws(tag(";")))), |e| vec![e]),
            ws(separated_list0(tag(";"), ws(expr))),
        )),
    )(s)?;
    let (s, _) = ws(tag("}"))(s)?;

    Ok((s, Section { matcher, exprs }))
}

fn matcher(s: &str) -> IResult<&str, Matcher, VE> {
    context(
        "matcher",
        alt((
            value(Matcher::End, tag("end")),
            value(Matcher::Begin, tag("begin")),
        )),
    )(s)
}

fn expr(s: &str) -> IResult<&str, Expr, VE> {
    context(
        "expr",
        alt((
            ws(assignment),
            ws(string),
            ws(fieldref),
            map(fn_call, |fn_call| Expr::FnCall(fn_call)),
        )),
    )(s)
}

fn assignment(s: &str) -> IResult<&str, Expr, VE> {
    let (s, identifier) = identifier(s)?;
    let (s, _) = whitespace_opt(s)?;
    let (s, _) = tag("=")(s)?;
    let (s, _) = whitespace_opt(s)?;
    let (s, expr) = expr(s)?;

    Ok((
        s,
        Expr::Assignment {
            lhs: identifier,
            rhs: Box::new(expr),
        },
    ))
}

fn string(s: &str) -> IResult<&str, Expr, VE> {
    let (s, escaped) = escaped_string(s)?;
    Ok((s, Expr::String(escaped.to_string())))
}

fn fieldref(s: &str) -> IResult<&str, Expr, VE> {
    let (s, _) = tag("$")(s)?;
    let (s, n) = digit1(s)?;
    let n = n.parse::<usize>().unwrap();

    Ok((s, Expr::FieldRef(n)))
}

fn escaped_string(s: &str) -> IResult<&str, &str, VE> {
    preceded(
        tag("\""),
        terminated(
            escaped(none_of("\\\"n"), '\\', one_of("\"trn\\")),
            tag("\""),
        ),
    )(s)
}

fn whitespace(s: &str) -> IResult<&str, (), VE> {
    let (s, _) = context("whitespace", multispace1)(s)?;
    Ok((s, ()))
}

fn whitespace_opt(s: &str) -> IResult<&str, (), VE> {
    let (s, _) = context("whitespace opt", multispace0)(s)?;
    Ok((s, ()))
}

fn fn_call(s: &str) -> IResult<&str, FnCall, VE> {
    let (s, identifier) = identifier(s)?;
    let (s, _) = ws(tag("("))(s)?;
    let (s, args) = separated_list0(tag(","), ws(expr))(s)?;
    let (s, _) = tag(")")(s)?;

    Ok((s, FnCall { identifier, args }))
}

fn identifier(s: &str) -> IResult<&str, Identifier, VE> {
    let (s, chars) = many1(one_of(
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!_-",
    ))(s)?;

    Ok((s, Identifier(String::from_iter(chars))))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_begin() {
        let input = "begin { field_separator = \",\" }";
        assert_eq!(
            Program {
                sections: vec![Section {
                    matcher: Some(Matcher::Begin),
                    exprs: vec![Expr::Assignment {
                        lhs: Identifier("field_separator".to_string()),
                        rhs: Box::new(Expr::String(",".to_string()))
                    }]
                }]
            },
            parse_program(input).unwrap()
        );

        let input = "begin { field_separator = \"\t\" }";
        assert_eq!(
            Program {
                sections: vec![Section {
                    matcher: Some(Matcher::Begin),
                    exprs: vec![Expr::Assignment {
                        lhs: Identifier("field_separator".to_string()),
                        rhs: Box::new(Expr::String("\t".to_string()))
                    }]
                }],
            },
            parse_program(input).unwrap()
        );

        let input = "begin {  }";
        assert_eq!(
            Program {
                sections: vec![Section {
                    matcher: Some(Matcher::Begin),
                    exprs: vec![]
                }],
            },
            parse_program(input).unwrap()
        );

        let input = "begin{}";
        assert_eq!(
            Program {
                sections: vec![Section {
                    matcher: Some(Matcher::Begin),
                    exprs: vec![]
                }],
            },
            parse_program(input).unwrap()
        )
    }

    #[test]
    fn empty_main() {
        let input = "{  }";
        assert_eq!(
            Program {
                sections: vec![Section {
                    matcher: None,
                    exprs: vec![]
                }],
            },
            parse_program(input).unwrap()
        );
        let input = "{}";
        assert_eq!(
            Program {
                sections: vec![Section {
                    matcher: None,
                    exprs: vec![]
                }],
            },
            parse_program(input).unwrap()
        );
    }

    #[test]
    fn begin_main() {
        let input = "begin {} {} {}";
        assert_eq!(
            Program {
                sections: vec![
                    Section {
                        matcher: Some(Matcher::Begin),
                        exprs: vec![]
                    },
                    Section {
                        matcher: None,
                        exprs: vec![]
                    },
                    Section {
                        matcher: None,
                        exprs: vec![]
                    }
                ],
            },
            parse_program(input).unwrap()
        );
    }

    #[test]
    fn print_fn() {
        let input = "{ print() }";
        assert_eq!(
            Program {
                sections: vec![Section {
                    matcher: None,
                    exprs: vec![Expr::FnCall(FnCall {
                        identifier: Identifier("print".to_string()),
                        args: vec![]
                    })]
                }],
            },
            parse_program(input).unwrap()
        );
    }

    #[test]
    fn all_three_empty() {
        let input = "begin{}{}end{}";
        assert_eq!(
            Program {
                sections: vec![
                    Section {
                        matcher: Some(Matcher::Begin),
                        exprs: vec![]
                    },
                    Section {
                        matcher: None,
                        exprs: vec![]
                    },
                    Section {
                        matcher: Some(Matcher::End),
                        exprs: vec![]
                    }
                ],
            },
            parse_program(input).unwrap()
        );
    }

    #[test]
    fn all_three_end_expr() {
        let input = "begin{}{} end{ print(\"hello\") }";
        assert_eq!(
            Program {
                sections: vec![
                    Section {
                        matcher: Some(Matcher::Begin),
                        exprs: vec![]
                    },
                    Section {
                        matcher: None,
                        exprs: vec![]
                    },
                    Section {
                        matcher: Some(Matcher::End),
                        exprs: vec![Expr::FnCall(FnCall {
                            identifier: Identifier("print".to_string()),
                            args: vec![Expr::String("hello".to_string())]
                        })]
                    }
                ],
            },
            parse_program(input).unwrap()
        );
    }

    #[test]
    fn all_three_fieldref() {
        let input = "begin{}{ print($1)} end{ print(\"hello\") }";
        assert_eq!(
            Program {
                sections: vec![
                    Section {
                        matcher: Some(Matcher::Begin),
                        exprs: vec![]
                    },
                    Section {
                        matcher: None,
                        exprs: vec![Expr::FnCall(FnCall {
                            identifier: Identifier("print".to_string()),
                            args: vec![Expr::FieldRef(1)]
                        })]
                    },
                    Section {
                        matcher: Some(Matcher::End),
                        exprs: vec![Expr::FnCall(FnCall {
                            identifier: Identifier("print".to_string()),
                            args: vec![Expr::String("hello".to_string())]
                        })]
                    }
                ],
            },
            parse_program(input).unwrap()
        );
    }
}
