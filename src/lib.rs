use anyhow::Result;
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag},
    character::complete::{multispace0, multispace1, none_of, one_of},
    combinator::{complete, eof, map, opt, peek, value},
    error::context,
    multi::{many1, separated_list0, separated_list1},
    sequence::{preceded, terminated, tuple},
    IResult,
};
use std::iter::FromIterator;

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
    FnCall(FnCall),
}

#[derive(Clone, Debug, PartialEq)]
struct Identifier(String);

#[derive(Clone, Debug, PartialEq)]
struct FnCall {
    identifier: Identifier,
    args: Vec<Expr>,
}

pub struct ExecutionResult;

type VE<'i> = nom::error::VerboseError<&'i str>;

// struct Runtime<'r> {
//     field_separator: u8,
//     record_separator: u8,
//     number_of_records: u128,
//     program: &'r Program,
// }

//////////////////////////////////////////////////
//////////////////////////////////////////////////
//////////////////////////////////////////////////

// impl<'r> Runtime<'r> {
//     fn new(program: &'r Program) -> Self {
//         Self {
//             field_separator: b',',
//             record_separator: b'\n',
//             number_of_records: 0,
//             program,
//         }
//     }

//     fn execute_pre(&mut self) {}

//     fn execute_record<W: Write>(&mut self, _record: &[u8], _output: W) {
//         self.number_of_records += 1;

//         for _expr in &self.program.sections {
//             println!("");
//         }
//     }

//     fn execute_post(&mut self) {}
// }

// pub fn execute_program<R: Read + BufRead, W: Write>(
//     program: &Program,
//     input: R,
//     mut output: W,
// ) -> Result<ExecutionResult> {
//     // todo: set these from program PRE if they exist
//     let mut runtime = Runtime::new(program);

//     runtime.execute_pre();

//     // let mut records = csv::ReaderBuilder::new()
//     //     .delimiter(runtime.field_separator)
//     //     .terminator(csv::Terminator::Any(runtime.record_separator))
//     //     .from_reader(input);

//     // for record in records.records() {
//     //     let record = record?;
//     //     runtime.execute_record(record, &mut output)
//     // }

//     for record in input.split(runtime.record_separator) {
//         let record = record?;
//         runtime.execute_record(&record, &mut output)
//     }

//     runtime.execute_post();

//     Ok(ExecutionResult)
// }

pub fn parse_program(s: &str) -> Result<Program> {
    let (s, sections) = complete(terminated(parse_sections, eof))(s).map_err(|e| match e {
        nom::Err::Error(e) | nom::Err::Failure(e) => {
            anyhow::anyhow!(nom::error::convert_error(s, e))
        }
        nom::Err::Incomplete(needed) => anyhow::anyhow!("More input needed: {:?}", needed),
    })?;

    Ok(Program { sections })
}

fn parse_sections(s: &str) -> IResult<&str, Vec<Section>, VE> {
    let (s, sections) = context(
        "sections separated_list1",
        complete(separated_list1(whitespace, parse_section)),
    )(s)?;
    Ok((s, sections))
}

/// a section is an optional matcher followed by a {}
fn parse_section(s: &str) -> IResult<&str, Section, VE> {
    let (s, matcher) = opt(matcher)(s)?;
    let (s, _) = whitespace_opt(s)?;
    let (s, _) = tag("{")(s)?;
    let (s, _) = whitespace_opt(s)?;
    let (s, exprs) = context(
        "sections exprs",
        alt((
            context("semicolon-separated exprs", separated_list1(tag(";"), expr)),
            context(
                "empty section",
                value(vec![], tuple((opt(whitespace), peek(tag("}"))))),
            ),
        )),
    )(s)?;
    let (s, _) = whitespace_opt(s)?;
    let (s, _) = tag("}")(s)?;

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
            assignment,
            string,
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
    let (s, _) = tag("(")(s)?;
    let (s, args) = separated_list0(tag(","), expr)(s)?;
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
    fn all_three() {
        let input = "begin{}{}end{ print(x) }";
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
}
