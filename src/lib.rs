use anyhow::Result;
use csv::{ReaderBuilder, StringRecord};
use nom::{
    bytes::complete::{escaped, tag},
    character::complete::{multispace1, none_of, one_of},
    combinator::opt,
    multi::{many0, many1, separated_list0},
    IResult,
};
use std::io::{BufRead, Read, Write};

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pre: Option<Section>,
    mains: Vec<Section>,
    post: Option<Section>,
}

#[derive(Clone, Debug, PartialEq)]
struct Section {
    exprs: Vec<Expr>,
}
#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Assignment(Assignment),
}

#[derive(Clone, Debug, PartialEq)]
enum Assignment {
    FieldSeparator(String),
    UserProvided { left: String, right: String },
}

#[derive(Clone, Debug)]
enum Configs {
    Separator(String),
    // LineTerminator(String),
    // etc
}

#[derive(Clone, Debug)]
struct Str(String);

pub struct ExecutionResult;

type VE<'i> = nom::error::VerboseError<&'i str>;

struct Runtime<'r> {
    field_separator: u8,
    record_separator: u8,
    number_of_records: u128,
    program: &'r Program,
}

impl<'r> Runtime<'r> {
    fn new(program: &'r Program) -> Self {
        Self {
            field_separator: b',',
            record_separator: b'\n',
            number_of_records: 0,
            program,
        }
    }

    fn execute_pre(&mut self) {}

    fn execute_record<W: Write>(&mut self, record: &[u8], output: W) {
        self.number_of_records += 1;

        for expr in &self.program.mains {
            println!("");
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

    // let mut records = csv::ReaderBuilder::new()
    //     .delimiter(runtime.field_separator)
    //     .terminator(csv::Terminator::Any(runtime.record_separator))
    //     .from_reader(input);

    // for record in records.records() {
    //     let record = record?;
    //     runtime.execute_record(record, &mut output)
    // }

    for record in input.split(runtime.record_separator) {
        let record = record?;
        runtime.execute_record(&record, &mut output)
    }

    runtime.execute_post();

    Ok(ExecutionResult)
}
pub fn parse_program(s: &str) -> Result<Program> {
    let (_s, pre) = opt(parse_pre)(s).map_err(|e| match e {
        nom::Err::Error(e) | nom::Err::Failure(e) => {
            anyhow::anyhow!(nom::error::convert_error(s, e))
        }
        nom::Err::Incomplete(needed) => anyhow::anyhow!("More input needed: {:?}", needed),
    })?;
    let (s, mains) = parse_mains(s).map_err(|e| match e {
        nom::Err::Error(e) | nom::Err::Failure(e) => {
            anyhow::anyhow!(nom::error::convert_error(s, e))
        }
        nom::Err::Incomplete(needed) => anyhow::anyhow!("More input needed: {:?}", needed),
    })?;
    // let (_s, post) = opt(parse_post)(s).map_err(|e| match e {
    //     nom::Err::Error(e) | nom::Err::Failure(e) => {
    //         anyhow::anyhow!(nom::error::convert_error(s, e))
    //     }
    //     nom::Err::Incomplete(needed) => anyhow::anyhow!("More input needed: {:?}", needed),
    // })?;

    Ok(Program {
        pre,
        mains, // mains.unwrap_or_else(|| vec![]),
        post: None,
    })
}

fn parse_pre(s: &str) -> IResult<&str, Section, VE> {
    let (s, _) = tag("begin")(s)?;
    let (s, section) = parse_section(s)?;
    Ok((s, section))
}

fn parse_mains(s: &str) -> IResult<&str, Vec<Section>, VE> {
    let (s, sections) = many0(parse_section)(s)?;
    Ok((s, sections))
}

fn parse_section(s: &str) -> IResult<&str, Section, VE> {
    let (s, _) = opt(whitespace)(s)?;
    let (s, _) = tag("{")(s)?;
    let (s, _) = opt(whitespace)(s)?;
    let (s, exprs) = many0(expr)(s)?;
    let (s, _) = opt(whitespace)(s)?;
    let (s, _) = tag("}")(s)?;

    Ok((s, Section { exprs }))
}

// fn parse_post(s: &str) -> IResult<&str, PostSection, VE> {
//     let (s, _) = tag("end")(s)?;
//     let (s, _) = whitespace(s)?;
//     let (s, _) = tag("{")(s)?;
//     let (s, _) = whitespace(s)?;
//     let (s, statements) = many1(post_statement)(s)?;
//     let (s, _) = whitespace(s)?;
//     let (s, _) = tag("}")(s)?;
//
//     Ok((
//         s,
//         PostSection {
//             section: statements,
//         },
//     ))
// }

fn expr(s: &str) -> IResult<&str, Expr, VE> {
    field_separator(s)
}

fn field_separator(s: &str) -> IResult<&str, Expr, VE> {
    let (s, _) = tag("field_separator")(s)?;
    let (s, _) = opt(whitespace)(s)?;
    let (s, _) = tag("=")(s)?;
    let (s, _) = opt(whitespace)(s)?;
    let (s, expr) = string(s)?;

    Ok((
        s,
        Expr::Assignment(Assignment::FieldSeparator(expr.to_string())),
    ))
}

fn string(s: &str) -> IResult<&str, String, VE> {
    let (s, _) = tag("\"")(s)?;
    let (s, inner) = escaped_string(s)?;
    let (s, _) = tag("\"")(s)?;

    Ok((s, inner.to_string()))
}

// fn post_statement(s: &str) -> IResult<&str, PostStatement, VE> {
//     todo!()
// }

fn escaped_string(s: &str) -> IResult<&str, &str, VE> {
    escaped(none_of("\\\"n"), '\\', one_of("\"trn\\"))(s)
}

fn whitespace(s: &str) -> IResult<&str, (), VE> {
    let (s, _) = opt(multispace1)(s)?;
    Ok((s, ()))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_begin() {
        let input = "begin { field_separator = \",\" }";
        assert_eq!(
            Program {
                pre: Some(Section {
                    exprs: vec![Expr::Assignment(Assignment::FieldSeparator(
                        ",".to_string()
                    ))]
                }),
                mains: vec![],
                post: None,
            },
            parse_program(input).unwrap()
        );

        let input = "begin { field_separator = \"\t\" }";
        assert_eq!(
            Program {
                pre: Some(Section {
                    exprs: vec![Expr::Assignment(Assignment::FieldSeparator(
                        "\t".to_string()
                    ))]
                }),
                mains: vec![],
                post: None,
            },
            parse_program(input).unwrap()
        );

        let input = "begin {  }";
        assert_eq!(
            Program {
                pre: Some(Section { exprs: vec![] }),
                mains: vec![],
                post: None,
            },
            parse_program(input).unwrap()
        );

        let input = "begin{}";
        assert_eq!(
            Program {
                pre: Some(Section { exprs: vec![] }),
                mains: vec![],
                post: None,
            },
            parse_program(input).unwrap()
        )
    }

    #[test]
    fn parses_bare_mains() {
        let input = "{}";
        assert_eq!(
            Program {
                pre: None,
                mains: vec![Section { exprs: vec![] }],
                post: None,
            },
            parse_program(input).unwrap()
        );
    }

    #[test]
    fn parses_end() {}
}
