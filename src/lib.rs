use anyhow::Result;
use nom::{
    bytes::complete::{escaped, tag},
    character::complete::{multispace1, none_of, one_of},
    combinator::opt,
    multi::{many1, separated_list0},
    IResult,
};
use std::io::{BufRead, Read};

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pre: Option<Pre>,
    mains: Option<Vec<Main>>,
    post: Option<Post>,
}

#[derive(Clone, Debug, PartialEq)]
struct Pre {
    statements: Vec<PreStatement>,
}

#[derive(Clone, Debug, PartialEq)]
struct Main {
    statements: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
struct Post {
    statements: Vec<PostStatement>,
}

#[derive(Clone, Debug, PartialEq)]
enum PreStatement {
    Assignment(Assignment),
}

#[derive(Clone, Debug, PartialEq)]
enum Assignment {
    FieldSeparator(String),
    UserProvided { left: String, right: String },
}

#[derive(Clone, Debug, PartialEq)]
enum Statement {}

#[derive(Clone, Debug, PartialEq)]
enum PostStatement {}

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

struct Runtime {
    field_separator: String,
    record_separator: String,
    number_of_records: u128,
}

pub fn execute_program<R: Read + BufRead>(program: &Program, input: R) -> Result<ExecutionResult> {
    let runtime = Runtime {
        field_separator: "\t".to_string(),
        record_separator: "\n".to_string(),
        number_of_records: 0,
    };

    for record in input.split(runtime.record_separator.as_bytes()[0]) {}

    Ok(ExecutionResult)
}
pub fn parse_program(s: &str) -> Result<Program> {
    let (_s, pre) = parse_pre(s).map_err(|e| match e {
        nom::Err::Error(e) | nom::Err::Failure(e) => {
            anyhow::anyhow!(nom::error::convert_error(s, e))
        }
        nom::Err::Incomplete(needed) => anyhow::anyhow!("More input needed: {:?}", needed),
    })?;
    let (s, mains) = opt(many1(parse_mains))(s).map_err(|e| match e {
        nom::Err::Error(e) | nom::Err::Failure(e) => {
            anyhow::anyhow!(nom::error::convert_error(s, e))
        }
        nom::Err::Incomplete(needed) => anyhow::anyhow!("More input needed: {:?}", needed),
    })?;
    let (_s, post) = opt(parse_post)(s).map_err(|e| match e {
        nom::Err::Error(e) | nom::Err::Failure(e) => {
            anyhow::anyhow!(nom::error::convert_error(s, e))
        }
        nom::Err::Incomplete(needed) => anyhow::anyhow!("More input needed: {:?}", needed),
    })?;

    Ok(Program {
        pre: Some(pre),
        mains, // mains.unwrap_or_else(|| vec![]),
        post,
    })
}

fn parse_pre(s: &str) -> IResult<&str, Pre, VE> {
    let (s, _) = tag("begin")(s)?;
    let (s, _) = opt(whitespace)(s)?;
    let (s, _) = tag("{")(s)?;
    let (s, _) = opt(whitespace)(s)?;
    let (s, statements) = opt(many1(pre_statement))(s)?;
    let (s, _) = opt(whitespace)(s)?;
    let (s, _) = tag("}")(s)?;

    Ok((
        s,
        Pre {
            statements: statements.unwrap_or_else(|| vec![]),
        },
    ))
}

fn parse_mains(s: &str) -> IResult<&str, Main, VE> {
    let statements = vec![];
    Ok((s, Main { statements }))
}

fn parse_post(s: &str) -> IResult<&str, Post, VE> {
    let (s, _) = tag("end")(s)?;
    let (s, _) = whitespace(s)?;
    let (s, _) = tag("{")(s)?;
    let (s, _) = whitespace(s)?;
    let (s, statements) = many1(post_statement)(s)?;
    let (s, _) = whitespace(s)?;
    let (s, _) = tag("}")(s)?;

    Ok((s, Post { statements }))
}

fn pre_statement(s: &str) -> IResult<&str, PreStatement, VE> {
    field_separator(s)
}

fn field_separator(s: &str) -> IResult<&str, PreStatement, VE> {
    let (s, _) = tag("field_separator")(s)?;
    let (s, _) = opt(whitespace)(s)?;
    let (s, _) = tag("=")(s)?;
    let (s, _) = opt(whitespace)(s)?;
    let (s, expr) = string(s)?;

    Ok((
        s,
        PreStatement::Assignment(Assignment::FieldSeparator(expr.to_string())),
    ))
}

fn string(s: &str) -> IResult<&str, String, VE> {
    let (s, _) = tag("\"")(s)?;
    let (s, inner) = escaped_string(s)?;
    let (s, _) = tag("\"")(s)?;

    Ok((s, inner.to_string()))
}

fn post_statement(s: &str) -> IResult<&str, PostStatement, VE> {
    todo!()
}

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
                pre: Some(Pre {
                    statements: vec![PreStatement::Assignment(Assignment::FieldSeparator(
                        ",".to_string()
                    ))]
                }),
                post: None,
                mains: None
            },
            parse_program(input).unwrap()
        );

        let input = "begin { field_separator = \"\t\" }";
        assert_eq!(
            Program {
                pre: Some(Pre {
                    statements: vec![PreStatement::Assignment(Assignment::FieldSeparator(
                        "\t".to_string()
                    ))]
                }),
                post: None,
                mains: None
            },
            parse_program(input).unwrap()
        );

        let input = "begin {  }";
        assert_eq!(
            Program {
                pre: Some(Pre { statements: vec![] }),
                post: None,
                mains: None
            },
            parse_program(input).unwrap()
        );

        let input = "begin{}";
        assert_eq!(
            Program {
                pre: Some(Pre { statements: vec![] }),
                post: None,
                mains: None
            },
            parse_program(input).unwrap()
        )
    }
    #[test]
    fn parses_end() {
        assert!(true)
    }
}
