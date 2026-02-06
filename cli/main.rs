use clap::Parser;
use dust::parser::{self, Token};
use std::fs;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

#[derive(Parser)]
#[command(name = "dust")]
#[command(about = "Dust language tool")]
struct Cli {
	#[command(subcommand)]
	command: Command,
}

#[derive(clap::Subcommand)]
enum Command {
	/// Parse a file and log diagnostics and/or AST
	Run {
		/// Path to the source file to parse
		#[arg(value_name = "FILE")]
		filepath: PathBuf,

		/// Log the parsed AST
		#[arg(long)]
		ast: bool,

		/// Log parse diagnostics (errors, warnings)
		#[arg(long)]
		diagnostics: bool,
	},

	/// Lex the input and print a syntax-highlighted version
	Highlight {
		/// Path to the source file (omit to read from stdin)
		#[arg(value_name = "FILE")]
		filepath: Option<PathBuf>,
	},
}

fn main() {
	let cli = Cli::parse();

	match cli.command {
		Command::Run {
			filepath,
			ast,
			diagnostics,
		} => {
			// let show_ast = ast;
			// let show_diagnostics = diagnostics;
			// // If neither flag given, show both
			// let show_both = !show_ast && !show_diagnostics;

			// let code = match fs::read_to_string(&filepath) {
			// 	Ok(s) => s,
			// 	Err(e) => {
			// 		eprintln!("error: failed to read {}: {}", filepath.display(), e);
			// 		std::process::exit(1);
			// 	}
			// };

			// let (expression, diags) = parser::parse(&code);

			// if show_diagnostics || show_both {
			// 	if diags.is_empty() {
			// 		eprintln!("No diagnostics.");
			// 	} else {
			// 		for d in &diags {
			// 			eprintln!("{:?}", d);
			// 		}
			// 	}
			// }

			// if show_ast || show_both {
			// 	match &expression {
			// 		Some(ast) => println!("{:#?}", ast),
			// 		None => println!("(parse did not produce an expression)"),
			// 	}
			// }
		}
		Command::Highlight { filepath } => {
			let code = match filepath {
				Some(path) => match fs::read_to_string(&path) {
					Ok(s) => s,
					Err(e) => {
						eprintln!("error: failed to read {}: {}", path.display(), e);
						std::process::exit(1);
					}
				},
				None => {
					let mut s = String::new();
					if io::stdin().read_to_string(&mut s).is_err() {
						eprintln!("error: failed to read stdin");
						std::process::exit(1);
					}
					s
				}
			};
			highlight(&code);
		}
	}
}

/// Atom One Dark–style palette (hex → RGB)
mod atom_one_dark {
	use termcolor::Color;
	pub fn keyword() -> Color {
		Color::Rgb(0xc6, 0x78, 0xdd) // purple
	}
	pub fn string() -> Color {
		Color::Rgb(0x98, 0xc3, 0x79) // green
	}
	pub fn number() -> Color {
		Color::Rgb(0xd1, 0x9a, 0x66) // orange
	}
	pub fn identifier() -> Color {
		Color::Rgb(0xe5, 0xc0, 0x7b) // gold
	}
	pub fn operator() -> Color {
		Color::Rgb(0x56, 0xb6, 0xc2) // cyan
	}
	pub fn punctuation() -> Color {
		Color::Rgb(0xab, 0xb2, 0xbf) // light gray
	}
	pub fn comment() -> Color {
		Color::Rgb(0x5c, 0x63, 0x70) // gray
	}
	pub fn error() -> Color {
		Color::Rgb(0xe0, 0x6c, 0x75) // red
	}
}

fn highlight(code: &str) {
	use atom_one_dark::*;
	let tokens = parser::lex(code);
	let mut stdout = StandardStream::stdout(ColorChoice::Always);
	for token in &tokens {
		let mut spec = ColorSpec::new();
		match token {
			Token::Pub | Token::Fn | Token::Let | Token::Struct | Token::Return => {
				spec.set_fg(Some(keyword()));
			}
			Token::String(_, _) | Token::RawString(_, _) => {
				spec.set_fg(Some(string()));
			}
			Token::Number(_) => {
				spec.set_fg(Some(number()));
			}
			Token::Ident(_) => {
				spec.set_fg(Some(identifier()));
			}
			Token::Operator(_) | Token::Comparison(_) => {
				spec.set_fg(Some(operator()));
			}
			Token::Paren(_)
			| Token::Brace(_)
			| Token::Equals
			| Token::Semi
			| Token::Comma
			| Token::Colon
			| Token::Arrow
			| Token::StringInteropolationStart
			| Token::StringInteropolationEnd => {
				spec.set_fg(Some(punctuation()));
			}
			Token::Whitespace(_) => {}
			Token::Comment(_) => {
				spec.set_fg(Some(comment()));
			}
			Token::Unknown(_) => {
				spec.set_fg(Some(Color::White)).set_bg(Some(error()));
			}
			Token::StringNotTerminated => {
				spec.set_fg(Some(error()));
			}
		}
		let _ = stdout.set_color(&spec);
		let _ = write!(&mut stdout, "{}", token);
		let _ = stdout.reset();
	}
	let has_unterminated = tokens.iter().any(|t| matches!(t, Token::StringNotTerminated));
	if has_unterminated {
		let _ = stdout.set_color(ColorSpec::new().set_fg(Some(error())));
		let _ = writeln!(&mut stdout, "\n[unterminated string]");
		let _ = stdout.reset();
	}
}
