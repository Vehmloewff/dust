use std::fs;
use std::path::PathBuf;

use clap::Parser;

use dust::parser;

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
}

fn main() {
	let cli = Cli::parse();

	match cli.command {
		Command::Run {
			filepath,
			ast,
			diagnostics,
		} => {
			let show_ast = ast;
			let show_diagnostics = diagnostics;
			// If neither flag given, show both
			let show_both = !show_ast && !show_diagnostics;

			let code = match fs::read_to_string(&filepath) {
				Ok(s) => s,
				Err(e) => {
					eprintln!("error: failed to read {}: {}", filepath.display(), e);
					std::process::exit(1);
				}
			};

			let (expression, diags) = parser::parse(&code);

			if show_diagnostics || show_both {
				if diags.is_empty() {
					eprintln!("No diagnostics.");
				} else {
					for d in &diags {
						eprintln!("{:?}", d);
					}
				}
			}

			if show_ast || show_both {
				match &expression {
					Some(ast) => println!("{:#?}", ast),
					None => println!("(parse did not produce an expression)"),
				}
			}
		}
	}
}
