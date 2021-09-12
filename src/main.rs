use term::Term;

fn main() -> Result<(), std::io::Error> {
    let _term = Term::builder("/bin/sh").spawn()?;
    Ok(())
}
