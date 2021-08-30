use term::Term;

fn main() -> Result<(), ()> {
    let _term = Term::builder("/bin/sh").spawn()?;
    Ok(())
}
