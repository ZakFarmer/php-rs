use std::path::PathBuf;

pub struct Source {
    path: Option<PathBuf>,
    contents: String,
}

pub fn create_source_code(source: &str) -> Source {
    Source {
        path: None,
        contents: source.to_string(),
    }
}
