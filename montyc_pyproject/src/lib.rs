use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct BuildSystem {
    pub requires: Option<Vec<String>>,
    pub build_backend: Option<String>,
    pub backend_path: Option<Vec<String>>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct PyProject {
    pub build_system: Option<BuildSystem>,
    pub tool: Option<toml::Value>,
}
