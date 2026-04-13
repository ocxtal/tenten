use anyhow::{Context, Result};
use std::fs::File;
use tempfile::NamedTempFile;
use tenten::{DotPlotHit, PlotHitMap};
use tiny_http::{Header, Method, Response, Server, StatusCode};

pub fn serve(file: NamedTempFile, hit_map: PlotHitMap, host: &str, port: u16, suffix: &str) -> Result<()> {
    let addr = format!("{host}:{port}");
    let server = Server::http(&addr).map_err(|err| anyhow::anyhow!("failed to start http server at {addr}: {err}"))?;
    let image_path = format!("/plot{suffix}");
    println!("http://{addr}/");

    for request in server.incoming_requests() {
        let url = request.url().to_string();
        match (request.method(), url.as_str()) {
            (&Method::Get, "/") => {
                request.respond(html_response(&image_path))?;
            }
            (&Method::Get, "/plot") => {
                request.respond(file_response(&file, suffix)?)?;
            }
            (&Method::Get, path) if path == image_path => {
                request.respond(file_response(&file, suffix)?)?;
            }
            (&Method::Get, path) if path.starts_with("/hit?") => {
                request.respond(hit_response(path, &hit_map))?;
            }
            _ => {
                request.respond(text_response(StatusCode(404), "not found", "text/plain; charset=utf-8"))?;
            }
        }
    }

    Ok(())
}

fn html_response(image_path: &str) -> Response<std::io::Cursor<Vec<u8>>> {
    let html = format!(
        r#"<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>tenten</title>
<style>
html, body {{ margin: 0; padding: 0; }}
img {{ display: block; max-width: 100%; height: auto; }}
#tip {{ position: absolute; display: none; padding: 2px 4px; border: 1px solid #000; background: #fff; color: #000; font: 12px sans-serif; pointer-events: none; }}
</style>
</head>
<body>
<img id="plot" src="{image_path}" alt="tenten plot">
<div id="tip"></div>
<script>
const img = document.getElementById('plot');
const tip = document.getElementById('tip');
img.addEventListener('click', async (event) => {{
  const rect = img.getBoundingClientRect();
  const x = Math.floor((event.clientX - rect.left) * img.naturalWidth / rect.width);
  const y = Math.floor((event.clientY - rect.top) * img.naturalHeight / rect.height);
  const response = await fetch(`/hit?x=${{x}}&y=${{y}}`);
  if (!response.ok) {{
    tip.style.display = 'none';
    return;
  }}
  const hit = await response.json();
  if (hit.text === null) {{
    tip.style.display = 'none';
    return;
  }}
  tip.textContent = hit.text;
  tip.style.left = `${{event.pageX}}px`;
  tip.style.top = `${{event.pageY}}px`;
  tip.style.display = 'block';
}});
</script>
</body>
</html>
"#
    );
    text_response(StatusCode(200), html, "text/html; charset=utf-8")
}

fn file_response(file: &NamedTempFile, suffix: &str) -> Result<Response<File>> {
    let file = File::open(file.path()).with_context(|| format!("failed to open {:?}", file.path()))?;
    let response = Response::from_file(file).with_header(header("Content-Type", content_type(suffix))?);
    Ok(response)
}

fn hit_response(path: &str, hit_map: &PlotHitMap) -> Response<std::io::Cursor<Vec<u8>>> {
    let Some((x, y)) = parse_xy(path) else {
        return text_response(StatusCode(400), r#"{"text":null}"#, "application/json");
    };

    let text = hit_map.hit_test(x, y).and_then(|hit| format_tip(&hit));
    let body = match text {
        Some(text) => format!(r#"{{"text":"{}"}}"#, json_escape(&text)),
        None => r#"{"text":null}"#.to_string(),
    };
    text_response(StatusCode(200), body, "application/json")
}

fn parse_xy(path: &str) -> Option<(u32, u32)> {
    let query = path.strip_prefix("/hit?")?;
    let mut x = None;
    let mut y = None;
    for pair in query.split('&') {
        let (key, value) = pair.split_once('=')?;

        match key {
            "x" => x = value.parse().ok(),
            "y" => y = value.parse().ok(),
            _ => {}
        }
    }
    Some((x?, y?))
}

fn format_tip(hit: &DotPlotHit) -> Option<String> {
    match (&hit.target, &hit.query) {
        (Some(target), Some(query)) => Some(format!("({}, {})", target.pos, query.pos)),
        (Some(target), None) => Some(format!("({})", target.pos)),
        (None, Some(query)) => Some(format!("({})", query.pos)),
        (None, None) => None,
    }
}

fn json_escape(text: &str) -> String {
    let mut out = String::new();
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ch if ch.is_control() => out.push_str(&format!("\\u{:04x}", ch as u32)),
            ch => out.push(ch),
        }
    }
    out
}

fn text_response(status: StatusCode, text: impl Into<String>, content_type: &str) -> Response<std::io::Cursor<Vec<u8>>> {
    Response::from_string(text.into())
        .with_status_code(status)
        .with_header(header("Content-Type", content_type).unwrap())
}

fn header(name: &str, value: &str) -> Result<Header> {
    Header::from_bytes(name.as_bytes(), value.as_bytes()).map_err(|_| anyhow::anyhow!("invalid HTTP header"))
}

fn content_type(suffix: &str) -> &'static str {
    match suffix {
        ".svg" => "image/svg+xml",
        _ => "image/png",
    }
}
