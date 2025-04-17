// @file layout.rs
// @author Hajime Suzuki
// @brief layout builder on top of plotters

use anyhow::{Result, anyhow};
use plotters::coord::Shift;
use plotters::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LayoutMargin {
    pub left: u32,
    pub right: u32,
    pub top: u32,
    pub bottom: u32,
}

impl LayoutMargin {
    pub fn new(left: u32, right: u32, top: u32, bottom: u32) -> LayoutMargin {
        LayoutMargin { left, right, top, bottom }
    }

    pub fn uniform(margin: u32) -> LayoutMargin {
        LayoutMargin {
            left: margin,
            right: margin,
            top: margin,
            bottom: margin,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum LayoutElem {
    #[serde(rename = "rect")]
    Rect { id: String, width: u32, height: u32 },

    #[serde(rename = "horizontal")]
    Horizontal { id: String, inner: Vec<LayoutElem> },

    #[serde(rename = "vertical")]
    Vertical { id: String, inner: Vec<LayoutElem> },

    #[serde(rename = "margined")]
    Margined {
        id: String,

        #[serde(flatten)]
        margin: LayoutMargin,
        inner: Box<LayoutElem>,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Layout(#[serde(with = "serde_yaml::with::singleton_map_recursive")] pub LayoutElem);

impl LayoutElem {
    pub fn get_dim(&self) -> (u32, u32) {
        match self {
            LayoutElem::Rect { width, height, .. } => (*width, *height),
            LayoutElem::Horizontal { inner, .. } => inner
                .iter()
                .map(|x| x.get_dim())
                .fold((0, 0), |acc, (w, h)| (acc.0 + w, acc.1.max(h))),
            LayoutElem::Vertical { inner, .. } => inner
                .iter()
                .map(|x| x.get_dim())
                .fold((0, 0), |acc, (w, h)| (acc.0.max(w), acc.1 + h)),
            LayoutElem::Margined { margin, inner, .. } => {
                let (w, h) = inner.get_dim();
                (w + margin.left + margin.right, h + margin.top + margin.bottom)
            }
        }
    }

    pub fn get_anchors(&self) -> (Vec<u32>, Vec<u32>) {
        match self {
            LayoutElem::Rect { width, height, .. } => (vec![0, *width], vec![0, *height]),
            LayoutElem::Horizontal { inner, .. } => {
                let mut v = vec![0];
                let mut w_acc = 0;
                let mut h_max = 0;
                for inner in inner.iter() {
                    let (w, h) = inner.get_dim();
                    w_acc += w;
                    v.push(w_acc);
                    h_max = h_max.max(h);
                }
                (v, vec![0, h_max])
            }
            LayoutElem::Vertical { inner, .. } => {
                let mut v = vec![0];
                let mut w_max = 0;
                let mut h_acc = 0;
                for inner in inner.iter() {
                    let (w, h) = inner.get_dim();
                    h_acc += h;
                    v.push(h_acc);
                    w_max = w_max.max(w);
                }
                (vec![0, w_max], v)
            }
            LayoutElem::Margined { margin, inner, .. } => {
                let (w, h) = inner.get_dim();
                (
                    vec![0, margin.left, margin.left + w, margin.left + w + margin.right],
                    vec![0, margin.top, margin.top + h, margin.top + h + margin.bottom],
                )
            }
        }
    }

    pub fn get_breakpoints(&self) -> (Vec<u32>, Vec<u32>) {
        let (w, h) = self.get_anchors();
        assert!(w.len() >= 2 && h.len() >= 2);

        (w[1..w.len() - 1].to_vec(), h[1..h.len() - 1].to_vec())
    }

    pub fn get_id(&self) -> &str {
        match self {
            LayoutElem::Rect { id, .. } => id,
            LayoutElem::Horizontal { id, .. } => id,
            LayoutElem::Vertical { id, .. } => id,
            LayoutElem::Margined { id, .. } => id,
        }
    }

    fn get_node_mut(&mut self, path: &str) -> Option<&mut LayoutElem> {
        if path == self.get_id() {
            return Some(self);
        }

        let mut path = path.splitn(2, '.');
        if path.next()? == self.get_id() {
            return None;
        }

        let path = path.next().unwrap_or("");
        match self {
            LayoutElem::Rect { .. } => None,
            LayoutElem::Horizontal { inner, .. } | LayoutElem::Vertical { inner, .. } => {
                inner.iter_mut().fold(None, |acc, x| acc.or(x.get_node_mut(path)))
            }
            LayoutElem::Margined { inner, .. } => inner.as_mut().get_node_mut(path),
        }
    }
}

pub struct StructuredDrawingArea<'a> {
    areas: Vec<DrawingArea<BitMapBackend<'a>, Shift>>,
    index: HashMap<String, usize>,
}

impl<'a> StructuredDrawingArea<'a> {
    const MARGIN_TAGS: [&'static str; 9] = [
        "left-top",
        "top",
        "right-top",
        "left",
        "center",
        "right",
        "left-bottom",
        "bottom",
        "right-bottom",
    ];

    fn append_key(&mut self, key: &str) -> Result<()> {
        if self.index.contains_key(key) {
            return Err(anyhow!("duplicate id: {key}"));
        }
        self.index.insert(key.to_string(), self.areas.len() - 1);
        Ok(())
    }

    fn append_elem(
        &mut self,
        parent_key: &str,
        parent_area: &DrawingArea<BitMapBackend<'a>, Shift>,
        inner: Option<&LayoutElem>,
    ) -> Result<()> {
        if self.index.contains_key(parent_key) {
            return Err(anyhow!("duplicate id: {parent_key}"));
        }
        self.areas.push(parent_area.clone());
        self.index.insert(parent_key.to_string(), self.areas.len() - 1);

        if inner.is_none() {
            return Ok(());
        }
        let inner = inner.unwrap();
        let (wbrk, hbrk) = inner.get_breakpoints();
        let areas = parent_area.split_by_breakpoints(&wbrk, &hbrk);
        match inner {
            LayoutElem::Rect { id, .. } => {
                self.append_elem(&format!("{parent_key}.{id}"), &areas[0], None)?;
            }
            LayoutElem::Horizontal { id, inner } | LayoutElem::Vertical { id, inner } => {
                for (i, (inner, area)) in inner.iter().zip(areas.iter()).enumerate() {
                    self.append_elem(&format!("{parent_key}.{id}[{i}]"), area, Some(inner))?;
                }
            }
            LayoutElem::Margined { id, inner, .. } => {
                for (i, area) in areas.iter().enumerate() {
                    let tag = Self::MARGIN_TAGS[i];
                    let inner = if i == 4 { Some(&**inner) } else { None };
                    self.append_elem(&format!("{parent_key}.{id}[{tag}]"), area, inner)?;
                }
            }
        }
        Ok(())
    }

    pub fn from_layout(layout: &Layout, id: &'a str) -> Result<StructuredDrawingArea<'a>> {
        let mut s = StructuredDrawingArea {
            areas: Vec::new(),
            index: HashMap::new(),
        };
        let root_area = BitMapBackend::new(id, layout.0.get_dim()).into_drawing_area();
        root_area.fill(&WHITE)?;
        s.append_elem("", &root_area, Some(&layout.0))?;
        Ok(s)
    }

    pub fn get_area(&self, key: &str) -> Result<&DrawingArea<BitMapBackend<'a>, Shift>> {
        if let Some(&i) = self.index.get(key) {
            Ok(&self.areas[i])
        } else {
            Err(anyhow!("area not found: {}", key))
        }
    }

    pub fn present(&self) -> Result<()> {
        self.areas[0].present()?;
        Ok(())
    }
}
