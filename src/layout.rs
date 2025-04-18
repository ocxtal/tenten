// @file layout.rs
// @author Hajime Suzuki
// @brief layout builder on top of plotters

use anyhow::{Result, anyhow};
use plotters::coord::Shift;
use plotters::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

#[derive(Copy, Clone, Debug, Default, Serialize, Deserialize)]
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
    Rect { id: Option<String>, width: u32, height: u32 },

    #[serde(rename = "horizontal")]
    Horizontal(Vec<LayoutElem>),

    #[serde(rename = "vertical")]
    Vertical(Vec<LayoutElem>),

    #[serde(rename = "margined")]
    Margined { margin: LayoutMargin, center: Box<LayoutElem> },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Layout(#[serde(with = "serde_yaml::with::singleton_map_recursive")] pub LayoutElem);

impl Deref for Layout {
    type Target = LayoutElem;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Layout {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl LayoutElem {
    pub fn get_dim(&self) -> (u32, u32) {
        match self {
            LayoutElem::Rect { width, height, .. } => (*width, *height),
            LayoutElem::Horizontal(inner) => inner
                .iter()
                .map(|x| x.get_dim())
                .fold((0, 0), |acc, (w, h)| (acc.0 + w, acc.1.max(h))),
            LayoutElem::Vertical(inner) => inner
                .iter()
                .map(|x| x.get_dim())
                .fold((0, 0), |acc, (w, h)| (acc.0.max(w), acc.1 + h)),
            LayoutElem::Margined { margin, center } => {
                let (w, h) = center.get_dim();
                (w + margin.left + margin.right, h + margin.top + margin.bottom)
            }
        }
    }

    pub fn set_dim(&mut self, dim: (u32, u32)) -> Result<()> {
        match self {
            LayoutElem::Rect { width, height, .. } => {
                *width = dim.0;
                *height = dim.1;
                Ok(())
            }
            _ => Err(anyhow!("set_dim is not supported for this layout element")),
        }
    }

    pub fn get_margin(&self) -> LayoutMargin {
        match self {
            LayoutElem::Rect { .. } => LayoutMargin::uniform(0),
            LayoutElem::Horizontal { .. } => LayoutMargin::uniform(0),
            LayoutElem::Vertical { .. } => LayoutMargin::uniform(0),
            LayoutElem::Margined { margin, .. } => *margin,
        }
    }

    pub fn set_margin(&mut self, margin: LayoutMargin) {
        match self {
            LayoutElem::Rect { .. } => {}
            LayoutElem::Horizontal { .. } => {}
            LayoutElem::Vertical { .. } => {}
            LayoutElem::Margined { margin: m, .. } => *m = margin,
        }
    }

    pub fn get_anchors(&self) -> (Vec<u32>, Vec<u32>) {
        match self {
            LayoutElem::Rect { width, height, .. } => (vec![0, *width], vec![0, *height]),
            LayoutElem::Horizontal(inner) => {
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
            LayoutElem::Vertical(inner) => {
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
            LayoutElem::Margined { margin, center, .. } => {
                let (w, h) = center.get_dim();
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

    pub fn get_id(&self) -> Option<&str> {
        match self {
            LayoutElem::Rect { id, .. } => id.as_deref(),
            _ => None,
        }
    }

    pub fn get_node_by_path_mut(&mut self, path: &str) -> Option<&mut LayoutElem> {
        if path.is_empty() {
            return Some(self);
        }

        let mut path = path.splitn(2, '.');
        let id = path.next()?;
        let rem = path.next().unwrap_or("");
        match self {
            LayoutElem::Rect { .. } => None,
            LayoutElem::Horizontal(inner) | LayoutElem::Vertical(inner) => {
                let index = id.parse::<usize>().ok().filter(|&x| x < inner.len())?;
                inner[index].get_node_by_path_mut(rem)
            }
            LayoutElem::Margined { center, .. } => {
                if id != "center" {
                    return None;
                }
                center.get_node_by_path_mut(rem)
            }
        }
    }

    pub fn get_node_by_id_mut(&mut self, id: &str) -> Option<&mut LayoutElem> {
        match self {
            LayoutElem::Rect { id: rect_id, .. } => {
                if rect_id.as_deref() == Some(id) {
                    Some(self)
                } else {
                    None
                }
            }
            LayoutElem::Horizontal(inner) | LayoutElem::Vertical(inner) => {
                inner.iter_mut().fold(None, |acc, inner| acc.or(inner.get_node_by_id_mut(id)))
            }
            LayoutElem::Margined { center, .. } => center.get_node_by_id_mut(id),
        }
    }

    pub fn get_node_mut(&mut self, path: &str) -> Option<&mut LayoutElem> {
        if let Some(path) = path.strip_prefix('.') {
            self.get_node_by_path_mut(path)
        } else {
            self.get_node_by_id_mut(path)
        }
    }
}

pub struct StructuredDrawingArea<'a> {
    areas: Vec<DrawingArea<BitMapBackend<'a>, Shift>>,
    index: HashMap<String, usize>,
}

impl<'a> StructuredDrawingArea<'a> {
    const MARGIN_TAGS: [&'static str; 9] = [
        "top-left",
        "top",
        "top-right",
        "left",
        "center",
        "right",
        "bottom-left",
        "bottom",
        "bottom-right",
    ];

    fn append_elem(&mut self, path: &str, area: &DrawingArea<BitMapBackend<'a>, Shift>, layout: Option<&LayoutElem>) -> Result<()> {
        if self.index.contains_key(path) {
            return Err(anyhow!("duplicate path: {path}"));
        }
        self.areas.push(area.clone());
        self.index.insert(path.to_string(), self.areas.len() - 1);

        if layout.is_none() {
            return Ok(());
        }
        let layout = layout.unwrap();
        let (wbrk, hbrk) = layout.get_breakpoints();
        let areas = area.split_by_breakpoints(&wbrk, &hbrk);
        match layout {
            LayoutElem::Rect { id, .. } => {
                if let Some(id) = id {
                    self.areas.push(area.clone());
                    self.index.insert(id.clone(), self.areas.len() - 1);
                }
            }
            LayoutElem::Horizontal(inner) | LayoutElem::Vertical(inner) => {
                for (i, (inner, area)) in inner.iter().zip(areas.iter()).enumerate() {
                    self.append_elem(&format!("{path}.{i}"), area, Some(inner))?;
                }
            }
            LayoutElem::Margined { center, .. } => {
                for (i, area) in areas.iter().enumerate() {
                    let tag = Self::MARGIN_TAGS[i];
                    let center = if i == 4 { Some(&**center) } else { None };
                    self.append_elem(&format!("{path}.{tag}"), area, center)?;
                }
            }
        }
        Ok(())
    }

    pub fn from_layout(layout: &Layout, name: &'a str) -> Result<StructuredDrawingArea<'a>> {
        let mut s = StructuredDrawingArea {
            areas: Vec::new(),
            index: HashMap::new(),
        };
        let root_area = BitMapBackend::new(name, layout.0.get_dim()).into_drawing_area();
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
