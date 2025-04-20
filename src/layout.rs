// @file layout.rs
// @author Hajime Suzuki
// @brief layout builder on top of plotters

use anyhow::{Result, anyhow};
use plotters::coord::Shift;
use plotters::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::ops::{Deref, DerefMut, Range};

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

#[derive(Clone)]
pub struct RectPosition {
    bg_dim: (u32, u32),
    x_range: Range<u32>,
    y_range: Range<u32>,
}

#[derive(Copy, Clone, Debug)]
pub enum RectAnchor {
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

impl RectPosition {
    pub fn get_relative_pos(&self, root: RectAnchor, target: RectAnchor) -> (i32, i32) {
        let x_adj = match root {
            RectAnchor::TopLeft | RectAnchor::BottomLeft => 0,
            RectAnchor::TopRight | RectAnchor::BottomRight => self.bg_dim.0 as i32,
        };
        let y_adj = match root {
            RectAnchor::TopLeft | RectAnchor::TopRight => 0,
            RectAnchor::BottomLeft | RectAnchor::BottomRight => self.bg_dim.1 as i32,
        };
        let x_pos = match target {
            RectAnchor::TopLeft | RectAnchor::BottomLeft => self.x_range.start as i32,
            RectAnchor::TopRight | RectAnchor::BottomRight => self.x_range.end as i32,
        };
        let y_pos = match target {
            RectAnchor::TopLeft | RectAnchor::TopRight => self.y_range.start as i32,
            RectAnchor::BottomLeft | RectAnchor::BottomRight => self.y_range.end as i32,
        };
        (x_pos - x_adj, y_pos - y_adj)
    }

    pub fn add_margin(&self, margin: &LayoutMargin) -> RectPosition {
        RectPosition {
            bg_dim: (
                margin.left + self.bg_dim.0 + margin.right,
                margin.top + self.bg_dim.1 + margin.bottom,
            ),
            x_range: self.x_range.start + margin.left..self.x_range.end + margin.left,
            y_range: self.y_range.start + margin.top..self.y_range.end + margin.top,
        }
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

    fn get_range_vec<F>(inner: &[LayoutElem], index: &str, path: &str, is_horizontal: bool, get: F) -> Option<RectPosition>
    where
        F: Fn(&LayoutElem, &str) -> Option<RectPosition>,
    {
        let index = index.parse::<usize>().ok().filter(|&x| x < inner.len())?;
        let range = get(&inner[index], path)?;

        let before = inner[..index].iter().map(|node| node.get_dim().0).sum::<u32>();
        let after = inner[index + 1..].iter().map(|node| node.get_dim().0).sum::<u32>();
        let range = if is_horizontal {
            range.add_margin(&LayoutMargin::new(before, after, 0, 0))
        } else {
            range.add_margin(&LayoutMargin::new(0, 0, before, after))
        };
        Some(range)
    }

    fn locate_margin(range: &RectPosition, margin: &LayoutMargin, key: &str) -> Option<RectPosition> {
        let x_range = match key {
            "top-left" | "left" | "bottom-left" => 0..margin.left,
            "top" | "center" | "bottom" => margin.left..range.bg_dim.0 - margin.right,
            "top-right" | "right" | "bottom-right" => range.bg_dim.0 - margin.right..range.bg_dim.0,
            _ => return None,
        };
        let y_range = match key {
            "top-left" | "top" | "top-right" => 0..margin.top,
            "left" | "center" | "right" => margin.top..range.bg_dim.1 - margin.bottom,
            "bottom-left" | "bottom" | "bottom-right" => range.bg_dim.1 - margin.bottom..range.bg_dim.1,
            _ => return None,
        };
        Some(RectPosition {
            bg_dim: range.bg_dim,
            x_range,
            y_range,
        })
    }

    fn get_range_by_path(&self, path: &str) -> Option<RectPosition> {
        // eprintln!("get_range_by_path: {path:?}, self: {self:?}");
        if path.is_empty() {
            let (w, h) = self.get_dim();
            return Some(RectPosition {
                bg_dim: (w, h),
                x_range: 0..w,
                y_range: 0..h,
            });
        }

        let mut path = path.splitn(2, '.');
        let id = path.next()?;
        let rem = path.next().unwrap_or("");
        match self {
            LayoutElem::Rect { .. } => None,
            LayoutElem::Horizontal(inner) | LayoutElem::Vertical(inner) => {
                let index = id.parse::<usize>().ok().filter(|&x| x < inner.len())?;
                let range = inner[index].get_range_by_path(rem)?;

                if matches!(self, LayoutElem::Horizontal(_)) {
                    let before = inner[..index].iter().map(|node| node.get_dim().0).sum::<u32>();
                    let after = inner[index + 1..].iter().map(|node| node.get_dim().0).sum::<u32>();
                    Some(range.add_margin(&LayoutMargin::new(before, after, 0, 0)))
                } else {
                    let before = inner[..index].iter().map(|node| node.get_dim().1).sum::<u32>();
                    let after = inner[index + 1..].iter().map(|node| node.get_dim().1).sum::<u32>();
                    Some(range.add_margin(&LayoutMargin::new(0, 0, before, after)))
                }
            }
            LayoutElem::Margined { margin, center } => {
                let range = center.get_range_by_path(rem)?.add_margin(margin);
                Self::locate_margin(&range, margin, id)
            }
        }
    }

    fn get_range_by_id(&self, id: &str) -> Option<RectPosition> {
        // eprintln!("get_range_by_id: {id:?}, self: {self:?}");
        match self {
            LayoutElem::Rect { id: rect_id, .. } => {
                if rect_id.as_deref() == Some(id) {
                    let (w, h) = self.get_dim();
                    Some(RectPosition {
                        bg_dim: (w, h),
                        x_range: 0..w,
                        y_range: 0..h,
                    })
                } else {
                    None
                }
            }
            LayoutElem::Horizontal(inner) | LayoutElem::Vertical(inner) => {
                let ranges = inner.iter().map(|node| node.get_range_by_id(id)).collect::<Vec<_>>();
                let index = ranges.iter().position(|x| x.is_some())?;

                if matches!(self, LayoutElem::Horizontal(_)) {
                    let before = inner[..index].iter().map(|node| node.get_dim().0).sum::<u32>();
                    let after = inner[index + 1..].iter().map(|node| node.get_dim().0).sum::<u32>();
                    let range = ranges[index].as_ref().unwrap().add_margin(&LayoutMargin::new(before, after, 0, 0));
                    Some(range)
                } else {
                    let before = inner[..index].iter().map(|node| node.get_dim().1).sum::<u32>();
                    let after = inner[index + 1..].iter().map(|node| node.get_dim().1).sum::<u32>();
                    let range = ranges[index].as_ref().unwrap().add_margin(&LayoutMargin::new(0, 0, before, after));
                    Some(range)
                }
            }
            LayoutElem::Margined { margin, center } => {
                let range = center.get_range_by_id(id)?.add_margin(margin);
                Self::locate_margin(&range, margin, id)
            }
        }
    }

    pub fn get_range(&self, path: &str) -> Option<RectPosition> {
        if let Some(path) = path.strip_prefix('.') {
            self.get_range_by_path(path)
        } else {
            self.get_range_by_id(path)
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

    pub fn get_margin(&self) -> Option<LayoutMargin> {
        match self {
            LayoutElem::Margined { margin, .. } => Some(*margin),
            _ => None,
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

    pub fn get_id(&self) -> Option<&str> {
        match self {
            LayoutElem::Rect { id, .. } => id.as_deref(),
            _ => None,
        }
    }

    fn get_node_by_path_mut(&mut self, path: &str) -> Option<&mut LayoutElem> {
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
                if id == "center" {
                    center.get_node_by_path_mut(rem)
                } else {
                    None
                }
            }
        }
    }

    fn get_node_by_id_mut(&mut self, id: &str) -> Option<&mut LayoutElem> {
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

    pub fn get_area(&self, key: &str) -> Option<&DrawingArea<BitMapBackend<'a>, Shift>> {
        if let Some(&i) = self.index.get(key) {
            Some(&self.areas[i])
        } else {
            None
        }
    }

    pub fn present(&self) -> Result<()> {
        self.areas[0].present()?;
        Ok(())
    }
}
