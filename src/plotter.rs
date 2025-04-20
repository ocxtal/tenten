// @file plotter.rs
// @author Hajime Suzuki
// @brief dotplot plotter

use anyhow::Result;
use tenten::{AxisAppearance, DotPlotAppearance, DensityColorMap, DotPlot, ColorScale, LengthScale, Layout, LayoutElem};
use plotters::prelude::*;
use std::ops::Range;
