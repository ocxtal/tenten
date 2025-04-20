use plotters::prelude::{RGBColor, RGBAColor};

#[derive(Copy, Clone, Debug)]
pub struct DensityColorMap {
    pub palette: [RGBColor; 2],
    pub max_density: f64,
    pub min_density: f64,
}

impl DensityColorMap {
    fn to_picker(self, base_per_pixel: f64) -> ColorPicker {
        let expansion = (1000.0 / base_per_pixel).powf(2.0);
        let max_count = self.max_density * expansion;
        let min_count = self.min_density * expansion;
        ColorPicker {
            palette: self.palette,
            expansion,
            offset: min_count.log2(),
            scale: 1.0 / (max_count.log2() - min_count.log2()),
        }
    }
}

#[derive(Clone, Debug)]
struct ColorPicker {
    palette: [RGBColor; 2],
    expansion: f64,
    offset: f64,
    scale: f64,
}

impl ColorPicker {
    fn get_color(&self, palette_index: usize, count: u32) -> RGBAColor {
        let intensity = self.scale * ((self.expansion * count as f64).log2() - self.offset);
        self.palette[palette_index].mix(intensity.clamp(0.0, 1.0))
    }
}
