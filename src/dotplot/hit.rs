use crate::dotplot::layout::{Layout, build_plot_layout};
use crate::dotplot::plot::{DotPlot, StackedSequenceRange};

#[derive(Clone, Debug)]
pub struct SequencePosition {
    pub sequence: String,
    pub pos: isize,
}

#[derive(Clone, Debug)]
pub struct DotPlotHit {
    pub target: Option<SequencePosition>,
    pub query: Option<SequencePosition>,
}

pub struct PlotHitMap {
    layout: Layout,
    dotplot_hit: DotPlotHitContext,
}

impl PlotHitMap {
    pub fn new(dotplot: &DotPlot, hide_scale: bool) -> PlotHitMap {
        PlotHitMap {
            layout: build_plot_layout(dotplot, hide_scale),
            dotplot_hit: dotplot.hit_context(),
        }
    }

    pub fn hit_test(&self, x: u32, y: u32) -> Option<DotPlotHit> {
        let hit = self.layout.hit_test(x, y)?;
        if hit.id.as_deref() == Some("dotplot") {
            self.dotplot_hit.hit_test(hit.local_pos)
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct DotPlotHitContext {
    x_seqs: StackedSequenceRange,
    y_seqs: StackedSequenceRange,
    base_per_pixel: usize,
    y_label_area_size: u32,
}

impl DotPlotHitContext {
    pub(crate) fn new(
        x_seqs: StackedSequenceRange,
        y_seqs: StackedSequenceRange,
        base_per_pixel: usize,
        y_label_area_size: u32,
    ) -> DotPlotHitContext {
        DotPlotHitContext {
            x_seqs,
            y_seqs,
            base_per_pixel,
            y_label_area_size,
        }
    }

    pub(crate) fn hit_test(&self, local_pos: (u32, u32)) -> Option<DotPlotHit> {
        let target = local_pos
            .0
            .checked_sub(self.y_label_area_size)
            .and_then(|x| self.x_seqs.hit(x, self.base_per_pixel));

        let query = if local_pos.1 < self.y_seqs.pixels() {
            let y = self.y_seqs.pixels() - local_pos.1 - 1;
            self.y_seqs.hit(y, self.base_per_pixel)
        } else {
            None
        };

        if target.is_none() && query.is_none() {
            None
        } else {
            Some(DotPlotHit { target, query })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dotplot::plot::StackedSequenceRange;
    use crate::dotplot::sequence::SequenceRange;

    fn seq(name: &str, len: usize) -> SequenceRange {
        SequenceRange {
            name: name.to_string(),
            range: 0..len,
            annotation: None,
            virtual_name: None,
            virtual_start: None,
        }
    }

    #[test]
    fn hit_test_returns_axis_independently() {
        let ctx = DotPlotHitContext::new(
            StackedSequenceRange::new(&[seq("target", 10)], 5, 1),
            StackedSequenceRange::new(&[seq("query", 10)], 5, 1),
            5,
            7,
        );

        let hit = ctx.hit_test((8, 1)).unwrap();
        assert_eq!(hit.target.unwrap().pos, 5);
        assert_eq!(hit.query.unwrap().pos, 5);

        let hit = ctx.hit_test((8, 3)).unwrap();
        assert_eq!(hit.target.unwrap().pos, 5);
        assert!(hit.query.is_none());

        let hit = ctx.hit_test((0, 1)).unwrap();
        assert!(hit.target.is_none());
        assert_eq!(hit.query.unwrap().pos, 5);

        assert!(ctx.hit_test((0, 3)).is_none());
    }
}
