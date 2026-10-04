use std::ops::Range;

#[derive(Clone, Debug)]
pub(super) struct LineStripe {
    pub diagonals: Vec<i32>,
    pub index: usize,
    pub last: Option<(bool, usize)>,
    pub changed_strand: bool,
    pub bandwidth: usize,
}

impl LineStripe {
    pub fn new(bandwidth: usize) -> Self {
        assert!(bandwidth > 0 && bandwidth <= i32::MAX as usize);
        Self {
            diagonals: Vec::new(),
            index: 0,
            last: None,
            changed_strand: false,
            bandwidth,
        }
    }

    pub fn interval(diagonal: i32, target_len: usize, query_len: usize) -> Range<usize> {
        let d = diagonal as i64;
        0.max(-d) as usize..(target_len as i64).min(query_len as i64 - d) as usize
    }

    pub fn flush(&mut self, target_len: usize, query_len: usize, mut emit: impl FnMut(i32, f64)) {
        self.diagonals.sort_unstable();
        // Normalize each seed by its diagonal's target-axis length inside the selected stripe.
        let weight = |d| 1.0 / Self::interval(d, target_len, query_len).len() as f64;
        let (mut left, mut right) = (0, 0);
        let mut sum = 0.0;
        let mut peak = None;

        // Sweep bandwidth-wide windows; represent each maximum plateau
        // by the median diagonal in its support.
        while left < self.diagonals.len() {
            let entry = self.diagonals.get(right).map_or(i64::MAX, |&d| d as i64);
            let exit = self.diagonals[left] as i64 + self.bandwidth as i64;
            let event = entry.min(exit);
            let mut delta = 0.0;
            let old_left = left;
            while left < right && self.diagonals[left] as i64 + self.bandwidth as i64 == event {
                left += 1;
            }
            if left > old_left {
                delta -= (left - old_left) as f64 * weight(self.diagonals[old_left]);
            }
            if left == right {
                if let Some((d, density)) = peak.take() {
                    emit(d, density);
                }
                sum = 0.0;
                delta = 0.0;
            }
            let old_right = right;
            while right < self.diagonals.len() && self.diagonals[right] as i64 == event {
                right += 1;
            }
            if right > old_right {
                delta += (right - old_right) as f64 * weight(self.diagonals[old_right]);
            }
            sum += delta;
            if delta > 0.0 {
                peak = Some((self.diagonals[(left + right - 1) / 2], sum));
            } else if delta < 0.0
                && let Some((d, density)) = peak.take()
            {
                emit(d, density);
            }
        }
        self.diagonals.clear();
    }
}
