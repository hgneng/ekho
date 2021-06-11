use std::{
    cmp::PartialOrd,
    convert,
    default::Default,
    ops::{Add, Div, Mul, Sub},
};

use conv::{ApproxFrom, DefaultApprox};

const CUTOFF: f64 = 0.93;
const SMALL_CUTOFF: f64 = 0.5;

pub enum PeakPickingStrategy {
    Naive,
    Better,
    Best,
}

pub fn periodicity<T>(x: &[T], strategy: Option<PeakPickingStrategy>) -> f64
where
    T: PartialOrd
        + Copy
        + Default
        + Sub<Output = T>
        + Add<Output = T>
        + Mul<Output = T>
        + Div<Output = T>
        + convert::From<i32>
        + convert::From<f32>
        + convert::From<f64>,
    f64: convert::From<T>,
{
    let peak_lags = match strategy {
        Some(strat) => match strat {
            PeakPickingStrategy::Naive => peak_picking_naive(x),
            PeakPickingStrategy::Better => peak_picking_better(x),
            PeakPickingStrategy::Best => peak_picking_best(x),
        },
        None => peak_picking_naive(x),
    };

    *peak_lags.last().unwrap() as f64 / peak_lags.len() as f64
}

pub fn peak_picking_best<T>(nsdf: &[T]) -> Vec<usize>
where
    T: PartialOrd
        + Copy
        + Default
        + Sub<Output = T>
        + Add<Output = T>
        + Mul<Output = T>
        + Div<Output = T>
        + convert::From<i32>
        + convert::From<f32>
        + convert::From<f64>,
    f64: convert::From<T>,
{
    let mut estimates: Vec<(usize, T)> = Vec::new();

    let max_positions = peak_picking_better(nsdf);

    //let mut highest_amplitude = T::min_value();
    let mut highest_amplitude = T::default();

    for i in max_positions {
        highest_amplitude = if highest_amplitude >= nsdf[i] {
            highest_amplitude
        } else {
            nsdf[i]
        };
        if nsdf[i] > T::from(SMALL_CUTOFF) {
            let x = parabolic_interpolation(nsdf, i);
            estimates.push(x);
            highest_amplitude = if highest_amplitude >= x.1 {
                highest_amplitude
            } else {
                x.1
            };
        }
    }

    let actual_cutoff = T::from(CUTOFF) * highest_amplitude;

    for i in estimates {
        if i.1 >= actual_cutoff {
            return vec![i.0];
        }
    }

    vec![]
}

fn parabolic_interpolation<T>(array: &[T], x: usize) -> (usize, T)
where
    T: PartialOrd
        + Copy
        + Default
        + Sub<Output = T>
        + Add<Output = T>
        + Mul<Output = T>
        + Div<Output = T>
        + convert::From<i32>
        + convert::From<f32>
        + convert::From<f64>,
    f64: convert::From<T>,
{
    let x_adjusted: usize;

    if x < 1 {
        x_adjusted = if array[x] <= array[x + 1] { x } else { x + 1 };
    } else if x > array.len() - 1 {
        x_adjusted = if array[x] <= array[x - 1] { x } else { x - 1 };
    } else {
        let den = array[x + 1] + array[x - 1] - T::from(2) * array[x];
        let delta = array[x - 1] - array[x + 1];
        return if den == T::default() {
            (x, array[x])
        } else {
            let tmp = T::from(x as i32) + delta / (T::from(2) * den);
            let tmp_usize: usize = if tmp < T::default() {
                0usize
            } else {
                <usize as ApproxFrom<_, DefaultApprox>>::approx_from(f64::from(tmp))
                    .unwrap_or(0usize)
            };
            (tmp_usize, array[x] - delta * delta / (T::from(8) * den))
        };
    }
    (x_adjusted, array[x_adjusted])
}

pub fn peak_picking_better<T>(nsdf: &[T]) -> Vec<usize>
where
    T: PartialOrd + Default,
{
    let mut max_positions: Vec<usize> = Vec::new();

    let mut pos = 0;
    let mut cur_max_pos = 0;
    let size = nsdf.len();

    while pos < (size - 1) / 3 && nsdf[pos] > T::default() {
        pos += 1;
    }
    while pos < size - 1 && nsdf[pos] <= T::default() {
        pos += 1;
    }

    if pos == 0 {
        pos = 1;
    }

    while pos < size - 1 {
        if nsdf[pos] > nsdf[pos - 1]
            && nsdf[pos] >= nsdf[pos + 1]
            && (cur_max_pos == 0 || nsdf[pos] > nsdf[cur_max_pos])
        {
            cur_max_pos = pos;
        }
        pos += 1;
        if pos < size - 1 && nsdf[pos] <= T::default() {
            if cur_max_pos > 0 {
                max_positions.push(cur_max_pos);
                cur_max_pos = 0;
            }
            while pos < size - 1 && nsdf[pos] <= T::default() {
                pos += 1;
            }
        }
    }

    if cur_max_pos > 0 {
        max_positions.push(cur_max_pos);
    }

    max_positions
}

pub fn peak_picking_naive<T>(x: &[T]) -> Vec<usize>
where
    T: PartialOrd,
{
    let mut ret: Vec<usize> = Vec::new();

    for i in 1..x.len() - 1 {
        if (x[i] > x[i - 1]) && (x[i] > x[i + 1]) {
            ret.push(i);
        }
    }

    ret
}

#[cfg(test)]
mod tests {
    use super::*;
    use autocorrelation::*;
    use nsdf::*;
    use open_e::get_open_e;
    use sinewave::generate_sinewave;

    #[test]
    fn test_peak_picking_sinewave_autocorrelation_naive() {
        let x = generate_sinewave(83.0, 4096, Some(48000));

        let detected_frequency = 48000 as f64 / periodicity(&autocorrelation2(&x, None), None);
        println!("{:#?}", detected_frequency);
    }

    #[test]
    fn test_peak_picking_sinewave_autocorrelation_better() {
        let x = generate_sinewave(83.0, 4096, Some(48000));

        let detected_frequency = 48000 as f64
            / periodicity(
                &autocorrelation2(&x, None),
                Some(PeakPickingStrategy::Better),
            );
        println!("{:#?}", detected_frequency);
    }

    #[test]
    fn test_peak_picking_open_e_autocorrelation_naive() {
        let x = get_open_e();

        let detected_frequency = 44100 as f64 / periodicity(&autocorrelation2(&x, None), None);
        println!("{:#?}", detected_frequency);
    }

    #[test]
    fn test_peak_picking_open_e_autocorrelation_better() {
        let x = get_open_e();

        let detected_frequency = 44100 as f64
            / periodicity(
                &autocorrelation2(&x, None),
                Some(PeakPickingStrategy::Better),
            );
        println!("{:#?}", detected_frequency);
    }

    #[test]
    fn test_peak_picking_open_e_autocorrelation_best() {
        let x = get_open_e();

        let detected_frequency = 44100 as f64
            / periodicity(&autocorrelation2(&x, None), Some(PeakPickingStrategy::Best));
        println!("{:#?}", detected_frequency);
    }

    #[test]
    fn test_peak_picking_open_e_nsdf_best() {
        let x = get_open_e();

        let detected_frequency =
            44100 as f64 / periodicity(&nsdf(&x), Some(PeakPickingStrategy::Best));
        println!("{:#?}", detected_frequency);
    }
}
