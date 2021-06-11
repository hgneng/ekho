use std::{
    default::Default,
    ops::{Add, AddAssign, Div, Mul, Sub},
};

pub fn nsdf<T>(x: &[T]) -> Vec<T>
where
    T: Mul<Output = T>
        + Add<Output = T>
        + Sub<Output = T>
        + Div<Output = T>
        + AddAssign
        + Copy
        + Default,
{
    let len = x.len();

    let mut r: Vec<T> = Vec::new();

    let window_size = len;

    // lag from total overlap to no overlap
    for tau in 0..len {
        let mut numerator: T = T::default();
        let mut denominator: T = T::default();

        for j in 0..window_size - tau {
            numerator += x[j] * x[j + tau];
            denominator += x[j] * x[j] + x[j + tau] * x[j + tau];
        }

        numerator += numerator;

        r.push(numerator / denominator);
    }

    r
}

#[cfg(test)]
mod tests {
    use super::*;
    use open_e::get_open_e;
    use peak_picking::*;
    use sinewave::generate_sinewave;

    #[test]
    fn test_nsdf_periodicity_naive_sinewave() {
        let x = generate_sinewave(83.0, 4096, Some(48000));

        let detected_frequency = 48000 as f64 / periodicity(&nsdf(&x), None);
        println!("{:#?}", detected_frequency);
    }

    #[test]
    fn test_nsdf_periodicity_open_e() {
        let x = get_open_e();

        let detected_frequency = 44100 as f64 / periodicity(&nsdf(&x), None);
        println!("{:#?}", detected_frequency);
    }
}
