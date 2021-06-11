use std::{
    default::Default,
    ops::{Add, AddAssign, Mul, Sub},
};

pub fn sdf<T>(x: &[T]) -> Vec<T>
where
    T: Mul<Output = T> + Add<Output = T> + Sub<Output = T> + AddAssign + Copy + Default,
{
    let len = x.len();

    let mut r: Vec<T> = Vec::new();

    let window_size = len;

    // lag from total overlap to no overlap
    for tau in 0..len {
        let mut entry: T = T::default();

        for j in 0..window_size - tau {
            entry += (x[j] - x[j + tau]) * (x[j] - x[j + tau]);
        }

        r.push(entry);
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
    fn test_sdf_periodicity_sinewave() {
        let x = generate_sinewave(83.0, 4096, Some(48000));

        let detected_frequency = 48000 as f64 / periodicity(&sdf(&x), None);
        println!("{:#?}", detected_frequency);
    }

    #[test]
    fn test_sdf_periodicity_open_e() {
        let x = get_open_e();

        let detected_frequency = 44100 as f64 / periodicity(&sdf(&x), None);
        println!("{:#?}", detected_frequency);
    }
}
