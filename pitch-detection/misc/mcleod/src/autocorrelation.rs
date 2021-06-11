use std::{
    default::Default,
    ops::{Add, AddAssign, Mul},
};

pub fn autocorrelation1<T>(x: &[T], w: Option<usize>) -> Vec<T>
where
    T: Mul<Output = T> + Add<Output = T> + AddAssign + Copy + Default,
{
    let len = x.len();

    let mut r: Vec<T> = Vec::new();

    let window_size = w.unwrap_or(len);

    // lag from total overlap to no overlap
    for tau in 0..len {
        let mut entry: T = T::default();

        for j in 0..window_size {
            if j + tau >= len {
                break;
            };

            entry += x[j] * x[j + tau];
        }

        r.push(entry);
    }

    r
}

pub fn autocorrelation2<T>(x: &[T], w: Option<usize>) -> Vec<T>
where
    T: Mul<Output = T> + Add<Output = T> + AddAssign + Copy + Default,
{
    let len = x.len();

    let mut r: Vec<T> = Vec::new();

    let window_size = w.unwrap_or(len);

    // lag from total overlap to no overlap
    for tau in 0..len {
        let mut entry: T = T::default();

        if window_size < tau {
            break;
        }

        for j in 0..window_size - tau {
            entry += x[j] * x[j + tau];
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
    use rand::{thread_rng, Rng};
    use sinewave::generate_sinewave;
    use test::Bencher;

    fn gen_fake_vec(l: usize) -> Vec<f64> {
        let mut rng = thread_rng();
        let mut v: Vec<f64> = Vec::new();

        for _ in 0..l {
            v.push(rng.gen());
        }

        v
    }

    #[test]
    fn test_autocorrelation_equivalent_for_zero_pad() {
        let x = vec![1.3, 1.4, 0.0];
        let window = Some(2);

        assert_eq!(autocorrelation1(&x, window), autocorrelation2(&x, window));
    }

    #[test]
    fn test_autocorrelation_not_equivalent_non_zero_pad() {
        let x = vec![1.3, 1.4, 5.5];
        let window = Some(2);

        assert_ne!(autocorrelation1(&x, window), autocorrelation2(&x, window));
    }

    #[test]
    fn test_autocorrelation_equivalent_for_max_window_size() {
        let x = vec![1.3, 1.4, 5.5, -34.2, -43.9, 438.4, 40.5];

        assert_eq!(autocorrelation1(&x, None), autocorrelation2(&x, None));
    }

    #[test]
    fn test_autocorrelation_sinewave() {
        let x = generate_sinewave(83.0, 4096, Some(48000));

        assert_eq!(autocorrelation1(&x, None), autocorrelation2(&x, None));
    }

    #[test]
    fn test_autocorrelation1_periodicity_sinewave() {
        let x = generate_sinewave(83.0, 4096, Some(48000));

        let detected_frequency = 48000 as f64 / periodicity(&autocorrelation1(&x, None), None);
        println!("{:#?}", detected_frequency);
    }

    #[test]
    fn test_autocorrelation2_periodicity_sinewave() {
        let x = generate_sinewave(83.0, 4096, Some(48000));

        let detected_frequency = 48000 as f64 / periodicity(&autocorrelation2(&x, None), None);
        println!("{:#?}", detected_frequency);
    }

    #[test]
    fn test_autocorrelation1_periodicity_open_e() {
        let x = get_open_e();

        let detected_frequency = 44100 as f64 / periodicity(&autocorrelation2(&x, None), None);
        println!("{:#?}", detected_frequency);
    }

    #[test]
    fn test_autocorrelation2_periodicity_open_e() {
        let x = get_open_e();

        let detected_frequency = 44100 as f64 / periodicity(&autocorrelation2(&x, None), None);
        println!("{:#?}", detected_frequency);
    }

    #[bench]
    #[ignore]
    fn bench_type1(b: &mut Bencher) {
        let fake = gen_fake_vec(32768);

        assert_eq!(autocorrelation1(&fake, None), autocorrelation2(&fake, None));
        b.iter(|| autocorrelation1(&fake, None));
    }

    #[bench]
    #[ignore]
    fn bench_type2(b: &mut Bencher) {
        let fake = gen_fake_vec(32768);

        assert_eq!(autocorrelation1(&fake, None), autocorrelation2(&fake, None));
        b.iter(|| autocorrelation2(&fake, None));
    }
}
