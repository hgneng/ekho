// http://siciarz.net/24-days-rust-hound/

use std::f64::consts::PI;

pub fn generate_sinewave(frequency: f64, size: u32, sample_rate: Option<u32>) -> Vec<f64> {
    let _sample_rate = sample_rate.unwrap_or(44100);
    let signal_amplitude = 16384f64;
    let mut ret: Vec<f64> = Vec::new();
    for n in 0..size {
        let t: f64 = f64::from(n) / f64::from(_sample_rate);
        ret.push(signal_amplitude * (t * frequency * 2.0 * PI).sin());
    }
    ret
}

#[cfg(test)]

mod test {
    use super::*;
    use autocorrelation::*;
    use nsdf::*;
    use peak_picking::*;
    use sdf::*;

    #[test]
    fn plot_sinewave_autocorrelation() {
        let sine = generate_sinewave(83.0f64, 4096, None);
        let autocorrelation = autocorrelation2(&sine, None);

        for i in 0..autocorrelation.len() {
            println!("{:?}", autocorrelation[i]);
        }
    }

    #[test]
    fn plot_sinewave_sdf() {
        let sine = generate_sinewave(83.0f64, 4096, None);
        let autocorrelation = sdf(&sine);

        for i in 0..autocorrelation.len() {
            println!("{:?}", autocorrelation[i]);
        }
    }

    #[test]
    fn plot_sinewave_nsdf() {
        let sine = generate_sinewave(83.0f64, 4096, None);
        let autocorrelation = nsdf(&sine);

        for i in 0..autocorrelation.len() {
            println!("{:?}", autocorrelation[i]);
        }
    }

    #[test]
    fn plot_sinewave_with_lag_autocorrelation() {
        let mut sine = generate_sinewave(83.0f64, 4096, None);
        let autocorrelation = autocorrelation2(&sine, None);
        let peak_lags = peak_picking_naive(&autocorrelation);

        eprintln!("{:#?}", peak_lags);

        let max_lag = *peak_lags.last().unwrap();
        let max_size = 4096 + max_lag as usize;

        let mut to_plot: Vec<Vec<f64>> = Vec::new();
        sine.extend(&vec![0f64; max_size - 4096]);

        to_plot.push(sine.clone());

        for lag in peak_lags {
            let mut vec_for_lag: Vec<f64> = vec![0f64; max_size];

            for i in lag..(lag + 4096) {
                vec_for_lag[i] = sine[i - lag];
            }

            to_plot.push(vec_for_lag);
        }

        for i in 0..max_size {
            let mut vals: Vec<f64> = Vec::new();
            for vec in to_plot.iter() {
                vals.push(vec[i]);
            }

            println!(
                "{}",
                vals.iter()
                    .map(|x| format!("{:.2}", x))
                    .collect::<Vec<String>>()
                    .join("\t")
            );
        }
    }

    #[test]
    fn plot_sinewave_with_lag_nsdf() {
        let mut sine = generate_sinewave(83.0f64, 4096, None);
        let autocorrelation = nsdf(&sine);
        let peak_lags = peak_picking_naive(&autocorrelation);

        eprintln!("{:#?}", peak_lags);

        let max_lag = *peak_lags.last().unwrap();
        let max_size = 4096 + max_lag as usize;

        let mut to_plot: Vec<Vec<f64>> = Vec::new();
        sine.extend(&vec![0f64; max_size - 4096]);

        to_plot.push(sine.clone());

        for lag in peak_lags {
            let mut vec_for_lag: Vec<f64> = vec![0f64; max_size];

            for i in lag..(lag + 4096) {
                vec_for_lag[i] = sine[i - lag];
            }

            to_plot.push(vec_for_lag);
        }

        for i in 0..max_size {
            let mut vals: Vec<f64> = Vec::new();
            for vec in to_plot.iter() {
                vals.push(vec[i]);
            }

            println!(
                "{}",
                vals.iter()
                    .map(|x| format!("{:.2}", x))
                    .collect::<Vec<String>>()
                    .join("\t")
            );
        }
    }
}
